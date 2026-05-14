#include "minias.h"

/* Cache of Parsev* by value. */
static const Parsev *
internparsev(Parsev *p)
{
    /*
     A simple direct mapped cache that prevents our parser
     from allocating duplicate values. Note that it uses memcmp
     for equality, even on pointer values, this works because the
     pointers themselves are also interned.

     This simplicity comes with one big cost - Parsev variants with padding
     can trigger a false positive on valgrind. It should still be safe,
     but the best fix is still to avoid the padding bytes in the Parsev
     variants.
  */
    size_t idx;
    const Parsev *interned;
    static const Parsev *cache[4096] = { 0 };

    idx = murmurhash64a((char *)p, sizeof(Parsev)) % sizeof(cache)
        / sizeof(cache[0]);
    interned = cache[idx];
    if (interned && memcmp(p, interned, sizeof(Parsev)) == 0)
        return interned;
    interned = (const Parsev *)xmemdup((char *)p, sizeof(Parsev));
    cache[idx] = interned;
    return interned;
}

/* Cache of char* by value. */
const char *
internstring(const char *s)
{
    size_t idx, len;
    const char *interned;
    static const char *cache[4096] = { 0 };

    len = strlen(s);
    idx = murmurhash64a(s, len) % sizeof(cache) / sizeof(cache[0]);
    interned = cache[idx];
    if (interned && strcmp(s, cache[idx]) == 0)
        return interned;
    interned = xstrdup(s);
    cache[idx] = interned;
    return interned;
}

static int local_label_counters[10];

static const char *
local_label_define(const char *num)
{
    char buf[64];
    int n, idx;

    n = num[0] - '0';
    if (n < 0 || n >= (int)(sizeof(local_label_counters) / sizeof(local_label_counters[0])))
        unreachable();
    idx = ++local_label_counters[n];
    snprintf(buf, sizeof(buf), ".Llocal.%d.%d", n, idx);
    return internstring(buf);
}

static const char *
local_label_reference(const char *num, const char *dir)
{
    char buf[64];
    int n, idx;

    n = num[0] - '0';
    if (n < 0 || n >= (int)(sizeof(local_label_counters) / sizeof(local_label_counters[0])))
        unreachable();
    idx = local_label_counters[n] + (*dir == 'f');
    snprintf(buf, sizeof(buf), ".Llocal.%d.%d", n, idx);
    return internstring(buf);
}

static int64_t
parse_const_expr(const char *s)
{
    char *end;
    int sign;
    int64_t total;

    total = 0;
    sign = 1;
    while (*s) {
        while (isspace((unsigned char)*s))
            s++;
        if (*s == '+') {
            sign = 1;
            s++;
            continue;
        }
        if (*s == '-') {
            sign = -1;
            s++;
            continue;
        }
        errno = 0;
        total += sign * (int64_t)strtoll(s, &end, 0);
        if (s == end || errno)
            fatal("invalid constant expression");
        s = end;
        sign = 1;
    }
    return total;
}

static const char *
label_define(const char *name)
{
    if (name[0] >= '0' && name[0] <= '9' && name[1] == '\0')
        return local_label_define(name);
    return name;
}

static String
decodestring(const char *s)
{
    char *end;
    size_t len = 0;
    size_t cap = 0;
    uint8_t *data = NULL;
    uint8_t c = 0;
    int i;

    /* The string is already validated by the parser so we omit some checks */
    while (*s) {
        if (*s == '\\') {
            s++;
            if (*s == 'x') {
                s++;
                c = strtoul(s, &end, 16);
                s = end - 1;
            } else if (*s == 'a') {
                c = '\a';
            } else if (*s == 'b') {
                c = '\b';
            } else if (*s == 'f') {
                c = '\f';
            } else if (*s == 'r') {
                c = '\r';
            } else if (*s == 'n') {
                c = '\n';
            } else if (*s == 't') {
                c = '\t';
            } else if (*s == 'v') {
                c = '\v';
            } else if (*s == '"') {
                c = '"';
            } else if (*s == '\\') {
                c = '\\';
            } else {
                for (i = 0; i < 3 && *s >= '0' && *s <= '7'; i++, s++)
                    c = c * 8 + (*s - '0');
                if (i == 0)
                    unreachable();
                s--;
            }
        } else {
            c = *s;
        }
        s++;
        if (len == cap) {
            cap = cap ? len * 2 : 8;
            data = realloc(data, cap);
        }
        data[len++] = c;
    }
    return (String) { .kind = ASM_STRING, .len = len, .data = data };
}

static int
needsmovabs(Imm *imm)
{
    int64_t mask, maskedc;

    if (imm->v.l)
        return 1;

    mask = 0xffffffff80000000;
    maskedc = (uint64_t)imm->v.c & mask;
    return (maskedc != mask && maskedc != 0);
}

#define OP(OPCODE)                                                             \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_OP, .prefix = -1,            \
            .opcode = OPCODE,                                                  \
        }                                                                      \
    }

#define INVALID_STRING_OPERANDS                                                \
    (Parsev)                                                                   \
    {                                                                          \
        .kind = ASM_INVALID_STRING_OPERANDS,                                   \
    }

static int32_t
stringopcode(int64_t segment, int32_t opcode)
{
    int n;
    uint32_t mask;

    if (segment < 0)
        return opcode;

    n = (int8_t)(uint8_t)((opcode & 0xff000000) >> 24);
    if (n >= 2)
        unreachable();

    mask = (1U << ((n + 1) * 8)) - 1;
    return ((n + 1) << 24) | ((uint8_t)segment << ((n + 1) * 8))
        | (opcode & mask);
}

#define OPP(PREFIX, OPCODE)                                                    \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_OP, .prefix = PREFIX,        \
            .opcode = OPCODE,                                                  \
        }                                                                      \
    }

#define OPREG(REX, PREFIX, OPCODE, REG, A1)                                    \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_OPREG, .prefix = PREFIX,     \
            .rex = (Rex)REX, .fixedreg = REG, .opcode = OPCODE,                \
            .arg1 = internparsev(&A1)                                          \
        }                                                                      \
    }

#define OPMEM(REX, PREFIX, OPCODE, REG, A1)                                    \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_OPMEM, .prefix = PREFIX,     \
            .rex = (Rex)REX, .fixedreg = REG, .opcode = OPCODE,                \
            .arg1 = internparsev(&A1)                                          \
        }                                                                      \
    }

#define R(REX, PREFIX, OPCODE, A1)                                             \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_R, .prefix = PREFIX,         \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
        }                                                                      \
    }

#define IMM(REX, PREFIX, OPCODE, A1, A2)                                       \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_IMM, .prefix = PREFIX,       \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2)                                          \
        }                                                                      \
    }

#define RIMM(REX, PREFIX, OPCODE, A1, A2)                                      \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_RIMM, .prefix = PREFIX,      \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2)                                          \
        }                                                                      \
    }

#define RELCALL(REX, PREFIX, OPCODE, A1)                                       \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_RELCALL, .prefix = PREFIX,   \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
        }                                                                      \
    }

#define IMMREG(REX, PREFIX, OPCODE, IMMREG, A1, A2)                            \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMREG, .prefix = PREFIX,    \
            .opcode = OPCODE, .rex = (Rex)REX, .fixedreg = IMMREG,             \
            .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)               \
        }                                                                      \
    }

#define IMMMEM(REX, PREFIX, OPCODE, IMMREG, A1, A2)                            \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMMEM, .prefix = PREFIX,    \
            .opcode = OPCODE, .rex = (Rex)REX, .fixedreg = IMMREG,             \
            .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)               \
        }                                                                      \
    }

#define REGMEM(REX, PREFIX, OPCODE, A1, A2)                                    \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_REGMEM, .prefix = PREFIX,    \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2)                                          \
        }                                                                      \
    }

#define MEMREG(REX, PREFIX, OPCODE, A1, A2)                                    \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_MEMREG, .prefix = PREFIX,    \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2)                                          \
        }                                                                      \
    }

#define REGREG(REX, PREFIX, OPCODE, A1, A2)                                    \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_REGREG, .prefix = PREFIX,    \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2)                                          \
        }                                                                      \
    }

#define REGREG2(REX, PREFIX, OPCODE, A1, A2)                                   \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_REGREG2, .prefix = PREFIX,   \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2)                                          \
        }                                                                      \
    }

#define IMMREGREG2(REX, PREFIX, OPCODE, A1, A2, A3)                            \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMREGREG2,                  \
            .prefix = PREFIX, .opcode = OPCODE, .rex = (Rex)REX,               \
            .arg1 = internparsev(&A1), .arg2 = internparsev(&A2),              \
            .arg3 = internparsev(&A3)                                          \
        }                                                                      \
    }

#define IMMMEMREG(REX, PREFIX, OPCODE, A1, A2, A3)                             \
    (Parsev)                                                                   \
    {                                                                          \
        .instr = (Instr)                                                       \
        {                                                                      \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMMEMREG, .prefix = PREFIX, \
            .opcode = OPCODE, .rex = (Rex)REX, .arg1 = internparsev(&A1),      \
            .arg2 = internparsev(&A2), .arg3 = internparsev(&A3)               \
        }                                                                      \
    }

#define REG(K)                                                                 \
    (Parsev) { .kind = ASM_##K }

#include "asm_parser.c"

static void
appendasmline(AsmLine **result, AsmLine **prevl, const Parsev *v,
    int64_t lineno)
{
    AsmLine *l;

    l = zalloc(sizeof(AsmLine));
    l->lineno = lineno;
    l->v = v;
    if (*prevl)
        (*prevl)->next = l;
    else
        *result = l;
    *prevl = l;
}

AsmLine *
parseasm(void)
{
    AsmLine *result, *prevl;
    asm_context_t *ctx;
    Parsev v;
    int64_t lineno;

    ctx = asm_create(NULL);
    result = NULL;
    prevl = NULL;
    lineno = 0;

    while (asm_parse(ctx, &v)) {
        lineno++;
        if (v.kind == ASM_STMT_PAIR) {
            appendasmline(&result, &prevl, v.pair.first, lineno);
            appendasmline(&result, &prevl, v.pair.second, lineno);
        } else {
            appendasmline(&result, &prevl, internparsev(&v), lineno);
        }
    }

    asm_destroy(ctx);
    return result;
}
