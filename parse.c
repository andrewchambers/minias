#include "minias.h"

/* Cache of Parsev* by value. */
static const Parsev*
internparsev(Parsev* p)
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
    const Parsev* interned;
    static const Parsev* cache[4096] = { 0 };

    idx = murmurhash64a((char*)p, sizeof(Parsev)) % sizeof(cache) / sizeof(cache[0]);
    interned = cache[idx];
    if (interned && memcmp(p, interned, sizeof(Parsev)) == 0)
        return interned;
    interned = (const Parsev*)xmemdup((char*)p, sizeof(Parsev));
    cache[idx] = interned;
    return interned;
}

/* Cache of char* by value. */
const char*
internstring(const char* s)
{
    size_t idx, len;
    const char* interned;
    static const char* cache[4096] = { 0 };

    len = strlen(s);
    idx = murmurhash64a(s, len) % sizeof(cache) / sizeof(cache[0]);
    interned = cache[idx];
    if (interned && strcmp(s, cache[idx]) == 0)
        return interned;
    interned = xstrdup(s);
    cache[idx] = interned;
    return interned;
}

static String
decodestring(char* s)
{
    char* end;
    size_t len = 0;
    size_t cap = 0;
    uint8_t* data = NULL;
    uint8_t c = 0;

    /* The string is already validated by the parser so we omit some checks*/
    while (*s) {
        if (*s == '\\') {
            s++;
            if (*s >= '0' && *s <= '7') {
                c = strtoul(s, &end, 8);
                s += 2;
            } else if (*s == 'x') {
                s++;
                c = strtoul(s, &end, 16);
                s = end - 1;
            } else if (*s == 'r') {
                c = '\r';
            } else if (*s == 'n') {
                c = '\n';
            } else if (*s == 't') {
                c = '\t';
            } else if (*s == '\\') {
                c = '\\';
            } else {
                unreachable();
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
    return (String){ .kind = ASM_STRING, .len = len, .data = data };
}

static int
needsmovabs(Imm* imm)
{
    int64_t mask, maskedc;

    if (imm->v.l)
        return 1;

    mask = 0xffffffff80000000;
    maskedc = (uint64_t)imm->v.c & mask;
    return (maskedc != mask && maskedc != 0);
}

#define OP(OPCODE)                                                                                 \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_OP, .prefix = -1, .opcode = OPCODE,              \
        }                                                                                          \
    }

#define OPREG(REX, PREFIX, OPCODE, REG, A1)                                                        \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_OPREG, .prefix = PREFIX, .rex = (Rex)REX,        \
            .fixedreg = REG, .opcode = OPCODE, .arg1 = internparsev(&A1)                           \
        }                                                                                          \
    }

#define OPMEM(REX, PREFIX, OPCODE, REG, A1)                                                        \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_OPMEM, .prefix = PREFIX, .rex = (Rex)REX,        \
            .fixedreg = REG, .opcode = OPCODE, .arg1 = internparsev(&A1)                           \
        }                                                                                          \
    }

#define R(REX, PREFIX, OPCODE, A1)                                                                 \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_R, .prefix = PREFIX, .opcode = OPCODE,           \
            .rex = (Rex)REX, .arg1 = internparsev(&A1),                                            \
        }                                                                                          \
    }

#define IMM(REX, PREFIX, OPCODE, A1, A2)                                                           \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_IMM, .prefix = PREFIX, .opcode = OPCODE,         \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)                  \
        }                                                                                          \
    }

#define RIMM(REX, PREFIX, OPCODE, A1, A2)                                                          \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_RIMM, .prefix = PREFIX, .opcode = OPCODE,        \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)                  \
        }                                                                                          \
    }

#define IMMREG(REX, PREFIX, OPCODE, IMMREG, A1, A2)                                                \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMREG, .prefix = PREFIX, .opcode = OPCODE,      \
            .rex = (Rex)REX, .fixedreg = IMMREG, .arg1 = internparsev(&A1),                        \
            .arg2 = internparsev(&A2)                                                              \
        }                                                                                          \
    }

#define IMMMEM(REX, PREFIX, OPCODE, IMMREG, A1, A2)                                                \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMMEM, .prefix = PREFIX, .opcode = OPCODE,      \
            .rex = (Rex)REX, .fixedreg = IMMREG, .arg1 = internparsev(&A1),                        \
            .arg2 = internparsev(&A2)                                                              \
        }                                                                                          \
    }

#define REGMEM(REX, PREFIX, OPCODE, A1, A2)                                                        \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_REGMEM, .prefix = PREFIX, .opcode = OPCODE,      \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)                  \
        }                                                                                          \
    }

#define MEMREG(REX, PREFIX, OPCODE, A1, A2)                                                        \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_MEMREG, .prefix = PREFIX, .opcode = OPCODE,      \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)                  \
        }                                                                                          \
    }

#define REGREG(REX, PREFIX, OPCODE, A1, A2)                                                        \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_REGREG, .prefix = PREFIX, .opcode = OPCODE,      \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)                  \
        }                                                                                          \
    }

#define REGREG2(REX, PREFIX, OPCODE, A1, A2)                                                       \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_REGREG2, .prefix = PREFIX, .opcode = OPCODE,     \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2)                  \
        }                                                                                          \
    }

#define IMMREGREG2(REX, PREFIX, OPCODE, A1, A2, A3)                                                \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMREGREG2, .prefix = PREFIX, .opcode = OPCODE,  \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2),                 \
            .arg3 = internparsev(&A3)                                                              \
        }                                                                                          \
    }

#define IMMMEMREG(REX, PREFIX, OPCODE, A1, A2, A3)                                                 \
    (Parsev)                                                                                       \
    {                                                                                              \
        .instr = (Instr)                                                                           \
        {                                                                                          \
            .kind = ASM_INSTR, .encoder = ENCODER_IMMMEMREG, .prefix = PREFIX, .opcode = OPCODE,   \
            .rex = (Rex)REX, .arg1 = internparsev(&A1), .arg2 = internparsev(&A2),                 \
            .arg3 = internparsev(&A3)                                                              \
        }                                                                                          \
    }

#define REG(K)                                                                                     \
    (Parsev) { .kind = ASM_##K }

#define YYSTYPE Parsev
#define YY_CTX_LOCAL
#define YY_CTX_MEMBERS Parsev v;
#include "asm.peg.inc"

AsmLine*
parseasm(void)
{
    AsmLine *result, *l, *prevl;
    yycontext ctx;

    memset(&ctx, 0, sizeof(yycontext));
    result = NULL;
    prevl = NULL;

    while (yyparse(&ctx)) {
        l = zalloc(sizeof(AsmLine));
        l->v = internparsev(&ctx.v);
        if (prevl)
            prevl->next = l;
        else
            result = l;
        prevl = l;
    }

    return result;
}
