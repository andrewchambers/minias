#include "minias.h"

/* Parsed assembly */
static AsmLine *allasm = NULL;

/* Number of assembly relaxation passes. */
static int nrelax = 1;

/* Symbols before writing to symtab section. */
static struct hashtable *symbols = NULL;

/* Array of all relocations before adding to the rel section. */
static Relocation *relocs = NULL;
static size_t nrelocs = 0;
static size_t reloccap = 0;

#define MAXSECTIONS 32
static Section sections[MAXSECTIONS];
static size_t nsections = 1; // first is reserved.

static Section *cursection = NULL;
static Section *shstrtab = NULL;
static Section *strtab = NULL;
static Section *symtab = NULL;
static Section *bss = NULL;
static Section *text = NULL;
static Section *data = NULL;
static Section *textrel = NULL;
static Section *datarel = NULL;

static char *infilename = "<stdin>";
static size_t curlineno = 0;

static void
lfatal(const char *fmt, ...)
{
    va_list ap;
    fprintf(stderr, "%s:%ld: ", infilename, curlineno);
    va_start(ap, fmt);
    vwarn(fmt, ap);
    va_end(ap);
    exit(1);
}

static Symbol *
getsym(const char *name)
{
    Symbol **ps, *s;
    struct hashtablekey htk;

    htabkey(&htk, name, strlen(name));
    ps = (Symbol **)htabput(symbols, &htk);
    if (!*ps) {
        *ps = xmalloc(sizeof(Symbol));
        **ps = (Symbol) {
            .name = name,
            .wco = -1,
            .bind = STB_LOCAL,
            .type = STT_NOTYPE,
        };
    }
    s = *ps;
    return s;
}

static void
secaddbytes(Section *s, const void *bytes, size_t n)
{

    if (s->hdr.sh_type == SHT_NOBITS) {
        s->hdr.sh_size += n;
        return;
    }

    while (s->capacity < s->hdr.sh_size + n) {
        s->capacity = s->capacity ? (s->capacity * 2) : 512;
        s->data = xrealloc(s->data, s->capacity);
    }
    memcpy(s->data + s->hdr.sh_size, bytes, n);

    s->hdr.sh_size += n;
}

static void
secaddbyte(Section *s, uint8_t b)
{
    secaddbytes(s, &b, 1);
}

static Elf64_Word
elfstr(Section *sec, const char *s)
{
    Elf64_Word i = sec->hdr.sh_size;
    secaddbytes(sec, s, strlen(s) + 1);
    return i;
}

static Section *
newsection()
{
    Section *s;
    if (nsections >= MAXSECTIONS)
        fatal("too many sections");
    s = &sections[nsections];
    s->idx = nsections;
    nsections += 1;
    return s;
}

static Section *
getsection(const char *name)
{
    size_t i;
    char *secname;
    Section *s;

    for (i = 0; i < nsections; i++) {
        secname = (char *)shstrtab->data + sections[i].hdr.sh_name;
        if (strcmp(secname, name) == 0)
            return &sections[i];
    }
    s = newsection();
    s->hdr.sh_name = elfstr(shstrtab, name);
    return s;
}

static void
initsections(void)
{
    Elf64_Sym elfsym;

    shstrtab = newsection();
    secaddbyte(shstrtab, 0);
    shstrtab->hdr.sh_name = elfstr(shstrtab, ".shstrtab");
    shstrtab->hdr.sh_type = SHT_STRTAB;

    strtab = newsection();
    secaddbyte(strtab, 0);
    strtab->hdr.sh_name = elfstr(shstrtab, ".strtab");
    strtab->hdr.sh_type = SHT_STRTAB;

    symtab = newsection();
    symtab->hdr.sh_name = elfstr(shstrtab, ".symtab");
    symtab->hdr.sh_type = SHT_SYMTAB;
    symtab->hdr.sh_link = strtab->idx;
    symtab->hdr.sh_entsize = sizeof(Elf64_Sym);
    memset(&elfsym, 0, sizeof(elfsym));
    secaddbytes(symtab, &elfsym, sizeof(Elf64_Sym));

    bss = newsection();
    bss->hdr.sh_name = elfstr(shstrtab, ".bss");
    bss->hdr.sh_type = SHT_NOBITS;
    bss->hdr.sh_flags = SHF_ALLOC | SHF_WRITE;
    bss->hdr.sh_addralign = 16; // XXX right value?

    data = newsection();
    data->hdr.sh_name = elfstr(shstrtab, ".data");
    data->hdr.sh_type = SHT_PROGBITS;
    data->hdr.sh_flags = SHF_ALLOC | SHF_WRITE;
    data->hdr.sh_addralign = 16; // XXX right value?

    text = newsection();
    text->hdr.sh_name = elfstr(shstrtab, ".text");
    text->hdr.sh_type = SHT_PROGBITS;
    text->hdr.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
    text->hdr.sh_addralign = 4;

    textrel = newsection();
    textrel->hdr.sh_name = elfstr(shstrtab, ".rela.text");
    textrel->hdr.sh_type = SHT_RELA;
    textrel->hdr.sh_info = text->idx;
    textrel->hdr.sh_link = symtab->idx;
    textrel->hdr.sh_entsize = sizeof(Elf64_Rela);

    datarel = newsection();
    datarel->hdr.sh_name = elfstr(shstrtab, ".rela.data");
    datarel->hdr.sh_type = SHT_RELA;
    datarel->hdr.sh_info = data->idx;
    datarel->hdr.sh_link = symtab->idx;
    datarel->hdr.sh_entsize = sizeof(Elf64_Rela);
}

static Relocation *
newreloc()
{
    if (nrelocs == reloccap) {
        reloccap = nrelocs ? nrelocs * 2 : 64;
        relocs = xreallocarray(relocs, reloccap, sizeof(Relocation));
    }
    return &relocs[nrelocs++];
}

/* Shorthand helpers to write section data. */

static void
sb(uint8_t b)
{
    secaddbyte(cursection, b);
}

static void
sbn(uint8_t *bytes, size_t n)
{
    secaddbytes(cursection, bytes, n);
}

static void
su16(uint16_t w)
{
    uint8_t buf[2] = { w & 0xff, (w & 0xff00) >> 8 };
    secaddbytes(cursection, buf, sizeof(buf));
}

static void
su32(uint32_t l)
{
    uint8_t buf[4] = {
        l & 0xff,
        (l & 0xff00) >> 8,
        (l & 0xff0000) >> 16,
        (l & 0xff000000) >> 24,
    };
    secaddbytes(cursection, buf, sizeof(buf));
}

static void
su64(uint64_t l)
{
    uint8_t buf[8] = {
        l & 0xff,
        (l & 0xff00) >> 8,
        (l & 0xff0000) >> 16,
        (l & 0xff000000) >> 24,
        (l & 0xff00000000) >> 32,
        (l & 0xff0000000000) >> 40,
        (l & 0xff000000000000) >> 48,
        (l & 0xff00000000000000) >> 56,
    };
    secaddbytes(cursection, buf, sizeof(buf));
}

/* Convert an AsmKind to register bits in reg/rm style.  */
static uint8_t
regbits(AsmKind k)
{
    return (k - (ASM_REG_BEGIN + 1)) % 16;
}

/* Register that requires the use of a rex prefix. */
static uint8_t
isrexreg(AsmKind k)
{
    return k > ASM_REG_BEGIN && k < ASM_REG_END
        && (regbits(k) & (1 << 3) || k == ASM_SPL || k == ASM_BPL
            || k == ASM_SIL || k == ASM_DIL);
}

static uint8_t
rexbyte(Rex rex)
{
    return ((1 << 6) | (rex.w << 3) | (rex.r << 2) | (rex.x << 1) | rex.b);
}

/* Compose a mod/reg/rm byte - See intel manual. */
static uint8_t
modregrmbyte(uint8_t mod, uint8_t reg, uint8_t rm)
{
    return (((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7));
}

/* Compose an sib byte - See intel manual. */
static uint8_t
sibbyte(uint8_t ss, uint8_t idx, uint8_t base)
{
    return (((ss & 3) << 6) | ((idx & 7) << 3) | (base & 7));
}

void
assembleconstant(int64_t c, int nbytes)
{
    switch (nbytes) {
    case 1:
        sb((uint8_t)c);
        break;
    case 2:
        su16((uint16_t)c);
        break;
    case 4:
        su32((uint32_t)c);
        break;
    case 8:
        su64((uint64_t)c);
        break;
    default:
        unreachable();
    }
}

/* The VarBytes type encodes a variable number of bytes.
   The top byte is how many bytes we encode less 1.

   examples:
   <nothing> encodes as -1
   0c encodes as 0x0000000c
   02 03 encodes as 0x01000203
*/
typedef int32_t VarBytes;

static void
assemblevbytes(VarBytes bytes)
{
    int i, n;
    uint8_t b, shift;

    n = (int8_t)(uint8_t)((bytes & 0xff000000) >> 24);
    for (i = n; i >= 0; i--) {
        shift = i * 8;
        b = (bytes & (0xff << shift)) >> shift;
        sb(b);
    }
}

static void
assemblerex(Rex rex)
{
    if (rex.required || rex.w || rex.r || rex.x || rex.b)
        sb(rexbyte(rex));
}

/* Assemble a symbolic value. */
static void
assemblereloc(const char *l, int64_t c, int nbytes, int type)
{
    Relocation *reloc;
    Symbol *sym;

    if (l != NULL) {
        reloc = newreloc();
        sym = getsym(l);
        reloc->type = type;
        reloc->section = cursection;
        reloc->sym = sym;
        reloc->offset = cursection->hdr.sh_size;
        reloc->addend = c;
        c = 0;
    }
    assembleconstant(c, nbytes);
}

/* Assemble a r <-> mem operation.

  In order to understand this function, you should check the intel
  manual which has tables showing the appropriate values of the modregrm
  byte and sib byte for the different addressing modes.
*/
static void
assemblemem(const Memarg *memarg, Rex rex, VarBytes prefix, VarBytes opcode,
    uint8_t reg, int32_t nexti)
{
    uint8_t mod, rm, scale, index, base;

    /* Rip relative addressing. */
    if (memarg->base == ASM_RIP) {
        rm = 0x05;
        assemblevbytes(prefix);
        assemblerex(rex);
        assemblevbytes(opcode);
        sb(modregrmbyte(0x00, reg, rm));

        if (memarg->disp.l) {
            assemblereloc(
                memarg->disp.l, memarg->disp.c - 4 - nexti, 4, R_X86_64_PC32);
        } else {
            assembleconstant(memarg->disp.c, 4);
        }
        return;
    }

    /* Direct memory access */
    if (memarg->base == ASM_NO_REG) {
        mod = 0;
        rm = 4;

        assemblevbytes(prefix);
        assemblerex(rex);
        assemblevbytes(opcode);
        sb(modregrmbyte(mod, reg, rm));

        sb(sibbyte(0, 4, 5));
        if (memarg->disp.l) {
            assemblereloc(memarg->disp.l, memarg->disp.c, 4, R_X86_64_32);
        } else {
            assembleconstant(memarg->disp.c, 4);
        }
        return;
    }

    rm = regbits(memarg->base);
    rex.b = !!(rm & (1 << 3));

    /* Case when we don't need sib */
    if (memarg->index == ASM_NO_REG && memarg->scale == 0 && ((rm & 7) != 4)) {

        if (memarg->disp.l != NULL || memarg->disp.c > INT8_MAX
            || memarg->disp.c < INT8_MIN) {
            mod = 2;
        } else if (memarg->disp.c != 0 || (rm & 7) == 5) {
            mod = 1;
        } else {
            mod = 0;
        }

        assemblevbytes(prefix);
        assemblerex(rex);
        assemblevbytes(opcode);
        sb(modregrmbyte(mod, reg, rm));

        if (mod == 1) {
            assembleconstant(memarg->disp.c, 1);
        } else if (mod == 2) {
            assemblereloc(memarg->disp.l, memarg->disp.c, 4, R_X86_64_32);
        }
        return;
    }

    /* Setup sib indexing. */
    base = rm;
    rm = 4;

    if (memarg->disp.c == 0 && memarg->disp.l == 0 && ((base & 7) != 5)) {
        mod = 0; /* +0 */
    } else {
        if (memarg->disp.l == NULL && memarg->disp.c >= INT8_MIN
            && memarg->disp.c <= INT8_MAX) {
            mod = 1; /* +disp8 */
        } else {
            mod = 2; /* +disp32 */
        }
    }

    if (memarg->index == ASM_NO_REG) {
        index = 4;
    } else {
        if (memarg->index == ASM_RSP)
            lfatal("rsp cannot be used as an index");
        index = regbits(memarg->index);
    }

    /* If our base is a bp register, we must use the index instead. */
    if ((base & 7) == 5 && memarg->index == ASM_NO_REG) {
        index = base;
    }

    rex.x = !!(index & (1 << 3));

    switch (memarg->scale) {
    case 0:
    case 1:
        scale = 0;
        break;
    case 2:
        scale = 1;
        break;
    case 4:
        scale = 2;
        break;
    case 8:
        scale = 3;
        break;
    default:
        lfatal("invalid addressing scale");
        return;
    }

    assemblevbytes(prefix);
    assemblerex(rex);
    assemblevbytes(opcode);
    sb(modregrmbyte(mod, reg, rm));
    sb(sibbyte(scale, index, base));

    if (mod == 1) {
        assembleconstant(memarg->disp.c, 1);
    } else if (mod == 2) {
        assemblereloc(memarg->disp.l, memarg->disp.c, 4, R_X86_64_32);
    }
}

static void
assemblejmp(const Jmp *j)
{
    Symbol *target;
    int64_t distance;
    int jmpsize;

    // clang-format off
    static uint8_t cc2op[31] = {
        0xe9, 0x84, 0x88, 0x8b, 0x8a, 0x8a, 0x80, 0x85,
        0x89, 0x8b, 0x81, 0x8f, 0x8d, 0x8c, 0x8e, 0x85,
        0x83, 0x87, 0x83, 0x82, 0x86, 0x8e, 0x8c, 0x8d,
        0x8f, 0x84, 0x82, 0x86, 0x82, 0x83, 0x87,
    };
    // clang-format on

    jmpsize = 4;
    target = getsym(j->target);

    if (cursection == target->section && target->wco != -1) {
        distance = target->wco - cursection->hdr.sh_size;
        if (distance - 2 >= INT8_MIN
            && distance - (j->cc ? 6 : 5) <= INT8_MAX) {
            jmpsize = 1;
        } else {
            jmpsize = 4;
        }
    }

    if (jmpsize == 4) {
        if (j->cc)
            sb(0x0f);
        sb(cc2op[j->cc]);
        assemblereloc(j->target, -4, 4, R_X86_64_PC32);
    } else {
        sb(cc2op[j->cc] + (j->cc ? -16 : 2));
        assemblereloc(j->target, -1, 1, R_X86_64_PC8);
    }
}

static void
assembleabsimm(const Imm *imm)
{
    int reltype;

    if (imm->nbytes == 1)
        reltype = R_X86_64_8;
    else if (imm->nbytes == 2)
        reltype = R_X86_64_16;
    else if (imm->nbytes == 4)
        reltype = R_X86_64_32;
    else if (imm->nbytes == 8)
        reltype = R_X86_64_64;
    else {
        unreachable();
        return;
    }

    assemblereloc(imm->v.l, imm->v.c, imm->nbytes, reltype);
}

static void
assembleinstr(const Instr *instr)
{
    Rex rex;
    const Memarg *memarg;
    const Imm *imm;
    uint8_t reg, rm;

    switch (instr->encoder) {
    case ENCODER_OP:
        assemblevbytes(instr->opcode);
        break;
    case ENCODER_OPREG:
        rm = regbits(instr->arg1->kind);
        rex = instr->rex;
        rex.required = isrexreg(instr->arg1->kind);
        rex.b = !!(rm & (1 << 3));
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode);
        sb(modregrmbyte(0x03, instr->fixedreg, rm));
        break;
    case ENCODER_OPMEM:
        memarg = &instr->arg1->memarg;
        rex = instr->rex;
        assemblemem(
            memarg, rex, instr->prefix, instr->opcode, instr->fixedreg, 0);
        break;
    case ENCODER_R:
        reg = regbits(instr->arg1->kind);
        rex = instr->rex;
        rex.required = isrexreg(instr->arg1->kind);
        rex.b = !!(reg & (1 << 3));
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode | (reg & 7));
        break;
    case ENCODER_RIMM:
        imm = &instr->arg1->imm;
        reg = regbits(instr->arg2->kind);
        rex = instr->rex;
        rex.required = isrexreg(instr->arg2->kind);
        rex.b = !!(reg & (1 << 3));
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode | (reg & 7));
        assembleabsimm(imm);
        break;
    case ENCODER_IMM:
        imm = &instr->arg1->imm;
        rex = instr->rex;
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode);
        assembleabsimm(imm);
        break;
    case ENCODER_RELCALL:
        memarg = &instr->arg1->memarg;
        rex = instr->rex;
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode);
        assemblereloc(memarg->disp.l, memarg->disp.c - 4, 4, R_X86_64_PC32);
        break;
    case ENCODER_IMMREG:
        imm = &instr->arg1->imm;
        reg = instr->fixedreg;
        rm = regbits(instr->arg2->kind);
        rex = instr->rex;
        rex.required = isrexreg(instr->arg2->kind);
        rex.b = !!(rm & (1 << 3));
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode);
        sb(modregrmbyte(0x03, reg, rm));
        assembleabsimm(imm);
        break;
    case ENCODER_IMMMEM:
        imm = &instr->arg1->imm;
        memarg = &instr->arg2->memarg;
        reg = instr->fixedreg;
        rex = instr->rex;
        assemblemem(memarg, rex, instr->prefix, instr->opcode, instr->fixedreg,
            imm->nbytes);
        assembleabsimm(imm);
        break;
    case ENCODER_REGMEM:
    case ENCODER_MEMREG:
        if (instr->encoder == ENCODER_MEMREG) {
            memarg = &instr->arg1->memarg;
            reg = regbits(instr->arg2->kind);
        } else {
            memarg = &instr->arg2->memarg;
            reg = regbits(instr->arg1->kind);
        }
        rex = instr->rex;
        rex.required
            = isrexreg(instr->arg1->kind) || isrexreg(instr->arg2->kind);
        rex.r = !!(reg & (1 << 3));
        assemblemem(memarg, rex, instr->prefix, instr->opcode, reg, 0);
        break;
    case ENCODER_REGREG:
    case ENCODER_REGREG2:
        if (instr->encoder == ENCODER_REGREG) {
            reg = regbits(instr->arg1->kind);
            rm = regbits(instr->arg2->kind);
        } else {
            reg = regbits(instr->arg2->kind);
            rm = regbits(instr->arg1->kind);
        }
        rex = instr->rex;
        rex.required
            = isrexreg(instr->arg1->kind) || isrexreg(instr->arg2->kind);
        rex.r = !!(reg & (1 << 3));
        rex.b = !!(rm & (1 << 3));
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode);
        sb(modregrmbyte(0x03, reg, rm));
        break;
    case ENCODER_IMMREGREG2:
        imm = &instr->arg1->imm;
        reg = regbits(instr->arg3->kind);
        rm = regbits(instr->arg2->kind);
        rex = instr->rex;
        rex.required
            = isrexreg(instr->arg1->kind) || isrexreg(instr->arg2->kind);
        rex.r = !!(reg & (1 << 3));
        rex.b = !!(rm & (1 << 3));
        assemblevbytes(instr->prefix);
        assemblerex(rex);
        assemblevbytes(instr->opcode);
        sb(modregrmbyte(0x03, reg, rm));
        assembleabsimm(imm);
        break;
    case ENCODER_IMMMEMREG:
        imm = &instr->arg1->imm;
        memarg = &instr->arg2->memarg;
        reg = regbits(instr->arg3->kind);
        rex = instr->rex;
        rex.required
            = isrexreg(instr->arg1->kind) || isrexreg(instr->arg2->kind);
        rex.r = !!(reg & (1 << 3));
        assemblemem(
            memarg, rex, instr->prefix, instr->opcode, reg, imm->nbytes);
        assembleabsimm(imm);
        break;
    default:
        unreachable();
    }
}

static void
assemble(void)
{
    Symbol *sym;
    AsmLine *l;
    const Parsev *v;

    cursection = text;
    curlineno = 0;
    for (l = allasm; l; l = l->next) {
        curlineno++;
        v = l->v;
        switch (v->kind) {
        case ASM_SYNTAX_ERROR:
            lfatal("syntax error");
            break;
        case ASM_BLANK:
            break;
        case ASM_DIR_GLOBL:
            sym = getsym(v->globl.name);
            sym->bind = STB_GLOBAL;
            break;
        case ASM_DIR_WEAK:
            sym = getsym(v->weak.sym);
            sym->bind = STB_WEAK;
            break;
        case ASM_DIR_SECTION: {
            const char *fp;
            Section *s;

            s = getsection(v->section.name);
            s->hdr.sh_type = v->section.type;
            fp = v->section.flags;
            while (fp && *fp) {
                switch (*(fp++)) {
                case 'a':
                    s->hdr.sh_flags |= SHF_ALLOC;
                    break;
                case 'w':
                    s->hdr.sh_flags |= SHF_WRITE;
                    break;
                case 'x':
                    s->hdr.sh_flags |= SHF_EXECINSTR;
                    break;
                default:
                    unreachable();
                }
            }
            cursection = s;
            break;
        }
        case ASM_DIR_DATA:
            cursection = data;
            break;
        case ASM_DIR_TEXT:
            cursection = text;
            break;
        case ASM_DIR_ASCII:
            sbn(v->ascii.data, v->ascii.len);
            break;
        case ASM_DIR_ASCIIZ:
            sbn(v->asciiz.data, v->asciiz.len);
            sb(0x00);
            break;
        case ASM_DIR_BALIGN: {
            int64_t offset, i, rem, amnt;
            amnt = 0;
            offset = cursection->hdr.sh_addralign + cursection->hdr.sh_size;
            rem = offset % v->balign.align;
            if (rem)
                amnt = v->balign.align - rem;
            for (i = 0; i < amnt; i++) {
                sb(0x00);
            }
            break;
        }
        case ASM_DIR_FILL: {
            ssize_t i = 0;

            for (i = 0; i < v->fill.repeat; i++) {
                switch (v->fill.size) {
                case 1:
                case 2:
                case 4:
                case 8:
                    assembleconstant(v->fill.value, v->fill.size);
                    break;
                default:
                    lfatal("unsupported fill size '%d'", v->fill.size);
                }
            }
            break;
        }
        case ASM_DIR_BYTE:
            assemblereloc(
                v->dirbyte.value.l, v->dirbyte.value.c, 1, R_X86_64_32);
            break;
        case ASM_DIR_SHORT:
            assemblereloc(
                v->dirshort.value.l, v->dirshort.value.c, 2, R_X86_64_32);
            break;
        case ASM_DIR_INT:
            assemblereloc(v->dirint.value.l, v->dirint.value.c, 4, R_X86_64_32);
            break;
        case ASM_DIR_QUAD:
            assemblereloc(
                v->dirquad.value.l, v->dirquad.value.c, 8, R_X86_64_64);
            break;
        case ASM_DIR_SET:
            sym = getsym(v->set.sym);
            sym->value = v->set.value;
            break;
        case ASM_DIR_TYPE:
            break;
        case ASM_DIR_SIZE:
            break;
        case ASM_LABEL:
            sym = getsym(v->label.name);
            if (sym->defined)
                lfatal("%s already defined", sym->name);
            sym->defined = 1;
            sym->section = cursection;
            sym->value.c = cursection->hdr.sh_size;
            sym->wco = sym->value.c;
            break;
        case ASM_INSTR:
            assembleinstr(&v->instr);
            break;
        case ASM_JMP:
            assemblejmp(&v->jmp);
            break;
        default:
            lfatal("assemble: unexpected kind: %d", v->kind);
        }
    }
}

/* Reset while remembering symbol offsets so we can size jumps. */
static void
relaxreset(void)
{
    Symbol *sym;
    Section *sec;
    size_t i;

    /* Reset relocations and section data but retain capacity. */
    nrelocs = 0;

    for (i = 0; i < nsections; i++) {
        sec = &sections[i];
        if (sec == shstrtab || sec == strtab || sec == symtab)
            continue;
        sec->hdr.sh_size = 0;
    }

    /* Reset symbols, saving the worst case offset for the second pass. */
    for (i = 0; i < symbols->cap; i++) {
        if (!symbols->keys[i].str)
            continue;
        sym = symbols->vals[i];
        *sym = (Symbol) {
            .name = sym->name, .section = sym->section, .wco = sym->wco
        };
    }
}

/* Try to resolve the address of a symbol, this will recursively look
   for the symbol address if it is defined relative to another symbol. */
static int
resolvesym2(Symbol *sym, int depth)
{
    Symbol *indirect;

    if (depth > 64)
        fatal("recursion limit hit when resolving symbol location");

    if (sym->value.l) {
        indirect = getsym(sym->value.l);
        if (!resolvesym2(indirect, depth + 1))
            return 0;
        sym->section = indirect->section;
        sym->value.l = NULL;
        sym->value.c += indirect->value.c;
        sym->wco = sym->value.c;
        sym->defined = 1;
        return 1;
    }

    if (!sym->defined)
        return 0;

    return 1;
}

static int
resolvesym(Symbol *sym)
{
    return resolvesym2(sym, 0);
}

/* Resolve all symbols to their final location if we can. */
static void
resolvesymbols(void)
{
    size_t i;

    for (i = 0; i < symbols->cap; i++) {
        if (!symbols->keys[i].str)
            continue;
        resolvesym(symbols->vals[i]);
    }
}

static void
addtosymtab(Symbol *sym)
{
    Elf64_Sym elfsym;

    sym->idx = symtab->hdr.sh_size / symtab->hdr.sh_entsize;

    if (!sym->section) {
        sym->bind = STB_GLOBAL;
    }

    elfsym.st_name = elfstr(strtab, sym->name);
    elfsym.st_value = sym->value.c;
    elfsym.st_size = sym->size;
    elfsym.st_info = ELF64_ST_INFO(sym->bind, sym->type);
    elfsym.st_shndx = sym->section ? sym->section->idx : SHN_UNDEF;
    elfsym.st_other = 0;
    secaddbytes(symtab, &elfsym, sizeof(Elf64_Sym));
}

static void
fillsymtab(void)
{
    Symbol *sym;
    size_t i;

    /* Mark unknown symbols as global. */
    for (i = 0; i < symbols->cap; i++) {
        if (!symbols->keys[i].str)
            continue;
        sym = symbols->vals[i];
        if (!sym->section)
            sym->bind = STB_GLOBAL;
    }

    /* Local symbols come first. */
    for (i = 0; i < symbols->cap; i++) {
        if (!symbols->keys[i].str)
            continue;
        sym = symbols->vals[i];
        if (sym->bind != STB_LOCAL)
            continue;
        addtosymtab(sym);
    }

    /* Set start of global symbols. */
    symtab->hdr.sh_info = symtab->hdr.sh_size / symtab->hdr.sh_entsize;

    /* Global symbols. */
    for (i = 0; i < symbols->cap; i++) {
        if (!symbols->keys[i].str)
            continue;

        sym = symbols->vals[i];
        if (sym->bind == STB_LOCAL)
            continue;
        addtosymtab(sym);
    }
}

static int
resolvereloc(Relocation *reloc)
{
    Symbol *sym;
    int64_t value;
    uint8_t *rdata;

    sym = reloc->sym;

    if (sym->section != reloc->section)
        return 0;

    switch (reloc->type) {
    case R_X86_64_32:
    case R_X86_64_64:
        return 0;
    case R_X86_64_PC8:
        rdata = &reloc->section->data[reloc->offset];
        value = sym->value.c - reloc->offset + reloc->addend;
        if (value > INT8_MAX || value < INT8_MIN)
            fatal("R_X86_64_PC8 relocation overflow");
        rdata[0] = value;
        return 1;
    case R_X86_64_PC32:
        rdata = &reloc->section->data[reloc->offset];
        value = sym->value.c - reloc->offset + reloc->addend;
        if (value > INT32_MAX || value < INT32_MIN)
            fatal("R_X86_64_PC32 relocation overflow");
        rdata[0] = ((uint32_t)value & 0xff);
        rdata[1] = ((uint32_t)value & 0xff00) >> 8;
        rdata[2] = ((uint32_t)value & 0xff0000) >> 16;
        rdata[3] = ((uint32_t)value & 0xff000000) >> 24;
        return 1;
    default:
        unreachable();
        return 0;
    }
}

static void
appendreloc(Relocation *reloc)
{
    Symbol *sym;
    Section *relsection;
    Elf64_Rela elfrel;

    memset(&elfrel, 0, sizeof(elfrel));

    sym = reloc->sym;
    if (reloc->section == text)
        relsection = textrel;
    else if (reloc->section == data)
        relsection = datarel;
    else {
        fatal("unexpected relocation for symbol '%s'", sym->name);
        return;
    }

    switch (reloc->type) {
    case R_X86_64_PC32:
    case R_X86_64_32:
    case R_X86_64_64:
        elfrel.r_info = ELF64_R_INFO(sym->idx, reloc->type);
        elfrel.r_offset = reloc->offset;
        elfrel.r_addend = reloc->addend;
        break;
    default:
        unreachable();
    }

    secaddbytes(relsection, &elfrel, sizeof(elfrel));
}

static void
handlerelocs(void)
{
    Relocation *reloc;
    size_t i;
    for (i = 0; i < nrelocs; i++) {
        reloc = &relocs[i];
        if (resolvereloc(reloc))
            continue;
        appendreloc(reloc);
    }
}

static void
out(const void *buf, size_t n)
{
    fwrite(buf, 1, n, stdout);
    if (ferror(stdout))
        fatal("fwrite:");
}

static void
outelf(void)
{
    size_t i;
    uint64_t offset;
    Elf64_Ehdr ehdr;

    memset(&ehdr, 0, sizeof(ehdr));
    ehdr.e_ident[0] = 0x7f;
    ehdr.e_ident[1] = 'E';
    ehdr.e_ident[2] = 'L';
    ehdr.e_ident[3] = 'F';
    ehdr.e_ident[4] = ELFCLASS64;
    ehdr.e_ident[5] = ELFDATA2LSB;
    ehdr.e_ident[6] = 1;
    ehdr.e_type = ET_REL;
    ehdr.e_machine = EM_X86_64;
    ehdr.e_flags = 0;
    ehdr.e_version = 1;
    ehdr.e_ehsize = sizeof(Elf64_Ehdr);
    ehdr.e_shoff = sizeof(Elf64_Ehdr);
    ehdr.e_shentsize = sizeof(Elf64_Shdr);
    ehdr.e_shnum = nsections;
    ehdr.e_shstrndx = 1;

    out(&ehdr, sizeof(ehdr));
    offset = sizeof(Elf64_Ehdr) + sizeof(Elf64_Shdr) * nsections;

    for (i = 0; i < nsections; i++) {
        sections[i].hdr.sh_offset = offset;
        out(&sections[i].hdr, sizeof(Elf64_Shdr));
        offset += sections[i].hdr.sh_size;
    }
    for (i = 0; i < nsections; i++) {
        if (sections[i].hdr.sh_type == SHT_NOBITS)
            continue;
        out(sections[i].data, sections[i].hdr.sh_size);
    }
    if (fflush(stdout) != 0)
        fatal("fflush:");
}

static void
usage(char *argv0)
{
    fprintf(stderr, "minias - a mini x86-64 assembler.\n\n");
    fprintf(stderr, "usage: %s [-r iter] [-o out] [input]\n", argv0);
    fprintf(stderr, "\n");
    fprintf(stderr, "  -r iter    Jump relaxation iterations (default 1).\n");
    fprintf(stderr, "  -o out     Output file to write (default stdout).\n");
    exit(2);
}

static void
parseargs(int argc, char *argv[])
{
    char *a, *argv0, *outfname;

    argv0 = argv[0];

    for (++argv; *argv; argv++) {
        if (argv[0][0] != '-')
            break;
        a = &argv[0][1];
        switch (*a) {
        case '-':
        case 'h':
            usage(argv0);
            break;
        case 'r':
            nrelax = atoi(*++argv);
            break;
        case 'o':
            if (argv[1] == NULL)
                usage(argv0);
            outfname = *++argv;
            if (!freopen(outfname, "w", stdout))
                fatal("unable to open %s:", outfname);
            break;
        default:
            usage(argv0);
        }
    }

    if (argv[0]) {
        if (argv[1])
            usage(argv0);
        infilename = argv[0];
        if (!freopen(infilename, "r", stdin))
            fatal("unable to open %s:", infilename);
    }
}

int
main(int argc, char *argv[])
{
    symbols = mkhtab(256);
    parseargs(argc, argv);
    allasm = parseasm();
    initsections();
    assemble();
    resolvesymbols();
    while (nrelax-- > 0) {
        relaxreset();
        assemble();
        resolvesymbols();
    }
    fillsymtab();
    handlerelocs();
    outelf();
    return 0;
}
