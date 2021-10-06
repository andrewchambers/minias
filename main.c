#include "minias.h"

static FILE *outf = NULL;
static AsmLine *allasm = NULL;

// Symbols in memory before
// writing out the symtab section.
static struct hashtable *symbols = NULL;

#define MAXSECTIONS 32
static Section sections[MAXSECTIONS];
static size_t nsections = 1; // first is reserved.

static Section *cursection;
static Section *shstrtab = NULL;
static Section *strtab = NULL;
static Section *symtab = NULL;
static Section *bss = NULL;
static Section *text = NULL;
static Section *data = NULL;

size_t curlineno = 0;

static Symbol *getsym(const char *name) {
  Symbol **ps, *s;
  struct hashtablekey htk;

  htabkey(&htk, name, strlen(name));
  ps = (Symbol **)htabput(symbols, &htk);
  if (!*ps) {
    *ps = zalloc(sizeof(Symbol));
    (*ps)->name = name;
  }
  s = *ps;
  return s;
}

static const char *secname(Section *s) {
  return (const char *)shstrtab->data + s->hdr.sh_name;
}

static void secaddbytes(Section *s, uint8_t *bytes, size_t n) {
  while (s->capacity < s->hdr.sh_size + n) {
    s->capacity = s->capacity ? (s->capacity * 2) : 64;
    s->data = xrealloc(s->data, s->capacity);
  }
  memcpy(s->data + s->hdr.sh_size, bytes, n);
  s->hdr.sh_size += n;
}

static void secaddbyte(Section *s, uint8_t b) { secaddbytes(s, &b, 1); }

static Elf64_Word elfstr(Section *sec, const char *s) {
  Elf64_Word i;
  for (i = 0; i < sec->hdr.sh_size; i++) {
    if (i == 0 || (sec->data[i - 1] == 0))
      if (strcmp(s, (char *)&sec->data[i]) == 0)
        return i;
  }
  secaddbytes(sec, (uint8_t *)s, strlen(s) + 1);
  return i;
}

static Section *newsection() {
  Section *s;
  if (nsections >= MAXSECTIONS)
    fatal("too many sections");
  s = &sections[nsections];
  s->idx = nsections;
  nsections += 1;
  return s;
}

static void initsections(void) {
  Elf64_Sym elfsym;

  shstrtab = newsection();
  secaddbyte(shstrtab, 0);
  shstrtab->hdr.sh_name = elfstr(shstrtab, ".shstrtab");
  shstrtab->hdr.sh_type = SHT_STRTAB;
  shstrtab->hdr.sh_entsize = 1;

  strtab = newsection();
  secaddbyte(strtab, 0);
  strtab->hdr.sh_name = elfstr(shstrtab, ".strtab");
  strtab->hdr.sh_type = SHT_STRTAB;
  strtab->hdr.sh_entsize = 1;

  symtab = newsection();
  symtab->hdr.sh_name = elfstr(shstrtab, ".symtab");
  symtab->hdr.sh_type = SHT_SYMTAB;
  symtab->hdr.sh_link = 2;
  symtab->hdr.sh_entsize = sizeof(Elf64_Sym);
  memset(&elfsym, 0, sizeof(elfsym));
  secaddbytes(symtab, (uint8_t *)&elfsym, sizeof(Elf64_Sym));

  bss = newsection();
  bss->hdr.sh_name = elfstr(shstrtab, ".bss");
  bss->hdr.sh_type = SHT_NOBITS;
  bss->hdr.sh_flags = SHF_ALLOC | SHF_WRITE;
  bss->hdr.sh_entsize = 1;
  bss->hdr.sh_addralign = 16; // XXX right value?

  data = newsection();
  data->hdr.sh_name = elfstr(shstrtab, ".data");
  data->hdr.sh_type = SHT_PROGBITS;
  data->hdr.sh_flags = SHF_ALLOC | SHF_WRITE;
  data->hdr.sh_entsize = 1;
  data->hdr.sh_addralign = 8; // XXX right value?

  text = newsection();
  text->hdr.sh_name = elfstr(shstrtab, ".text");
  text->hdr.sh_type = SHT_PROGBITS;
  text->hdr.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
  text->hdr.sh_entsize = 1;
  text->hdr.sh_addralign = 4;
}

static Parsev *dupv(Parsev *p) {
  Parsev *r = xmalloc(sizeof(Parsev));
  *r = *p;
  return r;
}

#define YYSTYPE Parsev
#define YY_CTX_LOCAL
#define YY_CTX_MEMBERS Parsev v;
#include "asm.peg.inc"

void parse(void) {
  AsmLine *l, *prevl;
  yycontext ctx;

  memset(&ctx, 0, sizeof(yycontext));
  prevl = NULL;
  curlineno = 0;

  while (yyparse(&ctx)) {
    curlineno += 1;
    if (ctx.v.kind == ASM_SYNTAX_ERROR) {
      lfatal("syntax error\n");
    }
    if (ctx.v.kind == ASM_BLANK)
      continue;
    l = zalloc(sizeof(AsmLine));
    l->v = ctx.v;
    l->lineno = curlineno;
    if (prevl)
      prevl->next = l;
    else
      allasm = l;
    prevl = l;
  }
}

/* Shorthand helpers to write section bytes. */

static void sb(uint8_t b) { secaddbyte(cursection, b); }

static void sb2(uint8_t b1, uint8_t b2) {
  uint8_t buf[2] = {b1, b2};
  secaddbytes(cursection, buf, sizeof(buf));
}

static void sb3(uint8_t b1, uint8_t b2, uint8_t b3) {
  uint8_t buf[3] = {b1, b2, b3};
  secaddbytes(cursection, buf, sizeof(buf));
}

static void sb4(uint8_t b1, uint8_t b2, uint8_t b3, uint8_t b4) {
  uint8_t buf[4] = {b1, b2, b3, b4};
  secaddbytes(cursection, buf, sizeof(buf));
}

static void sw(uint32_t w) {
  uint8_t buf[4] = {w & 0xff, (w & 0xff00) >> 8, (w & 0xff0000) >> 16,
                    (w & 0xff0000) >> 24};
  secaddbytes(cursection, buf, sizeof(buf));
}

static int isregkind(AsmKind k) { return k > ASM_REG_BEGIN && k < ASM_REG_END; }

/* Convert an AsmKind to register bits in reg/rm style.  */
static uint8_t modrmregbits(AsmKind k) {
  return (k - (ASM_REG_BEGIN+1)) % 16;
}

static uint8_t modrmregopcode(AsmKind k, char t) {
    uint8_t opcode;
    if (k == ASM_ADD) {
      opcode = 0x00;
    } else if (k == ASM_AND) {
      opcode = 0x20;
    } else if (k == ASM_MOV) {
      opcode = 0x88;
    } else if (k == ASM_OR) {
      opcode = 0x08;
    } else if (k == ASM_SUB) {
      opcode = 0x28;
    } else if (k == ASM_XCHG) {
      opcode = 0x86;
    } else if (k == ASM_XOR) {
      opcode = 0x30;
    } else {
      unreachable();
    }
    if (t != 'b')
      opcode += 1;
    return opcode;
}


static uint8_t modrmmemopcode(AsmKind k, char t) {
  if (k == ASM_LEA)
    return 0x8d;
  return modrmregopcode(k, t) + 2;
}

#define REX(W, R, X, B)                                                        \
  ((1 << 6) | (!!(W) << 3) | (!!(R) << 2) | (!!(X) << 1) | (!!(B) << 0))

static void assemble() {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;
  const char *label;

  cursection = text;

  for (l = allasm; l; l = l->next) {
    curlineno = l->lineno;
    v = &l->v;
    switch (l->v.kind) {
    case ASM_DIR_GLOBL:
      label = v->globl.name;
      sym = getsym(label);
      sym->global = 1;
      break;
    case ASM_DIR_DATA:
      cursection = data;
      break;
    case ASM_DIR_TEXT:
      cursection = text;
      break;
    case ASM_DIR_BALIGN: {
      int64_t i, rem, amnt;
      amnt = 0;
      rem = cursection->hdr.sh_size % v->balign.align;
      if (rem)
        amnt = v->balign.align - rem;
      for (i = 0; i < amnt; i++) {
        sb(0x00);
      }
      break;
    }
    case ASM_DIR_BYTE:
      sb(v->byte.b);
      break;
    case ASM_LABEL:
      label = v->label.name;
      sym = getsym(label);
      sym->offset = cursection->hdr.sh_size;
      break;
    case ASM_NOP:
      sb(0x90);
      break;
    case ASM_LEAVE:
      sb(0xc9);
      break;
    case ASM_RET:
      sb(0xc3);
      break;
    case ASM_MOVZX:
    case ASM_MOVSX: {
      fatal("TODO");
    }
    case ASM_ADD:
    case ASM_AND:
    case ASM_LEA:
    case ASM_MOV:
    case ASM_OR:
    case ASM_SUB:
    case ASM_XCHG:
    case ASM_XOR: {
      
      ModRMBinop *op;
      Memarg *memarg;
      uint8_t opcode;
      uint8_t rex, mod, reg, rm, sib;
      int wantsib;
      int64_t disp;
      int dispsz;
      int64_t imm;
      int immsz;

      op = &v->modrmbinop;
      memarg = NULL;
      mod = 0x03;
      wantsib = 0;
      sib = 0;
      dispsz = 0;
      immsz = 0;

      if (op->src->kind == ASM_MEMARG) {
        memarg = &op->src->memarg;
      } else if (op->dst->kind == ASM_MEMARG) {
        memarg = &op->dst->memarg;
      }

      if (memarg) {
        rm = modrmregbits(memarg->reg);

        /* We cannot address ESP/RSP/... */
        if ((rm & 7) == 4)
          lfatal("addressing mode unrepresentable");

        if (memarg->c == 0 && memarg->l == NULL) {
          if ((rm & 7) == 5) { // BP style registers need displacement
            mod = 0x01;
            wantsib = 1;
            sib = 0;
            disp = 0;
            dispsz = 1;
          } else {
            mod = 0x00;
          }
        } else {
          unreachable();
        }
      }

      if (isregkind(op->dst->kind)) {
        rm = modrmregbits(op->dst->kind);
      }

      if (isregkind(op->src->kind)) {
        opcode = modrmregopcode(op->kind, op->type);
        reg = modrmregbits(op->src->kind);
      } else if (op->src->kind == ASM_MEMARG) {
        opcode = modrmmemopcode(op->kind, op->type);
        reg = modrmregbits(op->dst->kind);
      } else if (op->src->kind == ASM_IMM) {
        opcode = 0x81;
        reg = 0x00;
        if (memarg) {
          rm = modrmregbits(memarg->reg);
        } else {
          rm = modrmregbits(op->dst->kind);
        }
      }

      if (op->type == 'w') {
        sb(0x66);
      }

      rex = REX(op->type == 'q', reg & (1 << 3), 0, rm & (1 << 3));

      if (rex != REX(0, 0, 0, 0)) {
        sb(rex);
      }

      sb2(opcode, ((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7));

      if (wantsib) {
        sb(sib);
      }

      switch (dispsz) {
      case 0:
        break;
      case 1:
        sb((uint8_t)disp);
        break;
      case 4:
        sw((uint32_t)disp);
        break;
      default:
        unreachable();
      }

      switch (immsz) {
      case 0:
        break;
      case 1:
        sb((uint8_t)imm);
        break;
      case 4:
        sw((uint32_t)imm);
        break;
      default:
        unreachable();
      }

      break;
    }
    case ASM_JMP: {
      int64_t distance;

      sym = getsym(v->jmp.target);
      if (sym->section && (sym->section == cursection)) {
        distance = sym->offset - cursection->hdr.sh_size;
      } else {
        distance = 0x7fffffff; // XXX
      }
      if (distance <= 128 && distance >= -127) {
        sb2(0xeb, (uint8_t)distance);
      } else {
        sb(0xe9);
        sw((uint32_t)distance);
      }
      break;
    }
    default:
      lfatal("assemble: unexpected kind: %d", l->v.kind);
    }
  }
}

static void addtosymtab(Symbol *sym) {
  Elf64_Sym elfsym;
  int stype;
  int sbind;
  stype = (sym->section->hdr.sh_flags & SHF_EXECINSTR) ? STT_FUNC : STT_OBJECT;
  sbind = sym->global ? STB_GLOBAL : STB_LOCAL;

  memset(&elfsym, 0, sizeof(elfsym));
  elfsym.st_name = elfstr(strtab, sym->name);
  elfsym.st_size = sym->size;
  elfsym.st_value = sym->offset;
  elfsym.st_info = ELF32_ST_BIND(sbind) | ELF32_ST_TYPE(stype);
  elfsym.st_shndx = sym->section->idx;
  secaddbytes(symtab, (uint8_t *)&elfsym, sizeof(Elf64_Sym));
}

static void fillsymtab(void) {
  Symbol *sym;
  size_t i;

  // Local symbols
  for (i = 0; i < symbols->cap; i++) {
    sym = symbols->vals[i];
    if (!sym || sym->global || !sym->section)
      continue;
    addtosymtab(sym);
  }
  // Global symbols
  for (i = 0; i < symbols->cap; i++) {
    sym = symbols->vals[i];
    if (!sym || !sym->global || !sym->section)
      continue;
    addtosymtab(sym);
  }
}

static void out(uint8_t *buf, size_t n) {
  fwrite(buf, 1, n, outf);
  if (ferror(outf))
    fatal("fwrite:");
}

static void outelf(void) {
  size_t i;
  uint64_t offset;
  Elf64_Ehdr ehdr = {0};

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

  out((uint8_t *)&ehdr, sizeof(ehdr));
  offset = sizeof(Elf64_Ehdr) + sizeof(Elf64_Shdr) * nsections;

  for (i = 0; i < nsections; i++) {
    sections[i].hdr.sh_offset = offset;
    out((uint8_t *)&sections[i].hdr, sizeof(Elf64_Shdr));
    offset += sections[i].hdr.sh_size;
  }
  for (i = 0; i < nsections; i++) {
    if (sections[i].hdr.sh_type == SHT_NOBITS)
      continue;
    out(sections[i].data, sections[i].hdr.sh_size);
  }
}

int main(void) {
  symbols = mkhtab(256);
  outf = stdout;

  initsections();
  parse();
  assemble();
  fillsymtab();
  outelf();
  if (fflush(outf) != 0)
    fatal("fflush:");
  return 0;
}