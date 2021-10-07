#include "minias.h"

static FILE *outf = NULL;
static AsmLine *allasm = NULL;

// Symbols in memory before
// writing out the symtab section.
static struct hashtable *symbols = NULL;

// Linked list of relocations
static Relocation *relocs = NULL;
static size_t nrelocs = 0;
static size_t reloccap = 0;

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

Relocation *newreloc() {
  if (nrelocs == reloccap) {
    reloccap = nrelocs ? nrelocs * 2 : 64;
    relocs = xreallocarray(relocs, reloccap, sizeof(Relocation));
  }
  return &relocs[nrelocs++];
}

static Parsev *dupv(Parsev *p) {
  Parsev *r = xmalloc(sizeof(Parsev));
  *r = *p;
  return r;
}

#define INSTR(V, S, D)                                                         \
  (Parsev) {                                                                   \
    .instr = (Instr) {                                                         \
      .kind = 0, .variant = V, .src = dupv(&S), .dst = dupv(&D)                \
    }                                                                          \
  }
#define REG(K)                                                                 \
  (Parsev) { .kind = K }

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
    if (ctx.v.kind == ASM_SYNTAX_ERROR)
      lfatal("syntax error\n");
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

static void sw(uint16_t w) {
  uint8_t buf[2] = {w & 0xff, (w & 0xff00) >> 8};
  secaddbytes(cursection, buf, sizeof(buf));
}

static void sl(uint32_t l) {
  uint8_t buf[4] = {
      l & 0xff,
      (l & 0xff00) >> 8,
      (l & 0xff0000) >> 16,
      (l & 0xff000000) >> 24,
  };
  secaddbytes(cursection, buf, sizeof(buf));
}

/* Convert an AsmKind to register bits in reg/rm style.  */
static uint8_t regbits(AsmKind k) { return (k - (ASM_REG_BEGIN + 1)) % 16; }

/* Is a register. */
static uint8_t isreg(AsmKind k) { return k > ASM_REG_BEGIN && k < ASM_REG_END; }

/* Is an r$n style register variant.  */
static uint8_t isreg8(AsmKind k) { return k >= ASM_AL && k <= ASM_R15B; }

/* Is an r$n style register variant.  */
static uint8_t isreg16(AsmKind k) { return k >= ASM_AX && k <= ASM_R15W; }

/* Is an r$n style register variant.  */
static uint8_t isreg64(AsmKind k) { return k >= ASM_RAX && k <= ASM_R15; }

/* Is an r$n style register variant.  */
static uint8_t isregr(AsmKind k) { return !!(regbits(k) & (1 << 3)); }

/* Compose a rex prefix - See intel manual. */
static uint8_t rexbyte(uint8_t w, uint8_t r, uint8_t x, uint8_t b) {
  return ((1 << 6) | ((!!w) << 3) | ((!!r) << 2) | ((!!x) << 1) | (!!b));
}

/* Compose a mod/reg/rm byte - See intel manual. */
static uint8_t modregrm(uint8_t mod, uint8_t reg, uint8_t rm) {
  return (((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7));
}

/* Assemble op +rw | op + rd. */
static void assembleplusr(uint8_t opcode, AsmKind reg) {
  uint8_t bits = regbits(reg);
  uint8_t rex = rexbyte(isreg64(reg), 0, 0, bits & (1 << 3));
  if (isreg16(reg))
    sb(0x66);
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);
  sb(opcode | (bits & 7));
}

static void assembleimm(Imm *imm) {
  Relocation *reloc;
  Symbol *sym;

  if (imm->l != NULL) {
    reloc = newreloc();
    sym = getsym(imm->l);
    reloc->kind = 0; // XXX
    reloc->section = cursection;
    reloc->sym = sym;
    reloc->offset = cursection->hdr.sh_size;
  }

  switch (imm->nbytes) {
  case 1:
    sb((uint8_t)imm->c);
    break;
  case 2:
    sw((uint16_t)imm->c);
    break;
  case 4:
    sl((uint32_t)imm->c);
    break;
  case 8:
    fatal("TODO 8 byte imm");
    break;
  default:
    unreachable();
  }
}

/* Assemble op + imm -> r/m. */
static void assembleimmrm(Instr *instr, uint8_t opcode, uint8_t immreg,
                          uint8_t opsz) {

  Memarg *memarg;
  uint8_t rex, rexw, mod, rm, sib;
  int wantsib;

  wantsib = 0;

  if (instr->dst->kind == ASM_MEMARG) {
    memarg = &instr->dst->memarg;
    rexw = opsz == 8;
    rm = regbits(memarg->reg);
    /* We cannot address ESP/RSP/... */
    if ((rm & 7) == 4)
      lfatal("addressing mode unrepresentable");
    if (memarg->c == 0 && memarg->l == NULL) {
      if ((rm & 7) == 5) { // BP style registers need sib
        mod = 0x01;
        wantsib = 1;
        sib = 0;
      } else {
        mod = 0x00;
      }
    } else {
      lfatal("TODO X");
    }

  } else {
    memarg = NULL;
    mod = 0x3;
    rexw = isreg64(instr->dst->kind);
    rm = regbits(instr->dst->kind);
  }

  if (opsz == 2)
    sb(0x66);

  rex = rexbyte(rexw, 0, 0, rm & (1 << 3));
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);

  sb2(opcode, modregrm(mod, immreg, rm));

  if (wantsib)
    sb(sib);

  assembleimm(&instr->src->imm);
}

/* Assemble op + r <-> r/m. */
static void assemblerrm(Instr *instr, uint8_t opcode) {

  Memarg *memarg;
  AsmKind regarg;
  uint8_t rex, mod, reg, rm, sib;
  int wantsib;

  wantsib = 0;

  if (instr->src->kind == ASM_MEMARG) {
    memarg = &instr->src->memarg;
    regarg = instr->dst->kind;
    reg = regbits(instr->dst->kind);
  } else if (instr->dst->kind == ASM_MEMARG) {
    memarg = &instr->dst->memarg;
    regarg = instr->src->kind;
    reg = regbits(instr->src->kind);
  } else {
    mod = 0x03;
    memarg = NULL;
    regarg = instr->src->kind;
    reg = regbits(instr->src->kind);
    rm = regbits(instr->dst->kind);
  }

  if (memarg) {
    rm = regbits(memarg->reg);
    /* We cannot address ESP/RSP/... */
    if ((rm & 7) == 4)
      lfatal("addressing mode unrepresentable");
    if (memarg->c == 0 && memarg->l == NULL) {
      if ((rm & 7) == 5) { /* BP style registers need sib */
        mod = 0x01;
        wantsib = 1;
        sib = 0;
      } else {
        mod = 0x00;
      }
    } else {
      lfatal("TODO X");
    }
  }

  if (isreg16(regarg))
    sb(0x66);

  rex = rexbyte(isreg64(regarg), reg & (1 << 3), 0, rm & (1 << 3));
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);

  sb2(opcode, modregrm(mod, reg, rm));

  if (wantsib)
    sb(sib);
}

/* Assemble a 'basic op' which is just a repeated op pattern we have named. */
static void assemblebasicop(Instr *instr, uint8_t opcode, uint8_t immreg) {
  if (instr->variant < 4) {
    if (isreg16(instr->dst->kind))
      sb(0x66);
    if (instr->dst->kind == ASM_RAX)
      sb(rexbyte(1, 0, 0, 0));
    sb(opcode);
    assembleimm(&instr->src->imm);
  } else if (instr->variant < 12) {
    // Note, uses a pattern in the variant array.
    uint8_t opsize = 1 << (instr->variant % 4); // 1 2 4 8
    assembleimmrm(instr, opcode, immreg, opsize);
  } else {
    assemblerrm(instr, opcode);
  }
}

static void assemblexchg(Instr *xchg) {
  static uint8_t variant2op[14] = {0x90, 0x90, 0x90, 0x90, 0x90, 0x90, 0x86,
                                   0x86, 0x87, 0x87, 0x87, 0x87, 0x87, 0x87};
  uint8_t opcode = variant2op[xchg->variant];
  if (xchg->variant <= 5) {
    assembleplusr(opcode,
                  (xchg->variant % 2) ? xchg->src->kind : xchg->dst->kind);
  } else {
    assemblerrm(xchg, opcode);
  }
}

static void assemblemov(Instr *mov) {
  uint8_t opcode, rex, mod, rm;

  static uint8_t variant2op[20] = {
      0xb0, 0xb8, 0xb8, 0xc7, 0xc6, 0xc7, 0xc7, 0xc7, 0x8a, 0x8b,
      0x8b, 0x8b, 0x88, 0x89, 0x89, 0x89, 0x88, 0x89, 0x89, 0x89,
  };

  opcode = variant2op[mov->variant];
  if (mov->variant >= 8) {
    assemblerrm(mov, opcode);
  } else if (mov->variant >= 3) {
    // Note, uses a pattern in the variant array.
    uint8_t opsize = 1 << (mov->variant % 4);
    assembleimmrm(mov, opcode, 0x00, opsize);
  } else {
    assembleplusr(opcode, mov->dst->kind);
    assembleimm(&mov->src->imm);
  }
}

static void assemble(void) {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;

  cursection = text;

  for (l = allasm; l; l = l->next) {
    curlineno = l->lineno;
    v = &l->v;
    switch (l->v.kind) {
    case ASM_DIR_GLOBL:
      sym = getsym(v->globl.name);
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
      sym = getsym(v->label.name);
      sym->section = cursection;
      sym->offset = cursection->hdr.sh_size;
      sym->defined = 1;
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
    case ASM_LEA:
      assemblerrm(&v->instr, 0x8d);
      break;
    case ASM_MOV: {
      assemblemov(&v->instr);
      break;
    }
    case ASM_ADD: {
      static uint8_t variant2op[24] = {
          0x04, 0x05, 0x05, 0x05, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x02, 0x03, 0x03, 0x03,
          0x00, 0x01, 0x01, 0x01, 0x00, 0x01, 0x01, 0x01,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x00);
      break;
    }
    case ASM_AND: {
      static uint8_t variant2op[24] = {
          0x24, 0x25, 0x25, 0x25, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x22, 0x23, 0x23, 0x23,
          0x20, 0x21, 0x21, 0x21, 0x20, 0x21, 0x21, 0x21,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x04);
      break;
    }
    case ASM_OR: {
      static uint8_t variant2op[24] = {
          0x0c, 0x0d, 0x0d, 0x0d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x0a, 0x0b, 0x0b, 0x0b,
          0x08, 0x09, 0x09, 0x09, 0x08, 0x09, 0x09, 0x09,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x01);
      break;
    }
    case ASM_SUB: {
      static uint8_t variant2op[24] = {
          0x2c, 0x2d, 0x2d, 0x2d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x2a, 0x2b, 0x2b, 0x2b,
          0x28, 0x29, 0x29, 0x29, 0x28, 0x29, 0x29, 0x29,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x05);
      break;
    }
    case ASM_XOR: {
      static uint8_t variant2op[24] = {
          0x34, 0x35, 0x35, 0x35, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x32, 0x33, 0x33, 0x33,
          0x30, 0x31, 0x31, 0x31, 0x30, 0x31, 0x31, 0x31,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x06);
      break;
    }
    case ASM_XCHG: {
      assemblexchg(&v->instr);
      break;
    }
    case ASM_JMP: {
      Symbol *sym;
      Relocation *reloc;

      sb(0xe9);
      sym = getsym(v->jmp.target);
      reloc = newreloc();
      reloc->kind = 0; // XXX
      reloc->section = cursection;
      reloc->sym = sym;
      reloc->offset = cursection->hdr.sh_size;
      sw(0x00000000);
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
    if (!symbols->keys[i].str)
      continue;
    sym = symbols->vals[i];
    if (sym->global || !sym->section)
      continue;
    addtosymtab(sym);
  }
  // Global symbols
  for (i = 0; i < symbols->cap; i++) {
    if (!symbols->keys[i].str)
      continue;
    sym = symbols->vals[i];
    if (!sym->global || !sym->section)
      continue;
    addtosymtab(sym);
  }
}

static void handlerelocs(void) {
  Symbol *sym;
  Relocation *reloc;
  size_t i;

  for (i = 0; i < nrelocs; i++) {
    reloc = &relocs[i];
    sym = reloc->sym;
    if (sym->section == reloc->section) {
      switch (reloc->kind) {
      case 0: {
        uint8_t *rdata;
        int32_t addend, value;
        rdata = &reloc->section->data[reloc->offset];
        addend = (int32_t)rdata[0] | (int32_t)(rdata[1] << 8) |
                 (int32_t)(rdata[2] << 16) | (int32_t)(rdata[3] << 24);
        // XXX overflow?
        value = sym->offset - (int32_t)reloc->offset + addend;
        fprintf(stderr, "%lu %ld %d %d\n", reloc->offset, sym->offset, addend,
                value);
        rdata[0] = (value & 0xff);
        rdata[1] = (value & 0xff00) >> 8;
        rdata[2] = (value & 0xff000) >> 16;
        rdata[3] = (value & 0xff00000) >> 24;
        break;
      }
      default:
        unreachable();
      }
    }
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
  handlerelocs();
  outelf();
  if (fflush(outf) != 0)
    fatal("fflush:");
  return 0;
}