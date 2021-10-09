#include "minias.h"

static FILE *outf = NULL;
static AsmLine *allasm = NULL;

// Symbols in memory before
// writing out the symtab section.
static struct hashtable *symbols = NULL;

// Array of all relocations before adding to the rel section.
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

static Section *getsection(const char *name) {
  size_t i;
  Section *s;

  for (i = 0; i < nsections; i++) {
    if (strcmp(secname(&sections[i]), name) == 0)
      return &sections[i];
  }
  s = newsection();
  s->hdr.sh_name = elfstr(shstrtab, name);
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
  symtab->hdr.sh_link = strtab->idx;
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

  textrel = newsection();
  textrel->hdr.sh_type = SHT_REL;
  textrel->hdr.sh_info = text->idx;
  textrel->hdr.sh_link = symtab->idx;
  textrel->hdr.sh_entsize = sizeof(Elf64_Rel);

  datarel = newsection();
  datarel->hdr.sh_type = SHT_REL;
  datarel->hdr.sh_info = data->idx;
  datarel->hdr.sh_link = symtab->idx;
  datarel->hdr.sh_entsize = sizeof(Elf64_Rel);
}

Relocation *newreloc() {
  if (nrelocs == reloccap) {
    reloccap = nrelocs ? nrelocs * 2 : 64;
    relocs = xreallocarray(relocs, reloccap, sizeof(Relocation));
  }
  return &relocs[nrelocs++];
}

static String decodestring(char *s) {
  int i;
  char *end;
  size_t len = 0;
  size_t cap = 0;
  uint8_t *data = NULL;
  uint8_t c = 0;

  /* The string is already validated by the parser so we omit some checks*/
  while (*s) {
    if (*s == '\\') {
      s++;
      if (*s >= '0' && *s <= '7') {
        c = strtoul(s, &end, 8);
        s += 3;
      } else if (*s == 'x') {
        s++;
        c = strtoul(s, &end, 16);
        s = end;
      } else if (*s == 'r') {
        c = '\r';
      } else if (*s == 'n') {
        c = '\n';
      } else if (*s == 't') {
        c = '\t';
      } else {
        unreachable();
      }
    } else {
      c = *s;
      s++;
    }
    if (len == cap) {
      cap = cap ? len * 2 : 8;
      data = realloc(data, cap);
    }
    data[len++] = c;
  }
  return (String){.kind = ASM_STRING, .len = len, .data = data};
}

static Parsev *dupv(Parsev *p) {
  Parsev *r = xmalloc(sizeof(Parsev));
  *r = *p;
  return r;
}

#define INSTR1(V, A)                                                           \
  (Parsev) {                                                                   \
    .instr1 = (Instr1) { .kind = 0, .variant = V, .arg = dupv(&A), }           \
  }

#define INSTR2(V, S, D)                                                        \
  (Parsev) {                                                                   \
    .instr2 = (Instr2) {                                                       \
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

static void sbn(uint8_t *bytes, size_t n) { secaddbytes(cursection, bytes, n); }

static void su16(uint16_t w) {
  uint8_t buf[2] = {w & 0xff, (w & 0xff00) >> 8};
  secaddbytes(cursection, buf, sizeof(buf));
}

static void su32(uint32_t l) {
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

static uint8_t isreg(AsmKind k) { return k > ASM_REG_BEGIN && k < ASM_REG_END; }
static uint8_t isreg8(AsmKind k) { return k >= ASM_AL && k <= ASM_R15B; }
static uint8_t isreg16(AsmKind k) { return k >= ASM_AX && k <= ASM_R15W; }
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

/* Compose an sib byte - See intel manual. */
static uint8_t sibbyte(uint8_t ss, uint8_t idx, uint8_t base) {
  return (((ss & 3) << 6) | ((idx & 7) << 3) | (base & 7));
}

/* Assemble op +rw | op + rd. */
static void assembleplusr(uint8_t opcode, uint8_t rexw, AsmKind reg) {
  uint8_t bits = regbits(reg);
  uint8_t rex = rexbyte(rexw, 0, 0, bits & (1 << 3));
  if (isreg16(reg))
    sb(0x66);
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);
  sb(opcode | (bits & 7));
}

void assembleconstant(int64_t c, int nbytes) {
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
    fatal("TODO 8 byte");
    break;
  default:
    unreachable();
  }
}

/* Assemble a symbolic value. */
static void assemblereloc(const char *l, int64_t c, int nbytes, int type) {
  Relocation *reloc;
  Symbol *sym;

  if (l != NULL) {
    reloc = newreloc();
    sym = getsym(l);
    reloc->type = type;
    reloc->section = cursection;
    reloc->sym = sym;
    reloc->offset = cursection->hdr.sh_size;
  }
  assembleconstant(c, nbytes);
}

/* Assemble a r <-> (%rip) operation. */
static void assembleriprel(Memarg *memarg, uint8_t rexw, uint8_t opcode,
                           uint8_t reg, uint8_t opsz) {
  uint8_t rex;

  if (opsz == 2)
    sb(0x66);
  rex = rexbyte(rexw, 0, 0, 0);
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);
  sb2(opcode, modregrm(0x00, reg, 0x05));

  if (memarg->l) {
    assemblereloc(memarg->l, memarg->c - 4, 4, R_X86_64_PC32);
  } else {
    assembleconstant(memarg->c, 4);
  }
}

/* Assemble a r <-> mem operation.  */
static void assemblemem(Memarg *memarg, uint8_t rexw, uint8_t opcode,
                        uint8_t reg, uint8_t opsz) {
  uint8_t rex, rm, sib;

  rm = regbits(memarg->reg);
  if (opsz == 2)
    sb(0x66);
  rex = rexbyte(rexw, reg & (1 << 3), 0, rm & (1 << 3));
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);

  if (memarg->c == 0 && memarg->l == NULL) {
    /* No offset cases, uses the smallest we can. */
    if ((rm & 7) == 4) { /* Matches '(%rsp/%esp...)'. */
      sb3(opcode, modregrm(0, reg, 4), sibbyte(0, 4, 4));
    } else if ((rm & 7) == 5) { /* Matches '(%rbp/%ebp...)'. */
      sb3(opcode, modregrm(1, reg, 5), 0);
    } else {
      sb2(opcode, modregrm(0, reg, rm));
    }
  } else {
    /* TODO choose smaller size if not label .*/
    if ((rm & 7) == 4) { /* Matches '(%rsp/%esp...)'. */
      sb3(opcode, modregrm(2, reg, 4), sibbyte(0, 4, 4));
    } else if ((rm & 7) == 5) { /* Matches '(%rbp/%ebp...)'. */
      sb2(opcode, modregrm(2, reg, 5));
    } else {
      sb2(opcode, modregrm(2, reg, rm));
    }
    assemblereloc(memarg->l, memarg->c, 4, R_X86_64_32);
  }
}

/* Assemble op + imm -> r/m. */
static void assembleimmrm(Instr2 *instr, uint8_t opcode, uint8_t immreg,
                          uint8_t opsz) {
  Imm *imm;

  imm = &instr->src->imm;

  if (instr->dst->kind == ASM_MEMARG && instr->dst->memarg.reg == ASM_RIP) {
    assembleriprel(&instr->dst->memarg, opsz == 8, opcode, immreg, opsz);
    assemblereloc(imm->l, imm->c, imm->nbytes, R_X86_64_32);
  } else if (instr->dst->kind == ASM_MEMARG) {
    assemblemem(&instr->dst->memarg, opsz == 8, opcode, immreg, opsz);
    assemblereloc(imm->l, imm->c, imm->nbytes, R_X86_64_32);
  } else {
    uint8_t rex, mod, rm;
    mod = 0x3;
    rm = regbits(instr->dst->kind);
    if (opsz == 2)
      sb(0x66);
    rex = rexbyte(isreg64(instr->dst->kind), 0, 0, rm & (1 << 3));
    if (rex != rexbyte(0, 0, 0, 0))
      sb(rex);
    sb2(opcode, modregrm(mod, immreg, rm));
    assemblereloc(imm->l, imm->c, imm->nbytes, R_X86_64_32);
  }
}

/* Assemble op + r <-> r/m. */
static void assemblerrm(Instr2 *instr, uint8_t opcode, uint8_t opsz) {
  Memarg *memarg;
  AsmKind regarg;

  if (instr->src->kind == ASM_MEMARG && instr->src->memarg.reg == ASM_RIP) {
    memarg = &instr->src->memarg;
    regarg = instr->dst->kind;
    assembleriprel(memarg, isreg64(regarg), opcode, regbits(regarg), opsz);
  } else if (instr->dst->kind == ASM_MEMARG &&
             instr->dst->memarg.reg == ASM_RIP) {
    memarg = &instr->dst->memarg;
    regarg = instr->src->kind;
    assembleriprel(memarg, isreg64(regarg), opcode, regbits(regarg), opsz);
  } else if (instr->src->kind == ASM_MEMARG) {
    memarg = &instr->src->memarg;
    regarg = instr->dst->kind;
    assemblemem(memarg, isreg64(regarg), opcode, regbits(regarg), opsz);
  } else if (instr->dst->kind == ASM_MEMARG) {
    memarg = &instr->dst->memarg;
    regarg = instr->src->kind;
    assemblemem(memarg, isreg64(regarg), opcode, regbits(regarg), opsz);
  } else {
    uint8_t rex, mod, reg, rm;
    mod = 0x03;
    regarg = instr->src->kind;
    reg = regbits(instr->src->kind);
    rm = regbits(instr->dst->kind);
    if (opsz == 2)
      sb(0x66);
    rex = rexbyte(isreg64(regarg), reg & (1 << 3), 0, rm & (1 << 3));
    if (rex != rexbyte(0, 0, 0, 0))
      sb(rex);
    sb2(opcode, modregrm(0x03, reg, rm));
  }
}

/* Assemble a 'basic op' which is just a repeated op pattern we have named. */
static void assemblebasicop(Instr2 *instr, uint8_t opcode, uint8_t immreg) {
  Imm *imm;
  uint8_t opsz = 1 << (instr->variant % 4);
  if (instr->variant < 4) {
    if (opsz == 2)
      sb(0x66);
    if (instr->dst->kind == ASM_RAX)
      sb(rexbyte(1, 0, 0, 0));
    sb(opcode);
    imm = &instr->src->imm;
    assemblereloc(imm->l, imm->c, imm->nbytes, R_X86_64_32);
  } else if (instr->variant < 12) {
    assembleimmrm(instr, opcode, immreg, opsz);
  } else {
    assemblerrm(instr, opcode, opsz);
  }
}

static void assemblexchg(Instr2 *xchg) {
  static uint8_t variant2op[18] = {0x90, 0x90, 0x90, 0x90, 0x90, 0x90,
                                   0x86, 0x87, 0x87, 0x87, 0x86, 0x87,
                                   0x87, 0x87, 0x86, 0x87, 0x87, 0x87};
  uint8_t opcode = variant2op[xchg->variant];
  if (xchg->variant < 6) {
    AsmKind reg = (xchg->variant % 2) ? xchg->src->kind : xchg->dst->kind;
    assembleplusr(opcode, isreg64(reg), reg);
  } else {
    uint8_t opsz = 1 << ((xchg->variant - 6) % 4);
    assemblerrm(xchg, opcode, opsz);
  }
}

static void assemblemov(Instr2 *mov) {
  Imm *imm;
  uint8_t opcode, rex, mod, rm;

  static uint8_t variant2op[20] = {
      0xc6, 0xc7, 0xc7, 0xc7, 0xb0, 0xb8, 0xb8, 0xc7, 0x8a, 0x8b,
      0x8b, 0x8b, 0x88, 0x89, 0x89, 0x89, 0x88, 0x89, 0x89, 0x89,
  };

  opcode = variant2op[mov->variant];
  if (mov->variant >= 4 && mov->variant <= 6) {
    imm = &mov->src->imm;
    assembleplusr(opcode, isreg64(mov->dst->kind), mov->dst->kind);
    assemblereloc(imm->l, imm->c, imm->nbytes, R_X86_64_32);
  } else if (mov->variant == 7 || mov->variant < 4) {
    uint8_t opsz = 1 << (mov->variant % 4);
    assembleimmrm(mov, opcode, 0x00, opsz);
  } else {
    uint8_t opsz = 1 << (mov->variant % 4);
    assemblerrm(mov, opcode, opsz);
  }
}

static void assemble(void) {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;

  cursection = text;

  for (l = allasm; l; l = l->next) {
    v = &l->v;
    curlineno = l->lineno;
    switch (l->v.kind) {
    case ASM_DIR_GLOBL:
      sym = getsym(v->globl.name);
      sym->global = 1;
      break;
    case ASM_DIR_SECTION: {
      size_t i;
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
      if (sym->defined)
        lfatal("%s already defined", sym->name);
      sym->defined = 1;
      break;
    case ASM_CALL: {
      Symbol *sym;
      Relocation *reloc;

      sb(0xe8);
      assemblereloc(v->call.target, -4, 4, R_X86_64_PC32);
      break;
    }
    case ASM_JMP: {
      Symbol *sym;
      Relocation *reloc;

      sb(0xe9);
      assemblereloc(v->call.target, -4, 4, R_X86_64_PC32);
      break;
    }
    case ASM_PUSH:
      if (v->instr1.arg->kind == ASM_MEMARG) {
        assemblemem(&v->instr1.arg->memarg, 0, 0xff, 0x06, 8);
      } else {
        assembleplusr(0x50, 0, v->instr1.arg->kind);
      }
      break;
    case ASM_POP:
      if (v->instr1.arg->kind == ASM_MEMARG) {
        assemblemem(&v->instr1.arg->memarg, 0, 0x8f, 0x00, 8);
      } else {
        assembleplusr(0x58, 0, v->instr1.arg->kind);
      }
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
    case ASM_LEA: {
      uint8_t opsz = 1 << (v->instr2.variant + 1);
      assemblerrm(&v->instr2, 0x8d, opsz);
      break;
    }
    case ASM_MOV:
      assemblemov(&v->instr2);
      break;
    case ASM_ADD: {
      static uint8_t variant2op[24] = {
          0x04, 0x05, 0x05, 0x05, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x02, 0x03, 0x03, 0x03,
          0x00, 0x01, 0x01, 0x01, 0x00, 0x01, 0x01, 0x01,
      };
      assemblebasicop(&v->instr2, variant2op[v->instr2.variant], 0x00);
      break;
    }
    case ASM_AND: {
      static uint8_t variant2op[24] = {
          0x24, 0x25, 0x25, 0x25, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x22, 0x23, 0x23, 0x23,
          0x20, 0x21, 0x21, 0x21, 0x20, 0x21, 0x21, 0x21,
      };
      assemblebasicop(&v->instr2, variant2op[v->instr2.variant], 0x04);
      break;
    }
    case ASM_OR: {
      static uint8_t variant2op[24] = {
          0x0c, 0x0d, 0x0d, 0x0d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x0a, 0x0b, 0x0b, 0x0b,
          0x08, 0x09, 0x09, 0x09, 0x08, 0x09, 0x09, 0x09,
      };
      assemblebasicop(&v->instr2, variant2op[v->instr2.variant], 0x01);
      break;
    }
    case ASM_SUB: {
      static uint8_t variant2op[24] = {
          0x2c, 0x2d, 0x2d, 0x2d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x2a, 0x2b, 0x2b, 0x2b,
          0x28, 0x29, 0x29, 0x29, 0x28, 0x29, 0x29, 0x29,
      };
      assemblebasicop(&v->instr2, variant2op[v->instr2.variant], 0x05);
      break;
    }
    case ASM_XOR: {
      static uint8_t variant2op[24] = {
          0x34, 0x35, 0x35, 0x35, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x32, 0x33, 0x33, 0x33,
          0x30, 0x31, 0x31, 0x31, 0x30, 0x31, 0x31, 0x31,
      };
      assemblebasicop(&v->instr2, variant2op[v->instr2.variant], 0x06);
      break;
    }
    case ASM_XCHG: {
      assemblexchg(&v->instr2);
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

  if (sym->defined) {
    stype =
        (sym->section->hdr.sh_flags & SHF_EXECINSTR) ? STT_FUNC : STT_OBJECT;
    sbind = sym->global ? STB_GLOBAL : STB_LOCAL;
  } else {
    stype = 0;
    sbind = STB_GLOBAL;
  }

  sym->idx = symtab->hdr.sh_size / symtab->hdr.sh_entsize;

  elfsym.st_name = elfstr(strtab, sym->name);
  elfsym.st_value = sym->offset;
  elfsym.st_size = sym->size;
  elfsym.st_info = ELF64_ST_INFO(sbind, stype);
  elfsym.st_shndx = sym->section ? sym->section->idx : SHN_UNDEF;
  elfsym.st_other = 0;
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
    if (!sym->defined || sym->global)
      continue;
    addtosymtab(sym);
  }

  // Global symbols

  // Set start of global symbols.
  symtab->hdr.sh_info = symtab->hdr.sh_size / symtab->hdr.sh_entsize;

  for (i = 0; i < symbols->cap; i++) {
    if (!symbols->keys[i].str)
      continue;
    sym = symbols->vals[i];

    if (sym->defined && !sym->global)
      continue;
    addtosymtab(sym);
  }
}

static void resolvereloc(Relocation *reloc) {
  Symbol *sym;
  uint8_t *rdata;
  int32_t addend, value;

  sym = reloc->sym;

  switch (reloc->type) {
  case R_X86_64_32: {
    rdata = &reloc->section->data[reloc->offset];
    addend = (int32_t)rdata[0] | (int32_t)(rdata[1] << 8) |
             (int32_t)(rdata[2] << 16) | (int32_t)(rdata[3] << 24);
    value = sym->offset - (int32_t)reloc->offset + addend;
    rdata[0] = ((uint32_t)value & 0xff);
    rdata[1] = ((uint32_t)value & 0xff00) >> 8;
    rdata[2] = ((uint32_t)value & 0xff0000) >> 16;
    rdata[3] = ((uint32_t)value & 0xff000000) >> 24;
    break;
  case R_X86_64_PC32:
    fatal("TODO local R_X86_64_PC32");
    break;
  }
  default:
    unreachable();
  }
}

static void appendreloc(Relocation *reloc) {
  Symbol *sym;
  Section *relsection;
  Elf64_Rel elfrel;

  memset(&elfrel, 0, sizeof(elfrel));

  sym = reloc->sym;
  if (reloc->section == text)
    relsection = textrel;
  else if (reloc->section == data)
    relsection = datarel;
  else
    fatal("unexpected relocation for symbol '%s'", sym->name);

  switch (reloc->type) {
  case R_X86_64_PC32:
  case R_X86_64_32:
    elfrel.r_info = ELF64_R_INFO(sym->idx, reloc->type);
    elfrel.r_offset = reloc->offset;
    break;
  default:
    unreachable();
  }

  secaddbytes(relsection, (uint8_t *)&elfrel, sizeof(elfrel));
}

static void handlerelocs(void) {
  Relocation *reloc;
  size_t i;
  for (i = 0; i < nrelocs; i++) {
    reloc = &relocs[i];
    if (reloc->sym->section == reloc->section) {
      resolvereloc(reloc);
      continue;
    }
    appendreloc(reloc);
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
  if (fflush(outf) != 0)
    fatal("fflush:");
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
  return 0;
}