#include "minias.h"
#include <getopt.h>

/* Parsed assembly */
static AsmLine *allasm = NULL;

/* Symbol table. */
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

void lfatal(const char *fmt, ...) {
  va_list ap;
  fprintf(stderr, "%s:%ld: ", infilename, curlineno);
  va_start(ap, fmt);
  vwarn(fmt, ap);
  va_end(ap);
  exit(1);
}

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

static void secaddbytes(Section *s, const void *bytes, size_t n) {

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

static void secaddbyte(Section *s, uint8_t b) { secaddbytes(s, &b, 1); }

static Elf64_Word elfstr(Section *sec, const char *s) {
  Elf64_Word i = sec->hdr.sh_size;
  secaddbytes(sec, s, strlen(s) + 1);
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
  secaddbytes(symtab, &elfsym, sizeof(Elf64_Sym));

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

Relocation *newreloc() {
  if (nrelocs == reloccap) {
    reloccap = nrelocs ? nrelocs * 2 : 64;
    relocs = xreallocarray(relocs, reloccap, sizeof(Relocation));
  }
  return &relocs[nrelocs++];
}

/* Shorthand helpers to write section data. */

static void sb(uint8_t b) { secaddbyte(cursection, b); }

static void sb2(uint8_t b1, uint8_t b2) {
  uint8_t buf[2] = {b1, b2};
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

static void su64(uint64_t l) {
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
static uint8_t regbits(AsmKind k) { return (k - (ASM_REG_BEGIN + 1)) % 16; }

static uint8_t isreg64(AsmKind k) { return k >= ASM_RAX && k <= ASM_R15; }

/* Compose a rex prefix - See intel manual. */
static uint8_t rexbyte(uint8_t w, uint8_t r, uint8_t x, uint8_t b) {
  return ((1 << 6) | ((!!w) << 3) | ((!!r) << 2) | ((!!x) << 1) | (!!b));
}

/* Compose a mod/reg/rm byte - See intel manual. */
static uint8_t modregrmbyte(uint8_t mod, uint8_t reg, uint8_t rm) {
  return (((mod & 3) << 6) | ((reg & 7) << 3) | (rm & 7));
}

/* Compose an sib byte - See intel manual. */
static uint8_t sibbyte(uint8_t ss, uint8_t idx, uint8_t base) {
  return (((ss & 3) << 6) | ((idx & 7) << 3) | (base & 7));
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
    su64((uint64_t)c);
    break;
  default:
    unreachable();
  }
}

/* The VarBytes type encodes a variadic number of bytes.
   The top byte is how many bytes we encode less 1.

   examples:
   <nothing> encodes as -1
   0c encodes as 0x0000000c
   02 03  encodes as 0x01000203
*/
typedef int32_t VarBytes;

static void assemblevbytes(VarBytes bytes) {
  int i, n;
  uint8_t b, shift;

  n = (int8_t)(uint8_t)((bytes & 0xff000000) >> 24);
  for (i = n; i >= 0; i--) {
    shift = i * 8;
    b = (bytes & (0xff << shift)) >> shift;
    sb(b);
  }
}

/* Assemble op +rw | op + rd. */
static void assembleplusr(uint8_t rexw, VarBytes prefix, VarBytes opcode,
                          uint8_t reg) {
  uint8_t rex;

  assemblevbytes(prefix);
  rex = rexbyte(rexw, 0, 0, reg & (1 << 3));
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);
  assemblevbytes(opcode | (reg & 7));
}

static void assemblemodregrm(uint8_t rex, VarBytes prefix, VarBytes opcode,
                             uint8_t mod, uint8_t reg, uint8_t rm) {
  assemblevbytes(prefix);
  if (rex != rexbyte(0, 0, 0, 0))
    sb(rex);
  assemblevbytes(opcode);
  sb(modregrmbyte(mod, reg, rm));
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
    reloc->addend = c;
    c = 0;
  }
  assembleconstant(c, nbytes);
}

/* Assemble a r <-> mem operation.  */
static void assemblemem(const Memarg *memarg, uint8_t rexw, VarBytes prefix,
                        VarBytes opcode, uint8_t reg) {

  uint8_t rex, mod, rm, scale, index, base;

  /* Direct memory access */
  if (memarg->base == ASM_NO_REG) {
    mod = 0;
    rm = 4;
    rex = rexbyte(rexw, reg & (1 << 3), 0, 0);
    assemblemodregrm(rex, prefix, opcode, mod, reg, rm);
    sb(sibbyte(0, 4, 5));
    if (memarg->disp.l) {
      assemblereloc(memarg->disp.l, memarg->disp.c, 4, R_X86_64_PC32);
    } else {
      assembleconstant(memarg->disp.c, 4);
    }
    return;
  }

  /* rip relative addressing. */
  if (memarg->base == ASM_RIP) {
    rm = 0x05;
    rex = rexbyte(rexw, reg & (1 << 3), 0, rm & (1 << 3));
    assemblemodregrm(rex, prefix, opcode, 0x00, reg, rm);
    if (memarg->disp.l) {
      assemblereloc(memarg->disp.l, memarg->disp.c - 4, 4, R_X86_64_PC32);
    } else {
      assembleconstant(memarg->disp.c, 4);
    }
    return;
  }

  rm = regbits(memarg->base);

  /* Case when we don't need sib */
  if (memarg->index == ASM_NO_REG && memarg->scale == 0 && ((rm & 7) != 4)) {

    if (memarg->disp.l == 0 && memarg->disp.c == 0) {
      if ((rm & 7) == 5) {
        mod = 1;
      } else {
        mod = 0;
      }
    } else {
      mod = 2;
    }

    rex = rexbyte(rexw, reg & (1 << 3), 0, rm & (1 << 3));
    assemblemodregrm(rex, prefix, opcode, mod, reg, rm);

    if (mod == 1) {
      assemblereloc(memarg->disp.l, memarg->disp.c, 1, R_X86_64_32);
    } else if (mod == 2) {
      assemblereloc(memarg->disp.l, memarg->disp.c, 4, R_X86_64_32);
    }
    return;
  }

  /* Setup sib indexing. */
  base = rm;
  rm = 4;

  // TODO: if our disp fits in a +disp8, use that instead.
  if (memarg->disp.c == 0 && memarg->disp.l == 0 && ((base & 7) != 5)) {
    mod = 0; /* +0 */
  } else {
    if (memarg->disp.c == 0 && memarg->disp.l == 0) {
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

  rex = rexbyte(rexw, reg & (1 << 3), index & (1 << 3), base & (1 << 3));
  assemblemodregrm(rex, prefix, opcode, mod, reg, rm);
  sb(sibbyte(scale, index, base));

  if (mod)
    assemblereloc(memarg->disp.l, memarg->disp.c, (mod == 2) ? 4 : 1,
                  R_X86_64_32);
}

/* Assemble op + imm -> r/m. */
static void assembleimmrm(const Instr *instr, uint8_t rexw, VarBytes prefix,
                          VarBytes opcode, uint8_t immreg) {
  uint8_t rex, rm;
  const Imm *imm;

  imm = &instr->arg1->imm;

  if (instr->arg2->kind == ASM_MEMARG) {
    assemblemem(&instr->arg2->memarg, rexw, prefix, opcode, immreg);
  } else {
    rm = regbits(instr->arg2->kind);
    rex = rexbyte(rexw, immreg & (1 << 3), 0, rm & (1 << 3));
    assemblemodregrm(rex, prefix, opcode, 0x03, immreg, rm);
  }
  assemblereloc(imm->v.l, imm->v.c, imm->nbytes, R_X86_64_32);
}

/* Assemble op + r <-> r/m. */
static void assemblerrm(const Instr *instr, VarBytes prefix, VarBytes opcode,
                        int invert) {
  const Parsev *arg1, *arg2;
  const Memarg *memarg;
  AsmKind regarg;
  uint8_t rexw, rex, reg1, reg2;

  if (invert) {
    arg1 = instr->arg2;
    arg2 = instr->arg1;
  } else {
    arg1 = instr->arg1;
    arg2 = instr->arg2;
  }

  if (arg1->kind == ASM_MEMARG) {
    memarg = &arg1->memarg;
    regarg = arg2->kind;
    assemblemem(memarg, isreg64(regarg), prefix, opcode, regbits(regarg));
  } else if (arg2->kind == ASM_MEMARG) {
    memarg = &arg2->memarg;
    regarg = arg1->kind;
    assemblemem(memarg, isreg64(regarg), prefix, opcode, regbits(regarg));
  } else {
    rexw = isreg64(arg1->kind) || isreg64(arg2->kind);
    reg1 = regbits(arg1->kind);
    reg2 = regbits(arg2->kind);
    rex = rexbyte(rexw, reg1 & (1 << 3), 0, reg2 & (1 << 3));
    assemblemodregrm(rex, prefix, opcode, 0x03, reg1, reg2);
  }
}

/* Assemble a 'basic op' which is just a repeated op pattern we have named. */
static void assemblebasicop(const Instr *instr, VarBytes opcode,
                            uint8_t immreg) {
  VarBytes prefix;
  const Imm *imm;
  uint8_t rexw;

  prefix = (instr->variant % 4) == 1 ? 0x66 : -1;
  rexw = (instr->variant % 4) == 3;

  if (instr->variant < 4) {
    imm = &instr->arg1->imm;
    assemblevbytes(prefix);
    if (rexw)
      sb(rexbyte(1, 0, 0, 0));
    assemblevbytes(opcode);
    assemblereloc(imm->v.l, imm->v.c, imm->nbytes, R_X86_64_32);
  } else if (instr->variant < 12) {
    assembleimmrm(instr, rexw, prefix, opcode, immreg);
  } else {
    assemblerrm(instr, prefix, opcode, 0);
  }
}

static void assemblexchg(const Instr *xchg) {
  VarBytes prefix, opcode;
  static uint8_t variant2op[18] = {0x90, 0x90, 0x90, 0x90, 0x90, 0x90,
                                   0x86, 0x87, 0x87, 0x87, 0x86, 0x87,
                                   0x87, 0x87, 0x86, 0x87, 0x87, 0x87};
  opcode = variant2op[xchg->variant];
  if (xchg->variant < 6) {
    AsmKind reg = (xchg->variant % 2) ? xchg->arg1->kind : xchg->arg2->kind;
    prefix = (xchg->variant < 2) ? 0x66 : -1;
    assembleplusr(isreg64(reg), prefix, opcode, regbits(reg));
  } else {
    prefix = (((xchg->variant - 6) % 4) == 1) ? 0x66 : -1;
    assemblerrm(xchg, prefix, opcode, 0);
  }
}

static void assemblemov(const Instr *mov) {
  const Imm *imm;
  VarBytes prefix, opcode;
  uint8_t rexw;

  static uint8_t variant2op[20] = {0x88, 0x89, 0x89, 0x89, 0x88, 0x89, 0x89,
                                   0x89, 0x8a, 0x8b, 0x8b, 0x8b, 0xc6, 0xc7,
                                   0xc7, 0xc7, 0xb0, 0xb8, 0xb8, 0xc7};

  prefix = ((mov->variant % 4) == 1) ? 0x66 : -1;
  opcode = variant2op[mov->variant];

  if (mov->variant < 12) {
    assemblerrm(mov, prefix, opcode, 0);
  } else if (mov->variant < 16) {
    rexw = ((mov->variant % 4) == 3);
    assembleimmrm(mov, rexw, prefix, opcode, 0x00);
  } else if (mov->variant == 19) {
    uint8_t reg, rex, rm;
    uint64_t mask, maskedc;

    imm = &mov->arg1->imm;
    rexw = 1;
    mask = 0xffffffff80000000;
    maskedc = ((uint64_t)imm->v.c) & mask;

    if ((maskedc == mask || maskedc == 0) && imm->v.l == NULL) {
      /* Sign extension works for this value, regular mov */
      rm = regbits(mov->arg2->kind);
      rex = rexbyte(rexw, 0, 0, rm & (1 << 3));
      assemblemodregrm(rex, prefix, opcode, 0x03, 0x00, rm);
      assemblereloc(imm->v.l, imm->v.c, 4, R_X86_64_32);
    } else {
      /* 64 bit immediate required. */
      reg = regbits(mov->arg2->kind);
      rex = rexbyte(rexw, 0, 0, reg & (1 << 3));
      sb2(rex, 0xb8 | (reg & 7));
      assemblereloc(imm->v.l, imm->v.c, 8, R_X86_64_64);
    }
  } else {
    imm = &mov->arg1->imm;
    assembleplusr(isreg64(mov->arg2->kind), prefix, opcode,
                  regbits(mov->arg2->kind));
    assemblereloc(imm->v.l, imm->v.c, imm->nbytes, R_X86_64_32);
  }
}

static void assemblemovextend(const Instr *mov, VarBytes opcode) {
  VarBytes prefix;
  if (mov->variant == 0 || mov->variant == 5)
    prefix = 0x66;
  else
    prefix = -1;
  assemblerrm(mov, prefix, opcode, 1);
}

static void assembledivmulneg(const Instr *instr, uint8_t reg) {
  VarBytes prefix, opcode;
  uint8_t rexw, rex, rm;

  rexw = (instr->variant % 4) == 3;
  prefix = (instr->variant % 4) == 1 ? 0x66 : -1;
  opcode = (instr->variant % 4) == 1 ? 0xf6 : 0xf7;

  if (instr->arg1->kind == ASM_MEMARG) {
    assemblemem(&instr->arg1->memarg, rexw, prefix, opcode, reg);
  } else {
    rex = rexbyte(rexw, reg & (1 << 3), 0, 0);
    rm = regbits(instr->arg1->kind);
    assemblemodregrm(rex, prefix, opcode, 0x03, reg, rm);
  }
}

static void assembleshift(const Instr *instr, uint8_t immreg) {
  VarBytes prefix, opcode;
  uint8_t rexw, rex, rm;

  opcode = (instr->variant < 6) ? 0xd3 : 0xc1;
  rexw = (instr->variant % 3) == 2;
  prefix = (instr->variant % 3) == 0 ? 0x66 : -1;

  if (instr->arg1->kind == ASM_IMM) {
    assembleimmrm(instr, rexw, prefix, opcode, immreg);
  } else if (instr->arg2->kind == ASM_MEMARG) {
    assemblemem(&instr->arg2->memarg, rexw, prefix, opcode, immreg);
  } else {
    rexw = isreg64(instr->arg2->kind);
    rm = regbits(instr->arg2->kind);
    rex = rexbyte(rexw, immreg & (1 << 3), 0, rm & (1 << 3));
    assemblemodregrm(rex, prefix, opcode, 0x03, immreg, rm);
  }
}

static void assemblemovsmmx(const Instr *instr, VarBytes prefix) {
  VarBytes opcode;
  if (instr->variant == 2) {
    opcode = 0x01000f11;
    assemblerrm(instr, prefix, opcode, 0);
  } else {
    opcode = 0x01000f10;
    assemblerrm(instr, prefix, opcode, 1);
  }
}

static void assembletest(const Instr *instr) {
  VarBytes prefix;
  uint8_t rexw, byteop;

  byteop = ((instr->variant % 4) == 0);
  prefix = ((instr->variant % 4) == 1) ? 0x66 : -1;
  rexw = ((instr->variant % 4) == 3);

  if (instr->variant < 4) {
    const Imm *imm;
    assemblevbytes(prefix);
    if (rexw)
      sb(rexbyte(1, 0, 0, 0));
    assemblevbytes(byteop ? 0xa8 : 0xa9);
    imm = &instr->arg1->imm;
    assemblereloc(imm->v.l, imm->v.c, imm->nbytes, R_X86_64_32);
  } else if (instr->variant < 12) {
    assembleimmrm(instr, rexw, prefix, byteop ? 0xf6 : 0xf7, 0);
  } else {
    assemblerrm(instr, prefix, byteop ? 0x84 : 0x85, 0);
  }
}

static void assembleset(const Instr *instr) {
  VarBytes prefix, opcode;
  uint8_t rex, reg, rm;
  static uint8_t variant2op[30] = {
      0x94, 0x98, 0x9b, 0x9a, 0x9a, 0x90, 0x95, 0x99, 0x9b, 0x91,
      0x9f, 0x9d, 0x9c, 0x9e, 0x95, 0x93, 0x97, 0x93, 0x92, 0x96,
      0x9e, 0x9c, 0x9d, 0x9f, 0x94, 0x92, 0x96, 0x92, 0x93, 0x97,
  };
  opcode = 0x01000f00 | variant2op[instr->variant % 31];
  prefix = -1;
  if (instr->arg1->kind == ASM_MEMARG) {
    assemblemem(&instr->arg1->memarg, 0, prefix, opcode, 0);
  } else {
    reg = 0;
    rm = regbits(instr->arg1->kind);
    rex = rexbyte(isreg64(instr->arg1->kind), reg & (1 << 3), 0, rm & (1 << 3));
    assemblemodregrm(rex, prefix, opcode, 0x03, reg, rm);
  }
}

static void assemble(void) {
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
      sym->global = 1;
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
      assemblereloc(v->dirbyte.value.l, v->dirbyte.value.c, 1, R_X86_64_32);
      break;
    case ASM_DIR_SHORT:
      assemblereloc(v->dirshort.value.l, v->dirshort.value.c, 2, R_X86_64_32);
      break;
    case ASM_DIR_INT:
      assemblereloc(v->dirint.value.l, v->dirint.value.c, 4, R_X86_64_32);
      break;
    case ASM_DIR_QUAD:
      assemblereloc(v->dirquad.value.l, v->dirquad.value.c, 8, R_X86_64_64);
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
      uint8_t rm, rex;

      if (v->call.indirect) {
        if (v->call.target.indirect->kind == ASM_MEMARG) {
          assemblemem(&v->call.target.indirect->memarg, 0, -1, 0xff, 0x02);
        } else {
          rm = regbits(v->call.target.indirect->kind);
          rex = rexbyte(0, 0, 0, rm & (1 << 3));
          assemblemodregrm(rex, -1, 0xff, 0x03, 0x02, rm);
        }
      } else {
        sb(0xe8);
        assemblereloc(v->call.target.direct.l, v->call.target.direct.c - 4, 4,
                      R_X86_64_PC32);
      }
      break;
    }
    case ASM_JMP: {
      static uint8_t variant2op[31] = {
          0xe9, 0x84, 0x88, 0x8b, 0x8a, 0x8a, 0x80, 0x85, 0x89, 0x8b, 0x81,
          0x8f, 0x8d, 0x8c, 0x8e, 0x85, 0x83, 0x87, 0x83, 0x82, 0x86, 0x8e,
          0x8c, 0x8d, 0x8f, 0x84, 0x82, 0x86, 0x82, 0x83, 0x87,
      };
      if (v->jmp.variant)
        sb(0x0f);
      sb(variant2op[v->jmp.variant]);

      assemblereloc(v->jmp.target, -4, 4, R_X86_64_PC32);
      break;
    }
    case ASM_PUSH:
      if (v->instr.arg1->kind == ASM_MEMARG) {
        assemblemem(&v->instr.arg1->memarg, 0, -1, 0xff, 0x06);
      } else {
        assembleplusr(0, -1, 0x50, regbits(v->instr.arg1->kind));
      }
      break;
    case ASM_POP:
      if (v->instr.arg1->kind == ASM_MEMARG) {
        assemblemem(&v->instr.arg1->memarg, 0, -1, 0x8f, 0x00);
      } else {
        assembleplusr(0, -1, 0x58, regbits(v->instr.arg1->kind));
      }
      break;
    case ASM_NOP:
      sb(0x90);
      break;
    case ASM_LEAVE:
      sb(0xc9);
      break;
    case ASM_CLTD:
      sb(0x99);
      break;
    case ASM_CQTO:
      sb2(0x48, 0x99);
      break;
    case ASM_CVTSI2SD:
      assemblerrm(&v->instr, 0xf2, 0x01000f2a, 1);
      break;
    case ASM_CVTSI2SS:
      assemblerrm(&v->instr, 0xf3, 0x01000f2a, 1);
      break;
    case ASM_CVTSS2SD:
      assemblerrm(&v->instr, 0xf3, 0x01000f5a, 1);
      break;
    case ASM_CVTSD2SS:
      assemblerrm(&v->instr, 0xf2, 0x01000f5a, 1);
      break;
    case ASM_CVTTSD2SI:
      assemblerrm(&v->instr, 0xf2, 0x01000f2c, 1);
      break;
    case ASM_CVTTSS2SI:
      assemblerrm(&v->instr, 0xf3, 0x01000f2c, 1);
      break;
    case ASM_RET:
      sb(0xc3);
      break;
    case ASM_LEA: {
      VarBytes prefix;
      prefix = v->instr.variant == 0 ? 0x66 : -1;
      assemblerrm(&v->instr, prefix, 0x8d, 0);
      break;
    }
    case ASM_MOVAPS: {
      VarBytes prefix, opcode;
      prefix = -1;
      opcode = v->instr.variant == 2 ? 0x01000f29 : 0x01000f28;
      assemblerrm(&v->instr, prefix, opcode, 1);
      break;
    }
    case ASM_MOV:
      assemblemov(&v->instr);
      break;
    case ASM_MOVSD:
      assemblemovsmmx(&v->instr, 0xf2);
      break;
    case ASM_MOVSS:
      assemblemovsmmx(&v->instr, 0xf3);
      break;
    case ASM_MOVSX: {
      VarBytes opcode;
      if (v->instr.variant >= 10) {
        opcode = 0x63; // movsxd
      } else {
        static uint8_t variant2op[10] = {0xbe, 0xbe, 0xbe, 0xbf, 0xbf,
                                         0xbe, 0xbe, 0xbe, 0xbf, 0xbf};
        opcode = 0x01000f00 | variant2op[v->instr.variant];
      }
      assemblemovextend(&v->instr, opcode);
      break;
    }
    case ASM_MOVZX: {
      VarBytes opcode;
      static uint8_t variant2op[10] = {0xb6, 0xb6, 0xb6, 0xb7, 0xb7,
                                       0xb6, 0xb6, 0xb6, 0xb7, 0xb7};
      opcode = 0x01000f00 | variant2op[v->instr.variant];
      assemblemovextend(&v->instr, opcode);
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
    case ASM_CMP: {
      static uint8_t variant2op[24] = {
          0x3c, 0x3d, 0x3d, 0x3d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x3a, 0x3b, 0x3b, 0x3b,
          0x38, 0x39, 0x39, 0x39, 0x38, 0x39, 0x39, 0x39,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x07);
      break;
    }
    case ASM_DIV:
      assembledivmulneg(&v->instr, 0x06);
      break;
    case ASM_IDIV:
      assembledivmulneg(&v->instr, 0x07);
      break;
    case ASM_MUL:
      assembledivmulneg(&v->instr, 0x04);
      break;
    case ASM_MULSD:
      assemblerrm(&v->instr, 0xf2, 0x01000f59, 1);
      break;
    case ASM_DIVSD:
      assemblerrm(&v->instr, 0xf2, 0x01000f5e, 1);
      break;
    case ASM_MULSS:
      assemblerrm(&v->instr, 0xf3, 0x01000f59, 1);
      break;
    case ASM_IMUL: {
      VarBytes prefix, opcode;

      if (v->instr.variant < 8) {
        assembledivmulneg(&v->instr, 0x05);
      } else if (v->instr.variant < 14) {
        opcode = 0x01000faf;
        prefix = ((v->instr.variant - 8) % 3) == 0 ? 0x66 : -1;
        assemblerrm(&v->instr, prefix, opcode, 0);
      } else {
        const Imm *imm;
        imm = &v->instr.arg3->imm;
        opcode = 0x69;
        prefix = ((v->instr.variant - 14) % 3) == 0 ? 0x66 : -1;
        assemblerrm(&v->instr, prefix, opcode, 1);
        assemblereloc(imm->v.l, imm->v.c, imm->nbytes, R_X86_64_32);
      }
      break;
    }
    case ASM_NEG:
      assembledivmulneg(&v->instr, 0x03);
      break;
    case ASM_OR: {
      static uint8_t variant2op[24] = {
          0x0c, 0x0d, 0x0d, 0x0d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x0a, 0x0b, 0x0b, 0x0b,
          0x08, 0x09, 0x09, 0x09, 0x08, 0x09, 0x09, 0x09,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x01);
      break;
    }
    case ASM_PXOR: {
      assemblerrm(&v->instr, 0x66, 0x01000fef, 1);
      break;
    }
    case ASM_SET:
      assembleset(&v->instr);
      break;
    case ASM_SAL:
      /* fallthrough */
    case ASM_SHL:
      assembleshift(&v->instr, 0x04);
      break;
    case ASM_SAR:
      assembleshift(&v->instr, 0x07);
      break;
    case ASM_SHR:
      assembleshift(&v->instr, 0x05);
      break;
    case ASM_SUB: {
      static uint8_t variant2op[24] = {
          0x2c, 0x2d, 0x2d, 0x2d, 0x80, 0x81, 0x81, 0x81,
          0x80, 0x81, 0x81, 0x81, 0x2a, 0x2b, 0x2b, 0x2b,
          0x28, 0x29, 0x29, 0x29, 0x28, 0x29, 0x29, 0x29,
      };
      assemblebasicop(&v->instr, variant2op[v->instr.variant], 0x05);
      break;
    }
    case ASM_TEST:
      assembletest(&v->instr);
      break;
    case ASM_UCOMISD:
      assemblerrm(&v->instr, 0x66, 0x01000f2e, 1);
      break;
    case ASM_UCOMISS:
      assemblerrm(&v->instr, -1, 0x01000f2e, 1);
      break;
    case ASM_XORPD:
      assemblerrm(&v->instr, 0x66, 0x01000f57, 1);
      break;
    case ASM_XORPS:
      assemblerrm(&v->instr, -1, 0x01000f57, 1);
      break;
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
    default:
      lfatal("assemble: unexpected kind: %d", v->kind);
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
  secaddbytes(symtab, &elfsym, sizeof(Elf64_Sym));
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

static int resolvereloc(Relocation *reloc) {
  Symbol *sym;
  uint8_t *rdata;
  int64_t value;

  sym = reloc->sym;

  if (sym->section != reloc->section)
    return 0;

  switch (reloc->type) {
  case R_X86_64_32:
  case R_X86_64_64:
    return 0;
  case R_X86_64_PC32: {
    rdata = &reloc->section->data[reloc->offset];
    value = sym->offset - reloc->offset + reloc->addend;
    rdata[0] = ((uint32_t)value & 0xff);
    rdata[1] = ((uint32_t)value & 0xff00) >> 8;
    rdata[2] = ((uint32_t)value & 0xff0000) >> 16;
    rdata[3] = ((uint32_t)value & 0xff000000) >> 24;
    return 1;
  }
  default:
    unreachable();
    return 0;
  }
}

static void appendreloc(Relocation *reloc) {
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

static void handlerelocs(void) {
  Relocation *reloc;
  size_t i;
  for (i = 0; i < nrelocs; i++) {
    reloc = &relocs[i];
    if (resolvereloc(reloc))
      continue;
    appendreloc(reloc);
  }
}

static void out(const void *buf, size_t n) {
  fwrite(buf, 1, n, stdout);
  if (ferror(stdout))
    fatal("fwrite:");
}

static void outelf(void) {
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

static void usage(char *argv0) {
  fprintf(stderr, "usage: %s [-o out] [input]\n", argv0);
  exit(2);
}

static void parseargs(int argc, char *argv[]) {
  char *a, *argv0, *outfname;

  argv0 = argv[0];

  for (++argv; *argv; argv++) {
    if (argv[0][0] != '-')
      break;
    for (a = &argv[0][1]; *a; a++) {
      switch (*a) {
      case 'h':
        usage(argv0);
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
  }

  if (argv[0]) {
    if (argv[1])
      usage(argv0);
    infilename = argv[0];
    if (!freopen(infilename, "r", stdin))
      fatal("unable to open %s:", infilename);
  }
}

int main(int argc, char *argv[]) {
  symbols = mkhtab(256);
  parseargs(argc, argv);
  allasm = parseasm();
  initsections();
  assemble();
  fillsymtab();
  handlerelocs();
  outelf();
  return 0;
}