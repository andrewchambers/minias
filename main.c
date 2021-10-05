#include "dumbas.h"

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

static const char *dbg_str[] = {"Evaluating rule", "Matched rule",
                                "Abandoning rule"};
#define PCC_DEBUG(event, rule, level, pos, buffer, length)                     \
  fprintf(stderr, "%*s%s %s @%zu [%.*s]\n", (int)((level)*2), "",              \
          dbg_str[event], rule, pos, (int)(length), buffer)

#include "asmparser.c" // XXX resolve dependency cycle.

void parse(void) {
  int more;
  uint64_t lineno;
  Parsev v;
  AsmLine *l, *prevl;
  asmparser_context_t *ctx;

  ctx = asmparser_create(NULL);
  prevl = NULL;
  lineno = 0;

  do {
    more = asmparser_parse(ctx, &v);
    lineno += 1;
    if (v.kind == ASM_SYNTAX_ERROR) {
      fprintf(stderr, "<stdin>:%lu: syntax error\n", lineno);
      exit(1);
    }
    if (v.kind == ASM_BLANK)
      continue;
    l = zalloc(sizeof(AsmLine));
    l->v = v;
    l->lineno = lineno;
    if (prevl)
      prevl->next = l;
    else
      allasm = l;
    prevl = l;
  } while (more);

  asmparser_destroy(ctx);
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

/* Compose a ModR/M byte.  */
static uint8_t modrm(uint8_t mod, uint8_t regop, uint8_t rm) {
  return ((mod&3) << 6) | ((regop&7) << 3) | (rm&7);
}

/* Is one of the r$n style registers. */
static uint8_t isextr64(AsmKind k) {
  return k >= ASM_R8 && k <= ASM_R15;
}

/* Convert an ASM_KIND to register bits */
static uint8_t r64bits(AsmKind k) {
  if (isextr64(k)) {
    return (1<<4) | ((k - ASM_R8) & 0xff);
  } else {
    return (k - ASM_RAX) & 0xff;  
  }
}

static uint8_t r32bits(AsmKind k) { return (k - ASM_EAX) & 0xff; }

#define REX(W, R, X, B)                                                        \
  ((1 << 6) | (!!(W) << 3) | (!!(R) << 2) | (!!(X) << 1) | (!!(B) << 0))
#define REX_W REX(1, 0, 0, 0)

static void assemble() {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;
  const char *label;

  cursection = text;

  for (l = allasm; l; l = l->next) {
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
    case ASM_ADD: {
      Add *add = &v->add;

      switch (add->type) {
      case 'q':
        switch (add->src->kind) {
        case ASM_IMM:
          if (add->dst->kind == ASM_MEMARG) {
            uint8_t rbits = r64bits(add->dst->memarg.reg);
            uint8_t rex = REX(1,0,0,rbits&(1<<4));
            if ((rbits&7) == 4) {
              fatal("%d: cannot address destination register", l->lineno);
            }
            if (add->dst->memarg.c == 0 && add->dst->memarg.l == NULL) {
              if ((rbits&7) == 5) { /* BP style registers need displacement */
                sb4(rex, 0x81, modrm(0x01, 0x00, rbits), 0x00);
              } else {
                sb3(rex, 0x81, modrm(0x00, 0x00, rbits));
              }
            } else {
              fatal("TODO mem arg with disp");
            }
          } else {
            uint8_t rbits = r64bits(add->dst->kind);
            uint8_t rex = REX(1,0,0,rbits&(1<<4));
            sb3(rex, 0x81, modrm(0x03, 0x00, rbits));
          }
          sw(0);
          break;
        case ASM_MEMARG:
          fatal("TODO");
          break;
        default:
          switch (add->src->kind) {
          case ASM_MEMARG:
            fatal("TODO");
            break;
          default:
            sb3(REX_W, 0x03,
                modrm(0x03, r64bits(add->dst->kind), r64bits(add->src->kind)));
            break;
          }
          break;
        }
        break;
      default:
        fatal("unknown type");
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
      fatal("assemble: unexpected kind: %d", l->v.kind);
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

FILE *outf = NULL;

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