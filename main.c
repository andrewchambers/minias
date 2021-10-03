#include "dumbas.h"

AsmLine *allasm = NULL;

// Symbols in memory before
// writing out the symtab section.
struct hashtable *symbols = NULL;

#define MAXSECTIONS 32
static Section sections[MAXSECTIONS];
static size_t nsections = 1; // first is reserved.

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

static void out(uint8_t *buf, size_t n) {
  if (write(STDOUT_FILENO, buf, n) != n)
    fatal("io error");
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
    lineno += 1;
  } while (more);

  asmparser_destroy(ctx);
}

/*
   First pass deals with finding the symbol information
   and computing the worst case offsets for each instruction
   and symbol.
*/
static void prepass(void) {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;
  Section *cursection;
  const char *label;
  struct hashtablekey htk;

  cursection = text;

  for (l = allasm; l; l = l->next) {
    v = &l->v;
    l->wco = cursection->wco;
    switch (v->kind) {
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
    case ASM_LABEL:
      label = v->label.name;
      sym = getsym(label);
      sym->section = cursection;
      sym->wco = cursection->wco;
      break;
    case ASM_DIR_BYTE:
    case ASM_NOP:
    case ASM_RET:
      cursection->wco += 1;
      break;
    case ASM_MOVQ:
      if (isr64kind(v->movq.src->kind) && isr64kind(v->movq.dst->kind)) {
        cursection->wco += 2;
      } else {
        cursection->wco += 16; // XXX likely wrong.
      }
      break;
    case ASM_PUSHQ:
      if (isr64kind(v->pushq.arg->kind)) {
        cursection->wco += 2;
      } else {
        cursection->wco += 9; // XXX very pessimistic.
      }
      break;
    case ASM_JMP:
      cursection->wco += 5;
      break;
    default:
      fatal("prepass: unexpected kind: %d", v->kind);
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

#define MODREGI 0x3
#define REX_W 0x48

static uint8_t kindr64bits(AsmKind k) {
  return (k - ASM_RAX) & 0xff;
}

static uint8_t composemodrm(uint8_t mod, uint8_t regop,  uint8_t rm) {
  return (mod<<6) + (regop<<3) + rm;
}


static void assemble() {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;
  Section *cursection;
  const char *label;

  cursection = text;

  for (l = allasm; l; l = l->next) {
    v = &l->v;
    switch (l->v.kind) {
    case ASM_DIR_GLOBL:
      break;
    case ASM_DIR_DATA:
      cursection = data;
      break;
    case ASM_DIR_TEXT:
      cursection = text;
      break;
    case ASM_DIR_BYTE:
      secaddbyte(cursection, v->byte.b);
      break;
    case ASM_LABEL:
      label = v->label.name;
      sym = getsym(label);
      sym->offset = cursection->hdr.sh_size;
      break;
    case ASM_NOP:
      secaddbyte(cursection, 0x90);
      break;
    case ASM_RET:
      secaddbyte(cursection, 0xc3);
      break;
    case ASM_PUSHQ: {
      Parsev *arg;

      arg = v->pushq.arg;

      if (isr64kind(arg->kind)) {
        uint8_t ibuf[2] = {0x50, kindr64bits(arg->kind)};
        secaddbytes(cursection, ibuf, sizeof(ibuf));
      } else if (arg->kind == ASM_NUMBER) {
        fatal("TODO");
      } else if (arg->kind == ASM_IDENT) {
        fatal("TODO");
      } else {
        fatal("BUG: unexpected pushq arg");
      }

      break;
    }

    case ASM_MOVQ: {
      Parsev *src, *dst;

      src = v->movq.src;
      dst = v->movq.dst;

      if (isr64kind(src->kind) && isr64kind(dst->kind)) {
        uint8_t ibuf[3] = {
          REX_W,
          0x89,
          composemodrm(MODREGI, kindr64bits(src->kind), kindr64bits(dst->kind)),
        };
        secaddbytes(cursection, ibuf, sizeof(ibuf));
      } else {
        fatal("TODO");
      }
      break;
    }

    case ASM_JMP: {
      sym = getsym(v->jmp.target);
      if (sym->section && (sym->section == cursection)) {
        int64_t distance;
        distance = sym->wco - cursection->wco;
        if (distance <= 128 && distance >= -127) {
          uint8_t ibuf[2] = {0xeb, 0x00};
          secaddbytes(cursection, ibuf, sizeof(ibuf));
        } else {
          uint8_t ibuf[5] = {0xe9, 0x00, 0x00, 0x00, 0x00};
          secaddbytes(cursection, ibuf, sizeof(ibuf));
        }
      } else {
        fatal("TODO, jmp to undefined symbol");
      }
      break;
    }
    default:
      fatal("assemble: unexpected kind: %d", l->v.kind);
    }
  }
}

int main(void) {
  symbols = mkhtab(256);
  initsections();
  parse();
  prepass();
  assemble();
  fillsymtab();
  outelf();
  return 0;
}