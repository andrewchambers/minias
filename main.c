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

static Symbol *getsym(const char *label) {
  Symbol **ps, *s;
  struct hashtablekey htk;

  htabkey(&htk, label, strlen(label));
  ps = (Symbol **)htabput(symbols, &htk);
  if (!*ps) {
    *ps = zalloc(sizeof(Symbol));
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
  s = &sections[nsections++];
  if (nsections > MAXSECTIONS)
    fatal("too many sections");
  return s;
}

static void initsections(void) {
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

  bss = newsection();
  bss->hdr.sh_name = elfstr(shstrtab, ".bss");
  bss->hdr.sh_type = SHT_NOBITS;
  bss->hdr.sh_flags = SHF_ALLOC | SHF_WRITE;
  bss->hdr.sh_entsize = 1;
  bss->hdr.sh_addralign = 16; // XXX right value?

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
static void prepass() {
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
    case ASM_LABEL:
      label = v->label.name;
      sym = getsym(label);
      sym->section = cursection;
      sym->wco = cursection->wco;
      break;
    case ASM_NOP:
    case ASM_RET:
      cursection->wco += 1;
      break;
    case ASM_JMP:
      cursection->wco += 5;
      break;
    default:
      fatal("unexpected kind: %d", v->kind);
    }
  }
}

static int64_t pessimisticdistance(AsmLine *line, Symbol *s) {
  if (!s->section)
    fatal("bug: symbol has unknown section.");
  return s->wco - line->wco;
}

static void assemble() {
  Symbol *sym;
  Parsev *v;
  AsmLine *l;
  Section *cursection;

  cursection = text;

  for (l = allasm; l; l = l->next) {
    v = &l->v;
    switch (l->v.kind) {
    case ASM_DIR_GLOBL:
    case ASM_LABEL:
      break;
    case ASM_NOP:
      secaddbyte(cursection, 0x90);
      break;
    case ASM_RET:
      secaddbyte(cursection, 0xc3);
      break;
    case ASM_JMP: {
      sym = getsym(v->instr.jmp.target);
      if (sym->section && (sym->section == cursection)) {
        int64_t distance;
        distance = sym->wco - cursection->wco;
        if (distance <= 128 && distance >= -127) {
          uint8_t jbuf[2] = {0xeb, 0x00};
          secaddbytes(cursection, jbuf, sizeof(jbuf));
        } else {
          uint8_t jbuf[5] = {0xe9, 0x00, 0x00, 0x00, 0x00};
          secaddbytes(cursection, jbuf, sizeof(jbuf));
        }
      } else {
        fatal("TODO, jmp to undefined symbol");
      }
      break;
    }
    default:
      fatal("unexpected kind: %d", l->v.kind);
    }
  }
}

int main(void) {
  symbols = mkhtab(256);
  initsections();
  parse();
  prepass();
  assemble();
  outelf();
  return 0;
}