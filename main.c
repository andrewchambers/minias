#include "dumbas.h"

AsmLine *allasm = NULL;

// Label worst case offsets
struct hashtable *labelwco = NULL;

#define MAXSECTIONS 32
static Section sections[MAXSECTIONS];
static size_t nsections = 1; // first is reserved.

static Section *cursection = NULL;
static Section *shstrtab = NULL;
static Section *strtab = NULL;
static Section *symtab = NULL;
static Section *bss = NULL;
static Section *text = NULL;

static void secbytes(Section *s, uint8_t *bytes, size_t n) {
  while (s->capacity < s->hdr.sh_size + n) {
    s->capacity = s->capacity ? (s->capacity * 2) : 64;
    s->data = xrealloc(s->data, s->capacity);
  }
  memcpy(s->data + s->hdr.sh_size, bytes, n);
  s->hdr.sh_size += n;
}

static void secbyte(Section *s, uint8_t b) { secbytes(s, &b, 1); }

static Elf64_Word elfstr(Section *sec, const char *s) {
  Elf64_Word i;
  for (i = 0; i < sec->hdr.sh_size; i++) {
    if (i == 0 || (sec->data[i - 1] == 0))
      if (strcmp(s, (char *)&sec->data[i]) == 0)
        return i;
  }
  secbytes(sec, (uint8_t *)s, strlen(s) + 1);
  return i;
}

/*
static Elf64_sym *getsym(const char *name) {
  const char *symname;
  Elf64_sym newsym, *sym;
  Elf64_Word i, nsyms;

  nsyms = symtab->hdr.sh_size / sizeof(Elf64_sym);
  for (i = 0; i < nsyms; i++) {
    sym = (Elf64_sym*)symtab->data + i;
    symname = (char*)strtab->data + sym->name;
    if (strcmp(symname, name) == 0)
      return sym;
  }

  return NULL;
}
*/

static Section *newsection() {
  Section *s;
  s = &sections[nsections++];
  if (nsections > MAXSECTIONS)
    fatal("too many sections");
  return s;
}

static void initsections(void) {
  shstrtab = newsection();
  secbyte(shstrtab, 0);
  shstrtab->hdr.sh_name = elfstr(shstrtab, ".shstrtab");
  shstrtab->hdr.sh_type = SHT_STRTAB;
  shstrtab->hdr.sh_entsize = 1;

  strtab = newsection();
  secbyte(strtab, 0);
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

  cursection = text;
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
    l = xmalloc(sizeof(AsmLine));
    l->v = v;
    l->lineno = lineno;
    l->next = NULL;
    if (prevl)
      prevl->next = l;
    else
      allasm = l;
    prevl = l;
    lineno += 1;
  } while (more);

  asmparser_destroy(ctx);
}

/* Compute worst case offsets in the text section
   once we have worst case offsets we can compute
   worst case jump distances. */
static void computewco() {
  Parsev *v;
  AsmLine *l;
  const char *label;
  struct hashtablekey htk;
  int64_t offset = 0;

  for (l = allasm; l; l = l->next) {
    l->wco = offset;
    v = &l->v;
    switch (v->kind) {
    case ASM_LABEL:
      label = v->label.value;
      htabkey(&htk, label, strlen(label));
      *((int64_t *)htabput(labelwco, &htk)) = offset;
      break;
    case ASM_NOP:
    case ASM_RET:
      offset += 1;
      break;
    case ASM_JMP:
      offset += 5;
      break;
    default:
      fatal("unexpected kind: %d", v->kind);
    }
  }
}

static int64_t pessimisticdistance(AsmLine *line, const char *label) {
  struct hashtablekey htk;
  htabkey(&htk, label, strlen(label));
  return (int64_t)htabget(labelwco, &htk) - line->wco;
}

static void assemble() {
  Parsev *v;
  AsmLine *l;

  for (l = allasm; l; l = l->next) {
    v = &l->v;
    switch (l->v.kind) {
    case ASM_LABEL:
      break;
    case ASM_NOP:
      secbyte(cursection, 0x90);
      break;
    case ASM_RET:
      secbyte(cursection, 0xc3);
      break;
    case ASM_JMP: {
      int64_t distance;
      distance = pessimisticdistance(l, v->instr.jmp.target);
      if (distance <= 128 && distance >= -127) {
        uint8_t jbuf[2] = {0xeb, 0x00};
        secbytes(cursection, jbuf, sizeof(jbuf));
      } else {
        uint8_t jbuf[5] = {0xe9, 0x00, 0x00, 0x00, 0x00};
        secbytes(cursection, jbuf, sizeof(jbuf));
      }
      break;
    }
    default:
      fatal("unexpected kind: %d", l->v.kind);
    }
  }
}

int main(void) {
  labelwco = mkhtab(256);
  initsections();
  parse();
  computewco();
  assemble();
  outelf();
  return 0;
}