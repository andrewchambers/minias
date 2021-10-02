#include "minias.h"

typedef struct {
  Elf64_Shdr hdr;
  size_t capacity;
  uint8_t *data;
} Section;

#define MAXSECTIONS 32
static Section sections[MAXSECTIONS];
static size_t nsections = 0;

static Section *strtab = NULL;
static Section *symtab = NULL;
static Section *textsec = NULL;

void secappend(Section *s, uint8_t *bytes, size_t n) {
  while (s->capacity < s->hdr.sh_size + n) {
    s->capacity = s->capacity * 2;
    s->data = xrealloc(s->data, s->capacity);
  }
  memcpy(s->data + s->hdr.sh_size, bytes, n);
  s->hdr.sh_size += n;
}

Elf64_Word elfstr(const char *s) {
  Elf64_Word i;
  for (i = 0; i < strtab->hdr.sh_size; i++) {
    if (i == 0 || (strtab->data[i - 1] == 0))
      if (strcmp(s, (char *)&strtab->data[i]) == 0)
        return i;
  }
  secappend(strtab, (uint8_t *)s, strlen(s) + 1);
  return i;
}

Section *newsection(const char *name) {
  Section *s;
  s = &sections[nsections++];
  if (nsections > MAXSECTIONS)
    die("too many sections");
  s->hdr.sh_name = elfstr(name);
  s->capacity = 16;
  s->data = xmalloc(s->capacity);
  return s;
}

void initsections(void) {
  /* Manually init string table */
  strtab = &sections[nsections++];
  strtab->hdr.sh_type = SHT_STRTAB;
  strtab->capacity = 16;
  strtab->data = xmalloc(strtab->capacity);
  strtab->hdr.sh_name = elfstr(".strtab");
  strtab->hdr.sh_entsize = 1;

  symtab = newsection(".symtab");
  symtab->hdr.sh_type = SHT_SYMTAB;
  symtab->hdr.sh_link = 1;
  symtab->hdr.sh_entsize = sizeof(Elf64_Sym);

  textsec = newsection(".text");
  textsec->hdr.sh_type = SHT_PROGBITS;
  textsec->hdr.sh_flags = SHF_EXECINSTR | SHF_ALLOC;
  textsec->hdr.sh_entsize = 1;
}

void out(uint8_t *buf, size_t n) {
  if (write(STDOUT_FILENO, buf, n) != n)
    die("io error");
}

void outelf(void) {
  size_t i;
  uint64_t offset = 0;
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
  ehdr.e_shentsize = sizeof(Elf64_Shdr);
  ehdr.e_shoff = sizeof(Elf64_Ehdr);
  ehdr.e_shnum = nsections;
  ehdr.e_shstrndx = 1;
  offset = sizeof(Elf64_Shdr) * nsections;

  out((uint8_t *)&ehdr, sizeof(ehdr));
  for (i = 0; i < nsections; i++) {
    sections[i].hdr.sh_offset = offset;
    out((uint8_t *)&sections[i].hdr, sizeof(Elf64_Shdr));
    offset += sections[i].hdr.sh_size;
  }
  for (i = 0; i < nsections; i++) {
    out(sections[i].data, sections[i].hdr.sh_size);
  }
}

int main() {
  initsections();
  outelf();
  return 0;
}