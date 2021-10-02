#include <elf.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

typedef struct {
  Elf64_Shdr hdr;
  size_t capacity;
  uint8_t *data;
} Section;

enum AsmKind {
  ASM_NOP,
  ASM_RET
};

typedef struct {
  enum AsmKind kind;
} Instr;

typedef union {
  enum AsmKind kind;
  Instr instr;
} Parsev;

static void die(char *s) {
  write(STDERR_FILENO, s, strlen(s));
  exit(1);
}

static void *xcalloc(size_t nmemb, size_t n) {
  void *p;
  p = calloc(nmemb, n);
  if (!p)
    die("out of memory");
  return p;
}

static void *xmalloc(size_t n) {
  void *p;
  p = malloc(n);
  if (!p)
    die("out of memory");
  return p;
}

static void *xrealloc(void *p, size_t n) {
  p = realloc(p, n);
  if (!p)
    die("out of memory");
  return p;
}