#include <assert.h>
#include <elf.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef struct {
  Elf64_Shdr hdr;
  size_t capacity;
  uint8_t *data;
} Section;

enum AsmKind {
  ASM_SYNTAX_ERROR,
  ASM_BLANK,
  ASM_LABEL,
  ASM_NOP,
  ASM_RET,
  ASM_JMP
};

typedef struct {
  enum AsmKind kind;
  union {
    struct {
      const char *target;
    } jmp;
  };
} Instr;

typedef struct {
  enum AsmKind kind;
  const char *value;
} Label;

typedef union {
  enum AsmKind kind;
  Instr instr;
  Label label;
  const char *ident;
} Parsev;

typedef struct AsmLine AsmLine;
struct AsmLine {
  int64_t lineno;
  int64_t wco; // Worst case offset
  Parsev v;
  AsmLine *next;
};

/* util.c */

void fatal(const char *fmt, ...);

void *xmalloc(size_t);
void *xrealloc(void *, size_t);
void *xreallocarray(void *, size_t, size_t);
char *xmemdup(const char *, size_t);
char *xstrdup(const char *s);

struct hashtablekey {
  uint64_t hash;
  const char *str;
  size_t len;
};

void htabkey(struct hashtablekey *, const char *, size_t);
struct hashtable *mkhtab(size_t);
void delhtab(struct hashtable *, void(void *));
void **htabput(struct hashtable *, struct hashtablekey *);
void *htabget(struct hashtable *, struct hashtablekey *);
uint64_t murmurhash64a(const void *, size_t);