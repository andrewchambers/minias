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
  int16_t idx;
  int64_t wco;
  int64_t offset;
  size_t capacity;
  uint8_t *data;
} Section;

typedef struct {
  const char *name;
  int64_t wco;
  int64_t offset;
  int64_t size;
  int global : 1;
  Section *section;
} Symbol;

enum AsmKind {
  ASM_SYNTAX_ERROR,
  ASM_BLANK,
  ASM_DIR_GLOBL,
  ASM_DIR_DATA,
  ASM_DIR_TEXT,
  ASM_DIR_BYTE,
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
  const char *name;
} Label;

typedef struct {
  enum AsmKind kind;
  const char *name;
} Globl;

typedef struct {
  enum AsmKind kind;
  uint8_t b;
} Byte;

typedef union {
  enum AsmKind kind;
  Instr instr;
  Label label;
  Globl globl;
  Byte byte;
  const char *ident;
  int64_t number;
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
void *zalloc(size_t n);


struct hashtable {
  size_t len, cap;
  struct hashtablekey *keys;
  void **vals;
};

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