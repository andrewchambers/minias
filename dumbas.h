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

typedef enum {
  // Misc
  ASM_SYNTAX_ERROR,
  ASM_BLANK,
  ASM_LABEL,
  ASM_IDENT,
  ASM_NUMBER,
  // Directives
  ASM_DIR_GLOBL,
  ASM_DIR_DATA,
  ASM_DIR_TEXT,
  ASM_DIR_BYTE,
  ASM_DIR_BALIGN,
  // Instructions
  ASM_NOP,
  ASM_RET,
  ASM_JMP,
  ASM_LEAVE,
  ASM_PUSHQ,
  ASM_MOVQ,
  ASM_XORL,
  // Registers, order matters.
  ASM_EAX,
  ASM_ECX,
  ASM_EDX,
  ASM_EBX,
  ASM_ESP,
  ASM_EBP,
  ASM_ESI,
  ASM_EDI,
  ASM_RAX,
  ASM_RCX,
  ASM_RDX,
  ASM_RBX,
  ASM_RSP,
  ASM_RBP,
  ASM_RSI,
  ASM_RDI,
} AsmKind;

static int isr64kind(AsmKind k) {
  return k >= ASM_RAX && k <= ASM_RDI;
}

static int isr32kind(AsmKind k) {
  return k >= ASM_EAX && k <= ASM_EDI;
}

typedef union Parsev Parsev;

typedef struct {
  AsmKind kind;
  const char *target;
} Jmp;

typedef struct {
  AsmKind kind;
  Parsev *arg;
} Pushq;

typedef struct {
  AsmKind kind;
  Parsev *src;
  Parsev *dst;
} Movq;

typedef struct {
  AsmKind kind;
  Parsev *src;
  Parsev *dst;
} Xorl;

typedef struct {
  AsmKind kind;
  const char *name;
} Label;

typedef struct {
  AsmKind kind;
  const char *name;
} Globl;

typedef struct {
  AsmKind kind;
  uint8_t b;
} Byte;

typedef struct {
  AsmKind kind;
  uint64_t align;
} Balign;

typedef struct {
  AsmKind kind;
  int64_t imm;
} Imm;

typedef struct {
  AsmKind kind;
  const char *name;
} Ident;

typedef struct {
  AsmKind kind;
  int64_t value;
} Number;

union Parsev {
  AsmKind kind;
  Label label;
  Globl globl;
  Balign balign;
  Jmp jmp;
  Pushq pushq;
  Movq movq;
  Xorl xorl;
  Byte byte;
  Ident ident;
  Number number;
};

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