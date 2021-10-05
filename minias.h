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
  int64_t offset;
  int64_t size;
  int global;
  Section *section;
} Symbol;

typedef enum {
  // Misc
  ASM_SYNTAX_ERROR,
  ASM_BLANK,
  ASM_LABEL,
  ASM_IMM,
  ASM_IDENT,
  ASM_NUMBER,
  ASM_MEMARG,
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
  ASM_ADD,
  ASM_AND,
  ASM_OR,
  ASM_SUB,
  ASM_XOR,
  // Registers, order matters.
  ASM_REG_BEGIN,
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
  ASM_R8,
  ASM_R9,
  ASM_R10,
  ASM_R11,
  ASM_R12,
  ASM_R13,
  ASM_R14,
  ASM_R15,
  ASM_REG_END,
} AsmKind;

typedef union Parsev Parsev;

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
  const char *l; /* label */
  int64_t c;     /* constant */
} Imm;

typedef struct {
  AsmKind kind;
  AsmKind reg;
  const char *l; /* label */
  int64_t c;     /* constant */
} Memarg;

typedef struct {
  AsmKind kind;
  const char *name;
} Ident;

typedef struct {
  AsmKind kind;
  int64_t v;
} Number;

typedef struct {
  AsmKind kind;
  const char *target;
} Jmp;

typedef struct {
  AsmKind kind;
  char type;
  Parsev *src;
  Parsev *dst;
} ModRMBinop;

typedef ModRMBinop Add;
typedef ModRMBinop And;
typedef ModRMBinop Or;
typedef ModRMBinop Sub;
typedef ModRMBinop Xor;

union Parsev {
  AsmKind kind;
  Label label;
  Globl globl;
  Balign balign;
  Memarg memarg;
  ModRMBinop modrmbinop;
  Add add;
  And and;
  Or or;
  Xor xor;
  Sub sub;
  Jmp jmp;
  Byte byte;
  Imm imm;
  Ident ident;
  Number number;
};

typedef struct AsmLine AsmLine;
struct AsmLine {
  int64_t lineno;
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