#include <assert.h>
#include <elf.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <ctype.h>
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
  int32_t idx;
  int64_t offset;
  int64_t size;
  int global;
  int defined;
  Section *section;
} Symbol;

typedef struct {
  Section *section;
  Symbol *sym;
  int type;
  int64_t offset;
} Relocation;

typedef enum {
  // Misc
  ASM_SYNTAX_ERROR,
  ASM_BLANK,
  ASM_LABEL,
  ASM_IMM,
  ASM_IDENT,
  ASM_NUMBER,
  ASM_STRING,
  ASM_MEMARG,
  // Directives
  ASM_DIR_GLOBL,
  ASM_DIR_SECTION,
  ASM_DIR_ASCII,
  ASM_DIR_ASCIIZ,
  ASM_DIR_DATA,
  ASM_DIR_TEXT,
  ASM_DIR_BYTE,
  ASM_DIR_BALIGN,
  // Instructions
  ASM_NOP,
  ASM_RET,
  ASM_PUSH,
  ASM_POP,
  ASM_CALL,
  ASM_JMP,
  ASM_LEAVE,
  ASM_ADD,
  ASM_AND,
  ASM_LEA,
  ASM_MOV,
  ASM_MOVSX,
  ASM_MOVZX,
  ASM_OR,
  ASM_SUB,
  ASM_XCHG,
  ASM_XOR,
  // Registers, order matters.
  ASM_REG_BEGIN,
 
  ASM_AL,
  ASM_CL,
  ASM_DL,
  ASM_BL,
  ASM_SPL,
  ASM_BPL,
  ASM_SIL,
  ASM_DIL,
  ASM_R8B,
  ASM_R9B,
  ASM_R10B,
  ASM_R11B,
  ASM_R12B,
  ASM_R13B,
  ASM_R14B,
  ASM_R15B,

  ASM_AX,
  ASM_CX,
  ASM_DX,
  ASM_BX,
  ASM_SP,
  ASM_BP,
  ASM_SI,
  ASM_DI,
  ASM_R8W,
  ASM_R9W,
  ASM_R10W,
  ASM_R11W,
  ASM_R12W,
  ASM_R13W,
  ASM_R14W,
  ASM_R15W,

  ASM_EAX,
  ASM_ECX,
  ASM_EDX,
  ASM_EBX,
  ASM_ESP,
  ASM_EBP,
  ASM_ESI,
  ASM_EDI,
  ASM_R8D,
  ASM_R9D,
  ASM_R10D,
  ASM_R11D,
  ASM_R12D,
  ASM_R13D,
  ASM_R14D,
  ASM_R15D,

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
  
  /* RIP is in a special class of its own. */
  ASM_RIP,

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
  const char *name;
  const char *flags;
  int type;
} DirSection;

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
  uint8_t nbytes;
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
  size_t len;
  uint8_t *data;
} String;
typedef String Ascii;
typedef String Asciiz;

typedef struct {
  AsmKind kind;
  const char *target;
} Call;

typedef struct {
  AsmKind kind;
  const char *target;
} Jmp;

typedef struct {
  AsmKind kind;
  uint8_t variant;
  Parsev *arg;
} Instr1;

typedef struct {
  AsmKind kind;
  uint8_t variant;
  Parsev *src;
  Parsev *dst;
} Instr2;

union Parsev {
  AsmKind kind;
  Label label;
  Globl globl;
  DirSection section;
  Balign balign;
  Ascii ascii;
  Asciiz asciiz;
  Memarg memarg;
  Instr1 instr1;
  Instr2 instr2;
  Call call;
  Jmp jmp;
  Byte byte;
  Imm imm;
  Ident ident;
  Number number;
  String string;
  // Temporary values.
  const char *charptr;
  int64_t i64;
};

typedef struct AsmLine AsmLine;
struct AsmLine {
  int64_t lineno;
  Parsev v;
  AsmLine *next;
};

extern size_t curlineno;

/* util.c */

void lfatal(const char *fmt, ...);
void fatal(const char *fmt, ...);
void unreachable(void);

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