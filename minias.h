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
  int64_t addend;
} Relocation;

typedef enum {
  // Misc
  ASM_SYNTAX_ERROR,
  ASM_BLANK,
  ASM_LABEL,
  ASM_IMM,
  ASM_STRING,
  ASM_MEMARG,
  // Directives
  ASM_DIR_GLOBL,
  ASM_DIR_SECTION,
  ASM_DIR_ASCII,
  ASM_DIR_ASCIIZ,
  ASM_DIR_DATA,
  ASM_DIR_TEXT,
  ASM_DIR_FILL,
  ASM_DIR_BYTE,
  ASM_DIR_SHORT,
  ASM_DIR_INT,
  ASM_DIR_QUAD,
  ASM_DIR_BALIGN,
  // Instructions
  ASM_NOP,
  ASM_RET,
  ASM_PUSH,
  ASM_POP,
  ASM_CALL,
  ASM_CLTD,
  ASM_CQTO,
  ASM_JMP,
  ASM_LEAVE,
  ASM_ADD,
  ASM_AND,
  ASM_CMP,
  ASM_CVTSS2SD,
  ASM_CVTSD2SS,
  ASM_CVTSI2SD,
  ASM_CVTSI2SS,
  ASM_DIV,
  ASM_IDIV,
  ASM_LEA,
  ASM_MUL,
  ASM_MULSD,
  ASM_MULSS,
  ASM_IMUL,
  ASM_MOV,
  ASM_MOVAPS,
  ASM_MOVSD,
  ASM_MOVSS,
  ASM_MOVSX,
  ASM_MOVZX,
  ASM_NEG,
  ASM_OR,
  ASM_PXOR,
  ASM_SAL,
  ASM_SAR,
  ASM_SET,
  ASM_SHL,
  ASM_SHR,
  ASM_SUB,
  ASM_TEST,
  ASM_UCOMISD,
  ASM_UCOMISS,
  ASM_XCHG,
  ASM_XOR,
  ASM_XORPD,
  ASM_XORPS,
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

  ASM_XMM0,
  ASM_XMM1,
  ASM_XMM2,
  ASM_XMM3,
  ASM_XMM4,
  ASM_XMM5,
  ASM_XMM6,
  ASM_XMM7,
  ASM_XMM8,
  ASM_XMM9,
  ASM_XMM10,
  ASM_XMM11,
  ASM_XMM12,
  ASM_XMM13,
  ASM_XMM14,
  ASM_XMM15,
  
  /* RIP is in a special class of its own. */
  ASM_RIP,
  ASM_NO_REG,

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
  int64_t c;
  const char *l;
} Value;

typedef struct {
  AsmKind kind;
  Value value;
} Byte;

typedef struct {
  AsmKind kind;
  Value value;
} Short;

typedef struct {
  AsmKind kind;
  Value value;
} Int;

typedef struct {
  AsmKind kind;
  Value value;
} Quad;

typedef struct {
  AsmKind kind;
  uint64_t align;
} Balign;

typedef struct {
  AsmKind kind;
  int32_t size;
  int32_t repeat;
  int64_t value;
} Fill;

typedef struct {
  AsmKind kind;
  uint8_t nbytes;
  Value v;
} Imm;

typedef struct {
  AsmKind kind;
  AsmKind base;
  AsmKind index;
  uint8_t scale;
  Value disp;
} Memarg;

typedef struct {
  AsmKind kind;
  size_t len;
  uint8_t *data;
} String;
typedef String Ascii;
typedef String Asciiz;

typedef struct {
  AsmKind kind;
  uint8_t indirect;
  union {
    Parsev *indirect;
    Value  direct;
  } target;
} Call;

typedef struct {
  AsmKind kind;
  uint8_t variant;
  const char *target;
} Jmp;

typedef struct {
  AsmKind kind;
  uint8_t variant;
  Parsev *arg1;
  Parsev *arg2;
  Parsev *arg3;
} Instr;

union Parsev {
  AsmKind kind;
  Label label;
  Globl globl;
  DirSection section;
  Balign balign;
  Ascii ascii;
  Asciiz asciiz;
  Memarg memarg;
  Instr instr;
  Call call;
  Jmp jmp;
  Fill fill;
  Byte dirbyte;
  Short dirshort;
  Int dirint;
  Quad dirquad;
  Imm imm;
  String string;
  // Temporary values.
  Value value;
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