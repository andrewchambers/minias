#!/bin/sh
set -eu

tmps="$(mktemp)"
tmpo="$(mktemp)"
tmpb="$(mktemp)"

trap "rm -f \"$tmps\" \"$tmpo\" \"$tmpb\"" EXIT

t () {
  echo -e "$1" > "$tmps"
  clang -Wno-everything -c -x assembler "$tmps" -o "$tmpo"
  objcopy -j ".text" -O binary "$tmpo" "$tmpb"
  want="$(xxd -ps "$tmpb" | head -n 1 | cut  -d ' ' -f 2-)"
  if ! ./minias < "$tmps" > "$tmpo"
  then
    echo "failed to assemble: $1"
    exit 1
  fi
  objcopy -j ".text" -O binary "$tmpo" "$tmpb"
  got="$(xxd -ps "$tmpb" | head -n 1 | cut  -d ' ' -f 2-)"
  if test "$got" != "$want"
  then
    echo ""
    echo "want: $1 -> $want"
    echo "got:"
    objdump -d "$tmpo"
    exit 1
  fi
  echo -n "."
}

must_fail () {
  echo -e "$1" > "$tmps"
  if ./minias < "$tmps" > "$tmpo" 2>"$tmpb"
  then
    echo ""
    echo "unexpectedly assembled: $1"
    exit 1
  fi
  echo -n "."
}

must_fail_containing () {
  echo -e "$1" > "$tmps"
  if ./minias < "$tmps" > "$tmpo" 2>"$tmpb"
  then
    echo ""
    echo "unexpectedly assembled: $1"
    exit 1
  fi
  if ! grep -q "$2" "$tmpb"
  then
    echo ""
    echo "expected error containing '$2' for: $1"
    cat "$tmpb"
    exit 1
  fi
  echo -n "."
}

reloc_must_contain () {
  echo -e "$1" > "$tmps"
  if ! ./minias < "$tmps" > "$tmpo"
  then
    echo "failed to assemble: $1"
    exit 1
  fi
  if ! readelf -Wr "$tmpo" | grep -q "$2"
  then
    echo ""
    echo "expected relocation $2 for: $1"
    readelf -Wr "$tmpo"
    exit 1
  fi
  echo -n "."
}

# Various regression tests first.
printf "ret\n" | ./minias -o "$tmpo" -
t "ud2"
t "movl \$3735936685>>32, 4+-16(%rbp)"
t "movsd \".Lfp3\"(%rip), %xmm1\n.Lfp3:\n.quad 0"
echo -e ".bss\n.balign 1\nx:\n.fill 2,1,0" > "$tmps"
if ! ./minias < "$tmps" > "$tmpo"
then
  echo "failed to assemble .bss directive"
  exit 1
fi
echo -n "."
must_fail ".bss\n.byte 1"
must_fail ".bss\n.quad foo"
echo -e ".comm x,4,4" > "$tmps"
if ! ./minias < "$tmps" > "$tmpo"
then
  echo "failed to assemble .comm directive"
  exit 1
fi
if ! readelf -Ws "$tmpo" | grep -q "GLOBAL DEFAULT  COM x"
then
  echo "expected common symbol for .comm directive"
  readelf -Ws "$tmpo"
  exit 1
fi
echo -n "."
echo -e ".local x\n.comm x,4,4" > "$tmps"
if ! ./minias < "$tmps" > "$tmpo"
then
  echo "failed to assemble local .comm directive"
  exit 1
fi
if ! readelf -Ws "$tmpo" | grep -q "OBJECT  LOCAL .* x"
then
  echo "expected local object symbol for local .comm directive"
  readelf -Ws "$tmpo"
  exit 1
fi
echo -n "."
t "1: jmp 1b"
t "1: jmp 1f\n1: nop"
t "movb %ah, %bl"
t "movb %ah, (%rax)"
must_fail "movb %ah, %r8b"
must_fail "movb %ah, (%r8)"
reloc_must_contain "movl foo@GOTPCREL(%rip), %eax" "R_X86_64_GOTPCRELX"
reloc_must_contain "movq foo@GOTPCREL(%rip), %rax" "R_X86_64_REX_GOTPCRELX"
reloc_must_contain ".globl foo\nfoo:\nret\ncall foo" "foo"
reloc_must_contain ".weak foo\nfoo:\nret\ncall foo" "foo"
must_fail ".quad foo@GOTPCREL"
t "testl -740(%rbp), %r11d"
t "movss  %xmm15,-0x128(%rbp)"
t "xchgq %r13, %rax"
t "movl \$1000, %r8d"
t "movb %sil, (%rdi)"
t "movsbq (%rax), %rbx"
t "movq $-4132994306676758123, %rcx"
t "mov \$17293822569102704639, %rax"
t "callq *%rax"
t "callq *%r10"
t "callq *(%r10)"
t "movb %r11b, (%rsi, %r12, 1)"
t "mov %rdi, -0x60(%rbp)"
t "imul \$0x28, %rbx, %rcx"

for r in rax r10
do
  for x in xmm0 xmm13
  do
    t "movq %${x}, %${r}"
    t "movq %${r}, %${x}"
    t "movq %${x}, (%${r})"
    t "movq (%${r}), %${x}"
  done
done


t "cvttsd2si %xmm1, %rax"
t "cvttsd2si %xmm10, %rax"
t "cvttsd2si %xmm10, %r9"
t "cvttsd2si %xmm1, %eax"
t "cvttsd2si %xmm10, %eax"

t "cvttss2si %xmm1, %rax"
t "cvttss2si %xmm10, %rax"
t "cvttss2si %xmm1, %eax"
t "cvttss2si %xmm10, %eax"

t "cvtsd2ss %xmm0, %xmm0"
t "cvtsd2ss (%rax), %xmm1"
t "cvtsd2ss (%rax), %xmm10"

t "cvtsi2sd %rax, %xmm1"
t "cvtsi2sd %rax, %xmm10"
t "cvtsi2sd %r9, %xmm10"
t "cvtsi2sd (%rax), %xmm1"
t "cvtsi2sd (%rax), %xmm10"
t "cvtsi2sd %eax, %xmm1"
t "cvtsi2sd %eax, %xmm10"

t "cvtsi2ss %rax, %xmm1"
t "cvtsi2ss %rax, %xmm10"
t "cvtsi2ss %r9, %xmm10"
t "cvtsi2ss (%rax), %xmm1"
t "cvtsi2ss (%rax), %xmm10"
t "cvtsi2ss %eax, %xmm1"
t "cvtsi2ss %eax, %xmm10"


xmmops="
pxor movaps cvtss2sd xorps xorpd
movss addsd addss subsd subss
divss divsd mulss mulsd ucomiss
ucomisd
"
for op in $xmmops
do
  t "${op} %xmm0, %xmm1"
  t "${op} (%rax), %xmm1"
  t "${op} %xmm10, %xmm1"
  t "${op} %xmm1, %xmm10"
  t "${op} %xmm10, %xmm11"
  t "${op} (%rax), %xmm11"
done


for r in a b
do
  t "lea (%rsp), %r${r}x"
  t "lea (%rbp), %r${r}x"
  t "lea (%r${r}x), %r${r}x"
  t "lea 1(%rsp), %r${r}x"
  t "lea 127(%rsp), %r${r}x"
  t "lea 128(%rsp), %r${r}x"
  t "lea -129(%rsp), %r${r}x"
  t "lea -127(%rsp), %r${r}x"
  t "lea 2147483647(%rsp), %r${r}x"
  t "lea 2147483647(%rbp), %r${r}x"
  t "lea 2147483647(%r${r}x), %r${r}x"

  t "lea (%r9, %r10), %r${r}x"
  t "lea (%rsp, %r${r}x), %r${r}x"
  t "lea (%rbp, %r${r}x), %r${r}x"
  t "lea (%r${r}x, %r${r}x), %r${r}x"
  t "lea 1(%rsp, %r${r}x), %r${r}x"
  t "lea 127(%rbp, %r${r}x), %r${r}x"
  t "lea 128(%rbp, %r${r}x), %r${r}x"
  t "lea -129(%rbp, %r${r}x), %r${r}x"
  t "lea -127(%r${r}x, %r${r}x), %r${r}x"
  t "lea 2147483647(%rsp, %r${r}x), %r${r}x"
  t "lea 2147483647(%rbp, %r${r}x), %r${r}x"
  t "lea 2147483647(%r${r}x, %r${r}x), %r${r}x"

  t "lea (%rsp, %r${r}x, 4), %r${r}x"
  t "lea (%rbp, %r${r}x, 4), %r${r}x"
  t "lea (%r${r}x, %r${r}x, 4), %r${r}x"
  t "lea 1(%rsp, %r${r}x, 4), %r${r}x"
  t "lea 127(%rbp, %r${r}x, 4), %r${r}x"
  t "lea 128(%r${r}x, %r${r}x, 4), %r${r}x"
  t "lea -129(%r${r}x, %r${r}x, 4), %r${r}x"
  t "lea -127(%r${r}x, %r${r}x, 4), %r${r}x"
  t "lea 2147483647(%rsp, %r${r}x, 4), %r${r}x"
  t "lea 2147483647(%rbp, %r${r}x, 4), %r${r}x"
  t "lea 2147483647(%r${r}x, %r${r}x, 4), %r${r}x"
done

t "ret"
t "cbtw"
t "cwtd"
t "cltd"
t "cqto"

conditioncodes="
  a ae b be c e
  z g ge l le na
  nae nb nbe nc
  ne ng nge nl
  nle no np ns nz
  o p pe po s z
"

t "l:\n .fill 1, 1, 0x00 \njmp l"
t "jmp l\n .fill 1, 1, 0x00 \nl:"
for cc in $conditioncodes
do
  t "l:\n .fill 1, 1, 0x00 \nj${cc} l"
  t "j${cc} l\n .fill 1, 1, 0x00 \nl:"
done

echo -e ".fill 14, 1, 0x90\njz l\n.fill 120, 1, 0x90\n.p2align 4\nl:\nnop" > "$tmps"
if ! ./minias < "$tmps" > "$tmpo"
then
  echo "failed to relax forward jump across alignment"
  exit 1
fi
echo -n "."

# Check boundary on jump relaxing.
for fill in 0 $(seq 120 140)
do
  t "l:\n .fill $fill, 1, 0x00 \njmp l"
  t "jmp l\n .fill $fill, 1, 0x00 \nl:"
  t "l:\n .fill $fill, 1, 0x00 \njz l"
  t "jz l\n .fill $fill, 1, 0x00 \nl:"
done

for cc in $conditioncodes
do
  t "set${cc} %al"
  t "set${cc} (%rax)"
done

for op in sal sar shl shr
do
  t "${op} \$3, %rax"
  t "${op} %cl, %rax"
  t "${op} \$3, %eax"
  t "${op} %cl, %eax"
  t "${op} \$3, %ax"
  t "${op} %cl, %ax"
  t "${op}w \$3, (%rax)"
  t "${op}w %cl, (%rax)"
  t "${op}l \$3, (%rax)"
  t "${op}l %cl, (%rax)"
  t "${op}q \$3, (%rax)"
  t "${op}q %cl, (%rax)"
done

t "div %rax"
t "divq (%rax)"
t "divq (%rip)"
t "idiv %rax"
t "idivq (%rax)"
t "idivq (%rip)"
t "mul %rax"
t "mulq (%rax)"
t "mulq (%rip)"
t "neg %rax"
t "negq (%rax)"
t "negq (%rip)"

t "imul %rax"
t "imulq (%rax)"
t "imulq (%rip)"

t "imul %rax, %rbx"
t "imulq (%rax), %rbx"
t "imul %eax, %ebx"
t "imull (%rax), %ebx"

t "imul \$2147483647, %rax, %rbx"
t "imul \$2147483647, (%rax), %rbx"
t "imul \$2147483647, %eax, %ebx"
t "imul \$2147483647, (%rax), %ebx"
t "imul \$32767, %ax, %bx"
t "imul \$32767, (%rax), %bx"

t "pushq (%r9)"
t "pushq %r9"
t "pushq %rax"
t "popq (%r9)"
t "popq %r9"
t "popq %rax"

t "movb \$127, (%rsp)"
t "movb \$127, (%rbp)"
t "movb \$127, 2147483647(%rsp)"
t "movb \$127, 2147483647(%rbp)"

for x in s z
do
  t "mov${x}bw %al, %bx"
  t "mov${x}bl %al, %ebx"
  t "mov${x}bq %al, %rbx"
  t "mov${x}wl %ax, %ebx"
  t "mov${x}wq %ax, %rbx"
  if test "$x" = s
  then
    t "mov${x}lq %eax, %rbx"
  fi
  t "mov${x}bw (%rax), %bx"
  t "mov${x}bl (%rax), %ebx"
  t "mov${x}bq (%rax), %rbx"
  t "mov${x}wl (%rax), %ebx"
  t "mov${x}wq (%rax), %rbx"
  if test "$x" = s
  then
    t "mov${x}lq (%rax), %rbx"
  fi
done

for r in a b
do
  t "xchg %${r}l, %${r}l"
  t "xchg %${r}x, %${r}x"
  t "xchg %${r}x, %bx"
  t "xchg %bx, %${r}x"
  # t "xchg %e${r}x, %e${r}x" # clang disagrees
  t "xchg %e${r}x, %ebx"
  t "xchg %ebx, %e${r}x"
  t "xchg %r${r}x, %rbx"
  t "xchg %rbx, %r${r}x"
  t "xchg %r${r}x, (%r${r}x)"
  t "xchg %e${r}x, (%r${r}x)"
  t "xchg %${r}x, (%r${r}x)"
  t "xchg %${r}l, (%r${r}x)"
  t "xchg (%r${r}x), %r${r}x"
  t "xchg (%r${r}x), %e${r}x"
  t "xchg (%r${r}x), %${r}x"
  t "xchg (%r${r}x), %${r}l"
done

for op in mov add and cmp or sub xor test
do
  # rip relative
  t "${op}b \$127, (%rip)"
  t "${op}w \$32767, (%rip)"
  t "${op}l \$2147483647, (%rip)"
  t "${op}q \$2147483647, (%rip)"

  t "${op}q %r9, %r9"

  for r in a b
  do
    # immediate variants
    t "${op}b \$1, (%r${r}x)"
    t "${op}b \$127, (%r${r}x)"
    t "${op}w \$1, (%r${r}x)"
    t "${op}w \$32767, (%r${r}x)"
    t "${op}l \$1, (%r${r}x)"
    t "${op}l \$2147483647, (%r${r}x)"
    t "${op}q \$1, (%r${r}x)"
    t "${op}q \$2147483647, (%r${r}x)"
    t "${op}b \$127, %${r}l"
    t "${op}w \$32767, %${r}x"
    
    t "${op}w \$1, %${r}x"

    t "${op}l \$1, %e${r}x"
    t "${op}l \$1000, %e${r}x"
    t "${op}l \$1000000, %e${r}x"
    t "${op}l \$2147483647, %e${r}x"

    t "${op}q \$1, %r${r}x"
    t "${op}q \$1000, %r${r}x"
    t "${op}q \$1000000, %r${r}x"
    t "${op}q \$2147483647, %r${r}x"

    # r -> m variants
    t "${op}b %${r}l, (%rip)"
    t "${op}b %${r}l, (%rax)"
    t "${op}w %${r}x, (%rax)"
    t "${op}l %e${r}x, (%rax)"
    t "${op}q %r${r}x, (%rax)"
    t "${op}q %r${r}x, (%rbp)"
    t "${op}q %r${r}x, (%r8)"
    t "${op}q %r${r}x, (%r13)"
    t "${op}b %${r}l, %al"
    t "${op}w %${r}x, %ax"
    t "${op}l %e${r}x, %eax"
    t "${op}q %r${r}x, %rax"

    # m -> r variants    
    t "${op}b (%rip), %${r}l"
    t "${op}b (%rax), %${r}l"
    t "${op}b (%rax), %${r}l"
    t "${op}w (%rax), %${r}x"
    t "${op}l (%rax), %e${r}x"
    t "${op}q (%rax), %r${r}x"
    t "${op}q (%rbp), %r${r}x"
    t "${op}q (%r8), %r${r}x"
    t "${op}q (%r13), %r${r}x"
  done
done

# Smoke tests for supported instruction mnemonics that are not covered by the
# variant-heavy loops above.
for op in cltq cwtl cld std syscall leave hlt pause wait endbr64 endbr32
do
  t "$op"
done

t "rep"
t "rep ret"
t "rep bsf %eax, %ebx"
for op in movsb movsw movsl movsq stosb stosw stosl stosq
do
  t "$op"
  t "rep $op"
done
for op in outsb outsw outsl insb insw insl
do
  t "$op"
  t "rep $op"
done
t "outb %al, %dx"
t "outw %ax, %dx"
t "outl %eax, %dx"
t "inb %dx, %al"
t "inw %dx, %ax"
t "inl %dx, %eax"
t "movsb (%rsi), %es:(%rdi)"
t "rep movsb %ds:(%rsi), (%rdi)"
t "rep movsw (%rsi), %es:(%rdi)"
t "rep movsl (%rsi), %es:(%rdi)"
t "rep movsq (%rsi), %es:(%rdi)"
t "rep movsb %cs:(%rsi), %es:(%rdi)"
t "rep movsb %ss:(%rsi), %es:(%rdi)"
t "rep movsb %es:(%rsi), %es:(%rdi)"
t "rep movsb %fs:(%rsi), %es:(%rdi)"
t "rep movsq %gs:(%rsi), %es:(%rdi)"
t "movsw %fs:(%rsi), %es:(%rdi)"
t "movsq %gs:(%rsi), %es:(%rdi)"
t "stosb %al, %es:(%rdi)"
t "rep stosw %ax, %es:(%rdi)"
t "rep stosl %eax, (%rdi)"
t "rep stosq %rax, %es:(%rdi)"
must_fail_containing "rep movsb (%rax), %es:(%rdi)" "invalid string instruction operands"
must_fail_containing "rep movsb (%rsi), %fs:(%rdi)" "invalid string instruction operands"
must_fail_containing "rep stosb (%rax), %es:(%rbx)" "invalid string instruction operands"
must_fail_containing "rep stosq %eax, %es:(%rdi)" "invalid string instruction operands"

t "stmxcsr (%rax)"
t "ldmxcsr (%rax)"
t "shufps \$3, %xmm0, %xmm1"
t "shufpd \$3, %xmm0, %xmm1"
t "cmpltsd %xmm0, %xmm1"
t "pshufd \$3, %xmm0, %xmm1"
t "pshuflw \$3, %xmm0, %xmm1"
t "pshufhw \$3, %xmm0, %xmm1"
t "psrldq \$3, %xmm1"

t "bsf %eax, %ebx"
t "bsr %eax, %ebx"
t "btr \$3, %eax"
t "btr %eax, %ebx"
t "bts \$3, %eax"
t "btc \$3, %eax"
t "bt \$3, %eax"
t "bswap %eax"
t "not %rax"
t "inc %rax"
t "dec %rax"
t "xadd %eax, %ebx"
t "cmpxchg %eax, %ebx"
t "adcq %rax, %rbx"
t "sbbq %rax, %rbx"
t "rol \$3, %rax"
t "ror \$3, %rax"
t "shrd \$3, %eax, %ebx"
t "shld \$3, %eax, %ebx"
t "lock addl %eax, (%rax)"
t "movabsq \$17293822569102704639, %rax"

for cc in $conditioncodes
do
  t "cmov${cc} %eax, %ebx"
done

x87ops="
  f2xm1 fabs faddp fpatan fprem fprem1
  frndint fscale fsqrt fyl2x fyl2xp1
  fld1 fldl2e fldlg2 fldln2 fldz
  fdivp fdivrp fnclex fchs fmulp
  fsubp fsubrp fxch
"
for op in $x87ops
do
  t "$op"
done

for op in fadd fdiv fdivr fmul fsub fsubr fld
do
  t "${op} %st(1)"
done
t "fstp %st(1)"

for op in fcomi fcomip fcompi fucomi fucomip fucompi
do
  t "${op} %st(1), %st"
done

for cc in nbe nb ne nu be b e u
do
  t "fcmov${cc} %st(1), %st"
done

x87memops="
  faddl fadds fldcw fldenv fldl flds fldt
  fdivl fdivs fdivrl fildl fildq fildll
  fimull fiaddl fidivl fidivrl fisubl fisubrl
  fistl fistpll fistpl fistpq fisttpll
  fnstcw fnstenv fstcw fsts fstl fstpl fstps
  fstpt fmuls fmull fsubl fsubs fsubrs
"
for op in $x87memops
do
  t "${op} (%rax)"
done
t "fnstsw %ax"

xmm_smoke_ops="
  addpd andpd andps andnpd andnps subpd
  sqrtsd sqrtss cvtdq2pd cvttpd2dq
  maxsd maxss minsd minss movups movapd
  movupd movdqa movdqu mulpd comiss comisd
  por orpd orps pand pandn pcmpeqb pcmpeqw
  pcmpeqd pmuludq pmullw pcmpgtb pcmpgtw
  pcmpgtd packuswb paddb paddw paddd paddq psubq
  psubd punpcklbw punpcklwd punpckldq
  punpckhbw punpckhdq punpckhwd punpckhqdq
  punpcklqdq unpcklpd unpckhpd unpcklps
  unpckhps psrlw psrld psrlq psrad
  psllw pslld psllq
"
for op in $xmm_smoke_ops
do
  t "${op} %xmm0, %xmm1"
done

t "cvtsd2si %xmm0, %eax"
t "cvtss2si %xmm0, %eax"
t "movhps (%rax), %xmm1"
t "movhpd (%rax), %xmm1"
t "movlpd (%rax), %xmm1"
t "movlps (%rax), %xmm1"
t "movhlps %xmm0, %xmm1"
t "movlhps %xmm0, %xmm1"
t "movd %xmm0, %eax"
t "movmskpd %xmm0, %eax"
t "movmskps %xmm0, %eax"
t "pextrw \$3, %xmm0, %eax"
t "pinsrw \$0, 54(%rax), %xmm0"
