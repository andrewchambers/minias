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

# Various regression tests first.
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

t () {
  if ! ./minias < "$1" > "$tmpo"
  then
    echo "failed to assemble: $1"
    exit 1
  fi
  clang "$tmpo" -o "$tmpb"
  if !"$tmpb" 1>&2 2>/dev/null
  then
    echo "$t failed"
    exit 1
  fi
  echo -n "."
}

for tc in $(echo test/execute/*.s)
do
  t "$tc"
done