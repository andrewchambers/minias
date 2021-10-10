#!/bin/sh
set -eu

tmps="$(mktemp --suffix .s)"
tmpo="$(mktemp --suffix .o)"
tmpb="$(mktemp --suffix .bin)"

trap "rm -f \"$tmps\" \"$tmpo\" \"$tmpb\"" EXIT

t () {
  echo "$1" > "$tmps"
  clang -Wno-everything -c -s "$tmps" -o "$tmpo" 
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


t "lea (%rsp), %rax"
t "lea (%rbp), %rax"
t "lea (%rax), %rax"
t "lea 2147483647(%rsp), %rax"
t "lea 2147483647(%rbp), %rax"
t "lea 2147483647(%rax), %rax"

t "lea (%r9, %r10), %rax"
t "lea (%rsp, %rax), %rax"
t "lea (%rbp, %rax), %rax"
t "lea (%rax, %rax), %rax"
t "lea 2147483647(%rsp, %rax), %rax"
t "lea 2147483647(%rbp, %rax), %rax"
t "lea 2147483647(%rax, %rax), %rax"

t "lea (%rsp, %rax, 4), %rax"
t "lea (%rbp, %rax, 4), %rax"
t "lea (%rax, %rax, 4), %rax"
t "lea 2147483647(%rsp, %rax, 4), %rax"
t "lea 2147483647(%rbp, %rax, 4), %rax"
t "lea 2147483647(%rax, %rax, 4), %rax"



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

t "imul %rax, %rax"
t "imulq (%rax), %rax"
t "imul %eax, %eax"
t "imull (%rax), %eax"

t "imul \$2147483647, %rax, %rax"
t "imul \$2147483647, (%rax), %rax"
t "imul \$2147483647, %eax, %eax"
t "imul \$2147483647, (%rax), %eax"
t "imul \$32767, %ax, %ax"
t "imul \$32767, (%rax), %ax"

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
  t "mov${x}bw %al, %ax"
  t "mov${x}bl %al, %eax"
  t "mov${x}bq %al, %rax"
  t "mov${x}wl %ax, %eax"
  t "mov${x}wq %ax, %rax"
  if test "$x" = s
  then
    t "mov${x}lq %eax, %rax"
  fi
  t "mov${x}bw (%rax), %ax"
  t "mov${x}bl (%rax), %eax"
  t "mov${x}bq (%rax), %rax"
  t "mov${x}wl (%rax), %eax"
  t "mov${x}wq (%rax), %rax"
  if test "$x" = s
  then
    t "mov${x}lq (%rax), %rax"
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


for r in a b
  do
  t "leaq (%r${r}x), %r${r}x"
  t "leal (%r${r}x), %e${r}x"
  t "leaw (%r${r}x), %${r}x"
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
    t "${op}b \$127, (%r${r}x)"
    t "${op}w \$32767, (%r${r}x)"
    t "${op}l \$2147483647, (%r${r}x)"
    t "${op}q \$2147483647, (%r${r}x)"
    t "${op}b \$127, %${r}l"
    t "${op}w \$32767, %${r}x"
    t "${op}l \$2147483647, %e${r}x"
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

    if test "$op" = "test" # m -> variants are not supported by test
    then
      continue
    fi

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

for tc in $(echo test/execute/*)
do
  t "$tc"
done