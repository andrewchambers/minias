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

t "div %rax"
t "divq (%rax)"
t "divq (%rip)"
t "idiv %rax"
t "idivq (%rax)"
t "idivq (%rip)"
t "mul %rax"
t "mulq (%rax)"
t "mulq (%rip)"

t "imul %rax"
t "imulq (%rax)"
t "imulq (%rip)"

t "imul %rax, %rax"
t "imulq (%rax), %rax"
t "imul %eax, %eax"
t "imull (%rax), %eax"
t "imul %eax, %eax"

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

for op in mov add and or sub xor
do
  # rip relative
  t "${op}b \$127, (%rip)"
  t "${op}w \$32767, (%rip)"
  t "${op}l \$2147483647, (%rip)"
  t "${op}q \$2147483647, (%rip)"

  t "${op}q %r9, %r9"

  for r in a b
  do
    # immediate variants.
    t "${op}b \$127, (%r${r}x)"
    t "${op}w \$32767, (%r${r}x)"
    t "${op}l \$2147483647, (%r${r}x)"
    t "${op}q \$2147483647, (%r${r}x)"
    t "${op}b \$127, %${r}l"
    t "${op}w \$32767, %${r}x"
    t "${op}l \$2147483647, %e${r}x"
    t "${op}q \$2147483647, %r${r}x"

    # r rm variants
    t "${op}b (%rip), %${r}l"
    t "${op}b (%rax), %${r}l"
    t "${op}b (%rax), %${r}l"
    t "${op}w (%rax), %${r}x"
    t "${op}l (%rax), %e${r}x"
    t "${op}q (%rax), %r${r}x"
    t "${op}q (%rbp), %r${r}x"
    t "${op}q (%r8), %r${r}x"
    t "${op}q (%r13), %r${r}x"
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