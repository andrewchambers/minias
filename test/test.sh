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
    echo "got:  $1 -> $got"
    exit 1
  fi
  echo -n "."
}



for op in add and or sub xor
do

  t "${op}b (%rax), %al"
  t "${op}w (%rax), %ax"
  t "${op}l (%rax), %eax"
  t "${op}q (%rax), %rax"
  t "${op}q (%rbp), %rax"
  t "${op}b %al, (%rax)"
  t "${op}w %ax, (%rax)"
  t "${op}l %eax, (%rax)"
  t "${op}q %rax, (%rax)"
  t "${op}q %rax, (%rbp)"
  t "${op}b %al, %al"
  t "${op}w %ax, %ax"
  t "${op}l %eax, %eax"
  t "${op}q %rax, %rax"
done

t "xchg %al, %al"
t "xchg %ax, %ax"
t "xchg %ax, %r9w"
t "xchg %r9w, %ax"
t "xchg %ax, %bx"
t "xchg %bx, %ax"
#t "xchg %eax, %eax" # XXX We encode this as nop, but clang does not.
t "xchg %eax, %r9d"
t "xchg %r9d, %eax"
t "xchg %eax, %ebx"
t "xchg %ebx, %eax"
t "xchg %rax, %r9"
t "xchg %r9, %rax"
t "xchg %rax, %rbx"
t "xchg %rbx, %rax"
t "xchg %rax, (%rax)"
t "xchg %eax, (%rax)"
t "xchg %ax, (%rax)"
t "xchg %al, (%rax)"
t "xchg (%rax), %rax"
t "xchg (%rax), %eax"
t "xchg (%rax), %ax"
t "xchg (%rax), %al"
