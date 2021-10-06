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
	./minias < "$tmps" > "$tmpo" 
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

t "leaq (%rax), %rax"

t "addq (%rax), %rax"
t "andq (%rax), %rax"
t "subq (%rax), %rax"
t "xorq (%rax), %rax"

t "nop"
t "ret"
t "leave"
t "addq %rax, %rax"
t "addl %eax, %eax"
t "subq %rax, %rax"
t "subl %eax, %eax"
t "addq %rax, %rbx"
t "addl %eax, %ebx"
t "addq %rax, (%rax)"
t "addq %rax, (%rbx)"
t "addl %eax, (%rax)"
t "addl %eax, (%r9)"
t "addl %ebx, (%r9)"

t "xorq %rax, %rax"
t "xorq %rax, (%rax)"
t "xorl %eax, %eax"

t "andq %rax, %rax"
t "andq %rax, (%rax)"
t "andl %eax, %eax"



#t "addq %rbp, (%rax)"


#t "addq %rax, %rbx"
#t "addl %eax, %ebx"
