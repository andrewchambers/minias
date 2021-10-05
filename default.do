#!/bin/sh
set -eu

case "$1" in
  *.o)
    test "$1" = "main.o" && redo-ifchange asm.peg.inc
    redo-ifchange "${1%.o}.c" minias.h
    set -x
    ${CC:- cc} ${CFLAGS:- -O -Og} -c -o "$3" "${1%.o}.c"
  ;;
  asm.peg.inc)
    redo-ifchange asm.peg
    set -x
    leg asm.peg > "$3"
  ;;
  minias)
    obj="main.o util.o "
    redo-ifchange $obj
    ${CC:- cc} ${LDFLAGS:-} -o "$3" $obj
  ;;
  fmt)
    clang-format -i main.c util.c >&2
  ;;
  *)
    echo "don't know how to build $1" 2>&1
    exit 1
  ;;
esac