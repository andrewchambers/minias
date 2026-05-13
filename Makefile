.POSIX:
.PHONY: all clean

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
CFLAGS+=-D _GNU_SOURCE
PACKCC=vendor/packcc/packcc

-include config.mk

OBJ=\
	main.o\
	parse.o\
	util.o

all: minias

minias: $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(OBJ)

$(PACKCC): vendor/packcc/packcc.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ vendor/packcc/packcc.c

asm_parser.c: asm.peg $(PACKCC)
	$(PACKCC) -o asm_parser asm.peg

asm_parser.h: asm_parser.c

parse.o: asm_parser.c asm_parser.h
main.o parse.o util.o: minias.h

fmt:
	clang-format \
	  -style="{BasedOnStyle: WebKit,\
	           AlwaysBreakAfterReturnType: TopLevelDefinitions,\
	           ColumnLimit: 80,\
	           PointerAlignment: Right}"\
	  -i *.c *.h

check:
	sh test/test.sh

clean:
	rm -f $(OBJ) minias asm_parser.c asm_parser.h $(PACKCC)

install: minias
	mkdir -p $(DESTDIR)$(BINDIR)
	cp minias $(DESTDIR)$(BINDIR)/
	ln -s ./minias $(DESTDIR)$(BINDIR)/minias-x86-64
