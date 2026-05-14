.POSIX:
.PHONY: all clean install

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
MANDIR=$(PREFIX)/share/man/man1
CFLAGS+=-D _GNU_SOURCE
MINIPEG=vendor/minipeg/minipeg

-include config.mk

OBJ=\
	main.o\
	parse.o\
	util.o

all: minias

minias: $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(OBJ)

$(MINIPEG): vendor/minipeg/minipeg.c
	$(CC) $(CFLAGS) $(LDFLAGS) -o $@ vendor/minipeg/minipeg.c

asm_parser.c: asm.peg $(MINIPEG)
	$(MINIPEG) -o asm_parser.c asm.peg

parse.o: asm_parser.c
main.o parse.o util.o: minias.h

fmt:
	clang-format \
	  -style="{BasedOnStyle: WebKit,\
	           AlwaysBreakAfterReturnType: TopLevelDefinitions,\
	           ColumnLimit: 80,\
	           PointerAlignment: Right}"\
	  -i *.c *.h

check: minias
	sh test/test.sh

clean:
	rm -f $(OBJ) minias asm_parser.c asm_parser.h $(MINIPEG)

install: minias minias.1
	mkdir -p $(DESTDIR)$(BINDIR)
	mkdir -p $(DESTDIR)$(MANDIR)
	cp minias $(DESTDIR)$(BINDIR)/
	ln -s ./minias $(DESTDIR)$(BINDIR)/minias-x86-64
	cp minias.1 $(DESTDIR)$(MANDIR)/
