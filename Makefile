.POSIX:
.PHONY: all clean

PREFIX=/usr/local
BINDIR=$(PREFIX)/bin
CFLAGS+=-D _GNU_SOURCE

-include config.mk

OBJ=\
	main.o\
	parse.o\
	util.o

all: minias

minias: $(OBJ)
	$(CC) $(LDFLAGS) -o $@ $(OBJ)

asm.peg.inc: asm.peg
	leg -o $@ asm.peg

parse.o: asm.peg.inc
main.o parse.o util.o: minias.h

fmt:
	clang-format -i *.c *.h

check:
	sh test/test.sh

clean:
	rm -f $(OBJ) minias asm.peg.inc

install: minias
	mkdir -p $(DESTDIR)$(BINDIR)
	cp minias $(DESTDIR)$(BINDIR)/
	ln -s ./minias $(DESTDIR)$(BINDIR)/minias-x86-64
