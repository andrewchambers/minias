# minias

A mini assembler for x86_64, written for fun and learning.

Minias can assemble itself and many/most things compiled with the [cproc](https://github.com/michaelforney/cproc) C compiler i.e., large amounts of real world software.

Project Goals:

- A simple, tiny, fast implementation (in that order).
- Assemble the output of [cproc](https://github.com/michaelforney/cproc)/[qbe](https://c9x.me/compile/) and [chibicc](https://github.com/rui314/chibicc).
- Relocatable elf output.

Non Goals:

- Assemble every assembly instruction.
- Assemble other architectures.
- Work as a library.

# Building

Install the [peg/leg](https://www.piumarta.com/software/peg/) parser generator, make and a C compiler and run:

```
make
```

or 

```
leg asm.peg > asm.peg.inc
cc -O2 *.c -o minias
```

# Roadmap

Essential features:

- [x] Self host with cproc.
- [ ] Self host with chibicc.

Bonus features:

- [ ] Two pass jump relaxing.
- [ ] Immediate relaxing.
- [ ] Simple immediate expressions.
- [ ] Assemble a libc.
- [ ] Test every opcode with all variants in our test suite.
- [ ] Parser that doesn't depend on peg/leg.

# Notes

- Minias deliberately does not free data as it all is
  freed by the OS at the end of execution. Memory usage is still
  quite light as it uses string and value interning. In the future
  we could use an arena allocator for minias and still avoid manual calls to free.

- Minias deliberately keps the peg grammar quite repetitive
  and simple, please keep it this way.

- Performance is limited by the parser, it would be interesting
  to see if we can improve the parser generator upstream. That being said,
  performance is often better than gnu as and much better than the clang assembler,

- One day it would be nice to write a 'minipeg' in a single .c
  file that can be bundled in projects.

# Resources

- [intel reference](https://software.intel.com/content/dam/develop/external/us/en/documents-tps/325383-sdm-vol-2abcd.pdf) - Specifically chapter 2.1 and chapter 3.1.
- [elf spec](https://refspecs.linuxfoundation.org/elf/elf.pdf)
- [osdev wiki](https://wiki.osdev.org/X86-64_Instruction_Encoding)
- [goas](https://github.com/DQNEO/goas)
- [neatas](https://repo.or.cz/neatas.git)
