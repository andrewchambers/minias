# minias

A mini assembler for x86_64, written for fun and learning.

Goals:

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

# Notes

- Minias deliberately does not free data as it all is
  freed by the OS at the end of execution. In the future
  we one ould use an arena allocator for minias and still
  avoid manual calls to free.

- Minias deliberately kept the peg grammar quite repetitive
  and simple, please keep it this.

- Our performance is quite fast, but with the current design
  it is limited by the parser, it would be interesting
  to see if we can improve the parser generator upstream. 

- One day it would be nice to write a 'minipeg' in a single .c
  file that can be bundled in projects.

# Resources

- [intel reference](https://software.intel.com/content/dam/develop/external/us/en/documents-tps/325383-sdm-vol-2abcd.pdf) - Specifically chapter 2.1 and chapter 3.1.
- [elf spec](https://refspecs.linuxfoundation.org/elf/elf.pdf)
- [goas](https://github.com/DQNEO/goas)
- [neatas](https://repo.or.cz/neatas.git)
