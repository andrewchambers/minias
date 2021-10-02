# dumbas

A dumb assembler for x86_64, written for fun and learning.

Goals:

- A simple, tiny, fast implementation (in that order).
- Assemble the output of [cproc](https://github.com/michaelforney/cproc)/[qbe](https://c9x.me/compile/).
- Relocatable elf output.

Non Goals:

- Assemble every assembly instruction.
- Assemble other architectures.
- Work as a library.

# Resources

- [elf spec](https://refspecs.linuxfoundation.org/elf/elf.pdf)
- [goas](https://github.com/DQNEO/goas)
- [neatas](https://repo.or.cz/neatas.git)
