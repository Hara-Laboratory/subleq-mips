#SUBLEQ=../subleq-toolchain/dist/build/subleq/subleq
SUBLEQ=subleq
SUBLEQFLAGS=-b=100

.PHONY: all
all: subleq-int-fix2-sim.sqo

%.sqo: %.sq
	$(SUBLEQ) $(SUBLEQFLAGS) -f elf2mem -o $@ $<
