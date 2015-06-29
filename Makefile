#SUBLEQ=../subleq-toolchain/dist/build/subleq/subleq
SUBLEQ=subleq
SUBLEQFLAGS=-b=100
TARGETS= subleq-int-fix2-sim subleq-int-fix2 subleq-int subleq-test subleqr-int-fix2 subleqr-test subneg-int 

.PHONY: all sq
all: subleq-int-fix2-sim.sqo

sq: $(addsuffix .sq, $(TARGETS))

%.sqo: %.sq
	$(SUBLEQ) $(SUBLEQFLAGS) -f elf2mem -o $@ $<

%.sq: %.sq.m4
	m4 $< > $@

