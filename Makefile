#SUBLEQ=../subleq-toolchain/dist/build/subleq/subleq
SUBLEQ=cabal exec subleq --
SUBLEQFLAGS=-b=100 -c='$(shell cat $(SUBLEQCFG))'
#SUBLEQFLAGS=-b=100 -c="$(SUBLEQCFG)"
#SUBLEQCFG=subleq-config.cfg
SUBLEQCFG=euc2015.cfg
TARGETS= subleq-int-fix2-sim subleqr-int-fix2-sim subleq-int-fix2 subleq-int subleq-test subleqr-int-fix2 subleqr-test subneg-int 

.PHONY: all sq
all: subleq-int-fix2-sim.sqo subleqr-int-fix2-sim.sqo

sq: $(addsuffix .sq, $(TARGETS))

%.sqo: %.sq $(SUBLEQCFG) Makefile
	$(SUBLEQ) $(SUBLEQFLAGS) -f elf2mem -o $@ $<

%.sq: %.sq.m4 subleq-lib.sq.m4
	m4 $< > $@

