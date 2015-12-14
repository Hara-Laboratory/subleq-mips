#SUBLEQ=../subleq-toolchain/dist/build/subleq/subleq
SUBLEQ=cabal exec subleq --
SUBLEQFLAGS=-b=100 -c='$(shell cat $(SUBLEQCFG))'
#SUBLEQFLAGS=-b=0x5000 -c='$(shell cat $(SUBLEQCFG))'
#SUBLEQFLAGS=-b=100 -c="$(SUBLEQCFG)"
#SUBLEQCFG=subleq-config.cfg
SUBLEQCFG=euc2015.cfg
TARGETS=subleq-int-fix2-sim subleqr-int-fix2-sim \
		subleq-int-fix2 subleq-int subleq-test \
		subleqr-int-fix2 subleqr-test subneg-int

SQO_TARGETS=subleq-int-fix2-sim subleqr-int-fix2-sim \
		subleq-int-fix2-sim-lw \
		subleq-int-fix2-sim-sw \
		subleq-int-fix2-sim-lhu \
		subleq-int-fix2-sim-sh \
		subleq-int-fix2-sim-lbu \
		subleq-int-fix2-sim-sb \
		subleqr-int-fix2-sim-lw \
		subleqr-int-fix2-sim-sw \
		subleqr-int-fix2-sim-lhu \
		subleqr-int-fix2-sim-sh \
		subleqr-int-fix2-sim-lbu \
		subleqr-int-fix2-sim-sb \


.PHONY: all sq sqo expanded-sq
all: sqo

sq: $(addsuffix .sq, $(TARGETS))

sqo: $(addsuffix .sqo, $(SQO_TARGETS))

expanded-sq: $(addsuffix .expanded.sq, $(SQO_TARGETS))

%.sqo: %.sq $(SUBLEQCFG) Makefile
	$(SUBLEQ) $(SUBLEQFLAGS) -f elf2mem -o $@ $<

%.expanded.sq: %.sq $(SUBLEQCFG) Makefile
	$(SUBLEQ) $(SUBLEQFLAGS) -f expand -o $@ $<

%.sq: %.sq.m4 subleq-lib.sq.m4
	m4 $< > $@

