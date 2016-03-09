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
		subleq-int-fix2-sim-v1 \
		subleq-int-fix2-sim-v2 \
		subleq-int-fix2-sim-v3 \
		subleq-int-fix2-sim-all \
		subleqr-int-fix2-sim-v1 \
		subleqr-int-fix2-sim-v2 \
		subleqr-int-fix2-sim-v3 \
		subleqr-int-fix2-sim-all \
		subleqr-int-fix2 \
		subleq-int-fix2 \


.PHONY: all sq sqo expanded-sq clean
all: sq sqo

sq: $(addsuffix .sq, $(TARGETS))

sqo: $(addsuffix .sqo, $(SQO_TARGETS))

expanded-sq: $(addsuffix .expanded.sq, $(TARGETS))

%.sqo: %.sq $(SUBLEQCFG) Makefile
	$(SUBLEQ) $(SUBLEQFLAGS) -f elf2mem -o $@ $<

%.expanded.sq: %.sq $(SUBLEQCFG) Makefile
	$(SUBLEQ) $(SUBLEQFLAGS) -f expand -o $@ $<

%.sq: %.sq.m4 subleq-lib.sq.m4
	m4 $< > $@

clean:
	rm $(addsuffix .sq, $(TARGETS)) $(addsuffix .sqo, $(SQO_TARGETS)) $(addsuffix .expanded.sq, $(SQO_TARGETS))
