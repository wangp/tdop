MMC := mmc
DIFF := diff -u

files := $(wildcard *.m)

sample: $(files) Mercury.options
	$(MMC) --make $@ && touch $@

Mercury.options:

.PHONY: test
test: sample.out
	$(DIFF) sample.exp sample.out && rm sample.out

sample.out: sample sample.inp
	./sample --echo <sample.inp >sample.out
