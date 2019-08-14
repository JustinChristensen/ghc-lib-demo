PROG := ghc-comp-test

.PHONY: all
all: $(PROG)

.PHONY: $(PROG)
$(PROG):
	cabal v2-build

.PHONY: clean
clean:
	rm -rf dist-newstyle
