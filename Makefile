COMP_TEST := ghc-lib-test
LIBS_TEST := libs-test
CABAL := cabal
CABAL_BUILD := $(CABAL) v2-build
CABAL_FLAGS := -O0
GHC_FLAGS :=

ifeq ($(origin DUMP_TO_FILE), environment)
GHC_FLAGS += -ddump-to-file
endif

.PHONY: all
all: $(COMP_TEST) $(LIBS_TEST)

.PHONY: run
run: $(COMP_TEST)
	cd foo && ../dist-newstyle/build/x86_64-osx/ghc-8.6.5/ghc-lib-test-0.0.1/x/ghc-lib-test/build/ghc-lib-test/ghc-lib-test -v3 -ddump-if-trace Foo.hs

.PHONY: $(COMP_TEST)
$(COMP_TEST):
	$(CABAL_BUILD) $(CABAL_FLAGS) --ghc-options='$(GHC_FLAGS)' $@

.PHONY: $(LIBS_TEST)
$(LIBS_TEST):
	$(CABAL_BUILD) $(CABAL_FLAGS) --ghc-options='$(GHC_FLAGS)' $@

.PHONY: dump_parse
DUMP_PARSE_FLAGS := -ddump-parsed -ddump-parsed-ast -dsource-stats
dump_parse: GHC_FLAGS += $(DUMP_PARSE_FLAGS)
dump_parse: $(COMP_TEST)

.PHONY: dump_rename
DUMP_RENAME_FLAGS := -ddump-rn -ddump-rn-ast -ddump-rn-stats -ddump-rn-trace
dump_rename: GHC_FLAGS += $(DUMP_RENAME_FLAGS)
dump_rename: $(COMP_TEST)

.PHONY: dump_parse_rename
dump_parse_rename: GHC_FLAGS += $(DUMP_PARSE_FLAGS) $(DUMP_RENAME_FLAGS)
dump_parse_rename: $(COMP_TEST)

.PHONY: clean
clean:
	rm -rf dist dist-newstyle
