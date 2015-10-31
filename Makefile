NEWLISP ?= newlisp

TESTS := $(basename $(notdir $(wildcard test/*.lsp)))

USAGE := $(MAKE) [test-all $(TESTS)]

default: test-all

test-%: test/test-%.lsp
	$(NEWLISP) -n $^ -e "(Test:run)"

test-all: $(TESTS)

help usage:
	@echo $(USAGE)

.PHONY: test-all test-% help usage
