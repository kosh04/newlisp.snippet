NEWLISP ?= newlisp

TESTS := $(basename $(notdir $(wildcard test/*.lsp)))

USAGE := $(MAKE) [test-all $(TESTS)]

default: test-all

define define-test
$1: test/$1.lsp
	$$(NEWLISP) -n $$^ -e "(Test:run)"
endef

$(foreach test, $(TESTS), $(eval $(call define-test,$(test))))

test-all: $(TESTS)

help usage:
	@echo $(USAGE)

.PHONY: test-all $(TESTS) help usage
