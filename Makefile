EMACS ?= emacs
TESTS ?= test/fennel-mode-test.el
ALL_TESTS ?= $(wildcard test/*-test.el)

test: clean
	$(EMACS) -Q -batch -L . -l $(TESTS) \
		-f ert-run-tests-batch-and-exit

testall: $(ALL_TESTS)
	@$(foreach test,$?,make test TESTS=$(test) || exit;)

clean: ; rm -f *.elc

.PHONY: test testall clean
