EMACS ?= emacs
TESTS ?= test/fennel-mode-test.el
LUA_MODE ?= $(HOME)/src/lua-mode/lua-mode.el

test: clean
	$(EMACS) -Q -batch -L . -l $(LUA_MODE) -l $(TESTS) \
		-f ert-run-tests-batch-and-exit

clean: ; rm -f *.elc

.PHONY: test testall clean
