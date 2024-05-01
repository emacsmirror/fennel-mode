EMACS ?= emacs
TESTS ?= test/fennel-mode-test.el
ALL ?= $(TESTS) test/fennel-proto-repl-test.el test/ob-fennel-test.el
LUA_MODE ?= $(HOME)/src/lua-mode/lua-mode.el

test: clean
	$(EMACS) -Q -batch -L . -l $(LUA_MODE) -l $(TESTS) \
		-f ert-run-tests-batch-and-exit

testall: $(MAKE) test TESTS=$(ALL)

clean: ; rm -f *.elc

.PHONY: test testall clean
