export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: compile
	cask emacs --batch -L . -L test -l orgext-test.el -f ert-run-tests-batch

.PHONY: test-interactvie
test-interactvie: compile
	@echo '-----------------------------'
	@echo '-----------------------------'
	@echo 'Run `ert` when emacs opens!'
	@echo '-----------------------------'
	@echo '-----------------------------'
	cask emacs -L . -L test -l orgext-test.el
