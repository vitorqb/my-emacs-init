export EMACS ?= $(shell command -v emacs 2>/dev/null)
CASK_DIR := $(shell cask package-directory)
NAME := my-notes
LOAD_PATH := -L . -L test

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch $(LOAD_PATH) \
          --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq byte-compile-warnings '(not docstrings-wide))" \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: test
test: compile
	cask emacs --batch $(LOAD_PATH) -l $(NAME)-test.el -f ert-run-tests-batch
