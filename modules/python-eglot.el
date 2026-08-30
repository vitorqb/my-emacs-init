;; -*- lexical-binding: t; -*-
(require 'eldoc)
(require 'my-mise)
(emacs-init-load-module-eglot)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)

;; How to run a python LSP
(defun my/python-eglot/pylsp (interactive project)
  "Defines how to run the pyright server. If a `tlsp`, `lsp` mise task is found, use it. Else runs `mise exec ...`"
  (if-let* ((mise-lsp (my/eglot/guess-lsp-via-mise)))
      mise-lsp
    '("mise" "exec" "node@latest" "npm:pyright@latest" "--" "pyright-langserver" "--stdio")))

;; Use python-treesit mode
(if (treesit-language-available-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (warn "No treesit grammar found for Python!"))

;; Snippets S2
(dolist (mode '(python-mode-hook python-ts-mode-hook))
  (add-hook mode #'yas-minor-mode-on)
  (add-to-list 'eglot-server-programs `(python-mode . ,#'my/python-eglot/pyright-server)))
