(require 'eldoc)
(emacs-init-load-module-eglot)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)

;; How to run `pyright`
(defvar my/python-eglot/pyright-server
  '("mise" "exec" "node@latest" "npm:pyright@latest" "--" "pyright-langserver" "--stdio"))

;; Use python-treesit mode
(if (treesit-language-available-p 'python)
    (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (warn "No treesit grammar found for Python!"))

;; Snippets S2
(dolist (mode '(python-mode-hook python-ts-mode-hook))
  (add-hook mode #'yas-minor-mode-on)
  (add-to-list 'eglot-server-programs `(python-mode . ,my/python-eglot/pyright-server)))
