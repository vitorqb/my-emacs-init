(require 'eldoc)
(emacs-init-load-module-eglot)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)

;; Snippets S2
(add-hook 'python-mode-hook #'yas-minor-mode-on)

;; How to run `pyright`
(defvar my/python-eglot/pyright-server
  '("mise" "exec" "--" "pyright-langserver" "--stdio"))

;; Tells eglot how to run pyrgith
(add-to-list 'eglot-server-programs `(python-mode . ,my/python-eglot/pyright-server))
