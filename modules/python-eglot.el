(require 'eldoc)
(emacs-init-load-module-eglot)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)

;; For virtual environments
(use-package pyvenv :ensure)

;; Snippets S2
(add-hook 'python-mode-hook #'yas-minor-mode-on)

;; Variable with start to start pyright
(defvar my/python-eglot/pyright-server (expand-file-name "~/tools/pyright/run.sh"))

;; Help for activating virtual environment
(defun my/python-eglot/start-pyright-langserver (arg)
  (list my/python-eglot/pyright-server))

(add-to-list 'eglot-server-programs `(python-mode . ,#'my/python-eglot/start-pyright-langserver))
