(require 'eldoc)
(emacs-init-load-module-eglot)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)

;; Snippets S2
(add-hook 'python-mode-hook #'yas-minor-mode-on)
