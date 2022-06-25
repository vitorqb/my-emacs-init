;; This requires gopls:
;; yay gopls
(emacs-init-load-module-eglot)
(require 'yasnippet)

(add-hook 'go-mode-hook #'yas-minor-mode-on)
(use-package go-mode :ensure)
