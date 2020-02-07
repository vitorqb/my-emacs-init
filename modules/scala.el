(emacs-init-load-module-eglot)

(use-package scala-mode
  :ensure
  :config (progn
            (add-hook 'scala-mode-hook #'yas-minor-mode-on)))
