(emacs-init-load-module-eglot)

(use-package typescript-mode :ensure
  :after
  (eglot)
  :hook
  ((typescript-mode . yas-minor-mode-on))
  :config
  (setq typescript-indent-level 2))
