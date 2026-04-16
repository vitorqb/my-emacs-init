(emacs-init-load-module-eglot)

(use-package typescript-mode :ensure
  :after
  (eglot)
  :hook
  ((typescript-mode . yas-minor-mode-on))
  :config
  (setq typescript-indent-level 2)
  :init
  (add-to-list 'eglot-server-programs `(typescript-mode . ("mise" "exec" "node@latest" "npm:typescript-language-server@latest" "--" "typescript-language-server" "--stdio"))))
