(defvar my/rust-analyzer-bin
  (expand-file-name "~/tools/rust-analyzer/rust-analyzer"))

(use-package rust-mode :ensure)

(use-package eglot
  :config (add-to-list 'eglot-server-programs
                       `(rust-mode . (,my/rust-analyzer-bin))))
