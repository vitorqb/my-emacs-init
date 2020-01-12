;; -----------------------------------------------------------------------------
;; Language Server Protocol
;; -----------------------------------------------------------------------------
(use-package eglot :ensure
  :config (progn
            ;; If we are using eglot, bind M-. to xref, that eglot itself uses
            (add-hook
             'eglot--managed-mode-hook
             (lambda () (local-set-key (kbd "M-.") 'xref-find-definitions)))))
