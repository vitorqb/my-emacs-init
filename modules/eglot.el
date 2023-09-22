;; -----------------------------------------------------------------------------
;; Language Server Protocol
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/eglot-hydra ()
  "Prepares an hydra for eglot."
  (defhydra my/eglot-hydra (:color blue)
    ("e" #'eglot "Start/restart" :column "Eglot")
    ("a" #'eglot-code-actions "Eglot code actions")
    ("r" #'eglot-rename "Eglot rename")))

(use-package eglot :ensure
  :config (progn
            ;; If we are using eglot, bind M-. to xref, that eglot itself uses
            (add-hook
             'eglot--managed-mode-hook
             (lambda () (local-set-key (kbd "M-.") 'xref-find-definitions)))

            ;; Refresh hydra
            (my/hydras-setup)))
