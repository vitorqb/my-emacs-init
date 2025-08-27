;; -----------------------------------------------------------------------------
;; Language Server Protocol
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/eglot-hydra ()
  "Prepares an hydra for eglot."
  (defhydra my/eglot-hydra (:color blue)
    ("a" #'eglot-code-actions "Eglot code actions" :column "Eglot")
    ("e" #'eglot "Start/restart")
    ("h" #'eglot-inlay-hints-mode "Inline Hints mode")
    ("r" #'eglot-rename "Eglot rename")))

(use-package eglot :ensure
  :custom (eglot-report-progress 'messages "Send eglot progress messages to *Message*")
  :config (progn
            ;; If we are using eglot, bind M-. to xref, that eglot itself uses
            (add-hook
             'eglot--managed-mode-hook
             (lambda () (local-set-key (kbd "M-.") 'xref-find-definitions)))

            ;; Refresh hydra
            (my/hydras-setup)))
