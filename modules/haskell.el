(use-package intero :ensure)
(use-package haskell-mode
  :ensure
  :after intero
  :hook ((intero-mode . (lambda () (setq flycheck-display-errors-delay 0.3)))))

(intero-global-mode 1)
;; Use most recent version
;; !!!! TODO -> Still needed?
(setq intero-package-version "0.1.32")
