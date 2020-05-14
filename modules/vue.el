;; Use dev version for mmm-mode - REQUIRES MANUAL STEPS.
;; See https://github.com/AdamNiederer/vue-mode/issues/99
;; https://github.com/purcell/mmm-mode/issues/99
(use-package mmm-mode
  :ensure
  :load-path "/home/vitor/git-others/mmm-mode")

(use-package vue-mode
  :ensure
  :config (progn
            (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
            (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))))
