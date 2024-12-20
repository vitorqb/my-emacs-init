;; https://github.com/Wilfred/deadgrep
;; Deadgrep is the fast, beautiful text search that your Emacs deserves.
(use-package deadgrep :ensure)

;; https://github.com/mhayashi1120/Emacs-wgrep
;; Writable grep buffer and apply the changes to files 
(use-package wgrep :ensure)
(use-package wgrep-deadgrep :ensure :after (wgrep deadgrep))

;; Runs deadgrep on project root
(defun my/deadgrep-project-root ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively #'deadgrep)))

;; Custom hydra
(defhydra my/deadgrep-hydra (:color blue)
  ("a" #'deadgrep "Simple search" :column "Deadgrep!")
  ("p" #'my/deadgrep-project-root "Search on project root"))
