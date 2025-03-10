;; https://github.com/Wilfred/deadgrep
;; Deadgrep is the fast, beautiful text search that your Emacs deserves.
(use-package deadgrep :ensure)

;; https://github.com/mhayashi1120/Emacs-wgrep
;; Writable grep buffer and apply the changes to files 
(use-package wgrep :ensure)
(use-package wgrep-deadgrep :ensure :after (wgrep deadgrep))

;; Runs deadgrep on project root
(defun my/deadgrep-current-dir ()
  (interactive)
  (let ((deadgrep-project-root-function (lambda () default-directory)))
    (call-interactively #'deadgrep )))

;; Custom hydra
(defhydra my/deadgrep-hydra (:color blue)
  ("a" #'my/deadgrep-current-dir "Simple search on default dir" :column "Deadgrep!")
  ("p" #'my/deadgrep-project-root "Search on project root"))
