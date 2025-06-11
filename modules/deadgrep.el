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

;; Customize grep-find to use `rg`
;; Copied from `grep.el`
;; Notice it also requires `fd`: https://github.com/sharkdp/fd
(setq deadgrep-find-command
      '("fd . --type f --exec rg --no-heading --color=auto -nH --null -e  \\{\\}" . 65))
(defun my/deadgrep/grep-find (command-args)
  (interactive
   (list (read-shell-command "Run find (like this): " deadgrep-find-command 'grep-find-history)))
  (grep command-args))

;; Custom hydra
(defhydra my/deadgrep-hydra (:color blue)
  ("a" #'my/deadgrep-current-dir "Simple search on default dir" :column "Deadgrep!")
  ("p" #'my/deadgrep-project-root "Search on project root")
  ("f" #'my/deadgrep/grep-find "grep-find on current dir")
  ("F" (lambda () (interactive)
         (projectile-with-default-dir (projectile-acquire-root)
           (call-interactively #'my/deadgrep/grep-find)))
   "grep-find on project root"))

(my/hydras-setup)
