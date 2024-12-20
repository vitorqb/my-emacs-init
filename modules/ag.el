;; Very cool search package!
(use-package ag
  :load-path (lambda () (get-dep-library-load-path "ag"))
  :ensure
  :config
  (progn
    (setq ag-highlight-search t)
    ;; Automatically goes to error when selected
    (add-hook 'ag-search-finished-hook 'next-error-follow-minor-mode)
    (global-set-key (kbd "C-c a") 'ag)))

(use-package wgrep :ensure)

(use-package wgrep-ag
  :ensure
  :after (wgrep ag))

(defhydra my/ag-hydra (:color blue)
    ("a" #'ag "Simply ag" :column "Ag!")
    ("A" #'ag-regexp "Ag with regexp")
    ("p" #'projectile-ag "Ag in project")
    ("P" (lambda () (interactive)
           (setq current-prefix-arg '(4))
           (call-interactively #'projectile-ag))))
