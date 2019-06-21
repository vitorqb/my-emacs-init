(use-package clojure-mode
  :ensure
  :after (lispy)
  :config (progn
            ;; Use 1 indent for match macro
            (put-clojure-indent 'match 1)
            (add-hook 'clojurescript-mode-hook #'yas-minor-mode-on)
            (add-hook 'clojurescript-mode-hook #'lispy-mode)
            (add-hook 'clojure-mode-hook #'yas-minor-mode-on)
            (add-hook 'clojure-mode-hook #'lispy-mode)))

(use-package clojure-mode-extra-font-locking :ensure)

;; For refactoring
(use-package clj-refactor :ensure
  :config (progn
            ;; Let's put a more usefull test import for cljs
            (setq-default cljr-cljs-clojure-test-declaration
                  (concat "[cljs.test :refer-macros [is are"
                          " deftest testing use-fixtures async]]"))))

(use-package cider
  :ensure
  :config
  (progn
    (setq nrepl-log-messages t)
    (define-key cider-mode-map (kbd "C-c C-o") #'myutils/clojure-occur-def)
    (define-key cider-mode-map (kbd "C-c C-f") nil)
    ;; Don's use linum mode on repl
    (add-hook 'cider-repl-mode-hook #'my/disable-linum)

    ;; Select a bunch of company backends
    (add-hook 'cider-mode-hook
              (lambda ()
                (make-local-variable 'company-backends)
                (setq-local company-backends
                            '((company-capf
                               company-dabbrev-code
                               company-gtags
                               company-etags
                               company-keywords
                               company-dabbrev)))))

    ;; Adds commands to fuzzy cmd selector
    (mfcs-add-command
     :description "Clojure Lein Test Refresh Watch"
     :command
     (lambda () (interactive)
       (-let [cmd (format "cd %s && lein test-refresh " (projectile-project-root))]
         (myutils/with-compile-opts "*LeinTestRefresh*" cmd
           (call-interactively #'compile)))))

    (mfcs-add-command
     :description "Clojure Lein Run"
     :command
     (lambda () (interactive)
       (-let [cmd (format "cd %s && lein run " (projectile-project-root))]
         (myutils/with-compile-opts "*LeinRun*" cmd
           (call-interactively #'compile)))))

    (mfcs-add-command
     :description "Lein Doo Firefox Test"
     :command
     (lambda () (interactive)
       (-let [cmd (format "cd %s && lein doo firefox " (projectile-project-root))]
         (myutils/with-compile-opts "*LeinDoo*" cmd
           (call-interactively #'compile)))))

    (mfcs-add-command
     :description "Cider Jack In Clojurescript Cljs"
     :command #'cider-jack-in-cljs)))
