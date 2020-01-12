(use-package clojure-mode
  :ensure
  :config (progn
            ;; Use 1 indent for match macro
            (put-clojure-indent 'match 1)
            (add-hook 'clojurescript-mode-hook #'yas-minor-mode-on)
            (add-hook 'clojurescript-mode-hook #'eldoc-mode)
            (add-hook 'clojurescript-mode-hook #'lightlispy-mode)
            (add-hook 'clojure-mode-hook #'yas-minor-mode-on)
            (add-hook 'clojure-mode-hook #'eldoc-mode)
            (add-hook 'clojure-mode-hook #'lightlispy-mode)))

(use-package clojure-mode-extra-font-locking :ensure)

;; For refactoring
(use-package clj-refactor :ensure
  :config (progn
            ;; Let's put a more usefull test import for cljs
            (setq-default cljr-cljs-clojure-test-declaration
                  (concat "[cljs.test :refer-macros [is are"
                          " deftest testing use-fixtures async]]"))
            ;; And for clojure
            (setq-default cljr-clojure-test-declaration
                          (concat "[clojure.test :as t :refer [is are deftest testing"
                                  " use-fixtures]]"))))

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
     :command #'cider-jack-in-cljs)

    (mfcs-add-command
     :description "Cider Jack In"
     :command #'cider-jack-in)))

;; Misc functions
(defun emacs-init-modules-clojure/map-with (str)
  "Given a space-separated list of words, inserts a clojure map with keys and values
   of those words."
  (interactive "sEnter a space-separated list of symbols: ")
  (->> str
       (s-split "\\s-")
       (-mapcat (lambda (x) (list (concat ":" x) x)))
       (s-join " ")
       insert))

(mfcs-add-command
 :description "Clojure insert map with keys and values [keys map insert clojure]"
 :command #'emacs-init-modules-clojure/map-with)
