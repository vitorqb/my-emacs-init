(use-package kotlin-mode :ensure)
(use-package eglot :ensure)

;; *WARNING*: THIS ASSUMES KOTLIN LSP IS INSTALLED AT THIS FOLDER
(add-to-list 'eglot-server-programs
             `(kotlin-mode ,(expand-file-name "~/kotlin-language-server/server/bin/kotlin-language-server")))

;; *WARNING* YOU MAY NEED TO ADD SOMETHING LIKE THIS TO YOUR .dir-locals.el so the correct jvm is targeted
;; See https://github.com/fwcd/kotlin-language-server/issues/72
;; See https://github.com/joaotavora/eglot#per-project-server-configuration
;; ((kotlin-mode
;;   . ((eglot-workspace-configuration
;;       . ((:kotlin . (:compiler (:jvm (:target "1.8")))))))))
