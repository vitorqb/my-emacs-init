(use-package rust-mode :ensure)

;; Sets how to execute `rust-analyzer`
(defvar my/rust-analyzer-exec
  `("mise" "exec" "--" ,(expand-file-name "~/tools/rust-analyzer/rust-analyzer")))

;; Extra rust-analyzer options
(defvar my/rust-analyzer-init-opts
  '(:procMacro (:enable t) :cargo (:targetDir t)))

;; Tells eglot how to run `rust-analyzer`
(add-to-list 'eglot-server-programs
             `((rust-ts-mode rust-mode) . (,@my/rust-analyzer-exec :initializationOptions ,my/rust-analyzer-init-opts)))

;; Some custom variables we like
(add-hook 'rust-mode-hook
          (lambda () (setq-local eglot-auto-shudown t
                                 eglot-prefer-plaintext t
                                 eglot-send-changes-idle-time 2
                                 eglot-extend-to-xref t
                                 eglot-connect-timeout 120
                                 jsonrpc-default-request-timeout 120)))
