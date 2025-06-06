(defvar my/rust-analyzer-bin
  (expand-file-name "~/tools/rust-analyzer/rust-analyzer"))

(use-package rust-mode :ensure)


(use-package eglot
  :config (progn
            (add-to-list 'eglot-server-programs
                         `(rust-mode . (,my/rust-analyzer-bin :initializationOptions
                                                              ( :procMacro (:enable t)))))
            (add-hook 'rust-mode-hook
                      (lambda () (setq-local eglot-auto-shudown t
                                             eglot-prefer-plaintext t
                                             eglot-send-changes-idle-time 2
                                             eglot-extend-to-xref t
                                             eglot-connect-timeout 120
                                             jsonrpc-default-request-timeout 120)))))
