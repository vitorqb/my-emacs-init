(use-package rust-mode :ensure)

;; Add this to `mise.toml`:
;; ```
;; [tools]
;; "rust" = { version = "1.90.0", components = "rust-analyzer" }
;; ```
;; You might need to `rust install --force rust`
(defvar my/rust-analyzer-exec
  `("mise" "exec" "--" "rust-analyzer"))

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

;; Use tree-sitter based rust mode if available.
;; To install:
;; M-x treesit-install-language-grammar
;; > rust
;; > https://github.com/tree-sitter/tree-sitter-rust
;; RET RET RET
(add-to-list 'major-mode-remap-alist '(rust-mode . rust-ts-mode))
