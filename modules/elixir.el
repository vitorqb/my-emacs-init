;; You will need to install the LSP somehow.
;; Follow https://github.com/elixir-lsp/elixir-ls#detailed-installation-instructions

(emacs-init-load-module-eglot)
(use-package elixir-mode :ensure
  ;; Set eglot-connect-timeout to 120 (default is 30) because the elixir lsp
  ;; takes a long time to start (it builds itself)
  :hook ((elixir-mode . (lambda () (setq eglot-connect-timeout 120)))))

(defvar emacs-init-elixir/lsp-program
  `("mise" "exec" "--" ,(expand-file-name "~/tools/elixir-ls/language_server.sh")))

(add-to-list 'eglot-server-programs `(elixir-mode . ,emacs-init-elixir/lsp-program))

;; Use tree-sitter based rust mode if available
(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
