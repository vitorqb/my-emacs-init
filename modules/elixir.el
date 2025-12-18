;; You will need to install the LSP somehow.
;; Follow https://github.com/elixir-lsp/elixir-ls#detailed-installation-instructions

(emacs-init-load-module-eglot)

(defun my/modules/elixir/mode-hook ()
  ;; Set eglot-connect-timeout to 120 (default is 30) because the elixir lsp
  ;; takes a long time to start (it builds itself)
  (setq-local eglot-connect-timeout 120)
  ;; Don't reformat on typing because it's too slow
  (setq-local eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))

(use-package elixir-mode :ensure
  :hook ((elixir-mode . my/modules/elixir/mode-hook)
         (elixir-ts-mode . my/modules/elixir/mode-hook)))

(defvar emacs-init-elixir/lsp-program
  `("mise" "exec" "--" ,(expand-file-name "~/tools/elixir-ls/language_server.sh")))

(add-to-list 'eglot-server-programs `(elixir-mode . ,emacs-init-elixir/lsp-program))

;; Use tree-sitter based rust mode if available
(add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))
