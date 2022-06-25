(emacs-init-load-module-eglot)
(use-package elixir-mode :ensure)

(defvar emacs-init-elixir/lsp-program
  (expand-file-name "~/tools/elixir-ls/language_server.sh"))

;; You will need to install the LSP somehow.
;; Follow https://github.com/elixir-lsp/elixir-ls#detailed-installation-instructions
(when (not (file-exists-p emacs-init-elixir/lsp-program))
  (display-warning
   :error
   (s-concat "Could not fund executable for elixir-ls "
             "(language server) in "
             emacs-init-elixir/lsp-program
             ". Maybe you need to install it?"
             " Check "
             "https://github.com/elixir-lsp/elixir-ls#detailed-installation-instructions")))

(add-to-list 'eglot-server-programs `(elixir-mode ,emacs-init-elixir/lsp-program))
