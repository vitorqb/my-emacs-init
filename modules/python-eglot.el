(require 'eldoc)
(emacs-init-load-module-eglot)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)

;; For virtual environments
(use-package pyvenv :ensure)

;; Snippets S2
(add-hook 'python-mode-hook #'yas-minor-mode-on)

;; Helper functions
(defun my/python/find-dominating-venv (dir)
  (-reduce-from (lambda (acc x)
                  (or acc (if-let ((parent (locate-dominating-file default-directory x))
                                   (venv (expand-file-name x parent)))
                              venv)))
                nil
                '("venv" ".venv")))

;; Help for activating virtual environment
(defun my/python-eglot/start-pyright-langserver (arg)
  (if-let ((venv (my/python/find-dominating-venv default-directory)))
      `("bash" "-c" ,(concat "source " venv "/bin/activate && pyright-langserver --stdio"))
    '("pyright-langserver" "--stdio")))

(add-to-list 'eglot-server-programs `(python-mode . ,#'my/python-eglot/start-pyright-langserver))
