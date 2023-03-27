(require 'eldoc)
(require 'yasnippet)

;; Eglot is usually good enough
(emacs-init-load-module-eglot)

;; js2-mode for everything else
(use-package js2-mode
  :ensure
  :after (:all flycheck)
  :config
  (progn

    ;; Auto load for js files
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

    ;; Use yas
    (add-hook 'js2-mode-hook #'yas-minor-mode-on)

    ;; Remove keys that we bind globally
    ;; Force C-c d to duplicate buffer, overriding existing bind
    (define-key js2-mode-map (kbd "C-c d") nil)
    (define-key js2-mode-map (kbd "C-c C-j") nil)
    (define-key js2-mode-map (kbd "<backtab>") nil)

    ;; Set's offset
    (setq js2-basic-offset 2)))

;; For ReactJS
(use-package rjsx-mode
  :ensure
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))))
