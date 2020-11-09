
;;
;; js2-mode for javascript
;;
(defun my/add-jest-errors-to-compilation-regexp ()
  "Make compilation aware of how to find the path to jest errors"
  (interactive)
  (push 'npm-jest-errors compilation-error-regexp-alist)
  (push '(npm-jest-errors
          "^[ ]*at.*[ ](?\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2)
        compilation-error-regexp-alist-alist))

(use-package js2-mode
  :ensure
  :after (:all flycheck)
  :config
  (progn

    ;; Auto load for js files
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

    ;; Use yas, flymake and js2-imenu-extra
    (dolist (fun (list #'js2-imenu-extras-mode #'yas-minor-mode-on))
      (add-hook 'js2-mode-hook fun)
      (add-hook 'js-mode-hook fun))

    ;; Remove keys that we bind globally
    ;; Force C-c d to duplicate buffer, overriding existing bind
    (define-key js2-mode-map (kbd "C-c d") nil)
    (define-key js2-mode-map (kbd "C-c C-j") nil)
    (define-key js2-mode-map (kbd "<backtab>") nil)

    ;; Set's offset
    (setq js2-basic-offset 2)

    ;; Adds jest errors to compilation
    (my/add-jest-errors-to-compilation-regexp)

    ;; Use js2 in mode interpreter
    (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))))

(use-package rjsx-mode
  :ensure
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))))

;; 
;; For development with nodejs
;; 
(use-package indium
  :ensure
  :after js2-mode
  :init   (add-hook 'js2-mode-hook #'indium-interaction-mode)
  :config
  (progn
    (define-key indium-interaction-mode-map (kbd "C-c d") nil)))


;;
;; Tide for company completion
;; 
(use-package tide
  :ensure
  :after js2-mode
  :init (progn
          (defun setup-tide-mode ()
            (interactive)
            (tide-setup)
            (flycheck-mode +1)
            (eldoc-mode +1))
          (add-hook 'js2-mode-hook #'setup-tide-mode)
          (add-hook 'js-mode-hook #'setup-tide-mode)))
