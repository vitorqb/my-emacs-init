
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
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (dolist (fun (list #'js2-imenu-extras-mode #'yas-minor-mode-on
                       #'flymake-mode-on))
      (add-hook 'js2-mode-hook fun)
      (add-hook 'js-mode-hook fun))
    ;; Force C-c d to duplicate buffer, overriding existing bind
    (define-key js2-mode-map (kbd "C-c d") #'myutils/duplicate-buffer)
    ;; Same with C-c C-j
    (define-key js2-mode-map (kbd "C-c C-j") #'org-journal-new-entry)
    ;; Use xref to find references. See http://docs.ctags.io/en/latest/ to generate
    ;; the tags file.
    (define-key js2-mode-map (kbd "M-.") #'xref-find-definitions)
    (define-key js-mode-map (kbd "M-.") #'xref-find-definitions)

    ;; Bound yas to shit + TAB to avoid conflict
    (define-key js2-mode-map (kbd "<backtab>") 'yas-expand)
    (setq js2-basic-offset 2)
    (my/add-jest-errors-to-compilation-regexp)

    ;; Also use the derived mode for jsx
    ;; See https://github.com/mooz/js2-mode/blob/bb73461c2c7048d811b38e6b533a30fb5fdcea93/js2-mode.el#L57
    (if (version< emacs-version "27")
        (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
      (add-hook 'js-mode-hook 'js2-minor-mode))
    
    (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

    ;; Adds usefull commands for js2 to mfcs
    (mfcs-add-command
     :description "Npm Js Javascript Run Test"
     :command
     (lambda () (interactive)
       (-let [cmd (format "cd %s && npm run test " (projectile-project-root))]
         (myutils/with-compile-opts "*NpmTest*" cmd
           (call-interactively #'compile)))))
    (mfcs-add-command
     :description "Npm Js Javascript Start"
     :command
     (lambda () (interactive)
       (-let [cmd (format "cd %s && npm run start " (projectile-project-root))]
         (myutils/with-compile-opts "*NpmStart*" cmd
           (call-interactively #'compile)))))))

;; 
;; For development with nodejs
;; 
(use-package indium
  :ensure
  :after js2-mode
  :init   (add-hook 'js2-mode-hook #'indium-interaction-mode)
  ;; Force C-c d to duplicate buffer, overriding existing bind
  :config
  (progn
    (define-key indium-interaction-mode-map (kbd "C-c d") #'myutils/duplicate-buffer)))


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
            (eldoc-mode +1)
            (tide-hl-identifier-mode +1)
            (company-mode +1))
          (add-hook 'js2-mode-hook #'setup-tide-mode)
          (add-hook 'js-mode-hook #'setup-tide-mode)))
