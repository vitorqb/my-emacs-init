(defun my/setup-tide-mode ()
  "Setup for tide mode (Typescript). From https://github.com/ananthakumaran/tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (yas-minor-mode-on))

(defun my/setup-tsx ()
  "Setup to edit tsx files. From https://github.com/ananthakumaran/tide"
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (my/setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; We usually want 2 indent spaces, not 4
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2))

(use-package tide :ensure t)
(use-package web-mode :ensure t) ;; Needed for tsx
(use-package typescript-mode :ensure
  :after (tide web-mode)
  :hook ((typescript-mode . my/setup-tide-mode))
  :config
  (progn
    (add-hook 'tide-mode-hook
              (lambda () (cl-pushnew 'company-tide company-backends)))
    (setq typescript-indent-level 2)
    (myutils/active-flycheck-for-typescript)
    (my/setup-tsx)))
