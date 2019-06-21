(use-package go-mode :ensure)
(use-package company-go :ensure
  :after company
  :config (progn
            (push 'company-go company-backends)))
