;; Source: https://www.lambdacat.com/post-modern-emacs-setup-for-elm/
(use-package elm-mode :ensure
  :config (add-hook 'elm-mode-hook
		    (lambda ()
		      (add-to-list (make-local-variable 'company-backends)
				   '(company-elm company-dabbrev-code))
		      (elm-oracle-setup-completion))))

(use-package flycheck-elm :ensure
  :config (progn
	    (add-hook 'flycheck-mode-hook #'flycheck-elm-setup) ;Add elm flycheck
	    (add-hook 'elm-mode-hook #'flycheck-mode)))		;Calls flycheck-mode on elm

;; Also depends one external elm-oracle
