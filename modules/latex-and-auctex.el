;; Auctex is a pain to install using use-package, so we don't
(defun my-latex-add-symbols ()
  "Add some symbols to Latex. Use on TeX-add-style-hook"
  (TeX-add-symbols
   '("frametitle" 1)
   '("framesubtitle" 1)))

(add-hook 'TeX-mode-hook 'my-latex-add-symbols)
(add-hook 'LaTeX-mode-hook 'jas-minor-mode-on)

;; This allows auctex to recognize the \usepackage and \documentclass
(setq TeX-parse-self t)
