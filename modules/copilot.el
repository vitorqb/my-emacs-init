;; Requires https://github.com/orgs/github/packages/npm/package/copilot-language-server
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :hook (prog-mode . copilot-mode)
  :bind (("C-, a" . copilot-accept-completion)
         ("C-, l" . copilot-accept-completion-by-line)
         ("C-, p" . copilot-accept-completion-by-paragraph)
         ("C-, w" . copilot-accept-completion-by-word)
         ("C-, N" . copilot-next-completion)
         ("C-, P" . copilot-previous-completion))
  :ensure t)
