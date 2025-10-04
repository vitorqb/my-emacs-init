;; Requires https://github.com/orgs/github/packages/npm/package/copilot-language-server
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest)
  :hook (prog-mode . copilot-mode)
  :bind (("C-, a" . copilot-accept-completion)
         ("C-, l" . copilot-accept-completion-by-line)
         ("C-, p" . copilot-accept-completion-by-paragraph)
         ("C-, w" . copilot-accept-completion-by-word)
         ("C-, N" . copilot-next-completion)
         ("C-, P" . copilot-previous-completion))
  :ensure t)
