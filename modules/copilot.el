;; -*- lexical-binding: t; -*-
;; Requires https://github.com/orgs/github/packages/npm/package/copilot-language-server
;; On mise.toml: `"github:github/copilot-language-server-release" = "xxx"`
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-, a" . copilot-accept-completion)
         ("C-, l" . copilot-accept-completion-by-line)
         ("C-, p" . copilot-accept-completion-by-paragraph)
         ("C-, w" . copilot-accept-completion-by-word)
         ("C-, N" . copilot-next-completion)
         ("C-, P" . copilot-previous-completion))
  :init
  (setq copilot-server-executable (->> (getenv "HOME")
                                       (format "mise -C %s exec -- which copilot-language-server")
                                       (shell-command-to-string)
                                       (s-trim)))
  :config
  (setq copilot-idle-delay 0.5)

  :ensure t)

;; To avoid clashes with inline completion, we need to use company-box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-enable t)
  (setq company-box-doc-delay 0.5)
  :ensure t)
