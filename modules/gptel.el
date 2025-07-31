(use-package gptel
  :ensure
  :config (setq
           gptel-model   'claude-3.7-sonnet
           gptel-backend (gptel-make-gh-copilot "Copilot")))
