(use-package aidermacs
  :ensure
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
  (aidermacs-default-model "claude-3-5-haiku-latest")
  (aidermacs-comint-multiline-newline-key "S-<return>")
  (aidermacs-vterm-multiline-newline-key "S-<return>"))

(my/hydras-setup)
