(defun my/copilot/ensure-installed ()
  "Ensure that the copilot binary installed."
  (message "Refreshing copilot stub...")
  (let ((proc (start-process "copilot-version" "*copilot-version*" copilot-server-executable "--version")))
    (set-process-sentinel proc (lambda (process event)
      (when (string= event "finished\n")
        (message "Copilot stub refreshed."))))))

;; Requires https://github.com/orgs/github/packages/npm/package/copilot-language-server
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-, a" . copilot-accept-completion)
         ("C-, l" . copilot-accept-completion-by-line)
         ("C-, p" . copilot-accept-completion-by-paragraph)
         ("C-, w" . copilot-accept-completion-by-word)
         ("C-, N" . copilot-next-completion)
         ("C-, P" . copilot-previous-completion))
  :init
  (progn
    (setq copilot-server-executable (file-name-concat my/path-to-stubs-dir "copilot-language-server"))
    (my/copilot/ensure-installed))
  :config
  (progn
    (setq copilot-idle-delay 0.5))
  :ensure t)

;; To avoid clashes with inline completion, we need to use company-box
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-doc-enable t)
  (setq company-box-doc-delay 0.5)
  :ensure t)
