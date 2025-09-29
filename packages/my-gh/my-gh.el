;;; my-gh.el --- Cusotm github utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'my-gh)
(require 'my-term)

(defvar my/gh/on-browser-open-request
  (lambda ()
    (interactive)
    (shell-command "i3-msg [urgent=latest] focus"))
  "Callback to be run when we send something to the browser")

(defvar my/gh/copy-to-clipboard
  (lambda (x) (kill-new x)))

(defun my/gh/open-repo-on-browser ()
  (interactive)
  (shell-command "gh repo view --web")
  (funcall my/gh/on-browser-open-request))

(defun my/gh/open-pr-on-browser ()
  (interactive)
  (shell-command "gh pr view --web")
  (funcall my/gh/on-browser-open-request))

(defun my/gh/new-pr ()
  "Opens tmxu with prompts for a new PR"
  (interactive)
  (my/term/run "gh pr create"))

(defun my/gh/print-pr-body ()
  "Prints the current PR body on the current buffer."
  (interactive)
  (insert (shell-command-to-string "gh pr view --json=body --jq='.body'")))

(defun my/gh/edit-pr-body ()
  "Allows editing current PR body."
  (interactive)
  (let ((buff (generate-new-buffer "*gh-pr-body*"))
        (default-directory default-directory))
    (switch-to-buffer buff)
    (my/gh/print-pr-body)
    (when (and (require 'markdown-mode nil 'noerror)
               (fboundp 'markdown-mode))
      (markdown-mode))))

(defun my/gh//browse-cmd (file-path &optional line-number no-browser)
  "Returns the `gh` command to be used for browsing to a file/line number"
  (let* ((default-directory (or (file-name-directory file-path) default-directory))
         (file-name (file-name-nondirectory file-path))
         (file-ref (if line-number (format "%s:%s" file-name line-number) file-name)))
    (if no-browser
        (format "gh browse --no-browser %s" file-ref)
      (format "gh browse %s" file-ref))))

(defun my/gh/browse (file-path &optional line-number)
  "Browses to the current file/line."
  (interactive (list buffer-file-name (line-number-at-pos)))
  (shell-command (my/gh//browse-cmd file-path line-number))
  (funcall my/gh/on-browser-open-request))

(defun my/gh/browse-url-to-clipboard (file-path &optional line-number)
  "Copies the browse URL to clipboard"
  (interactive (list buffer-file-name (line-number-at-pos)))
  (->> (my/gh//browse-cmd file-path line-number 't)
       (shell-command-to-string)
       (s-trim)
       (funcall my/gh/copy-to-clipboard)))

(defun my/gh/atlantis-plan ()
  (interactive)
  (let ((cmd-out (shell-command-to-string "gh pr comment --body='atlantis plan'")))
    (browse-url (string-trim cmd-out))))

(defun my/gh//browse-commit-cmd (commit-hash &optional no-browser)
  "Returns the `gh` command to be used for browsing to a commit"
  (if no-browser
      (format "gh browse --no-browser %s" commit-hash)
    (format "gh browse %s" commit-hash)))

(defun my/gh/browse-commit (commit)
  (interactive (list (thing-at-point 'symbol)))
  (->> commit (format "%s") my/gh//browse-commit-cmd shell-command)
  (funcall my/gh/on-browser-open-request))

(when (require 'hydra nil 'noerror)
  (defun my/setup-hydra/gh-hydra ()
    (defhydra my/gh-hydra (:color blue)
      ("b" #'my/gh/browse "Browses to file in github" :column "Github CLI!")
      ("B" #'my/gh/browse-url-to-clipboard "Copies github browse URL to clipboard")
      ("r" #'my/gh/open-repo-on-browser "Open repo on browser")
      ("p" #'my/gh/open-pr-on-browser "Open PR on browser")
      ("P" #'my/gh/new-pr "Creates a new PR")
      ("c" #'my/gh/browse-commit "See the commit on github web"))))

(defun my/gh/default-branch ()
  (-> (shell-command-to-string "gh repo view --json 'defaultBranchRef' --jq '.defaultBranchRef.name'")
      (string-trim)))

;;; my-gh.el ends here
