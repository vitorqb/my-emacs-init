;;; magit-ext.el --- Magit extensions -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-08-04
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'magit-ext)

(defvar magext--system-prompt
  "You are an experienced senior developer who cares deeply about writing maintainble and high-quality code following all best practices."
  "System Prompt for AI chat to generate commit messages.")

(defvar magext--user-prompt
  "Analyze carefully the given git diff. Create a short and clear commit message based on the changes, following best practices. Avoid phrases like \"Improving maintainability\" or \"emphasizing maintainability and best practices\". Focus on describing the changes and their concrete goals. Use the `git log` as inspiration."
  "User Prompt for AI chat to generate commit messages.")

(defun magext--staged-changes-to-file (dir)
  "Saves the staged changes to a file. Returns the file."
  (let ((tmpfile (make-temp-file "magext--staged-changes-to-file")))
    (call-process "git"                       ;Runs git
                  nil                         ;No INFILE
                  `(:file ,tmpfile)           ;Save to tempfile
                  nil                         ;Don't display result to user
                  "-C" dir "diff" "--staged"  ;Git args
                  )
    tmpfile))

(defun magext--git-history-to-file (dir)
  "Saves the last 10 commit messages to a file"
  (let ((tmpfile (make-temp-file "magext--last-10-commits-to-file")))
    (call-process "git"                             ;Runs git
                  nil                               ;No INFILE
                  `(:file ,tmpfile)                 ;Save to tempfile
                  nil                               ;Don't display result to user
                  "--no-pager" "-C" dir "log" "-10" ;Git args
                  )
    tmpfile))

(defun magext--commit-msg (diff-file git-history-file)
  "Generates a commit msg for a git diff output"
  (shell-command-to-string (format "aichat --prompt '%s' -f '%s' -f '%s' '%s'"
                                   magext--system-prompt
                                   diff-file
                                   git-history-file
                                   magext--user-prompt)))

(defun magext-prefill-commit-msg (dir)
  "Prefills the commit msg magit buffer with a generated one based on your git changes"
  (interactive (list default-directory))
  (let* ((default-directory dir)
         (diff-file (magext--staged-changes-to-file dir))
         (git-history-file (magext--git-history-to-file dir))
         (commit-msg (magext--commit-msg diff-file git-history-file)))
    (goto-char (point-min))
    (insert commit-msg)
    (delete-file diff-file)))
;;; magit-ext.el ends here
