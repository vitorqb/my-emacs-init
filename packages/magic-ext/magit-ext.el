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
(require 'magit)

(defvar magext--system-prompt
  "You are an experienced senior developer who cares deeply about writing maintainble and high-quality code following all best practices."
  "System Prompt for AI chat to generate commit messages.")

(defvar magext--user-prompt
  "Analyze carefully the given git diff. Create a short and clear commit message based on the changes, following best practices. Avoid phrases like \"Improving maintainability\" or \"emphasizing maintainability and best practices\". Focus on describing the changes and their concrete goals. Use the provided `git log` as inspiration. OUTPUT ONLY THE COMMIT MESSAGE AND NOTHING ELSE."
  "User Prompt for AI chat to generate commit messages.")

(defvar magext-aichat-model
  nil
  "Aichat model to use")

(defun magext--diff-to-file (dir)
  "Saves the current magit diff to a file. Returns the file."
  (let* ((default-directory dir)
         (tmpfile (make-temp-file "magext--diff-to-filedir"))
         (diffbuf (or (magit-get-mode-buffer 'magit-diff-mode)
                      (user-error "No buffer with magit diff was found!")))
         (diff    (with-current-buffer diffbuf
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (write-region diff nil tmpfile)
    tmpfile))

(defun magext--git-history-to-file (dir)
  "Saves the last 10 commit messages to a file"
  (let ((tmpfile (make-temp-file "magext--last-10-commits-to-file")))
    (call-process "git"                 ;Runs git
                  nil                   ;No INFILE
                  `(:file ,tmpfile)     ;Save to tempfile
                  nil                   ;Don't display result to user
                  "--no-pager" "-C" dir "log" "-10" ;Git args
                  )
    tmpfile))

(defun magext--commit-msg (diff-file git-history-file)
  "Generates a commit msg for a git diff output"
  (let* ((cmd (format "aichat --prompt '%s'" magext--system-prompt))
         (cmd (if magext-aichat-model
                  (format "%s --model '%s'" cmd magext-aichat-model)
                cmd))
         (cmd (format "%s -f '%s' -f '%s' '%s'" cmd diff-file git-history-file magext--user-prompt)))
    (message cmd)
    (shell-command-to-string cmd)))

(defun magext-prefill-commit-msg (dir)
  "Prefills the commit msg magit buffer with a generated one based on your git changes"
  (interactive (list default-directory))
  (let* ((default-directory dir)
         (diff-file (magext--diff-to-file dir))
         (git-history-file (magext--git-history-to-file dir))
         (commit-msg (magext--commit-msg diff-file git-history-file)))
    (goto-char (point-min))
    (insert commit-msg)
    ;; (delete-file diff-file)
    ))
;;; magit-ext.el ends here
