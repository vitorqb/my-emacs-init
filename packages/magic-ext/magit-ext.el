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

(setq magext--aichat-prompt "You are an experienced developer. Analyze carefully the git diff below. Create a concize and clear commit message based on the changes. Follow all best-practices for commit messages. Follow the \"Semantic Commits\" standard.")

(defun magext--staged-changes-to-file (dir)
  "Saves the staged changes to a file. Returns the file."
  (let ((tmpfile (make-temp-file "magext--staged-changes-to-file"))
        (git-diff-cmd (format "git -C '%s' diff --staged" dir)))
    (call-process "git"                       ;Runs git
                  nil                         ;No INFILE
                  `(:file ,tmpfile)           ;Save to tempfile
                  nil                         ;Don't display result to user
                  "-C" dir "diff" "--staged"  ;Git args
                  )
    tmpfile))

(defun magext--commit-msg (diff-file)
  "Generates a commit msg for a git diff output"
  (shell-command-to-string (format "aichat -f '%s' '%s'" diff-file magext--aichat-prompt)))

(defun magext-prefill-commit-msg (dir)
  "Prefills the commit msg magit buffer with a generated one based on your git changes"
  (interactive (list default-directory))
  (let* ((default-directory dir)
         (diff-file (magext--staged-changes-to-file dir))
         (commit-msg (magext--commit-msg diff-file)))
    (goto-char (point-min))
    (insert commit-msg)))

;;; magit-ext.el ends here
