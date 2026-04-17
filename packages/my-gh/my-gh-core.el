;;; my-gh-core.el --- Cusotm github utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(require 'dash)

(defun my/gh//current-branch ()
  (-> (shell-command-to-string "git branch --show-current")
      (s-trim)))

(defun my/gh//branch-exists? (name)
  ;; Exit 0 means exists
  (let ((verify-status (call-process "git" nil nil nil "rev-parse" "--verify" name)))
    (or (equal verify-status 0) (equal verify-status "0"))))

(defun my/gh//pr-number-from-branch (branch)
  (if (and branch (string-match "pr-\\([0-9]+\\)-merge" branch))
      (match-string 1 branch)))

(defun my/gh//pr-number-from-gh ()
  (-> (shell-command-to-string "gh pr view --json number --jq .number")
      (s-trim)))

(defun my/gh//current-pr-number ()
  (or (-> (my/gh//current-branch) (my/gh//pr-number-from-branch))
      (my/gh//pr-number-from-gh)))

(provide 'my-gh-core)

;;; my-gh-core.el ends here
