;;; my-gh-commit-msg.el --- Custom github commit message utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2026-06-21
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(require 'projectile)
(require 's)

(defvar my/gh/ai-commit-msg-command nil
  "An executable called to generate a commit msg. Must receive `--git-repo` argument with the path to a repository for which the git commit msg must be created.")

(defun my/gh//check-before-insert-commit-msg (repo buffer)
  "Sanity checks before we try to insert commit msg"
  (unless my/gh/ai-commit-msg-command
    (user-error "Variable my/gh/ai-commit-msg-command is not set. Set it to an executable that will generate a commit message and that accepts `--git-repo` as an argument."))
  (unless repo
    (user-error "No repository was given"))
  (unless (file-directory-p repo)
    (user-error "Not a directory: %s" repo))
  (unless (s-matches? ".*COMMIT_.*" (buffer-name buffer))
    (unless (yes-or-no-p "Your active buffer does not seem to be editing a commit. Continue anyway?")
      (user-error "Stopped by user"))))

(defun my/gh/insert-commit-msg (repo buffer)
  "Makes a new process that generates and inserts the commit message on a buffer."
  (interactive (list (projectile-project-root) (current-buffer)))
  (my/gh//check-before-insert-commit-msg repo buffer)
  (message "Asking for commit msg...")
  (make-process :name "my/gh/mk-ai-commit-msg"
                :buffer nil
                :command (list my/gh/ai-commit-msg-command "--git-repo" repo)
                :sentinel (lambda (p _)
                            (unless (process-live-p p)
                              (if (> (process-exit-status p) 0)
                                  (error "Failed to generate commit msg")
                                (message "Finished generating commit msg"))))
                :filter (lambda (_ string)
                          (when (and (buffer-live-p buffer)
                                     (eq (current-buffer) buffer))
                            (insert string)))
                :stderr (get-buffer-create "*my-gh-commit-msg-stderr*")))

(provide 'my-gh-commit-msg)
;;; my-gh-commit-msg.el ends here
