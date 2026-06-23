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

(defconst my/gh/git-scissors-line "# ------------------------ >8 ------------------------")

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

(defun my/gh/update-commit-msg (repo buffer previous-msg user-feedback)
  "Makes a new process that updates a commit msg given some user feedback."
  (interactive (list (projectile-project-root)
                     (current-buffer)
                     (my/gh//extract-previous-msg (current-buffer))
                     (read-string "Feedback to AI: ")))
  (my/gh//check-before-insert-commit-msg repo buffer)
  (message "Asking for a new commit msg...")
  (my/gh//prepare-buffer-for-update-commit-msg buffer)
  (make-process :name "my/gh/update-commit-msg"
                :buffer nil
                :command (list my/gh/ai-commit-msg-command
                               "--git-repo" repo
                               "--previous-msg" (base64-encode-string previous-msg 't)
                               "--user-feedback" user-feedback)
                :sentinel (lambda (p _)
                            (unless (process-live-p p)
                              (if (> (process-exit-status p) 0)
                                  (error "Failed to generate commit msg")
                                (message "Finished generating commit msg"))))
                :filter (lambda (_ string)
                          (when (and (buffer-live-p buffer)
                                     (eq (current-buffer) buffer))
                            (insert string)))))

(defun my/gh//extract-previous-msg (buffer)
  "Extracts the current commit msg from a buffer. Assume we are on a \"commit edit\" buffer."
  (let ((result))
    (with-current-buffer buffer
      (catch 'break
        (dolist (line (s-lines (buffer-substring-no-properties (point-min) (point-max))))
          (when (equal my/gh/git-scissors-line line)
            (throw 'break nil))
          (unless (s-starts-with? "#" line)
            (push line result)))))
    (->> result reverse (s-join "\n"))))

(defun my/gh//prepare-buffer-for-update-commit-msg (buffer)
  "Cleans up the previous commit message from a buffer (before inserting a new one)"
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal my/gh/git-scissors-line (s-trim (thing-at-point 'line t)))))
      (let ((line (thing-at-point 'line t)))
        (if (not (s-starts-with? "#" line))
            (progn
              (delete-region (line-beginning-position) (line-end-position))
              (unless (eobp)
                (delete-char 1)))
          (forward-line 1))))
    (goto-char (point-min))
    (insert "\n\n")
    (goto-char (point-min))))

(provide 'my-gh-commit-msg)
;;; my-gh-commit-msg.el ends here
