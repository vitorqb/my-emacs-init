;;; my-gh-edit-pr-body.el --- Edit a GH PR body -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(require 'magit)
(require 'dash)
(require 'markdown-mode)
(require 'my-gh-core)

(defvar my/gh/edit-pr-body-buffer-name "*my/gh/edit-pr*"
  "Buffer name to use when editing a PR body")

(defvar-local my/gh//edit-pr-metadata-current-pr-number nil
  "Used to store metadata on a buffer that edits a PR")

(defvar-local my/gh//edit-pr-metadata-default-directory nil
  "Used to store metadata on a buffer that edits a PR")

(defun my/gh/insert-pr-body (pr-number)
  "Prints the current PR body on the current buffer."
  (interactive)
  (call-process "gh" nil (current-buffer) t "pr" "view" pr-number "--json=body" "--template={{.body}}")
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defvar my/gh/edit-pr-body-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my/gh/edit-pr-body-done)
    (define-key map (kbd "C-c C-k") #'my/gh/edit-pr-body-abort)
    (set-keymap-parent map markdown-mode-map)
    map)
  "A map for the mode responsible of editing a PR body")

(define-derived-mode my/gh/edit-pr-body-mode markdown-mode "My/Gh/EditPrBody"
  "Mode used to edit PR bodies."
  :keymap my/gh/edit-pr-body-mode-map)

;; TODO -> Accept PR number as argument and default to current
(defun my/gh/edit-pr-body ()
  "Allows editing current PR body."
  (interactive)

  ;; Make sure we don't have two buffers editing PRs
  (when (buffer-live-p (get-buffer my/gh/edit-pr-body-buffer-name))
      (user-error "Buffer %s still exists - kill it first!" my/gh/edit-pr-body-buffer-name))

  (let ((root-default-directory default-directory)
        (pr-number (my/gh//current-pr-number))
        (buff (get-buffer-create my/gh/edit-pr-body-buffer-name)))
    (switch-to-buffer-other-window buff)
    (setq-local my/gh//edit-pr-metadata-default-directory root-default-directory)
    (setq-local my/gh//edit-pr-metadata-current-pr-number pr-number)
    (my/gh/insert-pr-body pr-number)
    (goto-char (point-min))
    (my/gh/edit-pr-body-mode)))

(defun my/gh/edit-pr-body-done ()
  "User saying they are done editing the PR body"
  (interactive)
  (let* ((default-directory my/gh//edit-pr-metadata-default-directory)
         (pr-number my/gh//edit-pr-metadata-current-pr-number)
         (tmpfile (make-temp-file "gh-pr-body-")))
    (unless default-directory
      (user-error "Failed to load default directory from metadata"))
    (unless pr-number
      (user-error "Failed to load pr-number from metadata"))
    (write-region nil nil tmpfile)      ;Write to file
    (message "Submitting PR Body for pr %s directory %s" pr-number default-directory)
    (let ((exitcode (call-process "gh" nil "*gh-submit-pr-body*" t "pr" "edit" pr-number "--body-file" tmpfile)))
      (unless (= exitcode 0)
        (user-error "Failed: check *gh-submit-pr-body*")))
    (kill-buffer)
    (message "Submitted!")))

(defun my/gh/edit-pr-body-abort ()
  "User saying they gave up editing the PR body"
  (interactive)
  (message "Bailing!")
  (kill-buffer))

(provide 'my-gh-edit-pr-body)
;;; my-gh.el ends here
