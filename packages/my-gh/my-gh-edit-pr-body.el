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

(defun my/gh/edit-pr-body (pr-number)
  "Allows editing current PR body."
  (interactive (list (my/gh//current-pr-number)))

  ;; Make sure we don't have two buffers editing PRs
  (when (buffer-live-p (get-buffer my/gh/edit-pr-body-buffer-name))
      (user-error "Buffer %s still exists - kill it first!" my/gh/edit-pr-body-buffer-name))

  (let ((root-default-directory default-directory)
        (buff (get-buffer-create my/gh/edit-pr-body-buffer-name)))
    (with-current-buffer buff
      (my/gh/edit-pr-body-mode)
      (my/gh/insert-pr-body pr-number)
      (goto-char (point-min))
      (setq-local my/gh//edit-pr-metadata-default-directory root-default-directory)
      (setq-local my/gh//edit-pr-metadata-current-pr-number pr-number))
    (switch-to-buffer-other-window buff)))

(cl-defstruct (my/gh//edit-pr-body-request
               (:constructor my/gh//create-edit-pr-body-request)
               (:conc-name my/gh//edit-pr-body-request/)
               (:copier my/gh//copy-edit-pr-body-request))
  "Represents a request to edit a PR Body"
  (gitrepo-directory nil :type string :documentation "Directory to git repo")
  (pr-number nil :type string :documentation "Number of PR to edit")
  (src-buffer nil :type buffer :documentation "Source buffer where to read new body string")
  (tmp-file nil :type string :documentation "Temp file to use to submit new body"))

(defun my/gh//validate-edit-pr-body-request (x)
  (unless (buffer-live-p (my/gh//edit-pr-body-request/src-buffer x))
    (user-error "Invalid request: source buffer is not alive"))
  (unless (stringp (my/gh//edit-pr-body-request/gitrepo-directory x))
    (user-error "Invalid request: gitrepo-directory is not a string"))
  (unless (stringp (my/gh//edit-pr-body-request/pr-number x))
    (user-error "Invalid request: pr-number is not a string"))
  (unless (stringp (my/gh//edit-pr-body-request/tmp-file x))
    (user-error "Invalid request: tmp-file is not a string")))

(defun my/gh//do-edit-pr-body (request)
  (my/gh//validate-edit-pr-body-request request)
  (let ((default-directory (my/gh//edit-pr-body-request/gitrepo-directory request))
        (pr-number         (my/gh//edit-pr-body-request/pr-number request))
        (src-buffer        (my/gh//edit-pr-body-request/src-buffer request))
        (tmp-file          (my/gh//edit-pr-body-request/tmp-file request)))
    (with-current-buffer src-buffer
      (write-region nil nil tmp-file))     ;Write to file
    (message "Submitting PR Body for pr %s directory %s" pr-number default-directory)
    (let ((exitcode (call-process "gh" nil "*gh-submit-pr-body*" t "pr" "edit" pr-number "--body-file" tmp-file)))
      (unless (= exitcode 0)
        (user-error "Failed: check *gh-submit-pr-body*")))
    (kill-buffer src-buffer)
    (delete-file tmp-file)
    (message "Submitted!")))

(defun my/gh/edit-pr-body-done ()
  "User saying they are done editing the PR body"
  (interactive)
  (let* ((request (my/gh//create-edit-pr-body-request
                   :gitrepo-directory my/gh//edit-pr-metadata-default-directory
                   :pr-number my/gh//edit-pr-metadata-current-pr-number
                   :src-buffer (current-buffer)
                   :tmp-file (make-temp-file "gh-pr-body-"))))
    (my/gh//do-edit-pr-body request)))

(defun my/gh/edit-pr-body-abort ()
  "User saying they gave up editing the PR body"
  (interactive)
  (message "Bailing!")
  (kill-buffer))

(provide 'my-gh-edit-pr-body)
;;; my-gh-edit-pr-body.el ends here
