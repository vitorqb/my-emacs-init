;;; my-notes.el --- AI Tools -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'my-notes)
(require 's)
(require 'json)

(defcustom my/notes/current-file (expand-file-name "~/org/current.org")
  "Path for file `current.org`, the file containing what you are currently working on"
  :type 'string
  :group 'my-notes)

(defcustom my/notes/notes-dir (expand-file-name "~/org/notes")
  "Directory for keeping notes"
  :type 'string
  :group 'my-notes)

(defun my/notes/open-current-file ()
  "Opens the `current.org` file. If prefix arg is given, opens in another window."
  (interactive)
  (my/notes/assert-current-file)
  (when current-prefix-arg
    (split-window-horizontally)
    (other-window 1))
  (find-file my/notes/current-file))

(defun my/notes/new (topic)
  "Creates a new note with `topic`"
  (interactive "MWrite a topic:")
  (my/notes/assert-notes-dir)
  (let* ((note-dir (->> topic
                        (s-dashed-words)
                        (file-name-concat my/notes/notes-dir)))
         (_ (make-directory note-dir))
         (readme-file (file-name-concat note-dir "Readme.org"))
         (metadata-file (file-name-concat note-dir "metadata.json")))
    (with-temp-buffer
      (insert (json-encode `(("created_at" . ,(current-time-string)) ("topic" . topic))))
      (write-file metadata-file))
    (with-temp-buffer
      (insert (format "* %s" topic))
      (insert "\n\n")
      (write-file readme-file))
    (find-file readme-file)
    (goto-char (point-max))))

(defun my/notes/assert-current-file ()
  "Ensures current-file is readable"
  (when (not (file-exists-p my/notes/current-file))
    (user-error "Current file %s does not exist" my/notes/current-file))
  (when (not (file-readable-p my/notes/current-file))
    (user-error "Current file %s is not readable!" my/notes/current-file))
  (when (not (file-regular-p my/notes/current-file))
    (user-error "%s is not a file!" my/notes/current-file)))

(defun my/notes/assert-notes-dir ()
  "Ensures notes-dir is a readable directory"
  (when (not (file-exists-p my/notes/notes-dir))
    (user-error "Notes dir %s does not exist" my/notes/notes-dir))
  (when (not (file-readable-p my/notes/notes-dir))
    (user-error "Notes dir %s is not readable!" my/notes/notes-dir))
  (when (not (file-directory-p my/notes/notes-dir))
    (user-error "Notes dir %s is not a directory!" my/notes/notes-dir)))
;;; my-notes.el ends here
