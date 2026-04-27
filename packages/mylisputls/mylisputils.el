;;; mylisputils.el --- Vitor's utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2018-10-28
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.
     
;; No warranties.

;;; code
(require 'dash)
(require 's)
(require 'projectile)

;; Customizable variables
(defvar myutils/known-datetime-formats
  '("%Y-%m-%dT%H:%M:%S"
    "%Y-%m-%d"
    "%Y%m%d"
    "%Y%m%dT%H%M%S")
  "A list of known date(time) formats.")

;; Functions
(defun myutils/relative-path (destination &optional origin)
  "Uses `realpath` to find the path for `destination` relative to `origin`"
  (let* ((origin (or origin default-directory))
         (quoted-origin (shell-quote-argument origin))
         (quoted-destination (shell-quote-argument destination)))
    (-> "realpath --relative-to=%s %s"
        (format quoted-origin quoted-destination)
        (shell-command-to-string)
        (s-trim))))

(defun myutils/copy-relative-path (destination)
  "Same as `myutils/relative-path`, but don't take origin and copies to kill ring"
  (interactive "fFile:")
  (kill-new (myutils/relative-path destination)))

(defun myutils/concat-file (dir file)
  "Concat a file in a dir"
  (concat (file-name-as-directory dir) file))

(defun myutils/date-in-all-formats ()
  "Returns a list of all date formats."
  (--map (format-time-string it) myutils/known-datetime-formats))

(defun myutils/insert-formated-date ()
  "Shows the user different formats for the date and asks it to choose one
 to insert or copy."
  (interactive)
  (ivy-read
   "Date(time): "
   (myutils/date-in-all-formats)
   :action '(1
             ("i" insert "Insert")
             ("w" kill-new "Copy"))))

(defun myutils/remove-whitespace-and-newline ()
  "Removes next character until it is no longer whitespace or newline"
  (interactive)
  (-let [chars-to-delete '(?\n ?\s ?\t)]
    (while (-as-> (following-char) it
                  (-partial #'char-equal it)
                  (-any? it chars-to-delete))
      (delete-char 1))))

(defun myutils/priv/file-path (relative-to-project-root?)
  "Returns the file to the path we are visiting. If relative-to-project-root?
   is set the path is relative to the projectile project root."
  (let* ((path (cond
                ((equal major-mode 'dired-mode) default-directory)
                ((equal major-mode 'magit-status-mode) default-directory)
                (t (buffer-file-name))))
         (abspath (expand-file-name path)))
    (if relative-to-project-root?
        (file-relative-name abspath (projectile-project-root))
      abspath)))

(defun myutils/copy-file-path-to-clipboard (relative-to-project-root?)
  "Copy the current buffer file path to the clipboard."
  (interactive "P")
  (let* ((path (myutils/priv/file-path relative-to-project-root?)))
    (kill-new path)
    (message "Copied %s to the clipboard!" path)))

(defun myutils/copy-file-path-from-other-window-to-clipboard (relative-to-project-root?)
  "Calls `other-window`, `copy-file-path-to-clipboard`, and comes back"
  (interactive "P")
  (save-window-excursion
    (call-interactively #'other-window)
    (myutils/copy-file-path-to-clipboard relative-to-project-root?)))

(defun myutils/duplicate-buffer ()
  "Displays a copy of the current buffer in a new buffer and switch to it"
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(defun myutils/chmod-current-buffer (mode)
  "Like chmod but with current buffer"
  (interactive (list (read-file-modes "New mode: " (buffer-file-name))))
  (chmod (buffer-file-name) mode))

(defun myutils/remove-with-elipsis ()
  "Removes the current line and inserts an elipsis [...]
   If an elipsis is on the current line, removes the previous one."
  (interactive)
  (cl-flet ((current-line-is-elipsis ()
             (string-match-p "^\\[\\.\\.\\.\\]$" (thing-at-point 'line t))))
    (if (current-line-is-elipsis)
        ;; Already have an elipsis, kill prev line
        (progn
          (beginning-of-line 0)
          (kill-whole-line))
      ;; No elipsis, put one
      (progn
        (kill-whole-line)
        (save-excursion (insert "[...]\n"))))))

(defun myutils/copy-buffer-contents ()
  (interactive)
  (copy-region-as-kill (point-min) (point-max)))

;; -----------------------------------------------------------------------------
;; JSON utils
;; -----------------------------------------------------------------------------
(defun myutils/parse-json-file (f)
  "Parses a file content as JSON"
  (with-temp-buffer
    (insert-file-contents-literally f)
    (goto-char (point-min))
    (json-parse-buffer)))

(provide 'mylisputils)
;;; mylisputils.el ends here
