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
(defvar myutils/clean-buffers-names-regexs
  '("\\*ag search.+" "\\*Occur\\*" "magit-\\(log\\|diff\\)")
  "A list of regexp for buffers to kill when cleaning, if name matches.")

(defvar myutils/known-datetime-formats
  '("%Y-%m-%dT%H:%M:%S"
    "%Y-%m-%d"
    "%Y%m%d"
    "%Y%m%dT%H%M%S")
  "A list of known date(time) formats.")

;; Functions
(defun myutils/add-to-generic-path (x y)
  "Adds `x` to some environmental variable `y` (like PYTHONPATH)"
  (setenv y (concat x ":" (getenv y))))

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

(defun myutils/fill-to-end ()
  "Fills the screen with '-' until column 80"
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char ?-))))

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

;; clean-buffers
(defun myutils/clean-buffers ()
  "Clean buffers whose names matches myutils/clean-buffers-names-regexs"
  (interactive)
  (cl-flet ((should-kill? (buff)
              (--> (buffer-name buff)
                   (-rpartial 'string-match-p it)
                   (-some? it myutils/clean-buffers-names-regexs))))
    (--> (buffer-list)
         (-filter #'should-kill? it)
         (-each it #'kill-buffer))))

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

(defun myutils/copy-file-path-from-other-window-to-clipboard ()
  "Calls `other-window`, `copy-file-path-to-clipboard`, and comes back"
  (interactive)
  (save-window-excursion
    (call-interactively #'other-window)
    (myutils/copy-file-path-to-clipboard)))

(defun myutils/duplicate-buffer ()
  "Displays a copy of the current buffer in a new buffer and switch to it"
  (interactive)
  (switch-to-buffer-other-window (current-buffer)))

(defun myutils/chmod-current-buffer (mode)
  "Like chmod but with current buffer"
  (interactive (list (read-file-modes "New mode: " (buffer-file-name))))
  (chmod (buffer-file-name) mode))

(defun myutils/message-if-display-is-empty (msg)
  "Sends the user a message if current-message returns null"
  (if (equal (current-message) nil)
      (message msg)))

(defmacro myutils/with-compile-opts (buffname cmd &rest body)
  "Evaluates body after binding compilation functions to set the
  buffer name and the default command"
  (declare (indent 2))
  `(-let ((compilation-buffer-name-function (-const ,buffname))
          (compile-command ,cmd))
     ,@body))

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
;; Python utils
;; -----------------------------------------------------------------------------
(defun myutils/python-activate-venv ()
  "Activates a virtual environment."
  (interactive)
  (-if-let (venv-dir (myutils/python-get-default-venv-path))
      (--> venv-dir
           (progn (pyvenv-activate it) it)
           (format "Activated venv %s!" it)
           (myutils/message-if-display-is-empty it))
    (call-interactively #'pyvenv-activate)))

(defun myutils/python-get-default-venv-path ()
  "Looks for a venv folder in the current dir. Either returns a string with the
   path for it or nil."
  (-some->> '("venv" ".venv")
    (-map (lambda (x)
            (-if-let (dominating-file (locate-dominating-file default-directory x))
                (file-name-concat dominating-file x))))
    (-filter #'identity)
    (-filter #'file-directory-p)
    (car)))

(provide 'mylisputils)
;;; mylisputils.el ends here
