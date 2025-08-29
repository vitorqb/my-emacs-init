;;; my-term.el --- Cusotm terminal utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'my-term)

(defvar my/term/new-pane
  (lambda (_cwd) (error "my/term/new-pane not set!"))
  "Function to be used to creating a new pane on a terminal multiplex.
 Receives the CWD the shell should start on.")

(defvar my/term/do-run
  (lambda (_cmd) (error "my/term/do-run not set!"))
  "Function to be used to run a command on a terminal, expecting user
interaction. It receives the command to run.")

(defvar my/term/focus-window
  (lambda () (error "my/term/focus-window not set!"))
  "Function called when we need to focus the window with the terminal.")

(defun my/term/run (cmd)
  "Runs a command on a terminal, waits for user interaction and exits."
  (interactive "sEnter command to run: ")
  (funcall my/term/do-run cmd)
  (funcall my/term/focus-window))

(defun my/term/open-on-current-dir (currdir)
  "Opens a new terminal pane on the given directory (defaults to current directory)"
  (interactive (list (expand-file-name default-directory)))
  (funcall my/term/new-pane currdir)
  (funcall my/term/focus-window))

;; -----------------------------------------------------------------------------
;; Zellij
;; -----------------------------------------------------------------------------
(defun my/term/zellij/current-session ()
  "Return the current session to use for zellij. If no session is running, errors."
  (let ((session (s-chomp (shell-command-to-string "zellij ls -n | grep -v EXITED | head -n 1 | awk '{print $1}'"))))
    (when (or (not session) (string-equal session ""))
      (error "Can not find current session"))
    session))

(defun my/term/zellij/new-pane (&optional cwd)
  "Creates a new zellij pane with cwd. Implements my/term/new-pane."
  (--> (format "zellij -s=%s action new-pane -c" (my/term/zellij/current-session))
       (if cwd (format "%s --cwd=%s" it cwd) it)
       (format "%s -- bash" it)
       (shell-command it)))

(defun my/term/zellij/do-run (cmd)
  "Runs a command on zellij, and exits after."
  (if-let ((session (my/term/zellij/current-session)))
      (shell-command (format "zellij -s=%s run -f -- %s" session cmd))
    (error "No zellij session")))

(defun my/term/use-zellij ()
  (setq my/term/new-pane #'my/term/zellij/new-pane)
  (setq my/term/do-run   #'my/term/zellij/do-run))

(defun my/term/on-zellij-scrollback-editing ()
  "Function to be called when we go to a zellij scrollback editing
using emacsclient. It will trim empty spaces and lines and move the
user to the end of the scroll."
  ;; Go to the bottom
  (goto-char (point-max))
  ;; Delete all trailing empty lines
  (catch 'done
    (while t
      (let* ((line (-> (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position))
                       (s-trim))))
        ;; if line is not empty string or buffer is too small
        (when (or (not (s-blank? line)) (< (point-max) 1))
          (throw 'done t))
        (delete-line)
        (backward-char 1))))
  ;; Go to end of buffer
  (goto-char (point-min))
  (goto-char (point-max)))

(defun my/term/maybe-prepare-zellij-scrollback-editing ()
  "To be used on server-mode-hook. Tries to guess we are editing a zellij buffer."
  ;; IMPORTANT - This depends on the frame containing zellij-scrollback in the name!
  (when (and (->> (frame-parameter nil 'name) (s-contains? "zellij-scrollback"))
             (->> (buffer-file-name) (s-ends-with? ".dump")))
    (my/term/on-zellij-scrollback-editing)))

(add-hook 'server-visit-hook #'my/term/maybe-prepare-zellij-scrollback-editing)


;; -----------------------------------------------------------------------------
;; Tmux
;; -----------------------------------------------------------------------------
(defvar my/term/tmux/interactive-window
  "emacs-tmux-interactive"
  "Tmux window to use for interactive commands.")

(defun my/term/tmux/current-session ()
  "Returns the current session to use for tmux. Returns nil if not found."
  (if-let ((session (-> "tmux ls -F'#{session_name}' | sort | head -n 1"
                          (shell-command-to-string)
                          (s-chomp))))
      (unless (or (s-blank? session) (s-contains? "no server running" session))
        session)))

(defun my/term/tmux/new-pane (&optional cwd)
  (if-let ((session (my/term/tmux/current-session)))
      (progn
        (shell-command (format "tmux neww -t%s:" session))
        (when (not (s-blank? cwd))
          (shell-command (format "tmux send-keys -t%s: 'cd %s' Enter" session cwd))))
    (error "No current session for tmux")))

(defun my/term/tmux/do-run (cmd)
  (if-let ((session (my/term/tmux/current-session)))
      (shell-command (format "tmux neww -t%s: -n%s '%s'"
                             session
                             my/term/tmux/interactive-window
                             cmd))
    (error "No current session for tmux")))

(defun my/term/use-tmux ()
  (setq my/term/new-pane #'my/term/tmux/new-pane)
  (setq my/term/do-run   #'my/term/tmux/do-run))

;;; my-term.el ends here
