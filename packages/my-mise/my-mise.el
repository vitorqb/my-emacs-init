;;; my-mise.el --- Cusotm github utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'my-mise)
(require 'compile)

(defvar my/mise/prompt-for-task-history nil
  "Completing history for prompting for a task")

(defun my/mise/list-tasks ()
  (->> (shell-command-to-string "mise tasks --json")
       (json-parse-string)
       (-map (lambda (x) (gethash "name" x)))))

(defun my/mise/prompt-for-task ()
  (let ((tasks (my/mise/list-tasks)))
    (completing-read "Chose a task: " tasks nil t nil 'my/mise/prompt-for-task-history)))

(defun my/mise/run-task (task)
  (interactive (list (my/mise/prompt-for-task)))
  (let* ((raw-command (format "mise run %s" (shell-quote-argument task)))
         (command     (if current-prefix-arg
                          (compilation-read-command raw-command)
                        raw-command)))
    (compile command)))
;;; my-mise.el ends here
