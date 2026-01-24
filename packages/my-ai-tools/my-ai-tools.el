;;; my-ai-tools.el --- AI Tools -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'my-ai-tools)
(require 'markdown-mode)
(require 'cl-lib)

(defvar my/ai-tools/prompt-builder-buffer-name "*ai-tools/prompt-builder*")

;; Represents the *context* the user is when they want to write an AI prompt
(cl-defstruct my/ai-tools/context
  buffer
  region-p
  region-begin
  region-end
  char-position)

(defun my/ai-tools/capture-context ()
  "Captures the current user context"
  (make-my/ai-tools/context
   :buffer        (current-buffer)
   :region-p      (region-active-p)
   :region-begin  (and (region-active-p) (region-beginning))
   :region-end    (and (region-active-p) (region-end))
   :char-position (point)))

(defun my/ai-tools/context-to-string (context)
  "Generates a string describing the user context (in markdown)"
  (let ((buffer        (my/ai-tools/context-buffer context))
        (region-p      (my/ai-tools/context-region-p context))
        (region-begin  (my/ai-tools/context-region-begin context))
        (region-end    (my/ai-tools/context-region-end context))
        (char-position (my/ai-tools/context-region-end context)))
    (with-temp-buffer
      (insert "## Context\n\n")
      (insert (format "File: @%s\n\n" (buffer-file-name buffer)))
      (if region-p
          (insert (format "Selected Region (lines %s to %s):\n```\n%s\n```"
                          (with-current-buffer buffer
                            (line-number-at-pos region-begin))
                          (with-current-buffer buffer
                            (line-number-at-pos region-end))
                          (with-current-buffer buffer
                            (buffer-substring-no-properties region-begin region-end))))
        (insert (format "Active line: %s\n" (line-number-at-pos char-position))))
      (buffer-string))))

(defun my/ai-tools/prompt-from-context (context)
  "Interactively constructs an AI prompt from the given user context"
  (interactive (list (my/ai-tools/capture-context)))
  (my/ai-tools/maybe-kill-builder-buff)
  (let ((prompt-builder-buf (my/ai-tools/get-or-create-builder-buff)))
    (switch-to-buffer prompt-builder-buf)
    (my/ai-tools/prompt-builder-mode)
    (insert (my/ai-tools/context-to-string context))
    (goto-char (point-min))
    (insert "## User Prompt\n\n\n\n")
    (line-move -2)))

(defun my/ai-tools/prompt-builder-done ()
  (interactive)
  (kill-region (buffer-end -1) (buffer-end 1))
  (set-buffer-modified-p nil)
  (kill-buffer)
  (message "[MyAiTools] Copied prompt to clipboard!"))

(defun my/ai-tools/prompt-builder-drop ()
  (interactive)
  (set-buffer-modified-p nil)
  (kill-buffer)
  (message "[MyAiTools] Canceled by the user!"))

(defvar my/ai-tools/prompt-builder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'my/ai-tools/prompt-builder-done)
    (define-key map (kbd "C-c C-k") #'my/ai-tools/prompt-builder-drop)
    (set-keymap-parent map markdown-mode-map)
    map))

(define-derived-mode my/ai-tools/prompt-builder-mode markdown-mode "My/AiTools/PromptBuilder"
  :keymap my/ai-tools/prompt-builder-mode-map)

(defun my/ai-tools/maybe-kill-builder-buff ()
  (when-let (builder-buff (get-buffer my/ai-tools/prompt-builder-buffer-name))
    (kill-buffer builder-buff)))

(defun my/ai-tools/get-or-create-builder-buff ()
  (get-buffer-create my/ai-tools/prompt-builder-buffer-name))
;;; my-ai-tools.el ends here
