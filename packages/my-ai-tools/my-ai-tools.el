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

(defvar my/ai-tools/prompt-builder-buffer-name "*ai-tools/prompt-builder*")

(defun my/ai-tools/generate-context (region-p region-begin region-end orig-buff)
  (with-temp-buffer
    (insert "## Context\n\n")
    (insert (format "File: @%s\n\n" (buffer-file-name orig-buff)))
    (when region-p
      (insert (format "Selected Region (lines %s to %s):\n```\n%s\n```"
                      (with-current-buffer orig-buff
                        (line-number-at-pos region-begin))
                      (with-current-buffer orig-buff
                        (line-number-at-pos region-end))
                      (with-current-buffer orig-buff
                        (buffer-substring-no-properties region-begin region-end)))))
    (buffer-string)))

(defun my/ai-tools/prompt-from-context (buff region-p region-begin region-end)
  "Interactively constructs an AI prompt from the given user context"
  (interactive (list (current-buffer) (region-active-p) (region-beginning) (region-end)))
  (my/ai-tools/maybe-kill-builder-buff)
  (let ((prompt-builder-buf (my/ai-tools/get-or-create-builder-buff)))
    (switch-to-buffer prompt-builder-buf)
    (my/ai-tools/prompt-builder-mode)
    (insert (my/ai-tools/generate-context region-p region-begin region-end buff))
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
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c C-c") #'my/ai-tools/prompt-builder-done)
    (define-key map (kbd "C-c C-k") #'my/ai-tools/prompt-builder-drop)
    map))

(define-derived-mode my/ai-tools/prompt-builder-mode markdown-mode "My/AiTools/PromptBuilder"
  :keymap my/ai-tools/prompt-builder-mode-map)

(defun my/ai-tools/maybe-kill-builder-buff ()
  (when-let (builder-buff (get-buffer my/ai-tools/prompt-builder-buffer-name))
    (kill-buffer builder-buff)))

(defun my/ai-tools/get-or-create-builder-buff ()
  (get-buffer-create my/ai-tools/prompt-builder-buffer-name))
;;; my-ai-tools.el ends here
