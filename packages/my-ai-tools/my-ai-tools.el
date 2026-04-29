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
(require 'projectile)
(require 'dired)

(defvar my/ai-tools/prompt-builder-buffer-name "*ai-tools/prompt-builder*")

;; Represents the *context* the user is when they want to write an AI prompt
(cl-defstruct my/ai-tools/context
  buffer
  major-mode
  default-directory
  region-p
  region-begin
  region-end
  project-root
  char-position)

(defun my/ai-tools/capture-context ()
  "Captures the current user context"
  (make-my/ai-tools/context
   :buffer            (current-buffer)
   :major-mode        major-mode
   :default-directory default-directory
   :region-p          (region-active-p)
   :region-begin      (and (region-active-p) (region-beginning))
   :region-end        (and (region-active-p) (region-end))
   :project-root      (projectile-project-root)
   :char-position     (point)))

(defun my/ai-tools/context-to-string (context)
  "Generates a string describing the user context (in markdown)"
  (let ((ctx-major-mode (my/ai-tools/context-major-mode context)))
    (format "## Context\n\n%s" (my/ai-tools//context-to-string-by-mode ctx-major-mode context))))

(cl-defgeneric my/ai-tools//context-to-string-by-mode (_ctx-major-mode context)
  "Formats the context considering it was captured on `ctx-major-mode`. The default implementation is useful for most modes displaying file contents. Special implementations are provided for modes like `dired-mode`."
  (let* ((project-root  (my/ai-tools/context-project-root context))
         (buffer        (my/ai-tools/context-buffer context))
         (region-p      (my/ai-tools/context-region-p context))
         (region-begin  (my/ai-tools/context-region-begin context))
         (region-end    (my/ai-tools/context-region-end context))
         (char-position (my/ai-tools/context-char-position context))
         (filepath      (if project-root
                            (file-relative-name (buffer-file-name buffer) project-root)
                          (buffer-file-name buffer))))
    (with-temp-buffer
      (insert (format "File: @%s\n\n" filepath))
      (if region-p
          (insert (format "Selected Region (lines %s to %s):\n```\n%s\n```"
                          (with-current-buffer buffer
                            (line-number-at-pos region-begin))
                          (with-current-buffer buffer
                            (line-number-at-pos region-end))
                          (with-current-buffer buffer
                            (buffer-substring-no-properties region-begin region-end))))
        (insert (format "Active line: %s\n" (with-current-buffer buffer (line-number-at-pos char-position)))))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod my/ai-tools//context-to-string-by-mode ((_ (eql 'dired-mode)) context)
  "Formats the context considering it was captured on dired-mode"
  (with-temp-buffer
    (let* ((ctx-project-root  (my/ai-tools/context-project-root context))
           (ctx-default-directory (my/ai-tools/context-default-directory context))
           (relative-ctx-default-directory (if ctx-project-root
                                               (file-relative-name ctx-default-directory ctx-project-root)
                                             ctx-default-directory))
           (ctx-buffer (my/ai-tools/context-buffer context))
           (marked-files (with-current-buffer ctx-buffer
                           (dired-get-marked-files 't 'marked))))
      (insert (format "Active Directory: %s\n\n" relative-ctx-default-directory))
      (when (length> marked-files 0)
        (insert (format "Marked Files:\n"))
        (dolist (marked-file marked-files)
          (let ((relative-marked-file (if ctx-project-root
                                          (file-relative-name marked-file ctx-project-root)
                                        marked-file)))
            (insert (format "  - %s\n" relative-marked-file))))))
    (buffer-substring-no-properties (point-min) (point-max))))

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
