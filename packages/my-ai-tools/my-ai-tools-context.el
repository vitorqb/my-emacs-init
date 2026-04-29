;;; my-ai-tools-context.el --- AI Tools -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code

(provide 'my-ai-tools-context)
(require 'projectile)
(require 'dired)

(cl-defstruct (my/ai-tools/context
               (:constructor my/ai-tools/make-context)
               (:conc-name my/ai-tools/context/))
  "Represents the *context* the user is when they want to write an AI prompt"
  buffer                                ;Active buffer
  major-mode                            ;Active major mode
  default-directory                     ;Default directory
  region                                ;Active region (if any) - see cl-defstruct below
  project-root                          ;Project root at time of capture
  char-position                         ;Position of char in buffer
  char-linum                            ;Line number of char in buffer
  attributes                            ;Other captured attributes (see capture-attribute)
  )

(cl-defgeneric my/ai-tools/context/capture-attributes (_major-mode)
  "Returns an `alist` of major-mode specific attributes for the user context"
  nil)

(cl-defmethod my/ai-tools/context/capture-attributes ((_ (eql 'dired-mode)))
  `((marked-files . ,(dired-get-marked-files 't 'marked))))

(defun my/ai-tools/context/capture ()
  "Captures the current user context (from the world)"
  (my/ai-tools/make-context
   :buffer            (current-buffer)
   :major-mode        major-mode
   :default-directory default-directory
   :region            (my/ai-tools/capture-region)
   :project-root      (projectile-project-root)
   :char-position     (point)
   :char-linum        (line-number-at-pos (point))
   :attributes        (my/ai-tools/context/capture-attributes major-mode)))

(defun my/ai-tools/context/to-string (context)
  "Generates a string describing the user context (in markdown)"
  (let ((ctx-major-mode (my/ai-tools/context/major-mode context)))
    (format "## Context\n\n%s" (my/ai-tools/context/to-string/by-mode ctx-major-mode context))))

(cl-defgeneric my/ai-tools/context/to-string/by-mode (_ctx-major-mode context)
  "Formats the context considering it was captured on `ctx-major-mode`. The default implementation is useful for most modes displaying file contents. Special implementations are provided for modes like `dired-mode`."
  (let* ((project-root  (my/ai-tools/context/project-root context))
         (buffer        (my/ai-tools/context/buffer context))
         (region        (my/ai-tools/context/region context))
         (char-linum    (my/ai-tools/context/char-linum context))
         (filepath      (if project-root
                            (file-relative-name (buffer-file-name buffer) project-root)
                          (buffer-file-name buffer))))
    (with-temp-buffer
      (insert (format "File: @%s\n\n" filepath))
      (if region
          (insert (format "Selected Region (lines %s to %s):\n```\n%s\n```"
                          (my/ai-tools/region/begin-linum region)
                          (my/ai-tools/region/end-linum region)
                          (my/ai-tools/region/content region)))
        (insert (format "Active line: %s\n" char-linum)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(cl-defmethod my/ai-tools/context/to-string/by-mode ((_ (eql 'dired-mode)) context)
  "Formats the context considering it was captured on dired-mode"
  (with-temp-buffer
    (let* ((ctx-project-root  (my/ai-tools/context/project-root context))
           (ctx-default-directory (my/ai-tools/context/default-directory context))
           (relative-ctx-default-directory (if ctx-project-root
                                               (file-relative-name ctx-default-directory ctx-project-root)
                                             ctx-default-directory))
           (marked-files (->> context
                              my/ai-tools/context/attributes
                              (alist-get 'marked-files))))
      (insert (format "Active Directory: %s\n\n" relative-ctx-default-directory))
      (when (length> marked-files 0)
        (insert (format "Marked Files:\n"))
        (dolist (marked-file marked-files)
          (let ((relative-marked-file (if ctx-project-root
                                          (file-relative-name marked-file ctx-project-root)
                                        marked-file)))
            (insert (format "  - %s\n" relative-marked-file))))))
    (buffer-substring-no-properties (point-min) (point-max))))

(cl-defstruct (my/ai-tools/region
               (:constructor my/ai-tools/make-region)
               (:conc-name my/ai-tools/region/))
  "Represents the *region* selected inside a *context*"
  begin
  begin-linum
  end
  end-linum
  content)

(defun my/ai-tools/capture-region ()
  "Captures the region from the environment"
  (if (region-active-p)
      (-if-let (((begin . end)) (region-bounds))
          (my/ai-tools/make-region
           :begin begin
           :begin-linum (line-number-at-pos begin)
           :end end
           :end-linum (line-number-at-pos end)
           :content (buffer-substring-no-properties begin end)))))
;;; my-ai-tools-context.el ends here
