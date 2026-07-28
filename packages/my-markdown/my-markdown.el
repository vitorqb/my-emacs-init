;;; my-markdown.el --- My markdown -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2027-07-23
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(require 's)
(require 'markdown-mode)

(defcustom my-markdown-image-viewer-executable "imv-x11"
  "Which executable to use for viewing images. Must support setting window title with -w"
  :type '(string)
  :group 'my-markdown)

(defcustom my-markdown-image-viewer-window-title "my-markdown-image-view"
  "X11 Window title to give when viewing a markdown"
  :type '(string)
  :group 'my-markdown)

(defcustom my-markdown-mermaid-cli-executable "mmdc"
  "Which executable to use for mermaid-cli."
  :type '(string)
  :group 'my-markdown)

(defun my-markdown-code-block-content (pos)
  "Returns the contents of the code block at point"
  (let ((bounds (markdown-code-block-at-pos pos)))
    (->> (buffer-substring-no-properties (nth 0 bounds) (nth 1 bounds))
         (s-lines)
         (cdr)                        ;Drop ```mermaid
         (butlast)                    ;Drop ``` (end)
         (s-join "\n"))))

(defun my-markdown-view-mermaidjs (pos)
  "Renders the mermaid block as an image on an external viewer"
  (interactive (list (point)))
  (let* ((block-content (my-markdown-code-block-content pos))
         (tempfile    (make-temp-file "my-markdown-view-mermaidjs" nil ".png"))
         (on-success  (lambda ()
                        (start-process "*my-markdown-view-mermaidjs*"
                                       "*my-markdown-view-mermaidjs*"
                                       my-markdown-image-viewer-executable
                                       "-w" my-markdown-image-viewer-window-title
                                       tempfile))))
    (my-markdown-mermaidjs-render-proc block-content tempfile on-success)))

(defun my-markdown-mermaidjs-render-proc (content destination on-success)
  "Returns a proc that renders a mermaidjs content"
  (message "[my-markdown] Rendering...")
  (let* ((tmpfile (make-temp-file "my-markdown-mermaidjs-render-proc" nil ".mermaid")))
    (with-temp-file tmpfile (insert content))
    (make-process :name    "*my-markdown-rendering*"
                  :buffer  "*my-markdown-rendering*"
                  :command `(,my-markdown-mermaid-cli-executable "-i" ,tmpfile "-o" ,destination)
                  :coding 'utf-8
                  :sentinel (lambda (p _)
                              (pcase `(,(process-status p) ,(process-exit-status p))
                                ('(exit 0) (funcall on-success))
                                (`(,a ,b) (message "Unexpected process status (code %s): %s" b a)))))))

(provide 'my-markdown)
;;; my-markdown.el ends here
