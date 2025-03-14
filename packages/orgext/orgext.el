;;; orgext.el --- Vitor's utilities -*- lexical-binding: t -*-

;; Copyright (C) 2019 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2019-06-18
;; Keywords: elisp org-mode
;; Homepage: https://github.com/vitorqb/orgext/blob/development/

;; This file is not part of GNU Emacs.
     
;; No warranties.

;;; code
(require 'org)

(defvar orgext-element-at-point-buffer-name "*OrgExtElementAtPoint*")
(defvar my/orgext/jira-base-url nil
  "Base URL of jira server for integration")
(defvar my/orgext/bitbucket-base-url nil
  "Base URL of bitbucket server for integration")

(defun orgext--user-error-no-block-at-point ()
  (user-error "No block found at point!"))

(defun orgext-mark-block ()
  "Marks the context of the org block at point."
  (interactive)
  (-if-let (el (org-element-context))
      (if (-any? (-partial #'equal (car el))
                 '(example-block src-block verse-block quote-block comment-block))
          (-let* ((block-begin (org-element-begin el))
                  (block-end (org-element-end el))
                  (post-blank (org-element-post-blank el))
                  ;; we want 1 line after begin and 2 before end
                  (begin (save-excursion
                           (goto-char block-begin)
                           (forward-line)
                           (beginning-of-line)
                           (point)))
                  (end (save-excursion
                         (goto-char block-end)
                         (forward-line
                          (- (+ 1 post-blank (s-count-matches "\n" (thing-at-point 'line t)))))
                         (when (string-match "^\\#\\+end_" (thing-at-point 'line t))
                           (forward-line -1))
                         (if (< (point) begin)
                             (goto-char begin))
                         (end-of-line)
                         (+ 1 (point)))))
            (goto-char begin)
            (set-mark end))
        (orgext--user-error-no-block-at-point))
    (orgext--user-error-no-block-at-point)))

(defun orgext-copy-block-from-above ()
  "Copies the first org block from above to after the current point."
  (interactive)
  (-let [point-at-entry (point)]
    (save-mark-and-excursion
      (org-previous-block 1)
      (orgext-mark-block)
      ;; We need to expand 1 line in each direction to capture the whole block
      (forward-line -1)
      (set-mark (save-excursion
                  (goto-char (region-end))
                  (forward-line)
                  (point)))
      (copy-region-as-kill nil nil t))
    (yank)
    (goto-char point-at-entry)))

(defun orgext-copy-block-contents-to-compile-command ()
  "Copies the contents of the code block at point to compile-command"
  (interactive)
  (save-mark-and-excursion
    (orgext-mark-block)
    (-let* ((((beg . end)) (region-bounds))
            (block-contents (buffer-substring-no-properties beg (- end 1))))
      (setq compile-command block-contents))))

(defun orgext-new-block-from-other-window (block-type)
  "Calls `org-insert-structure-template`. Then pastes the text from
`other-window` inside the block."
  ;; Copied from org-insert-structure-template
  (interactive
   (list (pcase (org--insert-structure-template-mks)
	   (`("\t" . ,_) (read-string "Structure type: "))
	   (`(,_ ,choice . ,_) choice))))
  (save-mark-and-excursion
    (org-insert-structure-template block-type)
    (search-forward "#+end")
    (beginning-of-line)
    (-let [other-window-contents (orgext--get-other-window-contents)]
      (insert other-window-contents))))

(defun orgext--get-other-window-contents ()
  "Calls `other-window`, copies the entire buffer string, calls
other-window and returns."
  (call-interactively #'other-window)
  (-let [buffer-contents (buffer-substring-no-properties (point-min) (point-max))]
    (call-interactively #'other-window)
    buffer-contents))

(defun orgext-jira-open (ticket)
  "Visits the jira ticket on a browser."
  (cl-assert my/orgext/jira-base-url)
  (-> my/orgext/jira-base-url file-name-as-directory (concat ticket) browse-url))

(defun orgext-bitbucket-server-pr-open (pr)
    "Visits a PR from bitbucket-server in a browser. `pr` must be a
string that contains the repo name and the pr number, separated by
/. Like this: foo-repo/69"
    (cl-assert my/orgext/bitbucket-base-url)
    (--> pr
         (s-split "/" it)
         (or (and (equal (length it) 2) it)
             (error "A single '/' is expected in a pr link"))
         (apply
          #'format
          "%s%s/pull-requests/%s"
          (file-name-as-directory my/orgext/bitbucket-base-url)
          it)
         (and (print it) it)
         (browse-url it)))

(defun orgext-element-at-point-on-new-buffer ()
  "Display the current element in point in a read-only new buffer"
  (interactive)
  (-let* ((el (org-element-at-point))
          (begin (org-element-begin el))
          (end   (org-element-end el))
          (element-content (buffer-substring begin end)))
    (with-current-buffer (generate-new-buffer orgext-element-at-point-buffer-name)
      (insert element-content)
      (read-only-mode)
      (goto-char 0)
      (org-mode)
      (use-local-map (copy-keymap org-mode-map))
      (set-buffer-modified-p nil)
      (local-set-key "q" #'quit-window)
      (switch-to-buffer-other-window (current-buffer)))))

(provide 'orgext)
;;; orgext.el ends here
