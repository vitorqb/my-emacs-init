;;; my-gh.el --- Cusotm github utilities -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-03-18
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(provide 'my-gh)
(require 'my-term)

(defvar my/gh/on-browser-open-request
  (lambda ()
    (interactive)
    (shell-command "i3-msg [urgent=latest] focus"))
  "Callback to be run when we send something to the browser")

(defvar my/gh/copy-to-clipboard
  (lambda (x) (kill-new x)))

(defun my/gh/open-repo-on-browser ()
  (interactive)
  (shell-command "gh repo view --web")
  (funcall my/gh/on-browser-open-request))

(defun my/gh/open-pr-on-browser ()
  (interactive)
  (shell-command "gh pr view --web")
  (funcall my/gh/on-browser-open-request))

(defun my/gh/new-pr ()
  "Opens tmxu with prompts for a new PR"
  (interactive)
  (my/term/run "gh pr create"))

(defun my/gh/print-pr-body ()
  "Prints the current PR body on the current buffer."
  (interactive)
  (insert (shell-command-to-string "gh pr view --json=body --jq='.body'")))

(defun my/gh/edit-pr-body ()
  "Allows editing current PR body."
  (interactive)
  (let ((buff (generate-new-buffer "*gh-pr-body*"))
        (default-directory default-directory))
    (switch-to-buffer buff)
    (my/gh/print-pr-body)
    (when (and (require 'markdown-mode nil 'noerror)
               (fboundp 'markdown-mode))
      (markdown-mode))))

(defun my/gh//get-pr-list ()
  "Returns a list of objects representing PRs"
  (message "Querying for list of PRs...")
  (-> (shell-command-to-string "gh pr list --json='author,id,title,url,state,createdAt,isDraft,number,headRefName'")
      (json-parse-string)))

(defun my/gh//prompt-user-to-select-pr (prlist)
  "Asks the user to select a PR from a list using `completing-read`"
  (let* ((candidates (-map (lambda (pr) (format "%s | %s | %s | %s"
                                                (gethash "number" pr)
                                                (gethash "state" pr)
                                                (->> pr
                                                     (gethash "author")
                                                     (gethash "login"))
                                                (gethash "title" pr)))
                           prlist))
         (selected-str (completing-read "Select a PR: " candidates nil 't))
         (selected-number (->> selected-str (s-split "|") seq-first s-trim string-to-number)))
    (message "Selected PR %s" selected-number)
    (-first (lambda (x) (= selected-number (gethash "number" x))) (append prlist nil))))

(defun my/gh//checkout-pr-by-number (number)
  (let ((cmd (format "gh pr checkout %s" number)))
    (message "Running %s" cmd)
    (shell-command cmd)))

(defun my/gh/checkout-pr ()
  (interactive)
  (->> (my/gh//get-pr-list)
       (my/gh//prompt-user-to-select-pr)
       (gethash "number")
       (my/gh//checkout-pr-by-number)))

(defun my/gh//browse-file-url (file-path &optional line-number)
  "Returns the url to browse a file"
  (let* ((default-directory (or (file-name-directory file-path) default-directory))
         (file-name (file-name-nondirectory file-path))
         (file-ref (if line-number (format "%s:%s" file-name line-number) file-name))
         ;; commit=last gives us a "permalink" to the last commit, rhather than the branch which may change
         (gh-cmd (format "gh browse --commit=last --no-browser %s" file-ref)))
    (shell-command-to-string gh-cmd)))

(defun my/gh/browse (file-path &optional line-number)
  "Browses to the current file/line."
  (interactive (list buffer-file-name (line-number-at-pos)))
  (let ((url (my/gh//browse-file-url file-path line-number)))
    (browse-url url)
    (funcall my/gh/on-browser-open-request)))

(defun my/gh/browse-url-to-clipboard (file-path &optional line-number)
  "Copies the browse URL to clipboard"
  (interactive (list buffer-file-name (line-number-at-pos)))
  (->> (my/gh//browse-file-url file-path line-number)
       (s-trim)
       (funcall my/gh/copy-to-clipboard)))

(defun my/gh/atlantis-plan ()
  (interactive)
  (let ((cmd-out (shell-command-to-string "gh pr comment --body='atlantis plan'")))
    (browse-url (string-trim cmd-out))))

(defun my/gh//browse-commit-cmd (commit-hash &optional no-browser)
  "Returns the `gh` command to be used for browsing to a commit"
  (if no-browser
      (format "gh browse --no-browser %s" commit-hash)
    (format "gh browse %s" commit-hash)))

(defun my/gh/browse-commit (commit)
  (interactive (list (thing-at-point 'symbol)))
  (->> commit (format "%s") my/gh//browse-commit-cmd shell-command)
  (funcall my/gh/on-browser-open-request))

(when (require 'hydra nil 'noerror)
  (defun my/setup-hydra/gh-hydra ()
    (defhydra my/gh-hydra (:color blue)
      ("b" #'my/gh/browse "Browses to file in github" :column "Github CLI!")
      ("B" #'my/gh/browse-url-to-clipboard "Copies github browse URL to clipboard")
      ("r" #'my/gh/open-repo-on-browser "Open repo on browser")
      ("p" #'my/gh/open-pr-on-browser "Open PR on browser")
      ("P" #'my/gh/new-pr "Creates a new PR")
      ("c" #'my/gh/browse-commit "See the commit on github web")
      ("o" #'my/gh/checkout-pr "Checkout PR"))))

(defun my/gh/default-branch ()
  (-> (shell-command-to-string "gh repo view --json 'defaultBranchRef' --jq '.defaultBranchRef.name'")
      (string-trim)))

;;; my-gh.el ends here
