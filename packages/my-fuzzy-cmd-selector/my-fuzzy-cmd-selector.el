;;; my-fuzzy-cmd-selector.el --- A fuzzy command selector -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2019-03-20
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-fuzzy-command-selector

;; This file is not part of GNU Emacs.
     
;; Do whatever you want. No warranties.
(require 'ivy)
(require 'dash)
(require 'cl-lib)

;;; code
(defvar mfcs-commands '()
  "A list mfcs-command to fuzzy match and call.
   Each mfcs-command must be a plist like (:description ... :command ...)
   For example:
   ((:description \"Run compile\"
      :command (lambda () (interactively) (call-interactively compile))))")

(defvar mfcs-read-command-history '() "Holds the history for commands read.")

(defun mfcs--get-desc (x) (plist-get x :description))

(defun mfcs--get-func (x) (plist-get x :command))

(defun mfcs--desc-eq (command description)
  "Returns truthfully if the command has description equal to `description`."
  (-> command (mfcs--get-desc) (string= description)))

(defun mfcs--pick-command-with-desc (description)
  "Returns the command plist for a given description."
  (--first (mfcs--desc-eq it description) mfcs-commands))

(defun mfcs-ivy-read-command ()
  "Uses ivy to read a command from the user."
  (ivy-read
   "Command: "
   (-map #'mfcs--get-desc mfcs-commands)
   :require-match t
   :caller :mfcs-call
   :history 'mfcs-read-command-history))

(cl-defun mfcs-add-command (&key description command)
  "Adds a command to mfcs-commands if no other command with the
  same description is there."
  (when (--none? (mfcs--desc-eq it description) mfcs-commands)
    (cl-pushnew `(:description ,description :command ,command) mfcs-commands)))

(cl-defun mfcs-remove-command (&key description)
  "Removes a command from mfcs-commands."
  (setq mfcs-commands
        (--remove-first (mfcs--desc-eq it description) mfcs-commands)))

(defun mfcs-call ()
  "Prompts the user to select from mfcs-commands and runs the selected function"
  (interactive)
  (unless mfcs-commands (error "mfcs-commands is empty"))
  (-some->> (mfcs-ivy-read-command)
            (mfcs--pick-command-with-desc)
            (mfcs--get-func)
            (call-interactively)))

(provide 'my-fuzzy-cmd-selector)

;;; my-fuzzy-cmd-selector.el ends here
