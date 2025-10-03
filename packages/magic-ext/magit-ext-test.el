;;; magit-ext-test.el --- Magit extensions -*- lexical-binding: t -*-

;; Copyright (C) 2010-2025 Vitor Quintanilha Barbosa

;; Author: Vitor <vitorqb@gmail.com>
;; Version: 0.0.1
;; Maintainer: Vitor <vitorqb@gmail.com>
;; Created: 2025-08-04
;; Keywords: elisp
;; Homepage: https://github.com/vitorqb/my-emacs-init

;; This file is not part of GNU Emacs.

;;; code
(require 'magit-ext)

(ert-deftest magext-test-magext--commit-msg ()
  ;; aichat-model is nil
  (let ((shell-cmd nil)
        (magext-aichat-model nil))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (x) (setq shell-cmd x))))
      (magext--commit-msg "difffile" "histfile")
      (should (s-contains? "aichat" shell-cmd))
      (should (s-contains? magext--system-prompt shell-cmd))
      (should (s-contains? "-f 'difffile'" shell-cmd))
      (should (s-contains? "-f 'histfile'" shell-cmd))))

  ;; aichat-model not nil
  (let ((shell-cmd nil)
        (magext-aichat-model "chatgpt"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (x) (setq shell-cmd x))))
      (magext--commit-msg "difffile" "histfile")
      (should (s-contains? "aichat" shell-cmd))
      (should (s-contains? magext--system-prompt shell-cmd))
      (should (s-contains? "--model 'chatgpt'" shell-cmd)))))

;;; magit-ext-test.el ends here
