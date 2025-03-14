;;; my-fuzzy-cmd-selector-test.el --- Tests for my-fuzzy-cmd-selector

(require 'my-fuzzy-cmd-selector)
(require 'dash)

(ert-deftest test-mfcs--get-desc ()

  (should
   (string=
    "FOO"
    (mfcs--get-desc '(:description "FOO"))))

  (should-not
   (string=
    "BAR"
    (mfcs--get-desc '(:description "FOO")))))

(ert-deftest test-mfcs--get-func ()

  (should
   (equal
    (mfcs--get-func '(:command "FOO"))
    "FOO"))

  (should-not
   (equal
    (mfcs--get-func '(:command "FOO"))
    "BAR")))

(ert-deftest test-mfcs--desc-eq ()

  (should (mfcs--desc-eq '(:description "FOO") "FOO"))

  (should-not (mfcs--desc-eq '(:description "FOO") "BAR")))

(ert-deftest test-mfcs--pick-command-with-desc ()

  (-let* ((command `(:description "FOO" :command ,(lambda () (interactive) 1)))
          (mfcs-commands `(,command)))
    (should (equal command (mfcs--pick-command-with-desc "FOO")))
    (should (equal nil     (mfcs--pick-command-with-desc "BAR")))))

(ert-deftest test-mfcs-add-command ()

  (-let ((mfcs-commands '())
         (command (lambda () (interactive) nil)))

    (mfcs-add-command :description "FOO" :command command)
    (should
     (equal
      mfcs-commands
      `((:description "FOO" :command ,command))))

    (mfcs-add-command :description "FOO" :command (lambda () (interactive) 2))
    (should
     (equal
      mfcs-commands
      `((:description "FOO" :command ,command)))))

  (should (equal mfcs-commands '())))

(ert-deftest test-mfcs-remove-command ()

  (-let ((mfcs-commands '())
         (command `(:description "FOO" :command ,(lambda () (interactive) 1))))
    (apply 'mfcs-add-command command)
    (should (equal mfcs-commands `(,command)))
    (mfcs-remove-command :description "FOO")
    (should (equal mfcs-commands '()))))

;;; my-fuzzy-cmd-selector-test.el ends here
