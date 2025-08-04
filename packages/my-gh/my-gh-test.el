;;; my-gh-test.el --- Tests for my-gh
(require 'hydra)
(require 'my-gh)

(ert-deftest my/gh//browse-cmd ()
  (should (equal (my/gh//browse-cmd "foo" nil) "gh browse foo"))
  (should (equal (my/gh//browse-cmd "foo" 12)  "gh browse foo:12")))

(ert-deftest my/gh//browse-commit-cmd ()
  (should (equal (my/gh//browse-commit-cmd "7fa72cc") "gh browse 7fa72cc"))
  (should (equal (my/gh//browse-commit-cmd '7fa72cc) "gh browse 7fa72cc")))


;;; my-gh-test.el ends here

