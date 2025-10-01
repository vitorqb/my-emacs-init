;;; my-gh-test.el --- Tests for my-gh
(require 'hydra)
(require 'my-gh)

(ert-deftest my/gh//browse-commit-cmd ()
  (should (equal (my/gh//browse-commit-cmd "7fa72cc") "gh browse 7fa72cc"))
  (should (equal (my/gh//browse-commit-cmd '7fa72cc) "gh browse 7fa72cc"))
  (should (equal (my/gh//browse-commit-cmd '7fa72cc 't) "gh browse --no-browser 7fa72cc")))

(ert-deftest my/gh/browse-url-to-clipboard ()
  (let* ((fake-clipboard "")
         (my/gh/copy-to-clipboard (lambda (x) (setq fake-clipboard x))))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (x)
                 (should (equal x "gh browse --no-browser foo:1"))
                 "http:://foo.com#1")))
      (my/gh/browse-url-to-clipboard "foo" 1)
      (should (equal fake-clipboard "http:://foo.com#1")))))

(ert-deftest my/gh/browse-file-url ()
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (x)
               (should (equal x "gh browse --no-browser foo:1"))
               "http:://foo.com#1")))
    (should (equal (my/gh//browse-file-url "foo" 1) "http:://foo.com#1")))
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (x)
               (should (equal x "gh browse --no-browser foo"))
               "http:://foo.com")))
    (should (equal (my/gh//browse-file-url "foo") "http:://foo.com"))))
;;; my-gh-test.el ends here

