;;; my-term-test.el --- Tests for my-gh
(require 'my-term)

(ert-deftest test-open-on-current-dir ()
  (let* ((new-pane-args '())
         (my/term/new-pane (lambda (x) (push x new-pane-args)))
         (focus-window-calls 0)
         (my/term/focus-window (lambda () (setq focus-window-calls (+ 1 focus-window-calls))))
         (default-directory "/foo"))
    (call-interactively #'my/term/open-on-current-dir)
    (should (s-equals? (car new-pane-args) "/foo"))
    (should (= 1 focus-window-calls))))
;; my-term-test.el ends here
