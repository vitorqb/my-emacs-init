;;; mylisputils-test.el --- Tests for mylisputils
(require 'cl-lib)
(require 'el-mock)
(require 'dash)
(require 'flycheck)
(require 'pyvenv)
(require 'mylisputils)
(require 'ivy)

(ert-deftest add-to-generic-path-works ()
  (let ((stub-env "A"))
    (cl-letf (((symbol-function 'getenv) (lambda (x) stub-env))
              ((symbol-function 'setenv) (lambda (y x) (setq stub-env x))))
      (myutils/add-to-generic-path "B" "")
      (should (equal stub-env "B:A")))))

(ert-deftest test-concat-file ()
  (should (equal (myutils/concat-file "/home/vitor" "file") "/home/vitor/file"))
  (should (equal (myutils/concat-file "/home/vitor/" "f") "/home/vitor/f")))

(ert-deftest test-myutils/remove-whitespace-and-newline/base ()
    (with-temp-buffer
      (insert "B   \n \t  C")
      (goto-char 2)
      (should (equal (string (char-after)) " "))
      (myutils/remove-whitespace-and-newline)
      (should (equal (char-after) ?C))))

(ert-deftest test-myutils/remove-whitespace-and-newline/delete-none ()
  (with-temp-buffer
    (-let [text "ABC"]
      (insert text)
      (myutils/remove-whitespace-and-newline)
      (should (equal (buffer-string) text)))))

(ert-deftest test-myutils/remove-whitespace-and-newline/end-of-buffer ()
  (with-temp-buffer
    (-let [text "ABC"]
      (insert text)
      (end-of-buffer)
      (myutils/remove-whitespace-and-newline)
      (should (equal (buffer-string) text)))))

(ert-deftest test-myutils/remove-with-elipsis ()
  (with-temp-buffer
    (dolist (prefix '("First" "Second" "Third" "Fourth"))
      (insert (concat prefix " Line."))
      (newline))
    ;; Put's the pointer at the third row
    (goto-line 3)

    ;; Calls it once
    (myutils/remove-with-elipsis)
    (beginning-of-buffer)
    (dolist (expected-line '("First Line.\n" "Second Line.\n" "[...]\n" "Fourth Line.\n"))
      (should (string-equal (thing-at-point 'line t) expected-line))
      (next-line))

    ;; Calls again
    (goto-line 3)
    (myutils/remove-with-elipsis)
    (beginning-of-buffer)
    (dolist (expected-line '("First Line.\n" "[...]\n" "Fourth Line.\n"))
      (should (string-equal (thing-at-point 'line t) expected-line))
      (next-line))))

(ert-deftest test-myutils/priv/file-path ()
  ;; Case 1 -> relative t project root /foo/bar
  (let* ((projectile-project-root-functions (list (lambda (_) "/foo/bar")))
         (default-directory "/foo/bar/baz/boz")
         (major-mode 'dired-mode))
    (should (string-equal "baz/boz" (myutils/priv/file-path 't))))
  ;; Case 2 -> absolute
  (let* ((projectile-project-root-functions (list (lambda (_) "/foo/bar")))
         (default-directory "/foo/bar/baz/boz")
         (major-mode 'dired-mode))
    (should (string-equal "/foo/bar/baz/boz" (myutils/priv/file-path nil)))))

(ert-deftest myutils/with-compile-opts ()
  (myutils/with-compile-opts "*buffname*" "my command"
    (should (equal (funcall compilation-buffer-name-function) "*buffname*"))
    (should (equal compile-command "my command"))))

(ert-deftest myutils/relative-path ()
  (-let* ((shell-command-to-string-vars) ;Stores args to shell-command-to-string
          (path "/home/foo bar baz")
          (current-dir "/boz")
          (fake-result "FAKE_RESULT"))
    ;; Mocks shell-command-to-string
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (x)
                 (setq shell-command-to-string-vars x)
                 fake-result)))

      ;; When calling, fake-result should be returned
      (should (string-equal (myutils/relative-path path current-dir) fake-result))

      ;; And the correct command sent to shell
      (should (string-equal shell-command-to-string-vars
                            "realpath --relative-to=/boz /home/foo\\ bar\\ baz")))))

(ert-deftest myutils/date-in-all-formats ()
  (should (equal (-map #'format-time-string myutils/known-datetime-formats)
                 (myutils/date-in-all-formats))))

(ert-deftest myutils/test-parse-json-file ()
  (let ((tempfile (make-temp-file "myutils__test-parse-json-file")))
    (write-region "{\"foo\": \"bar\"}" nil tempfile)
    (should (equal "bar"
                   (->> (myutils/parse-json-file tempfile)
                        (gethash "foo"))))))

;;; mylisputils-test.el ends here
