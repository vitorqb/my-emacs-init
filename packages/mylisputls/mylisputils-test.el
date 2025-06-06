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

(ert-deftest test-myutils/clean-buffers ()
  (-let [myutils/clean-buffers-names-regexs (list "AA")]
    (-let [buff (generate-new-buffer "AA")]
      (myutils/clean-buffers)
      (should (not (buffer-live-p buff))))
    (-let [buff (generate-new-buffer "BB")]
      (myutils/clean-buffers)
      (should (buffer-live-p buff))))
  (-let [myutils/clean-buffers-names-regexs (list "hola")]
    (-let [buff (generate-new-buffer "hola")]
      (myutils/clean-buffers)
      (should (not (buffer-live-p buff))))
    (-let [buff (generate-new-buffer "rola")]
      (myutils/clean-buffers)
      (should (buffer-live-p buff)))))

(ert-deftest test-myutils/fill-to-end ()
  ;; Single usage
  (with-temp-buffer
    (myutils/fill-to-end)
    (should (equal (point) 1))
    (should (equal (buffer-string) (make-string 80 ?-))))
  ;; Line with previous content
  (with-temp-buffer
    (insert (make-string 40 ?A))
    (myutils/fill-to-end)
    (should (equal (buffer-string)
                   (concat (make-string 40 ?A) (make-string 40 ?-)))))
  ;; Usage when col after 80
  (with-temp-buffer
    (-let [inserted-text (make-string 80 ?B)]
      (insert inserted-text)
      (should (equal (buffer-string) inserted-text))
      (myutils/fill-to-end)
      (should (equal (buffer-string) inserted-text)))))

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

(ert-deftest myutils/python-activate-venv/with-venv-dir ()
  (let ((pyvenv-activate-arg))
    (cl-letf (((symbol-function 'myutils/python-get-default-venv-path)
               (lambda () "abc"))
              ((symbol-function 'pyvenv-activate)
               (lambda (a) (setq pyvenv-activate-arg a))))
      (myutils/python-activate-venv)
      (should (equal pyvenv-activate-arg "abc")))))

(ert-deftest myutils/python-activate-venv/with-NO-venv-dir ()
  (let ((call-interactively-arg))
    (cl-letf (((symbol-function 'myutils/python-get-default-venv-path)
               (lambda () nil))
              ((symbol-function 'call-interactively)
               (lambda (a) (setq call-interactively-arg a))))
      (myutils/python-activate-venv)
      (should (equal call-interactively-arg #'pyvenv-activate)))))

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

;;; mylisputils-test.el ends here
