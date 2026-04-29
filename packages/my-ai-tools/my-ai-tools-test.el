;;; my-ai-tools-test.el --- Tests for my-ai-tools
(require 'my-ai-tools)
(require 's)

(ert-deftest test/my/ai-tools/context/capture ()

  ;; No region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (cd "/tmp")
    (setq-local major-mode 'foo)
    (goto-char 10)
    (let ((temp-buff (current-buffer))
          (context   (my/ai-tools/context/capture)))
      (should (equal 10 (my/ai-tools/context/char-position context)))
      (should (equal 3 (my/ai-tools/context/char-linum context)))
      (should (equal temp-buff (my/ai-tools/context/buffer context)))
      (should (equal "/tmp/" (my/ai-tools/context/default-directory context)))
      (should (equal 'foo (my/ai-tools/context/major-mode context)))
      (should (equal (my/ai-tools/context/region context) nil))))

  ;; Region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (goto-char 5)
    (push-mark 10 t t)
    (activate-mark)
    (let ((temp-buff (current-buffer))
          (context   (my/ai-tools/context/capture)))
      (should (equal 5 (my/ai-tools/context/char-position context)))
      (should (equal temp-buff (my/ai-tools/context/buffer context)))
      (should (equal (my/ai-tools/context/region context)
                     (my/ai-tools/make-region :begin 5
                                              :begin-linum 2
                                              :end 10
                                              :end-linum 3
                                              :content "Bar\nB")))))
  ;; Dired marked files
  (with-temp-buffer
    (with-dired-marked-files '("foo" "bar")
      (setq-local major-mode 'dired-mode)
      (let ((context   (my/ai-tools/context/capture)))
        (should (equal 'dired-mode (my/ai-tools/context/major-mode context)))
        (should (equal '((marked-files . ("foo" "bar")))
                       (my/ai-tools/context/attributes context)))))))

(ert-deftest test/my/ai-tools/context/to-string/default ()

  ;; No region
  (let* ((tmpfile (make-temp-file "test_my_ai-tools"))
         (tmpbuff (find-file-noselect tmpfile))
         (context (my/ai-tools/make-context :buffer tmpbuff
                                            :region nil
                                            :char-linum 1)))
    (with-current-buffer tmpbuff
      (insert "Foo\nBar"))
    (should (equal (my/ai-tools/context/to-string context)
                   (format "## Context\n\nFile: @%s\n\nActive line: 1\n" tmpfile)))
    (delete-file tmpfile)
    (set-buffer-modified-p nil)
    (kill-buffer tmpbuff))

  ;; Region
  (let* ((tmpfile (make-temp-file "test_my_ai-tools"))
         (tmpbuff (find-file-noselect tmpfile))
         (context (my/ai-tools/make-context :buffer tmpbuff
                                            :region (my/ai-tools/make-region
                                                     :begin 5
                                                     :begin-linum 2
                                                     :end 10
                                                     :end-linum 3
                                                     :content "Foo!")
                                            :char-position 2)))
    (should (equal (my/ai-tools/context/to-string context)
                   (format "## Context\n\nFile: @%s\n\nSelected Region (lines 2 to 3):\n```\nFoo!\n```"
                           tmpfile)))
    (delete-file tmpfile)
    (set-buffer-modified-p nil)
    (kill-buffer tmpbuff)))

(ert-deftest test/my/ai-tools/context/to-string/dired ()

  ;; With marked files
  (with-temp-buffer
    (let* ((context (my/ai-tools/make-context :buffer (current-buffer)
                                              :region nil
                                              :char-position 2
                                              :major-mode 'dired-mode
                                              :default-directory "/tmp/foo/"
                                              :attributes '((marked-files . ("foo" "bar"))))))
      (should (equal (my/ai-tools/context/to-string context)
                     "## Context\n\nActive Directory: /tmp/foo/\n\nMarked Files:\n  - foo\n  - bar\n")))))

(ert-deftest test/my/ai-tools/capture-region ()
  ;; No region
  (with-temp-buffer
    (should (not (my/ai-tools/capture-region))))

  ;; With region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (goto-char 5)
    (push-mark 10 t t)
    (activate-mark)
    (should (equal (my/ai-tools/capture-region)
                   (my/ai-tools/make-region
                    :begin 5
                    :begin-linum 2
                    :end 10
                    :end-linum 3
                    :content "Bar\nB")))))

(defmacro with-dired-marked-files (list-of-files &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'dired-get-marked-files) (lambda (_ _) ,list-of-files)))
     (progn ,@body)))

;;; my-mise-test.el ends here

