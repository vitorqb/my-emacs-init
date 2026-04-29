;;; my-ai-tools-test.el --- Tests for my-ai-tools
(require 'my-ai-tools)
(require 's)

(ert-deftest test/my/ai-tools/capture-context ()

  ;; No region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (cd "/tmp")
    (setq-local major-mode 'foo)
    (goto-char 10)
    (let ((temp-buff (current-buffer))
          (context   (my/ai-tools/capture-context)))
      (should (equal 10 (my/ai-tools/context/char-position context)))
      (should (equal temp-buff (my/ai-tools/context/buffer context)))
      (should (equal "/tmp/" (my/ai-tools/context/default-directory context)))
      (should (equal 'foo (my/ai-tools/context/major-mode context)))
      (should (equal nil (my/ai-tools/context/region-p context)))
      (should (equal nil (my/ai-tools/context/region-begin context)))
      (should (equal nil (my/ai-tools/context/region-end context)))))

  ;; Region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (goto-char 5)
    (push-mark 10 t t)
    (activate-mark)
    (let ((temp-buff (current-buffer))
          (context   (my/ai-tools/capture-context)))
      (should (equal 5 (my/ai-tools/context/char-position context)))
      (should (equal temp-buff (my/ai-tools/context/buffer context)))
      (should (equal t (my/ai-tools/context/region-p context)))
      (should (equal 5 (my/ai-tools/context/region-begin context)))
      (should (equal 10 (my/ai-tools/context/region-end context))))))

(ert-deftest test/my/ai-tools/context-to-string/default ()

  ;; No region
  (let* ((tmpfile (make-temp-file "test_my_ai-tools"))
         (tmpbuff (find-file-noselect tmpfile))
         (context (my/ai-tools/make-context :buffer tmpbuff
                                            :region-p nil
                                            :region-begin nil
                                            :region-end nil
                                            :char-position 2)))
    (with-current-buffer tmpbuff
      (insert "Foo\nBar"))
    (should (equal (my/ai-tools/context-to-string context)
                   (format "## Context\n\nFile: @%s\n\nActive line: 1\n" tmpfile)))
    (delete-file tmpfile)
    (set-buffer-modified-p nil)
    (kill-buffer tmpbuff))

  ;; Region
  (let* ((tmpfile (make-temp-file "test_my_ai-tools"))
         (tmpbuff (find-file-noselect tmpfile))
         (context (my/ai-tools/make-context :buffer tmpbuff
                                            :region-p t
                                            :region-begin 1
                                            :region-end 3
                                            :char-position 2)))
    (with-current-buffer tmpbuff
      (insert "Foo\nBar"))
    (should (equal (my/ai-tools/context-to-string context)
                   (format "## Context\n\nFile: @%s\n\nSelected Region (lines 1 to 1):\n```\nFo\n```"
                           tmpfile)))
    (delete-file tmpfile)
    (set-buffer-modified-p nil)
    (kill-buffer tmpbuff)))

(ert-deftest test/my/ai-tools/context-to-string/dired ()

  ;; With marked files
  (with-dired-marked-files '("foo" "bar")
    (with-temp-buffer
      (let* ((context (my/ai-tools/make-context :buffer (current-buffer)
                                                :region-p nil
                                                :region-begin nil
                                                :region-end nil
                                                :char-position 2
                                                :major-mode 'dired-mode
                                                :default-directory "/tmp/foo/")))
        (should (equal (my/ai-tools/context-to-string context)
                       "## Context\n\nActive Directory: /tmp/foo/\n\nMarked Files:\n  - foo\n  - bar\n"))))))

(defmacro with-dired-marked-files (list-of-files &rest body)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'dired-get-marked-files) (lambda (_ _) ,list-of-files)))
     (progn ,@body)))

;;; my-mise-test.el ends here

