;;; my-ai-tools-test.el --- Tests for my-ai-tools
(require 'my-ai-tools)
(require 's)

(ert-deftest test/my/ai-tools/capture-context ()

  ;; No region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (goto-char 10)
    (let ((temp-buff (current-buffer))
          (context   (my/ai-tools/capture-context)))
      (should (equal 10 (my/ai-tools/context-char-position context)))
      (should (equal temp-buff (my/ai-tools/context-buffer context)))
      (should (equal nil (my/ai-tools/context-region-p context)))
      (should (equal nil (my/ai-tools/context-region-begin context)))
      (should (equal nil (my/ai-tools/context-region-end context)))))

  ;; Region
  (with-temp-buffer
    (insert "Foo\nBar\nBaz")
    (goto-char 5)
    (push-mark 10 t t)
    (activate-mark)
    (let ((temp-buff (current-buffer))
          (context   (my/ai-tools/capture-context)))
      (should (equal 5 (my/ai-tools/context-char-position context)))
      (should (equal temp-buff (my/ai-tools/context-buffer context)))
      (should (equal t (my/ai-tools/context-region-p context)))
      (should (equal 5 (my/ai-tools/context-region-begin context)))
      (should (equal 10 (my/ai-tools/context-region-end context))))))

(ert-deftest test/my/ai-tools/context-to-string ()

  ;; No region
  (let* ((tmpfile (make-temp-file "test_my_ai-tools"))
         (tmpbuff (find-file-noselect tmpfile))
         (context (make-my/ai-tools/context :buffer tmpbuff
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
         (context (make-my/ai-tools/context :buffer tmpbuff
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

;;; my-mise-test.el ends here

