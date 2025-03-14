;;; orgext-test.el --- Tests for orgext
(require 'org)
(require 'dash)
(require 's)
(require 'orgext)

(defmacro orgext--in-buffer-with-content (lst-of-lines &rest body)
  "Evaluates body inside a `with-temp-buffer` after inserting `lst-of-lines`.
`lst-of-lines` must be a list of strings, which will be inserted into the buffer."
  (declare (indent 1))
  `(-let [content (s-join "\n" ,lst-of-lines)]
     (with-temp-buffer
       (insert content)
       (org-mode)
       ,@body)))

(defun test-cleanup ()
  (if-let ((buff (get-buffer orgext-element-at-point-buffer-name)))
      (with-current-buffer buff
        (set-buffer-modified-p nil)
        (kill-buffer))))

(defmacro with-cleanup (&rest body)
  `(unwind-protect (progn ,@body)
     (test-cleanup)))

(ert-deftest test-t-copy-block-from-above ()
  (with-cleanup
   ;; Case - without anything raises error
   (orgext--in-buffer-with-content
    '("This is a file"
      ""
      "In this file there is no org block.")
    (goto-char (point-max))
    (should-error (orgext-copy-block-from-above)))
  
   ;; Case - with a block above
   (-let [org-block-content (s-join "\n" '("#+begin_example"
                                           "SOMETHING INSIDE THE BLOCK"
                                           "#+end_example"))]
     (orgext--in-buffer-with-content
      `("Something here"
        ,org-block-content
        "Something else here"
        "\n")
      ;; We should be in the last line with an \n
      (should (string-equal (thing-at-point 'line) "\n"))
      ;; We call the function
      (goto-char (point-max))
      (orgext-copy-block-from-above)
      ;; And we should see the block ahead of us
      (should (string-equal (buffer-substring-no-properties (point) (point-max))
                            (concat org-block-content "\n")))))))

(ert-deftest test-orgexp-mark-block ()
  (with-cleanup
   ;; Case - when you are not in a block
   (orgext--in-buffer-with-content
    '("some text"
      "but no org block"
      "\n"
      "foo bar")
    (goto-char (point-max))
    (forward-line -2)
    (should-error (orgext-mark-block)))

   ;; Case - when you are at a block
   (-let* ((block-contents "Some\nThings\nwith space\n here!\n\n")
           (the-block (concat  "#+begin_example\n" block-contents "#+end_example")))
     (orgext--in-buffer-with-content
      `("some text"
        "and then a block:"
        ,the-block
        "Foo Bar\n"
        "Baz")
      (goto-char (point-max))
      (forward-line -3)
      (orgext-mark-block)
      (should (string-equal (buffer-substring (region-beginning) (region-end))
                            block-contents))))))

(ert-deftest test-orgext-element-at-point-on-new-buffer__content ()

  (with-cleanup

   (-let* (;; An example org-file for the original window
           (content (concat "* Section One\n"
                            "Content of section one\n"
                            "** Sub Section One\n"
                            "Content of sub section one\n"
                            "* Section Two\n"
                            "Content of section two\n")))
     (with-temp-buffer
       (org-mode)
       (insert content)
       (goto-char 1)
       (orgext-element-at-point-on-new-buffer)
       (with-current-buffer orgext-element-at-point-buffer-name
         (should (string-equal (buffer-string)
                               (concat "* Section One\n"
                                       "Content of section one\n"
                                       "** Sub Section One\n"
                                       "Content of sub section one\n"))))))))

;; This is failing because of the org version :`(
(ert-deftest test-orgext-new-block-from-other-window ()

  (with-cleanup

   ;; Selecting example block
   (-let* (;; A text for the `other-window`
           (other-contents "Foo bar ipson loren")
           ;; The block type
           (block-type "example")
           ;; What we expect to find in the end
           (expected-contents (concat  "#+begin_example\n"
                                       other-contents
                                       "#+end_example\n")))

     (cl-letf (;; Mocks the function to get the other window text
               ((symbol-function 'orgext--get-other-window-contents)
                (-const other-contents)))
       (with-temp-buffer
         (orgext-new-block-from-other-window "example")
         (should (equal (point) (point-min)))
         (should (string= (buffer-substring-no-properties (point-min) (point-max))
                          expected-contents))))

     ;; Selecting src block
     (-let* (;; A text for the `other-window`
             (other-contents "Foo!")
             ;; The block type
             (block-type "src")
             ;; What we expect to find in the end
             (expected-contents (concat  "#+begin_src \n" other-contents "#+end_src\n")))

       (cl-letf (;; Mocks the function to get the other window text
                 ((symbol-function 'orgext--get-other-window-contents)
                  (-const other-contents)))

         (with-temp-buffer
           (orgext-new-block-from-other-window block-type)
           (should (equal (point) (point-min)))
           (should (string= (buffer-substring-no-properties (point-min) (point-max))
                            expected-contents))))))))

;;; orgext-test.el ends here
