;;; my-gh-test.el --- Tests for my-gh
(require 'hydra)
(require 'my-gh)

(defun hashtable-equal? (ht1 ht2)
  "Compare if two hashtables are equal (values and keys)"
  (cl-labels ((is-equal? (x1 x2)
                (cond ((and (numberp x1) (numberp x2))
                       (= x1 x2))
                      ((and (stringp x1) (stringp x2))
                       (string-equal x1 x2))
                      ((and (booleanp x1) (booleanp x2))
                       (equal x1 x2))
                      ((and (keywordp x1) (keywordp x2))
                       (equal x1 x2))
                      ((and (hash-table-p x1) (hash-table-p x2))
                       (and (-all? (lambda (x1-pair)
                                     (let ((key (car x1-pair))
                                           (val (cdr x1-pair)))
                                       (is-equal? val (gethash key x2 :missing))))
                                   (map-pairs x1))
                            (-all? (lambda (x2-pair)
                                     (let ((key (car x2-pair))
                                           (val (cdr x2-pair)))
                                       (is-equal? val (gethash key x1 :missing))))
                                   (map-pairs x2))))
                      ('t nil))))
    (is-equal? ht1 ht2)))

(defun get-file-as-string (filename)
  (with-temp-buffer
    (insert-file filename)
    (buffer-string)))

(defun fake-pr ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "author"
             (let ((author (make-hash-table :test 'equal)))
               (puthash "login" "janedoe" author)
               author)
             table)
    (puthash "createdAt" "2025-10-01T08:49:59Z" table)
    (puthash "headRefName" "my-branch" table)
    (puthash "id" "PR_xxxxxxxxxxx" table)
    (puthash "state" "OPEN" table)
    (puthash "title" "MY TITLE" table)
    (puthash "number" 1234 table)
    table))

(ert-deftest my/gh//browse-commit-cmd ()
  (should (equal (my/gh//browse-commit-cmd "7fa72cc") "gh browse 7fa72cc"))
  (should (equal (my/gh//browse-commit-cmd '7fa72cc) "gh browse 7fa72cc"))
  (should (equal (my/gh//browse-commit-cmd '7fa72cc 't) "gh browse --no-browser 7fa72cc")))

(ert-deftest my/gh//current-branch ()
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (x)
               (should (equal x "git branch --show-current"))
               " pr-9999-merge \n")))
    (should (equal "pr-9999-merge" (my/gh//current-branch)))))

(ert-deftest my/gh//pr-number-from-gh ()
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (x)
               (should (equal x "gh pr view --json number --jq .number"))
               " 1234 \n")))
    (should (equal "1234" (my/gh//pr-number-from-gh)))))

(ert-deftest my/gh//current-pr-number ()
  ;; From current branch
  (cl-letf (((symbol-function 'my/gh//current-branch) (lambda () "pr-1234-merge")))
    (should (equal "1234" (my/gh//current-pr-number))))

  ;; From gh
  (cl-letf (((symbol-function 'my/gh//current-branch) (lambda () ""))
            ((symbol-function 'my/gh//pr-number-from-gh) (lambda () "1234")))
    (should (equal "1234" (my/gh//current-pr-number)))))

(ert-deftest my/gh//pr-number-from-branch ()
  (should (not (my/gh//pr-number-from-branch "")))
  (should (not (my/gh//pr-number-from-branch nil)))
  (should (not (my/gh//pr-number-from-branch "foo")))
  (should (not (my/gh//pr-number-from-branch "foo-bar-baz")))
  (should (not (my/gh//pr-number-from-branch "foo-12345-bar")))
  (should (equal "123" (my/gh//pr-number-from-branch "pr-123-merge"))))

(ert-deftest my/gh/browse-url-to-clipboard ()
  (let* ((fake-clipboard "")
         (my/gh/copy-to-clipboard (lambda (x) (setq fake-clipboard x))))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (x)
                 (should (equal x "gh browse --commit=last --no-browser foo:1"))
                 "http:://foo.com#1")))
      (my/gh/browse-url-to-clipboard "foo" 1)
      (should (equal fake-clipboard "http:://foo.com#1")))))

(ert-deftest my/gh/browse-file-url ()
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (x)
               (should (equal x "gh browse --commit=last --no-browser foo:1"))
               "http:://foo.com#1")))
    (should (equal (my/gh//browse-file-url "foo" 1) "http:://foo.com#1")))
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (x)
               (should (equal x "gh browse --commit=last --no-browser foo"))
               "http:://foo.com")))
    (should (equal (my/gh//browse-file-url "foo") "http:://foo.com"))))

(ert-deftest my/gh/test-get-pr-list ()
  (let ((gh-fake-output (-> (file-name-concat "test-assets" "fake-pr-list.json")
                            (get-file-as-string))))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (x)
                 (should (equal x "gh pr list --json='author,id,title,url,state,createdAt,isDraft,number,headRefName'"))
                 gh-fake-output)))
      (let ((result (my/gh//get-pr-list)))
        (should (equal (-> result (aref 0) (->> (gethash "id")))
                       "PR_fakeId1234567890a"))
        (should (equal (-> result (aref 0) (->> (gethash "isDraft")))
                       :false))
        (should (equal (-> result (aref 0) (->> (gethash "number")))
                       5001))
        (should (equal (-> result (aref 0) (->> (gethash "state")))
                       "OPEN"))
        (should (equal (-> result (aref 0) (->> (gethash "title")))
                       "[FEATURE-101]: Add new authentication method"))
        (should (equal (-> result (aref 0) (->> (gethash "url")))
                       "https://github.com/example/repo/pull/5001"))
        (should (equal (length result) 10))))))

(ert-deftest my/gh/test-prompt-user-to-select-pr ()
  (let ((candidates (list (fake-pr))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_ candidates ___ ____) (car candidates))))
      (let ((result (my/gh//prompt-user-to-select-pr candidates)))
        (should (hashtable-equal? result (fake-pr)))))))

(ert-deftest my/gh/test-checkout-pr-by-number ()

  (my/gh-test/with-current-branch "foo"
    (my/gh-test/with-branch-exists? nil
      (let ((all_args (list)))
        (cl-letf (((symbol-function 'magit-run-git)
                   (lambda (&rest args) (push args all_args))))
          (my/gh//checkout-pr-by-number 123)
          (should (equal '(("fetch" "origin" "pull/123/merge:pr-123-merge")
                           ("checkout"  "pr-123-merge"))
                         (reverse all_args)))))))

  (my/gh-test/with-current-branch "pr-123-merge" ;Same as the one to be created
    (my/gh-test/with-branch-exists? t            ;Branch already exists
      (my/gh-test/with-default-branch "master"
        (let ((all_args (list)))
          (cl-letf (((symbol-function 'magit-run-git)
                     (lambda (&rest args) (push args all_args))))
            (my/gh//checkout-pr-by-number 123)
            (should (equal '(("checkout" "master")
                             ("branch" "-D" "pr-123-merge")
                             ("fetch" "origin" "pull/123/merge:pr-123-merge")
                             ("checkout"  "pr-123-merge"))
                           (reverse all_args)))))))))

(ert-deftest my/gh/edit-pr-body__success ()
  (my/gh-test/with-current-pr-number "123"
    (my/gh-test/with-buffer-cleanup my/gh/edit-pr-body-buffer-name
      (my/gh-test/with-insert-pr-body "BODY!"
        (let ((default-directory "/tmp/foo"))
          (call-interactively #'my/gh/edit-pr-body)
          (should (equal (buffer-name) my/gh/edit-pr-body-buffer-name))
          (should (equal (buffer-substring (point-min) (point-max)) "BODY!"))
          (should (equal my/gh//edit-pr-metadata-default-directory "/tmp/foo"))
          (should (equal my/gh//edit-pr-metadata-current-pr-number "123"))
          (should (equal major-mode 'my/gh/edit-pr-body-mode)))))))

(ert-deftest my/gh/edit-pr-body__fails-if-buffer-exists ()
  (my/gh-test/with-buffer-cleanup my/gh/edit-pr-body-buffer-name
    (let ((buff (get-buffer-create my/gh/edit-pr-body-buffer-name)))
      (should-error (call-interactively #'my/gh/edit-pr-body)
                    :type 'user-error))))

(ert-deftest my/gh/edit-pr-body-done ()
  (my/gh-test/with-current-pr-number "123"
    (my/gh-test/with-buffer-cleanup my/gh/edit-pr-body-buffer-name
      (my/gh-test/with-insert-pr-body "BODY!"
        (my/gh-test/capture-edit-pr-body-request request
          (let ((default-directory "/tmp/foo"))
            ;; User calls `my/gh/edit-pr-body`
            (call-interactively #'my/gh/edit-pr-body)
            ;; User calls `my/gh/edit-pr-body-done`
            (call-interactively #'my/gh/edit-pr-body-done)
            (should (my/gh-test/edit-pr-body-request-equal?
                     request
                     (my/gh//create-edit-pr-body-request
                      :gitrepo-directory "/tmp/foo"
                      :pr-number "123"
                      :src-buffer (get-buffer my/gh/edit-pr-body-buffer-name))))))))))

;;
;; Helpers
;; 
(defmacro my/gh-test/with-current-branch (branchname &rest program)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'my/gh//current-branch)
              (lambda () ,branchname)))
     (progn ,@program)))

(defmacro my/gh-test/with-current-pr-number (pr-number &rest program)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'my/gh//current-pr-number)
              (lambda () ,pr-number)))
     (progn ,@program)))

(defmacro my/gh-test/with-branch-exists? (val &rest program)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'my/gh//branch-exists?)
              (lambda (_) ,val)))
     (progn ,@program)))

(defmacro my/gh-test/with-default-branch (branchname &rest program)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'my/gh//default-branch)
              (lambda (_) ,branchname)))
     (progn ,@program)))

(defmacro my/gh-test/with-insert-pr-body (body &rest program)
  (declare (indent 1))
  `(cl-letf (((symbol-function 'my/gh/insert-pr-body)
              (lambda (_) (insert ,body))))
     (progn ,@program)))

(defmacro my/gh-test/with-buffer-cleanup (buffname &rest program)
  (declare (indent 1))
  `(unwind-protect
       (progn ,@program)
     (when-let ((buff (get-buffer ,buffname)))
       (when (buffer-live-p buff)
         (kill-buffer buff)))))

(defmacro my/gh-test/capture-edit-pr-body-request (bind &rest program)
  "Capture the argument passed to `my/gh//do-edit-pr-body` and bind it to BIND for assertions in the test."
  (declare (indent 1))
  `(let ((,bind nil))
     (cl-letf (((symbol-function 'my/gh//do-edit-pr-body)
                (lambda (req) (setq ,bind req))))
       (progn ,@program))))

(defun my/gh-test/edit-pr-body-request-equal? (req1 req2)
  "Compare if two `my/gh//edit-pr-body-request' are equal (ignoring the `tmp-file' field)"
  (let ((new-req1 (my/gh//copy-edit-pr-body-request req1))
        (new-req2 (my/gh//copy-edit-pr-body-request req2)))
    (setf (my/gh//edit-pr-body-request/tmp-file new-req1) "")
    (setf (my/gh//edit-pr-body-request/tmp-file new-req2) "")
    (equal new-req1 new-req2)))

;;; my-gh-test.el ends here

