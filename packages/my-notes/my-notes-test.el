;;; my-notes-test.el --- Tests for my-notes
(require 'my-notes)
(require 's)

(ert-deftest test/my/notes//request-for ()
  (let ((my/notes/notes-dir "/tmp/notes"))
    (should (equal (make-my/notes/request
                    :base-dir "/tmp/notes/my-cool-note"
                    :readme-file '("/tmp/notes/my-cool-note/Readme.org"
                                   .
                                   "* My Cool Note\n\n")
                    :metadata-file '("/tmp/notes/my-cool-note/metadata.json"
                                     .
                                     "{\"created_at\":\"2025-01-01\",\"topic\":\"My Cool Note\"}"))
                   (my/notes//request-for "My Cool Note" "2025-01-01")))))
;;; my-mise-test.el ends here

