;;; my-mise-test.el --- Tests for my-gh
(require 'my-mise)
(require 's)

(defvar test/my/mise/fake-mise-tasks-json
  "[{\"name\": \"tci\",\"aliases\": [],\"description\": \"CI checks\",\"source\": \"/home/user/project/mise.toml\",\"depends\": [],\"depends_post\": [],\"wait_for\": [],\"env\": [],\"dir\": null,\"hide\": false,\"raw\": false,\"sources\": [],\"outputs\": [],\"shell\": null,\"quiet\": false,\"silent\": false,\"tools\": {},\"run\": [\"mise run foo\",\"mise run bar\"],\"file\": null},{\"name\": \"tformat\",\"aliases\": [],\"description\": \"Formats\",\"source\": \"/home/user/project/mise.toml\",\"depends\": [],\"depends_post\": [],\"wait_for\": [],\"env\": [],\"dir\": null,\"hide\": false,\"raw\": false,\"sources\": [],\"outputs\": [],\"shell\": null,\"quiet\": false,\"silent\": false,\"tools\": {},\"run\": [\"format --all\"],\"file\": null}]")

(ert-deftest test/my/mise/list-tasks ()
  (cl-letf (((symbol-function 'shell-command-to-string)
             (lambda (_) test/my/mise/fake-mise-tasks-json)))
    (should (equal (my/mise/list-tasks) '("tci" "tformat")))))
;;; my-mise-test.el ends here

