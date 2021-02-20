;; Org integrations
(defun ohmycards/jira/link-follow (link)
  (browse-url (concat "http://134.209.87.114:7000/#/cards/display?id=" link)))

(when (not (boundp 'ohmycards/jira/initialized?))
  (message "Adding CARD shortcut to ohmycards")
  (org-link-set-parameters "CARD" :follow #'ohmycards/jira/link-follow)
  (setq ohmycards-tools/jira/initialized? t))
