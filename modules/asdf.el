 ;; Requires you to manually download it: https://github.com/tabfugnic/asdf.el
(load (concat user-emacs-directory "asdf.el"))
(require 'asdf)
(setq asdf-binary "/opt/asdf-vm/bin/asdf")
(asdf-enable)
