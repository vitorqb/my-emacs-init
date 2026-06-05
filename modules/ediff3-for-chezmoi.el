;; A specific function that can be called with 3 files and will nicely start an
;; ediff3 session for merging the files
;;
;; Use it like this:
;;   #!/bin/bash
;;   emacsclient --create-frame -e "(my/ediff3-for-chezmoi \"$1\" \"$2\" \"$3\")"
(defun my/ediff3-for-chezmoi (file1 file2 file3)
  "Start an ediff3 session for merging three files from chezmoi."
  (ediff-files3 file1 file2 file3 (list 'my/ediff3chez/set-exit-key)))

(defun my/ediff3chez/on-exit ()
  "Exit hook for when the user is done"
  (interactive)
  (my/ediff3chez/save-all-buffers)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
    (ediff-quit nil))
  (ediff-janitor nil nil)
  (delete-frame))

(defun my/ediff3chez/set-exit-key ()
  "Sets the exit key for when user is done"
  (keymap-local-set "C-c C-c" 'my/ediff3chez/on-exit)
  (keymap-local-set "q" 'my/ediff3chez/on-exit))

(defun my/ediff3chez/save-all-buffers ()
  (dolist (sym '(A B C))
    (let ((buf (ediff-get-buffer sym)))
      (with-current-buffer buf
        (save-buffer)))))
