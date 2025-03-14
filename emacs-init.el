;; -----------------------------------------------------------------------------
;; Customizable variables
;; -----------------------------------------------------------------------------
;; We define all variables that can be customized among different laptops here,
;; and then we load a file ~/config/emacs_init.el, hoping that this file will
;; customize any variable it needs to customize.
(defvar my-font-size 14 "The default font size used.")
(defvar my-font-name nil "The name for the font")
(defvar my-current-profile nil
  (concat "A profile that can be used to customize your computer-specific settings."
          " For example: :work or :home."))
(defvar my/journal-files-dir-base "files"
  (concat "A folder (relative to `org-journal-dir` unless it starts with '/') where"
          " to put files for the org-journal"))
(defvar my/emacs-init-deps-path "~/.emacs.d/emacs_init_deps/"
  "Path to a directory which contains all emacs-init dependencies.")
(defvar my/user-temp-directory "~/mytmp"
  "A directory used to save temporary files.")
(defvar my/default-browser-cmd "firefox %s"
  "The default command to open a browser. The '%s' will be substituted by the url to be openned.")
(defvar my/path-to-modules-dir (concat (file-name-directory load-file-name) "modules")
  "The directory where the emacs-init `modules` can be found.")
(defcustom my/terminal-multiplex 'zellij
  "Which terminal multiplex to use (tmux/zellij)"
  :type '(symbol)
  :options '(zellij tmux))

;; Loads the config. This is your oportunity to customize any of the variables.
(let ((config-file-name (expand-file-name "~/.config/emacs_init/config.el")))
  (when (file-exists-p config-file-name)
    (load config-file-name)))

;; -----------------------------------------------------------------------------
;; straight.el
;; -----------------------------------------------------------------------------
;; Code from https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ------------------------------------------------------------
;; Some common requirements
;; ------------------------------------------------------------
(use-package dash :ensure)
(use-package s :ensure)

;; -----------------------------------------------------------------------------
;; Packages and load settings
;; -----------------------------------------------------------------------------
;; Don't customize anything
(setq custom-file "/dev/null")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(require 'bind-key)

;; Add our custom library to the load-path. Add anything inside
;; my/emacs-init-deps-path that does not start with "."
(seq-doseq (file (directory-files my/emacs-init-deps-path 't))
  (when (and (not (->> file (file-name-nondirectory) (string-prefix-p ".")))
             (file-directory-p file))
    (add-to-list 'load-path file)))

;; -----------------------------------------------------------------------------
;; Global requirements
;;   the rest of the config depends on these 
;; -----------------------------------------------------------------------------
(use-package counsel :ensure)
(use-package ivy
  :ensure
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))
(use-package ivy-hydra :ensure)

;; https://github.com/vitorqb/mylisputils/
(use-package mylisputils)

;; Ensure the tempdir is created
(and my/user-temp-directory
     (not (file-directory-p my/user-temp-directory))
     (progn (mkdir my/user-temp-directory t)
            (message "Created %s" my/user-temp-directory)))

;; We like recursion
(setq max-lisp-eval-depth (* 10 max-lisp-eval-depth))

;; ------------------------------------------------------------
;; Counsell/Ivy customization
;; ------------------------------------------------------------
;; Allow searching the bash history
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (shell-command "history -r")          ; reload history
  (let* ((history (s-lines (with-temp-buffer
                             (insert-file-contents (file-truename "~/.bash_history"))
                             (buffer-string))))
         (collection (nreverse history)))
    (when (and collection (> (length collection) 0))
      (when-let ((val (ivy-read (format "Bash history:") collection)))
        (kill-new val)
        (message "%s => kill-ring" val)))))

;; -----------------------------------------------------------------------------
;; Emacs Init Modules
;; -----------------------------------------------------------------------------
(defun my/load-module (file-name)
  "Loads a module file."
  (message "Loading module %s" file-name)
  (load (concat my/path-to-modules-dir "/" file-name))
  (yas-reload-all))

(defmacro my/defmodule (module-name)
  "Returns a `defun` to load a module called `module-name`."
  (-let* ((module-name-str (symbol-name module-name))
          (load-module-fn-name (->> module-name-str
                                    (concat "emacs-init-load-module-")
                                    intern))
          (load-module-fn-docstring (concat "Loads module " module-name-str))
          (load-module-file-name (concat module-name-str ".el")))
    `(defun ,load-module-fn-name ()
       ,load-module-fn-docstring
       (interactive)
       (my/load-module ,load-module-file-name))))

;; ------------------------------------------------------------------------------
;; Global Appearence
;; -----------------------------------------------------------------------------
;; Don't show menu, scroll, toolbar
(menu-bar-mode -1)
(custom-set-variables '(scroll-bar-mode nil))
(tool-bar-mode -1)

;; Choose font
;; This way of setting fonts works both for emacsclient and emacs.
(when (and my-font-size my-font-name)
  (setq default-frame-alist `((font . ,(format "%s %s" my-font-name my-font-size)))))

;; Choose theme
(load-theme 'modus-vivendi-tinted t nil)

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;;Display line and col numbers
(global-display-line-numbers-mode)
(column-number-mode)

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Delete active region
(delete-selection-mode +1)

;; Always use y-or-n rather than yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Nice mode
(which-key-mode 1)

;; -----------------------------------------------------------------------------
;; Fuzzy command selector
;; -----------------------------------------------------------------------------
;; https://github.com/vitorqb/my-fuzzy-cmd-selector
(use-package my-fuzzy-cmd-selector
  :config (cl-pushnew '(:mfcs-call . 20) ivy-height-alist))

;; -----------------------------------------------------------------------------
;; Yas and Snippets
;; -----------------------------------------------------------------------------
(defun personal-snippets-dir ()
  (s-concat (getenv "VITOR_MYGIT_DIR") "/snippets/snippets"))

(use-package yasnippet :ensure
  :config
  (progn
    (custom-set-variables
     '(yas-snippet-dirs (-snoc yas-snippet-dirs (personal-snippets-dir))))))

;; -----------------------------------------------------------------------------
;; Typing shortcuts functions
;; -----------------------------------------------------------------------------
(global-set-key (kbd "<f12>") 'myutils/insert-formated-date)
(global-set-key (kbd "<S-f10>") #'menu-bar-open)
(global-set-key (kbd "C-c u") 'myutils/remove-whitespace-and-newline)

;; We use `fill-paragraph a lot, and we want to have 90 characters
(setq fill-column 90)

(defun my/copy-line-from (lineNum)
  "Copies a line to the current line"
  (interactive (list (read-number (format "Line: "))))
  (save-excursion
    (goto-line lineNum)
    (kill-ring-save
     (line-beginning-position)
     (line-end-position)))
  (yank))

(defun my/setup-hydra/typing-hydra ()
  "Prepares an hydra for typing shortcuts."
  (defhydra my/typing-hydra (:color blue)
    ("c" #'myutils/remove-whitespace-and-newline
     "Clean - remove whitesapces and newlines."
     :column "Typing")
    ("d" #'myutils/insert-formated-date "Insert the date.")
    ("e" #'myutils/remove-with-elipsis "Remove with elipsis." :color pink)
    ("l" #'my/copy-line-from "Copies from another line.")
    ("w" #'delete-trailing-whitespace "Delete trailing whitespaces.")))

;; -----------------------------------------------------------------------------
;; Flycheck
;; -----------------------------------------------------------------------------
(use-package flycheck :ensure)
(use-package flycheck-inline :ensure
  :config
  (progn
    (set-face-attribute
     'flycheck-inline-error
     nil
     :box '(:line-width 3 :color "salmon" :style released-button))
    (add-hook 'flycheck-mode-hook 'turn-on-flycheck-inline)))

;; -----------------------------------------------------------------------------
;; Flymake
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/flymake-hydra ()
  (defhydra my/flymake-hydra (:color blue)
    ("n" #'flymake-goto-next-error "Next error" :column "Flymake!")
    ("p" #'flymake-goto-prev-error "Prev error")
    ("d" #'flymake-show-diagnostics-buffer "Diagnostic buffer")))

;; We use compilation a lot and we don't want flymake to be prevented from running
(setq-default flymake-compilation-prevents-syntax-check nil)

;; -----------------------------------------------------------------------------
;; Buffer and buffer contents manipulation
;; -----------------------------------------------------------------------------
(custom-set-variables '(hi-lock-auto-select-face t))
(global-set-key (kbd "C-c d") #'myutils/duplicate-buffer)
(global-set-key (kbd "C-x C-g") #'push-mark-and-avy-goto-char)

(defun my/highligh-region (beg end)
  "Highlights text equal to the text between beg and end"
  (interactive "r")
  (-> (buffer-substring-no-properties beg end)
      (regexp-quote)
      (highlight-phrase (hi-lock-read-face-name))))  

(defun my/setup-hydra/buffer-hydra ()
  (defhydra my/buffer-hydra (:color blue)
    ("c" #'myutils/copy-buffer-contents "Copy buffer contents.")
    ("g" #'push-mark-and-avy-goto-char "Avy go to char (tree)")
    ("r" #'rename-buffer "Rename buffer")
    ("l" #'goto-line "Go to a specific line")
    ("o" #'my/occur-symbol-at-point "Occur with current symbol.")
    ("p" #'myutils/copy-file-path-to-clipboard "Copy file path.")
    ("P" #'myutils/copy-file-path-from-other-window-to-clipboard "Copy file path (other window)")
    ("t" #'toggle-truncate-lines "Toggle truncate lines")))

(defun my/setup-hydra/highlight-hydra ()
  (defhydra my/highlight-hydra (:color blue)
    ("h" #'highlight-symbol-at-point "Symbol at point." :column "Highlight!")
    ("r" #'my/highligh-region "Selected region.")
    ("p" #'highlight-phrase "Phrase.")))

(defun my/setup-hydra/hideshow-hdyra ()
  (defhydra my/hideshow-hydra (:color blue)
    ("h" #'hs-hide-block "Hide" :column "HideShow!")
    ("H" #'hs-hide-all "Hide All")
    ("s" #'hs-show-block "Show")
    ("S" #'hs-show-all "Show All")
    ("t" #'hs-toggle-hiding "Toggle")))

(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'go-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)

;; Adds some shortcuts to fuzzy cmd match
(mfcs-add-command :description "Buffer Rename Buffer" :command #'rename-buffer)
(mfcs-add-command :description "ANSI Colorize Buffer")

;; ------------------------------------------------------------
;; Window manager manipulation
;; ------------------------------------------------------------
(defvar my/window-manager/focus-window nil
  "Function that receives a class name and focus the window of that class")

;; ------------------------------------------------------------
;; i3
;; ------------------------------------------------------------
(defun my/i3/focus-window (class)
  (shell-command (format "i3-msg \"[class=%s] focus\"" class)))

(setq my/window-manager/focus-window #'my/i3/focus-window)

;; ------------------------------------------------------------
;; Terminal Multiplex Manipulation
;; ------------------------------------------------------------
(defvar my/shell/new-pane nil
  "Function to be used to creating a new pane on a terminal multiplex. Receives the CWD the shell should start on.")

(defvar my/shell/do-run nil
  "Function to be used to run a command on a shell and then exit after the command is done. Receives the command to run.")

(defvar my/shell/window-class "Alacritty"
  "The class of the i3 window that contains the shell/terminal")

(defun my/shell/focus-window ()
  (my/i3/focus-window my/shell/window-class))

(defun my/shell/run (cmd)
  "Runs a command on a shell and exists after."
  (interactive "sEnter command to run: ")
  (funcall my/shell/do-run cmd)
  (my/shell/focus-window))

(defun my/shell/open-on-current-dir (currdir)
  (interactive (list (expand-file-name default-directory)))
  (funcall my/shell/new-pane currdir)
  (my/shell/focus-window))

;; ------------------------------------------------------------
;; Zellij
;; ------------------------------------------------------------
(defun my/zellij/current-session ()
  "Return the current session to use for zellij. If no session is running, errors."
  (let ((session (s-chomp (shell-command-to-string "zellij ls -n | grep -v EXITED | head -n 1 | awk '{print $1}'"))))
    (when (or (not session) (string-equal session ""))
      (error "Can not find current session"))
    session))

(defun my/zellij/new-pane (&optional cwd)
  "Creates a new zellij pane with cwd. Implements my/shell/new-pane."
  (--> (format "zellij -s=%s action new-pane -c" (my/zellij/current-session))
       (if cwd (format "%s --cwd=%s" it cwd) it)
       (format "%s -- bash" it)
       (message it)
       (shell-command it)))

(defun my/zellij/do-run (cmd)
  "Runs a command on zellij, and exits after."
  (if-let ((session (my/zellij/current-session)))
      (shell-command (format "zellij -s=%s run -f -- %s" session cmd))
    (error "No zellij session")))

(when (equal 'zellij my/terminal-multiplex)
  (setq my/shell/new-pane #'my/zellij/new-pane)
  (setq my/shell/do-run #'my/zellij/do-run))

;; -----------------------------------------------------------------------------
;; Tmux integration
;; -----------------------------------------------------------------------------
(defvar my/tmux/interactive-window
  "emacs-tmux-interactive"
  "Tmux window to use for interactive commands.")

(defun my/tmux/current-session ()
  "Returns the current session to use for tmux. Returns nil if not found."
  (if-let ((session (-> "tmux ls -F'#{session_name}' | sort | head -n 1"
                          (shell-command-to-string)
                          (s-chomp))))
      (unless (or (s-blank? session) (s-contains? "no server running" session))
        session)))

(defun my/tmux/new-pane (&optional cwd)
  (if-let ((session (my/tmux/current-session)))
      (progn
        (shell-command (format "tmux neww -t%s:" session))
        (when (not (s-blank? cwd))
          (shell-command (format "tmux send-keys -t%s: 'cd %s' Enter" session currdir))))
    (error "No current session for tmux")))

(defun my/tmux/do-run (cmd)
  (if-let ((session (my/tmux/current-session)))
      (shell-command (format "tmux neww -t%s: -n%s '%s'" session my/tmux/interactive-window cmd))
    (error "No current session for tmux")))

(when (equal 'tmux my/terminal-multiplex)
  (setq my/shell/new-pane #'my/tmux/new-pane)
  (setq my/shell/do-run #'my/tmux/do-run))

;; -----------------------------------------------------------------------------
;; Github CLI integration
;; -----------------------------------------------------------------------------
(defvar my/gh/on-browser-open-request
  (lambda ()
    (interactive)
    (shell-command "i3-msg [urgent=latest] focus"))
  "Callback to be run when we send something to the browser")

(defun my/gh/open-repo-on-browser ()
  (interactive)
  (shell-command "gh repo view --web")
  (funcall my/gh/on-browser-open-request))

(defun my/gh/open-pr-on-browser ()
  (interactive)
  (shell-command "gh pr view --web")
  (funcall my/gh/on-browser-open-request))

(defun my/gh/new-pr ()
  "Opens tmxu with prompts for a new PR"
  (interactive)
  (my/shell/run "gh pr create"))

(defun my/gh/print-pr-body ()
  "Prints the current PR body on the current buffer."
  (interactive)
  (insert (shell-command-to-string "gh pr view --json=body --jq='.body'")))

(defun my/gh/edit-pr-body ()
  "Allows editing current PR body."
  (interactive)
  (let ((buff (generate-new-buffer "*gh-pr-body*"))
        (default-directory default-directory))
    (switch-to-buffer buff)
    (my/gh/print-pr-body)
    (markdown-mode)))

(defun my/gh/browse (file-path)
  "Browses to the current file."
  (interactive (list buffer-file-name))
  (let ((default-directory (or (file-name-directory file-path) default-directory))
        (file-name (file-name-nondirectory file-path )))
    (shell-command (format "gh browse %s" file-name))
    (funcall my/gh/on-browser-open-request)))

(defun my/gh/atlantis-plan ()
  (interactive)
  (let ((cmd-out (shell-command-to-string "gh pr comment --body='atlantis plan'")))
    (browse-url (string-trim cmd-out))))

(defun my/gh/browse-commit (commit)
  (interactive (list (thing-at-point 'symbol)))
  (--> (shell-command-to-string "gh repo view --json url -q \".url\"")
       (s-trim it)
       (format "%s/commit/%s" it commit)
       (browse-url it))
  (sit-for 1)
  (funcall my/gh/on-browser-open-request))

(defun my/setup-hydra/gh-hydra ()
  (defhydra my/gh-hydra (:color blue)
    ("b" #'my/gh/browse "Browses to file in github" :column "Github CLI!")
    ("r" #'my/gh/open-repo-on-browser "Open repo on browser")
    ("p" #'my/gh/open-pr-on-browser "Open PR on browser")
    ("P" #'my/gh/new-pr "Creates a new PR")
    ("c" #'my/gh/browse-commit "See the commit on github web")))

(defun my/gh/default-branch ()
  (-> (shell-command-to-string "gh repo view --json 'defaultBranchRef' --jq '.defaultBranchRef.name'")
      (string-trim)))

;; -----------------------------------------------------------------------------
;; Completion (Company)
;; -----------------------------------------------------------------------------
(use-package company
  :ensure t
  :config (progn
	    (add-hook 'after-init-hook 'global-company-mode)
            (setq company-dabbrev-downcase 0
	          company-idle-delay 0.2
	          company-dabbrev-code-modes t)))
(use-package company-web :ensure)

;; -----------------------------------------------------------------------------
;; Markdown mode
;; -----------------------------------------------------------------------------
(use-package markdown-mode :ensure)

;; -----------------------------------------------------------------------------
;; Org Mode
;; -----------------------------------------------------------------------------
(require 'org)

;; We don't like visual-line-mode
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))

;; We use "C-," for something else
(define-key org-mode-map (kbd "C-,") nil)

;; Only jump lines between headers if 3 empty lines
(custom-set-variables '(org-cycle-separator-lines 3 t))

;; Define custom apps to open files
(setq org-file-apps
      `((auto-mode . emacs)
        ("\\.mm\\'" . default)
        ("\\.x?html?\\'" . ,my/default-browser-cmd)
        ("\\.pdf\\'" . "evince %s")))

;; Org Babel configuration
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (python . t) (shell . t)))
(setq org-src-preserve-indentation t)
(setq org-babel-python-command "python")

;; Set the default header for source code blocks :result to verbatim, so org don't
;; try to create weird tables.
(setq org-babel-default-header-args
      (->> org-babel-default-header-args
           (-remove (-lambda ((k . _)) (equal k :results)))
           (cons '(:results . "replace verbatim"))))

;; Some extensions
;; https://github.com/vitorqb/orgext
(use-package orgext)

;; Journal configuration
(use-package org-journal
  :ensure
  :config
  (progn
    ;; Use nice datetime
    (custom-set-variables
     '(org-journal-date-format "%A, %Y%m%d"))

    ;; We don't liek visual-line-mode
    (add-hook 'org-journal-mode-hook (lambda () (visual-line-mode -1)))

    (defun my/journal-files-dir ()
      "Returns the path to the journal directory responsible for holding files"
      (if (string= (substring my/journal-files-dir-base 0 1) "/")
          my/journal-files-dir-base
        (myutils/concat-file org-journal-dir my/journal-files-dir-base)))

    (defun my/journal-find-file (arg)
      "Calls find-file inside the `journal-files-dir`"
      (interactive "P")
      (-let [default-directory (my/journal-files-dir)]
        (execute-extended-command arg "counsel-find-file")))

    (defun my/journal-open-daily-files-dir ()
      "Opens the directory for files for the current day"
      (interactive)
      (-let ((files-dir (my/journal-files-dir))
             (datestr (format-time-string "%Y%m%d")))
        (when (not (file-directory-p files-dir))
          (make-directory files-dir))
        (-let [default-directory files-dir]
          (when (not (file-directory-p datestr))
            (make-directory datestr))
          (find-file datestr))))

    (defun my/org-journal-find-last-file (arg)
      "Find-file on the last file for the journal.
   Sorts by string comparison, so depends on the journals being sortable
   this way (like 2018-01-01, 2018-01-02, ..."
      (interactive "P")
      (-> (org-journal--list-files)
          (sort #'string<)
          (last)
          (car)
          (->> (funcall (if arg #'find-file-other-window #'find-file)))))))

;; Org Hydra configuration
(defun my/setup-hydra/journal-hydra ()
  (defhydra my/journal-hydra (:color blue)
    ("j" #'org-journal-new-entry "New entry" :column "Org Journal")
    ("v" (lambda () (interactive) (let ((current-prefix-arg '(4)))
                                    (call-interactively #'org-journal-new-entry)))
     "Visit last entry")
    ("f" #'my/journal-open-daily-files-dir "Open 'files' directory")
    ("n" #'org-journal-next-entry "Open next entry")
    ("o" #'my/org-journal-find-last-file "Open most recent file")
    ("O" (lambda () (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively #'my/org-journal-find-last-file)))
     "Open most recent file new window")
    ("p" #'org-journal-previous-entry "Previous entry")
    ("s" #'org-journal-search "Search")))

(defun my/setup-hydra/org-hydra ()
  (defhydra my/org-hydra (:color blue)
    ("b" #'orgext-mark-block "Marks the entire block at point"
     :column "Org!")
    ("c" #'orgext-capture-with-task "Captures a new entry for a potential TODO")
    ("C" #'orgext-copy-block-from-above "Copies the above org block to point")
    ("l" #'org-store-link "Org store link")
    ("n" #'org-next-block "Jump to the next block" :color pink)
    ("p" #'org-previous-block "Jump to the previous block" :color pink)
    ("o" #'orgext-new-block-from-other-window "New block from other window")
    ("t" (lambda () (interactive) (find-file org-default-notes-file)) "Goto `todo` file.")
    ("v" #'orgext-element-at-point-on-new-buffer "Element at point on new buffer (read-only)")))

;; Nicer image handling
(setq org-image-actual-width nil)

;; -----------------------------------------------------------------------------
;; Git Magit
;; -----------------------------------------------------------------------------
;; Magit: Love is in the air S2
(use-package magit
  :ensure
  :bind ("C-c m" . magit-status)
  :config (progn
            (custom-set-variables '(magit-diff-refine-hunk 'all))
            (defun my/magit/fetch-and-goto (ref)
              (magit-run-git (cons "fetch" (cons "--all" "--prune")))
              (magit-run-git (cons "checkout" ref))
              (magit-run-git "pull"))
            (defun my/magit/fetch-and-goto-default ()
              (interactive)
              (-> (my/gh/default-branch) (my/magit/fetch-and-goto )))
            (defun my/magit/fetch-and-goto-main ()
              (interactive)
              (my/magit/fetch-and-goto "main"))
            (defun my/magit/fetch-and-goto-master ()
              (interactive)
              (my/magit/fetch-and-goto "master"))))

;; -----------------------------------------------------------------------------
;; Dired and files manipulation
;; -----------------------------------------------------------------------------
(setq dired-listing-switches "-alh")    ;Readable file sizes
(add-hook 'dired-load-hook '(lambda () (require 'dired-x))) ;Use dired-x
(add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode t)))
(setq dired-omit-mode t)

(defun my/find-file-home ()
  (interactive)
  (counsel-find-file "~"))

(defun find-file-my-temp-file (file-ext)
  (interactive "sEnter a file extension: .")
  (--> "%Y%m%d%H%M%S%3N"
       (format-time-string it)
       (myutils/concat-file my/user-temp-directory it)
       (if (not (equal file-ext "")) (concat it "." file-ext) it)
       (counsel-find-file it)))

(defun my/setup-hydra/files-hydra ()
  "Defines an hydra to file manipulation."

  (defhydra my/files-hydra (:color blue)
    ("w" #'write-file "Write file to..."
     :column "Files!")
    ("f" #'counsel-find-file "Find file")
    ("h" #'my/find-file-home "Find file at home")
    ("p" #'projectile-find-file "Projectile find file")
    ("P" #'projectile-find-file-other-window "Projectile find file other window")
    ("t" #'find-file-my-temp-file "New temporary file")
    ("e" (lambda () (interactive) (async-shell-command (buffer-file-name)))
     "Executes current buffer file as async shell command.")
    ("x" #'myutils/chmod-current-buffer "Chmod")
    ("z" (lambda () (interactive) (fzf/start default-directory))
     "Fuzzy find file on default-directory")
    ("Z" (lambda () (interactive) (fzf/start "~"))
     "Fuzzy find at home")))

;; -----------------------------------------------------------------------------
;; Lispy
;; -----------------------------------------------------------------------------
;; https://github.com/abo-abo/lispy
(use-package lispy :ensure
  :config (progn
            (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
            (define-key lispy-mode-map (kbd "C-,") nil)))

;; -----------------------------------------------------------------------------
;; Ediff
;; -----------------------------------------------------------------------------
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; -----------------------------------------------------------------------------
;; Web Development
;; -----------------------------------------------------------------------------
(use-package web-mode :ensure
  :init
  (progn
    (dolist (regxp (list "\\.html?\\'" "\\.css?\\'"))
      (add-to-list 'auto-mode-alist (cons regxp 'web-mode)))))

;; Removed for now since failing to instantiate
;; (use-package scss-mode :ensure
;;   :init
;;   (progn
;;     (add-hook 'scss-mode-hook 'flymake-mode-on)
;;     (setq scss-compile-at-save nil)
;;     (custom-set-variables '(css-indent-offset 2))
;;     (add-to-list 'auto-mode-alist (cons "\\.scss?\\'" 'scss-mode))))

;; -----------------------------------------------------------------------------
;; Ansi colors
;; -----------------------------------------------------------------------------
(require 'ansi-color)

(defun my/ansi-colorize-buffer ()
  " Colorize a buffer "
  (interactive)
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

;; Ansi on shell and comint
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'compilation-filter-hook
          (lambda ()
            (unless (equal major-mode #'ag-mode)
              (my/ansi-colorize-buffer))))

;; -----------------------------------------------------------------------------
;; Elisp and evaluation
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/eval-elisp-hydra ()
  "An hydra to evaluate elisp code quickly"

  (defhydra my/eval-elisp-hydra (:color blue)
    ("b" #'eval-buffer "Eval buffer" :column "Evaluate Elisp!")
    ("f" #'eval-defun "Eval defun")
    ("r" #'eval-region "Eval region")))

;; -----------------------------------------------------------------------------
;; Projectile
;; -----------------------------------------------------------------------------
(use-package projectile
  :ensure t
  :config
  (progn
    ;; Default setup from git repo
    (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (projectile-mode +1)

    ;; This one is on me
    (bind-key* "C-c C-f" 'projectile-find-file)
    (bind-key* "C-c C-d" 'projectile-find-dir)

    ;; And use ivy (S2) for completion
    (setq projectile-completion-system 'ivy)))

;; Adds a fn to insert relative files
(defun my/projectile/insert-relative-file ()
  (interactive)
  (let* ((project-root (projectile-ensure-project (projectile-project-root)))
         (file (projectile-completing-read "File: " (projectile-project-files project-root))))
    (insert (myutils/copy-relative-path (myutils/concat-file project-root file)))))

(defun my/setup-hydra/projectile-hydra ()
  "An hydra with projectile functionalities =D"

  (defhydra my/projectile-hydra (:color blue)
    ("d" #'projectile-find-dir "Find's a directory" :column "Projectile!")
    ("f" #'projectile-find-file "Find's a file")
    ("h" #'projectile-dired "Dired at to project root")
    ("H" #'projectile-dired-other-window "Dired at to project root (other window)")
    ("o" (lambda () (interactive)
           (-let [projectile-switch-project-action #'projectile-dired]
             (projectile-switch-project)))
     "Open project")
    ("k" #'projectile-kill-buffers "Kill buffers for project")
    ("r" #'my/projectile/insert-relative-file "Insert relative file")
    ("t" #'projectile-toggle-between-implementation-and-test
     "Toggle between implementation and test")))

;; -----------------------------------------------------------------------------
;; My Hydra!
;; -----------------------------------------------------------------------------
(defun my/hydras-setup ()
  " My custom setup for hydras "
  (my/setup-hydra/buffer-hydra)
  (my/setup-hydra/eval-elisp-hydra)
  (my/setup-hydra/files-hydra)
  (my/setup-hydra/flymake-hydra)
  (my/setup-hydra/gh-hydra)
  (my/setup-hydra/hideshow-hdyra)
  (my/setup-hydra/highlight-hydra)
  (my/setup-hydra/journal-hydra)
  (my/setup-hydra/org-hydra)
  (my/setup-hydra/projectile-hydra)
  (my/setup-hydra/typing-hydra)
  (when (functionp #'my/setup-hydra/eglot-hydra)
    (my/setup-hydra/eglot-hydra))
  
  (eval
   `(defhydra myhydra (:color blue)
      ,@(remove nil
                `(,(when (functionp #'my/ag-hydra/body)
                    '("a" #'my/ag-hydra/body "Ag Hydra" :column "Main"))
                 ,(when (functionp #'my/deadgrep-hydra/body)
                    '("a" #'my/deadgrep-hydra/body "Deadgrep Hydra" :column "Main"))
                 ("b" #'my/buffer-hydra/body "Buffer hydra" :column "Main")
                 ("c" #'mfcs-call "Calls fuzzy command selector")
                 ("d" #'my/dired-hydra/body "Dired hydra")
                 ("e" #'my/eval-elisp-hydra/body "Evaluate Elisp hydra")
                 ,(when (functionp #'my/eglot-hydra/body)
                    '("E" #'my/eglot-hydra/body "Eglot hydra"))
                 ("f" #'my/files-hydra/body "Files hydra!")
                 ("g" #'my/shell/open-on-current-dir "Open shell on current dir")
                 ("G" #'my/gh-hydra/body "Opens GithubCLI hydra")
                 ("h" #'my/hideshow-hydra/body "HideShow Hydra" :column "")
                 ("H" #'my/highlight-hydra/body "Highligh hydra!")
                 ("i" #'counsel-imenu "Imenu (find definitions)!")
                 ("j" #'my/journal-hydra/body "Hydra for org-journal")
                 ("l" (lambda () (interactive) (funcall my/language-hydra/body)) "Language specific hydra")
                 ("r" #'my/projectile-hydra/body "Projectile hydra ")
                 ("m" #'my/flymake-hydra/body "Flymake hydra")
                 ("o" #'my/org-hydra/body "Org hydra")
                 ("s" #'my/shell-hydra/body "A shell, sh, bash hydra!.")
                 ("k" #'compile-transient "Kompile dude")
                 ("t" #'my/typing-hydra/body "Typing hydra!"))))))

(use-package hydra :ensure
  :config (my/hydras-setup)
  :bind ("C-." . (lambda () (interactive) (myhydra/body))))

;; -----------------------------------------------------------------------------
;; Ivy - Counsell - Swipe
;; -----------------------------------------------------------------------------
;; Ivy configuration from https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/

;; Notice we installed ivy up because it is a dep for other things.
;; However, we configure it here.
(use-package counsel
  :ensure
  :after ivy
  :config (counsel-mode)
  :bind (("C-x C-f" . counsel-find-file))
  :config (progn
            ;; Adds extra action for find-file
            (ivy-add-actions
             'counsel-find-file
             '(("W" myutils/copy-relative-path "Copies relative path.")))))

(use-package swiper
  :ensure
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;; -----------------------------------------------------------------------------
;; Text movements/search/grep/selection/cleanup utils
;; -----------------------------------------------------------------------------
;; Let me use lowercase region and upcase region
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Set's the pgup and pgdown to move only 6 lines per hit
(global-set-key (kbd "<M-next>")  (myutils/li (scroll-other-window 6)))
(global-set-key (kbd "<M-prior>") (myutils/li (scroll-other-window -6)))
(global-set-key (kbd "<prior>")   (myutils/li (scroll-down 6)))
(global-set-key (kbd "C-v")       (myutils/li (scroll-up 6)))
(global-set-key (kbd "<next>")    (myutils/li (scroll-up 6)))
(global-set-key (kbd "M-v")       (myutils/li (scroll-down 6)))

;; We got too used with undo on C-M-q.
;; bind-key, shipped with use-package, does that for us
(bind-key* "C-M-q" #'undo)

(defun my/occur-symbol-at-point ()
  (interactive)
  (occur (symbol-name (symbol-at-point))))

;; Let's add next-error-follow-minnor-mode to mfcs
(mfcs-add-command
 :description "Next Error Follow Minnor Mode Follow Next Error"
 :command #'next-error-follow-minor-mode)

;; Allows you to jump to text on the screen!
;; Jumps to text
(use-package avy :ensure
  :config (progn
            (defun push-mark-and-avy-goto-char ()
              " Calls avy-goto-char, BUT push-mark before so we can go back "
              (interactive)
              (push-mark)
              (call-interactively #'avy-goto-char))
            (global-set-key (kbd "C-x C-g") #'push-mark-and-avy-goto-char)))

;; Expand-region is the best package ever. We love it.
(use-package expand-region
  :ensure
  :config (progn
            (global-set-key (kbd "C--") 'er/contract-region)
            (global-set-key (kbd "C-=") 'er/expand-region)
            ;; Adds org example blocks
            (add-hook 'org-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'er/try-expand-list)
                        (cl-pushnew #'orgext-mark-block er/try-expand-list)))))

;; Override xref-find-definition to xref-find-definition-other-window
(global-set-key (kbd "M-.") #'xref-find-definitions-other-window)

;; -----------------------------------------------------------------------------
;; Files management and backup
;; -----------------------------------------------------------------------------
;; Keep list of recent files, openable by C-x C-r
;; from https://www.emacswiki.org/emacs/RecentFiles
(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)
(setq
 backup-by-copying t
 backup-directory-alist `(("." . , "~/emacs-backups"))
 delete-old-versions t
 kept-new-versions 25
 kept-old-versions 15
 version-control t)

;; -----------------------------------------------------------------------------
;; Docker and related
;; -----------------------------------------------------------------------------
(use-package dockerfile-mode :ensure)

;; -----------------------------------------------------------------------------
;; Yaml
;; -----------------------------------------------------------------------------
(use-package yaml-mode :ensure)

;; -----------------------------------------------------------------------------
;; Flyspell
;; -----------------------------------------------------------------------------
;; We use C-., so lets make flyspell forget about it
(use-package flyspell
  :config (progn (define-key flyspell-mode-map (kbd "C-.") nil)))

;; -----------------------------------------------------------------------------
;; Browser
;; -----------------------------------------------------------------------------
;; Use the default browser for browsing, if we know it
(pcase my/default-browser-cmd

  ("firefox"
   (setq browse-url-browser-function 'browse-url-firefox
         browse-url-firefox-new-window-is-tab t))
  
  ((or "google-chrome" "google-chrome-stable")
   (setq browse-url-browser-function 'browse-url-chrome))

  (_ nil))

;; -----------------------------------------------------------------------------
;; Javascript/Js/Json
;; -----------------------------------------------------------------------------
;; This was just so annoying that I'll just put it here and pretend it's emacs' default
(setq js2-basic-offset 2)
(setq js-indent-level 2)

;; -----------------------------------------------------------------------------
;; Using emacsclient as EDITOR
;; -----------------------------------------------------------------------------
(add-hook 'server-visit-hook
          (lambda ()
            (local-set-key [(control c) (control c)]
                           (lambda ()
                             (interactive)
                             (save-buffer)
                             (server-edit)))))

;; -----------------------------------------------------------------------------
;; Language specific modules
;; -----------------------------------------------------------------------------
(my/defmodule R)
(my/defmodule ag)
(my/defmodule asdf)
(my/defmodule clojure)
(my/defmodule copilot)
(my/defmodule csharp)
(my/defmodule deadgrep)
(my/defmodule eglot)
(my/defmodule elixir)
(my/defmodule elm)
(my/defmodule fsharp)
(my/defmodule go)
(my/defmodule haskell)
(my/defmodule javascript)
(my/defmodule jenkinsfile)
(my/defmodule just)
(my/defmodule kotlin)
(my/defmodule latex-and-auctex)
(my/defmodule ohmycards)
(my/defmodule python)
(my/defmodule python-eglot)
(my/defmodule rust)
(my/defmodule scala)
(my/defmodule terraform)
(my/defmodule typescript)
(my/defmodule typescript-eglot)
(my/defmodule vue)

;; -----------------------------------------------------------------------------
;; Modules loaded by default
;; -----------------------------------------------------------------------------
(emacs-init-load-module-deadgrep)

;; -----------------------------------------------------------------------------
;; Computer specific hooks
;; -----------------------------------------------------------------------------
;; Tries to load computer-specific hooks
(when my-current-profile
  (-some--> (expand-file-name "~/.config/emacs_init/profile-hooks/")
    (and (file-directory-p it) it)
    (concat it (-> my-current-profile symbol-name (substring 1)) ".el")
    (and (file-exists-p it) it)
    (load it)))

;; -----------------------------------------------------------------------------
;; Emacs Server
;; -----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))
