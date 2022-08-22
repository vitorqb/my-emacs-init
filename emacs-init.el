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
(defvar my/custom-welcome-script nil
  "If set, inhibit emacs default init and executes this script instead.")
(defvar my/user-temp-directory "~/mytmp"
  "A directory used to save temporary files.")
(defvar my/default-browser-cmd "firefox %s"
  "The default command to open a browser. The '%s' will be substituted by the url to be openned.")
(defvar my/tfs-work-item-url-prefix nil
  "The url to visit a tfs work item. The id will be appended at the end.")
(defvar my/tfs-pr-url-prefix nil
  "The url to visit a tfs PR. The id will be appended at the end.")
(defvar my/tfs-commit-hash-prefix nil
  "The url to visit a tfs commit. The id will be appended at the end.")
(defvar my/path-to-modules-dir (concat (file-name-directory load-file-name) "modules")
  "The directory where the emacs-init `modules` can be found.")
(defcustom my/theme 'dark
  "Which theme to use (either dark or light)"
  :type '(symbol))

;; Loads the config. This is your oportunity to customize any of the variables.
(let ((config-file-name (expand-file-name "~/.config/emacs_init/config.el")))
  (if (file-exists-p config-file-name)
      (progn (message (concat "Loading config from " config-file-name))
             (load config-file-name))
    (message "WARNING: Could not locate emacs-init config file!")))

;; Loads the current profile from ~/.emacs_init_profile
(let ((current-profile-file (expand-file-name "~/.emacs_init_profile")))
  (when (file-exists-p current-profile-file)
    (message (concat "Reading profile from " current-profile-file))
    (with-temp-buffer
      (insert-file-contents current-profile-file)
      (let* ((selected-profile-str (replace-regexp-in-string "\n" "" (buffer-string)))
             (selected-profile-sym (make-symbol selected-profile-str)))
        (message (concat "Selected profile: " selected-profile-str))
        (setq my-current-profile  selected-profile-sym)))))

;; -----------------------------------------------------------------------------
;; Packages and load settings
;; -----------------------------------------------------------------------------
;; Don't customize anything
(setq custom-file "/dev/null")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(require 'bind-key)

(defun get-dep-library-load-path (x)
  "Returns the load-path for a dependency library."
  (concat my/emacs-init-deps-path "/" x))

;; -----------------------------------------------------------------------------
;; Global requirements
;; -----------------------------------------------------------------------------
;; The rest of init file assume those packages are installed
(use-package dash :ensure)
(use-package dash-functional :ensure)
(use-package s :ensure)
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
(use-package mylisputils
  ;; https://github.com/vitorqb/mylisputils/
  :load-path (lambda () (get-dep-library-load-path "mylisputils")))

;; Ensure the tempdir is created
(and my/user-temp-directory
     (not (file-directory-p my/user-temp-directory))
     (progn (mkdir my/user-temp-directory t)
            (message "Created %s" my/user-temp-directory)))

;; We like recursion
(setq max-lisp-eval-depth (* 100 max-lisp-eval-depth))

;; Allow searching the bash history
(defun counsel-yank-bash-history ()
  "Yank the bash history"
  (interactive)
  (shell-command "history -r") ; reload history
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
(defun my/load-dark-theme ()
  (use-package solarized-theme
    :ensure t
    :no-require t))

(defun my/load-light-theme ()
  (use-package leuven-theme
    :ensure t
    :no-require t
    :config (progn
              (load-theme 'leuven t))))

(cond
 ((equal my/theme 'dark)  (my/load-dark-theme))
 ((equal my/theme 'light) (my/load-light-theme))
 (:else
  (error "Invalid value for my/theme!")))

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Display date and time, line and col numbers
(setq display-time-day-and-date t)
(display-time-mode 1)
(if (version< emacs-version "26.1")
    (global-linum-mode 1)
  (global-display-line-numbers-mode))

(defun my/disable-linum ()
  "Disables linum or line-numbers, depending on emacs version"
  (if (version< emacs-version "26.1")
      (linum-mode -1)
    (display-line-numbers-mode -1)))

(column-number-mode)

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Delete active region
(delete-selection-mode +1)

;; Always use y-or-n rather than yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------
;; Which Key
;; -----------------------------------------------------------------------------
(use-package which-key
  :ensure
  :hook (after-init . which-key-mode))

;; -----------------------------------------------------------------------------
;; Show Definitions
;; -----------------------------------------------------------------------------
(use-package my-show-definitions
  ;; https://github.com/vitorqb/my-show-definitions
  :load-path (lambda () (get-dep-library-load-path "my-show-definitions"))
  :config (progn
            ;; Also deletes my-show-definition buffers when cleaning buffers.
            (add-to-list 'myutils/clean-buffers-names-regexs "\\*MyShowDefinitions\\*")))

;; -----------------------------------------------------------------------------
;; Fuzzy command selector
;; -----------------------------------------------------------------------------
(use-package my-fuzzy-cmd-selector
  ;; https://github.com/vitorqb/my-fuzzy-cmd-selector
  :load-path (lambda () (get-dep-library-load-path "my-fuzzy-cmd-selector"))
  :config (progn
            (cl-pushnew '(:mfcs-call . 20) ivy-height-alist)))

;; ------------------------------------------------------------
;; Runc - Runc Command
;; ------------------------------------------------------------
(use-package runc
  ;; http://github.com/vitorqb/runc
  :load-path (lambda () (get-dep-library-load-path "runc")))

;; -----------------------------------------------------------------------------
;; Compilation and processes
;; -----------------------------------------------------------------------------
(global-set-key (kbd "<f5>") 'recompile)

;; Don't use linum-mode in compilation buffers
(dolist (hook '(compilation-mode-hook comint-mode-hook))
  (add-hook hook #'my/disable-linum))

(use-package compile-transient
  ;; https://github.com/vitorqb/compile-transient
  :load-path (lambda () (get-dep-library-load-path "compile-transient")))

;; Adds a (by default ridicularly high) limit for the max number of rows in a
;; compilation buffer
(add-hook 'compilation-filter-hook 'comint-truncate-buffer)
(setq comint-buffer-maximum-size 100000)

;; -----------------------------------------------------------------------------
;; Shell
;; -----------------------------------------------------------------------------
(add-hook 'shell-mode-hook (lambda () (setq-local comint-prompt-read-only t)))

;; -----------------------------------------------------------------------------
;; Yas and Snippets
;; -----------------------------------------------------------------------------
(use-package yasnippet :ensure
  :config (progn
            (add-to-list 'yas-snippet-dirs
                         (s-concat (getenv "VITOR_MYGIT_DIR") "/snippets/snippets"))))

;; -----------------------------------------------------------------------------
;; Typing shortcuts functions
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-c f") 'myutils/fill-to-end)
(global-set-key (kbd "<f12>") 'myutils/insert-formated-date)
(global-set-key (kbd "C-c u") 'myutils/remove-whitespace-and-newline)
(global-set-key (kbd "C-~") 'delete-trailing-whitespace)
(global-set-key (kbd "<f11>") (myutils/li (insert (projectile-project-root))))
(global-set-key (kbd "<f10>") (myutils/li (kill-buffer (current-buffer))))
(global-set-key (kbd "<S-f10>") #'menu-bar-open)
(global-set-key (kbd "C-c C-/") 'other-window)

;; Copied from emacs wiki
(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(defun my/setup-hydra/typing-hydra ()
  "Prepares an hydra for typing shortcuts."
  (defhydra my/typing-hydra (:color blue)
    ("c" #'myutils/remove-whitespace-and-newline
     "Clean - remove whitesapces and newlines."
     :column "Typing")
    ("d" #'myutils/insert-formated-date "Insert the date.")
    ("e" #'myutils/remove-with-elipsis "Remove with elipsis." :color pink)
    ("f" #'myutils/fill-to-end "Fill to end with '-'.")
    ("l" #'copy-line "Copies current line down.")
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
(global-set-key (kbd "C-\\") (myutils/li (switch-to-buffer (other-buffer))))

(defun my/highligh-region (beg end)
  "Highlights text equal to the text between beg and end"
  (interactive "r")
  (-> (buffer-substring-no-properties beg end)
      (regexp-quote)
      (highlight-phrase (hi-lock-read-face-name))))  

(defun my/setup-hydra/buffer-hydra ()
  (defhydra my/buffer-hydra (:color blue)
    ("a" #'my/ansi-colorize-buffer "Colorize buffer with ansi colors" :column "Buffers!")
    ("c" #'myutils/copy-buffer-contents "Copy buffer contents.")
    ("g" #'push-mark-and-avy-goto-char "Avy go to char (tree)")
    ("r" #'rename-buffer "Rename buffer")
    ("l" #'goto-line "Go to a specific line")
    ("o" #'my/occur-symbol-at-point "Occur with current symbol.")
    ("p" #'myutils/copy-file-path-to-clipboard "Copy file path.")
    ("P" #'myutils/copy-file-path-from-other-window-to-clipboard "Copy file path (other window)")))

(defun my/setup-hydra/highlight-hydra ()
  (defhydra my/highlight-hydra (:color blue)
    ("h" #'highlight-symbol-at-point "Symbol at point." :column "Highlight!")
    ("r" #'my/highligh-region "Selected region.")
    ("p" #'highlight-phrase "Phrase.")))

;; Adds some shortcuts to fuzzy cmd match
(mfcs-add-command :description "Buffer Rename Buffer" :command #'rename-buffer)
(mfcs-add-command
 :description "Highlight Phrase Word Phrase Highlight"
 :command #'highlight-phrase)

;; -----------------------------------------------------------------------------
;; Registers manipulation
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/register-hydra ()
  (defhydra my/register-hydra (:color blue)
    ("p" #'point-to-register "Save point in register" :column "Registers!")
    ("P" (lambda () (interactive) (point-to-register 1)) "Save point in register 1")
    ("j" #'jump-to-register  "Jump to point register")
    ("J" (lambda () (interactive) (jump-to-register 1))  "Jump to point register 1")
    ("r" #'copy-to-register  "Copy region to register")
    ("i" #'insert-register   "Inserts copied region form register")))

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
 '((emacs-lisp . t) (python . t) (ditaa . t) (plantuml . t)
   (shell . t) (dot . t) (latex . t) (gnuplot . t) (maxima . t)))
(setq org-src-preserve-indentation t)
(setq org-babel-python-command "python")
(setq org-plantuml-jar-path (expand-file-name "/usr/local/bin/plantuml.jar"))

;; Set the default header for source code blocks :result to verbatim, so org don't
;; try to create weird tables.
(setq org-babel-default-header-args
      (->> org-babel-default-header-args
           (-remove (-lambda ((k . _)) (equal k :results)))
           (cons '(:results . "replace verbatim"))))

;; Some extensions
(use-package orgext
  ;; https://github.com/vitorqb/orgext
  :load-path (lambda () (get-dep-library-load-path "orgext")))

;; Journal configuration
(use-package org-journal
  :ensure
  :config
  (progn
    ;; Use nice datetime
    (custom-set-variables
     '(org-journal-date-format "%A, %Y%m%d"))
    
    (bind-key* "C-c C-j" #'org-journal-new-entry)

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

    (mfcs-add-command
     :description "Org Find File Journal Find File (Docs Files)"
     :command (myutils/li (call-interactively #'my/journal-find-file)))

    (setq org-todo-keywords
          '((sequence "TODO(t)" "|" "DONE(d)" "WONTFIX(n)")
            (sequence "BLOCKED()" "WAITING(w)")))))

(defun my-org-journal-find-last-file (arg)
  "Find-file on the last file for the journal.
   Sorts by string comparison, so depends on the journals being sortable
   this way (like 2018-01-01, 2018-01-02, ..."
  (interactive "P")
  (-> (org-journal--list-files)
      (sort #'string<)
      (last)
      (car)
      (->> (funcall (if arg #'find-file-other-window #'find-file)))))

(defun my/org-journal-search-regexp ()
  "Search org-journal using regex."
  (interactive)
  (-let [org-journal-search-forward-fn #'search-forward-regexp]
    (call-interactively #'org-journal-search)))

;; Org Hydra configuration
(defun my/setup-hydra/journal-hydra ()
  (defhydra my/journal-hydra (:color blue)
    ("j" #'org-journal-new-entry "New entry" :column "Org Journal")
    ("v" (lambda () (interactive) (let ((current-prefix-arg '(4)))
                                    (call-interactively #'org-journal-new-entry)))
     "Visit last entry")
    ("n" #'org-journal-open-next-entry "Open next entry")
    ("o" #'my-org-journal-find-last-file "Open most recent file")
    ("O" (lambda () (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively #'my-org-journal-find-last-file)))
     "Open most recent file new window")
    ("p" #'org-journal-open-previous-entry "Previous entry")
    ("s" #'org-journal-search "Search")
    ("r" #'my/org-journal-search-regexp "Regexp Search")))

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

;; Adds org store link and toggle link to mfcs
(mfcs-add-command
 :description "Org Store Link [Store Org]"
 :command #'org-store-link)
(mfcs-add-command
 :description "Org Toggle Link Display [Display Links Toggle Org]"
 :command #'org-toggle-link-display)

;; I3wm helpers
(defun my-org-capture-from-emacsclient ()
  "Calls `org-capture`, makes sure it's fullscreen, waits for the user to input something and,
   when the user closes the buffer, closes the frame.
   Thought to be used by a keyboard shortcut that opens an `emacsclient` for the suer to capture
   a TODO."
  (interactive)
  (orgext-capture-with-task)
  (delete-other-windows)
  (add-hook 'kill-buffer-hook #'delete-frame 0 t))

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
           (custom-set-variables
            '(magit-diff-refine-hunk 'all))))

;; Let us use magit links in org mode
(use-package orgit :ensure t)

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

(defun my/setup-hydra/dired-hydra ()
  "Defines an hydra for dired-related commands"

  (defhydra my/dired-hydra (:color blue)
    ("f" #'find-dired "Find dired" :column "Dired!")))

;; -----------------------------------------------------------------------------
;; (Light)Lispy
;; -----------------------------------------------------------------------------
;; https://github.com/vitorqb/lightlispy
(use-package lightlispy
  :load-path (lambda () (get-dep-library-load-path "lightlispy"))
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'lightlispy-mode)
            (defalias 'lispy-mode #'lightlispy-mode)))

;; -----------------------------------------------------------------------------
;; Ediff
;; -----------------------------------------------------------------------------
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; -----------------------------------------------------------------------------
;; tldr
;; -----------------------------------------------------------------------------
(use-package tldr :ensure)

;; -----------------------------------------------------------------------------
;; Templating
;; -----------------------------------------------------------------------------
;; The mustache.el is a implementation of mustache for elisp.
;; https://github.com/Wilfred/mustache.el
;; https://github.com/mustache/mustache
;; https://github.com/mustache/mustache/blob/master/examples/simple.mustache
(use-package mustache :ensure)

;; -----------------------------------------------------------------------------
;; Web Development
;; -----------------------------------------------------------------------------
(use-package web-mode :ensure
  :init
  (progn
    (dolist (regxp (list "\\.html?\\'" "\\.css?\\'"))
      (add-to-list 'auto-mode-alist (cons regxp 'web-mode)))))

(use-package scss-mode :ensure
  :init
  (progn
    (add-hook 'scss-mode-hook 'flymake-mode-on)
    (setq scss-compile-at-save nil)
    (custom-set-variables '(css-indent-offset 2))
    (add-to-list 'auto-mode-alist (cons "\\.scss?\\'" 'scss-mode))))

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
;; Shell, sh, bash. Scripting.
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/shell-hydra ()
  "Shell, sh, bash etc!"
  
  (defhydra my/shell-hydra (:color blue)
    ("s" #'shell "A new shell for you" :column "Shell!")
    ("c" #'myutils/call-shell-command
     "Calls a shell command, shows the result, and asks you if kill the buffer")))

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
    ("k" #'projectile-kill-buffers "Kill buffers for project")
    ("r" #'my/projectile/insert-relative-file "Insert relative file")
    ("t" #'projectile-toggle-between-implementation-and-test
     "Toggle between implementation and test")))

;; Adds create test file to mfcs
(mfcs-add-command
 :description "Projectile Create Test File For [File Test Create Projectile]"
 :command (lambda () (interactive) (-> (current-buffer)
                                       (buffer-file-name)
                                       (projectile-create-test-file-for)
                                       (find-file))))

;; -----------------------------------------------------------------------------
;; My Hydra!
;; -----------------------------------------------------------------------------
(defun my/hydras-setup ()
  " My custom setup for hydras "
  (my/setup-hydra/typing-hydra)
  (my/setup-hydra/eval-elisp-hydra)
  (my/setup-hydra/files-hydra)
  (my/setup-hydra/shell-hydra)
  (my/setup-hydra/projectile-hydra)
  (my/setup-hydra/flymake-hydra)
  (my/setup-hydra/journal-hydra)
  (my/setup-hydra/register-hydra)
  (my/setup-hydra/org-hydra)
  (my/setup-hydra/buffer-hydra)
  (my/setup-hydra/highlight-hydra)
  (my/setup-hydra/dired-hydra)

  ;; An hydra for ag!
  (defhydra my/ag-hydra (:color blue)
    ("a" #'ag "Simply ag" :column "Ag!")
    ("r" #'ag-regexp "Ag with regexp"))

  (defhydra myhydra (:color blue)
    ("0" #'my/register-hydra/body "Register Hydra" :column "Main Hydra")
    ("a" #'my/ag-hydra/body "Ag Hydra")
    ("b" #'my/buffer-hydra/body "Buffer hydra")
    ("c" #'mfcs-call "Calls fuzzy command selector")
    ("d" #'my/dired-hydra/body "Dired hydra")
    ("D" #'my-show-definitions "Show definitions")
    ("e" #'my/eval-elisp-hydra/body "Evaluate Elisp hydra")
    ("f" #'my/files-hydra/body "Files hydra!")
    ("h" #'my/highlight-hydra/body "Highligh hydra!")
    ("i" #'counsel-imenu "Imenu (find definitions)!")
    ("j" #'my/journal-hydra/body "Hydra for org-journal")
    ("l" (lambda () (interactive) (funcall my/language-hydra/body)) "Language specific hydra")
    ("r" #'my/projectile-hydra/body "Projectile hydra ")
    ("m" #'my/flymake-hydra/body "Flymake hydra")
    ("o" #'my/org-hydra/body "Org hydra")
    ("s" #'my/shell-hydra/body "A shell, sh, bash hydra!.")
    ("k" #'compile-transient "Kompile dude")
    ("t" #'my/typing-hydra/body "Typing hydra!")))

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
;; Connection/networks/internet utils
;; -----------------------------------------------------------------------------
;; Usefull to test REST APIs during development
(use-package restclient
  :ensure
  :config (setq-default restclient-inhibit-cookies t))

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

;; Very cool search package!
(use-package ag
  :load-path (lambda () (get-dep-library-load-path "ag"))
  :ensure
  :config
  (progn
    (setq ag-highlight-search t)
    ;; Automatically goes to error when selected
    (add-hook 'ag-search-finished-hook 'next-error-follow-minor-mode)
    (global-set-key (kbd "C-c a") 'ag)))

;; Let's add next-error-follow-minnor-mode to mfcs
(mfcs-add-command
 :description "Next Error Follow Minnor Mode Follow Next Error"
 :command #'next-error-follow-minor-mode)

;; Allows you to jump to text on the screen!
;; Jumps to text
(use-package avy :ensure)

(defun push-mark-and-avy-goto-char ()
  " Calls avy-goto-char, BUT push-mark before so we can go back "
  (interactive)
  (push-mark)
  (call-interactively #'avy-goto-char))

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

;; Edit ag and grep results
(use-package wgrep
  :ensure)
(use-package wgrep-ag
  :ensure
  :after wgrep)

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
(setq browse-url-new-window-flag t)
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
;; Language specific modules
;; -----------------------------------------------------------------------------
(my/defmodule clojure)
(my/defmodule elixir)
(my/defmodule go)
(my/defmodule haskell)
(my/defmodule latex-and-auctex)
(my/defmodule typescript)
(my/defmodule python)
(my/defmodule python-eglot)
(my/defmodule scala)
(my/defmodule elm)
(my/defmodule javascript)
(my/defmodule kotlin)
(my/defmodule fsharp)
(my/defmodule csharp)
(my/defmodule vue)
(my/defmodule eglot)
(my/defmodule R)
(my/defmodule rust)
(my/defmodule ohmycards)
(my/defmodule jenkinsfile)
(my/defmodule terraform)

;; -----------------------------------------------------------------------------
;; Loads custom welcome script
;; ----------------------------------------------------------------------------
(when (and my/custom-welcome-script (file-exists-p my/custom-welcome-script))
  (setq inhibit-startup-screen t)		;Don't show me the ugly emacs start
  (add-hook 'after-init-hook
	    (lambda () (load my/custom-welcome-script))))

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
(unless (server-running-p)
  (server-start))
