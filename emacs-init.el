;; -----------------------------------------------------------------------------
;; Customizable variables
;; -----------------------------------------------------------------------------
;; We define all variables that can be customized among different laptops here,
;; and then we load a file ~/config/emacs_init.el, hoping that this file will
;; customize any variable it needs to customize.
(defvar my-font-size 14 "The default font size used.")
(defvar my-current-profile :home
  (concat "A profile that can be used to customize your computer-specific settings."
          "for example: :work or :home"))
(defvar my-journal-dir "~/journal/" "Org Journal directory to use.")
(defvar my/custom-libraries-folder "~/.emacs.d/other/"
  "A folder in where custom libraries will be searched")
(defvar my/custom-libraries-names '()
  "Libraries to load from the `my/custom-libraries-folder`")
(defvar my/jira-base-url nil
  "The base url used to visit tickets in jira")
(defvar my/pr-base-url nil
  "The base url used to visit a PR. Currently works only for bitbucket.")
(defvar my/custom-welcome-script nil
  "If set, inhibit emacs default init and executes this script instead.")
(defvar my/user-temp-directory "~/mytmp"
  "A directory used to save temporary files.")

;; Loads the config
(load (expand-file-name "~/.config/emacs_init") t)


;; -----------------------------------------------------------------------------
;; Packages and load settings
;; -----------------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://melpa.org/packages/") t)
(package-initialize)
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(require 'bind-key)

;; 
;; Adds custom libraries to load list
;;
(defun add-to-load-path (x)
  "Adds a path fo load-path"
  (add-to-list 'load-path x))

(defun add-custom-lib-to-load-path (x)
  " Adds a folder in `my/custom-libraries-folder` to load-path "
  (add-to-load-path (concat (file-name-as-directory my/custom-libraries-folder) x)))

(add-to-load-path my/custom-libraries-folder)
(mapc #'add-custom-lib-to-load-path my/custom-libraries-names)

;; -----------------------------------------------------------------------------
;; Global requirements
;; -----------------------------------------------------------------------------
;; The rest of init file assume those packages are installed
(use-package dash :ensure)
(use-package dash-functional :ensure)
(use-package s :ensure)

;; mylisputils is mandatory requirement
(add-custom-lib-to-load-path "mylisputils")
(use-package mylisputils)

;; my-show-definitions as well
(add-custom-lib-to-load-path "my-show-definitions")
(use-package my-show-definitions)

;; We like recursion
(setq max-lisp-eval-depth (* 32000))

;; Ensure the tempdir is created
(and my/user-temp-directory
     (not (file-directory-p my/user-temp-directory))
     (progn (mkdir my/user-temp-directory t)
            (message "Created %s" my/user-temp-directory)))

;; ------------------------------------------------------------------------------
;; Global Appearence
;; -----------------------------------------------------------------------------
;; Don't show menu, scroll, toolbar
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Choose font
;; https://github.com/source-foundry/Hack
(set-frame-font (format "Hack %s" my-font-size) nil t)

;; Choose theme
(use-package distinguished-theme
  :ensure t
  :no-require t
  :config (load-theme 'distinguished t))

;; Highlight parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Display date and time, line and col numbers
(setq display-time-day-and-date t)
(display-time-mode 1)
(global-linum-mode 1)
(column-number-mode)

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; -----------------------------------------------------------------------------
;; ido-mode!
;; -----------------------------------------------------------------------------
(ido-mode 1)

;; -----------------------------------------------------------------------------
;; Compilation and processes
;; -----------------------------------------------------------------------------
(global-set-key (kbd "<f5>") 'recompile)

;; Don't use linum-mode in compilation buffers
(dolist (hook '(compilation-mode-hook comint-mode-hook))
  (add-hook hook (myutils/li (linum-mode -1))))

(defun my/setup-hydra/compile-hydra ()
  "Prepares an hydra for compilation mode."
  (defhydra my/compile-hydra (:color blue)
    "An hydra for compile!\n"
    ("k" #'compile "Simply compile!\n")
    ("r" #'recompile "REEEcompile\n")
    ("i" (lambda () (interactive) (execute-extended-command '(4) "compile"))
     "Compile interactively (comint)!\n")))

;; -----------------------------------------------------------------------------
;; Yas and Snippets
;; -----------------------------------------------------------------------------
(use-package yasnippet :ensure)

;; -----------------------------------------------------------------------------
;; Typing shortcuts functions
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-c f") 'myutils/fill-to-end)
(global-set-key (kbd "<f12>") 'myutils/insert-date)
(global-set-key (kbd "C-c u") 'myutils/remove-whitespace-and-newline)
(global-set-key (kbd "C-~") 'delete-trailing-whitespace)
(global-set-key (kbd "<f11>") (myutils/li (insert (projectile-project-root))))

(defun my/setup-hydra/typing-hydra ()
  "Prepares an hydra for typing shortcuts."
  (defhydra my/typing-hydra (:color blue)
    "An hydra for typing shortcuts!\n"
    ("f" #'myutils/fill-to-end "Fill to end with '-'.\n")
    ("d" #'myutils/insert-date "Insert the date.\n")
    ("c" #'myutils/remove-whitespace-and-newline
     "Clean - remove whitesapces and newlines.\n")
    ("w" #'delete-trailing-whitespace "Delete trailing whitespaces.\n")))

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
    "An hydra for flymake\n"
    ("n" #'flymake-goto-next-error "Next error\n")
    ("p" #'flymake-goto-prev-error "Prev error\n")
    ("d" #'flymake-show-diagnostics-buffer "Diagnostic buffer\n")))

;; -----------------------------------------------------------------------------
;; Buffer manipulation
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-c d") #'myutils/duplicate-buffer)

;; -----------------------------------------------------------------------------
;; Registers manipulation
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/register-hydra ()
  (defhydra my/register-hydra (:color blue)
    "An hydra for emacs registers\n"
    ("p" #'point-to-register "Save point in register\n")
    ("j" #'jump-to-register  "Jump to point register\n")
    ("r" #'copy-to-register  "Copy region to register\n")
    ("i" #'insert-register   "Inserts copied region form register\n")))

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
;; Language Server Protocol
;; -----------------------------------------------------------------------------
(use-package eglot :ensure
  :config (progn
            ;; If we are using eglot, bind M-. to xref, that eglot itself uses
            (add-hook
             'eglot--managed-mode-hook
             (lambda () (local-set-key (kbd "M-.") 'xref-find-definitions)))))

;; -----------------------------------------------------------------------------
;; Markdown mode
;; -----------------------------------------------------------------------------
(use-package markdown-mode :ensure)

;; -----------------------------------------------------------------------------
;; Org Mode
;; -----------------------------------------------------------------------------
;; Define custom apps to open files
(setq org-file-apps
      (quote
       ((auto-mode . emacs)
	("\\.mm\\'" . default)
	("\\.x?html?\\'" . "firefox %s")
	("\\.pdf\\'" . "evince %s"))))

;; Org Babel configuration
(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) (python . t) (ditaa . t) (plantuml . t)
   (shell . t) (dot . t) (latex . t) (gnuplot . t) (maxima . t)))
(setq org-src-preserve-indentation t)
(setq org-babel-python-command "python3.6")
(setq org-plantuml-jar-path (expand-file-name "/usr/local/bin/plantuml.jar"))

;; Journal configuration
(use-package org-journal
  :ensure
  :config (progn
            (custom-set-variables `(org-journal-dir ,my-journal-dir))
            (bind-key* "C-c C-j" #'org-journal-new-entry)))

(defun my-org-journal-find-last-file (arg)
  "Find-file on the last file for the journal.
   Sorts by string comparison, so depends on the journals being sortable
   this way (like 2018-01-01, 2018-01-02, ..."
  (interactive "P")
  (-> (org-journal-list-files)
      (sort #'string<)
      (last)
      (car)
      (->> (funcall (if arg #'find-file-other-window #'find-file)))))

;; Org Hydra configuration
(defun my/setup-hydra/journal-hydra ()
  (defhydra my/journal-hydra (:color blue)
    "An hydra for Org Journal\n"
    ("j" #'org-journal-new-entry "New entry\n")
    ("v" (lambda () (interactive) (let ((current-prefix-arg '(4)))
                                    (call-interactively #'org-journal-new-entry)))
     "Visit last entry\n")
    ("n" #'org-journal-open-next-entry "Open next entry\n")
    ("o" #'my-org-journal-find-last-file "Open most recent file\n")
    ("O" (lambda () (interactive)
           (let ((current-prefix-arg '(4)))
             (call-interactively #'my-org-journal-find-last-file)))
     "Open most recent file new window\n")
    ("p" #'org-journal-open-previous-entry "Previous entry\n")
    ("s" #'org-journal-search "Search\n")))

;; Adds jira as a link to org
(progn
  (defun org-jira-open (ticket)
    "Visit the jira ticket page on browser."
    (assert my/jira-base-url)
    (-> my/jira-base-url file-name-as-directory (concat ticket) browse-url))
  (org-add-link-type "jira" 'org-jira-open))

;; Adds PR as a link to org
(progn
  (defun org-pr-open (pr)
    "Visits the PR. pr must be a string that contains the repo name
and the pr number, separated by /. Like this: de-tv/69"
    (assert my/pr-base-url)
    (--> pr
         (s-split "/" it)
         (or (and (equal (length it) 2) it)
             (error "A single '/' is expected in a pr link"))
         (apply
          #'format
          "%s%s/pull-requests/%s"
          (file-name-as-directory my/pr-base-url)
          it)
         (and (print it) it)
         (browse-url it)))
  (org-add-link-type "pr" 'org-pr-open))

;; -----------------------------------------------------------------------------
;; Latex and AucTex
;; -----------------------------------------------------------------------------
;; Auctex is a pain to install using use-package, so we don't
(defun my-latex-add-symbols ()
  "Add some symbols to Latex. Use on TeX-add-style-hook"
  (TeX-add-symbols
   '("frametitle" 1)
   '("framesubtitle" 1)))

(add-hook 'TeX-mode-hook 'my-latex-add-symbols)
(add-hook 'LaTeX-mode-hook 'jas-minor-mode-on)

;; This allows auctex to recognize the \usepackage and \documentclass
(setq TeX-parse-self t)

;; -----------------------------------------------------------------------------
;; Git Magit
;; -----------------------------------------------------------------------------
;; Magit: Love is in the air S2
(use-package magit
  :ensure
  :bind ("C-c m" . magit-status))

;; -----------------------------------------------------------------------------
;; Dired and files manipulation
;; -----------------------------------------------------------------------------
(setq dired-listing-switches "-alh")    ;Readable file sizes
(add-hook 'dired-load-hook '(lambda () (require 'dired-x))) ;Use dired-x
(add-hook 'dired-mode-hook '(lambda () (dired-hide-details-mode t)))
(setq dired-omit-mode t)

(defun my/setup-hydra/files-hydra ()
  "Defines an hydra to file manipulation."

  (defun find-file-home ()
    (interactive)
    (-let [default-directory "~"]
      (call-interactively #'find-file)))

  (defun find-file-home-other-window ()
    (interactive)
    (-let [default-directory "~"]
      (call-interactively #'find-file-other-window)))

  (defun find-file-my-temp-file (file-ext)
    (interactive "sEnter a file extension: .")
    (--> "%Y%m%d%H%M%S%3N"
         (format-time-string it)
         (myutils/concat-file my/user-temp-directory it)
         (if (not (equal file-ext "")) (concat it "." file-ext) it)
         (find-file it)))

  (defhydra my/files-hydra (:color blue)
    "Manipulate files!"
    ("w" #'write-file "Write file to...\n")
    ("f" #'find-file "Find file\n")
    ("F" #'find-file-other-window "Find file other window\n")
    ("h" #'find-file-home "Find file at home\n")
    ("H" #'find-file-home-other-window "Find file at home other window\n")
    ("p" #'projectile-find-file "Projectile find file\n")
    ("P" #'projectile-find-file-other-window "Projectile find file other window\n")
    ("t" #'find-file-my-temp-file "New temporary file\n")
    ("e" (lambda () (interactive) (async-shell-command (buffer-file-name)))
     "Executes current buffer file as async shell command.\n")
    ("x" #'myutils/chmod-current-buffer "Chmod\n")))


;; -----------------------------------------------------------------------------
;; Haskell
;; -----------------------------------------------------------------------------
(use-package intero :ensure)
(use-package haskell-mode
  :ensure
  :after intero
  :hook ((intero-mode . (lambda () (setq flycheck-display-errors-delay 0.3)))))

(intero-global-mode 1)
;; Use most recent version
(setq intero-package-version "0.1.32")

;; -----------------------------------------------------------------------------
;; Clojure
;; -----------------------------------------------------------------------------
(use-package clojure-mode :ensure
  :config (progn
            ;; Use 1 indent for match macro
            (put-clojure-indent 'match 1)
            (add-hook 'clojure-mode-hook #'yas-minor-mode-on)))

(use-package clojure-mode-extra-font-locking
  :ensure
  :config )

(use-package cider
  :ensure
  :config (progn
            (define-key cider-mode-map (kbd "C-c C-o")
              #'myutils/clojure-occur-def)))

;; -----------------------------------------------------------------------------
;; Go
;; -----------------------------------------------------------------------------
(use-package go-mode :ensure)
(use-package company-go :ensure
  :after company
  :config (progn
            (push 'company-go company-backends)))

;; -----------------------------------------------------------------------------
;; TypeScript
;; -----------------------------------------------------------------------------
(defun my/setup-tide-mode ()
  "Setup for tide mode (Typescript). From https://github.com/ananthakumaran/tide."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (yas-minor-mode-on))

(defun my/setup-tsx ()
  "Setup to edit tsx files. From https://github.com/ananthakumaran/tide"
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (my/setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; We usually want 2 indent spaces, not 4
  (setq web-mode-code-indent-offset 2
        web-mode-markup-indent-offset 2))

(use-package tide :ensure t)
(use-package web-mode :ensure t) ;; Needed for tsx
(use-package typescript-mode :ensure
  :after (tide web-mode)
  :hook ((typescript-mode . my/setup-tide-mode)
         (before-save . tide-format-before-save))
  :config
  (progn
    (add-hook 'tide-mode-hook
              (lambda () (cl-pushnew 'company-tide company-backends)))
    (my/setup-tsx)))


;; -----------------------------------------------------------------------------
;; Python, Elpy, hylang
;; -----------------------------------------------------------------------------
(use-package elpy
  :ensure
  :after (:all yasnippet flycheck)
  :config
  (progn
    (elpy-enable)
    (setq elpy-syntax-check-command "flake8"
          elpy-rpc-backend "jedi"
          elpy-shell-display-buffer-after-send nil)

    ;; Uses pytest by default
    (elpy-set-test-runner #'elpy-test-pytest-runner)

    ;; Binds C-c k to elpy-shell-kill
    (-each (list elpy-mode-map inferior-python-mode-map)
      (-rpartial 'define-key (kbd "C-c k") 'elpy-shell-kill))

    ;; Use yas
    (-each (list 'inferior-python-mode-hook 'python-mode-hook)
      (-rpartial 'add-hook 'yas-minor-mode-on))

    ;; By default, use flycheck.
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package hy-mode :ensure
  :config (add-to-list 'auto-mode-alist '("\\.hy\\'" . hy-mode)))

;; This function calls (delete-trailing-whitespace) before saving if
;; main mode is python.
(add-hook 'before-save-hook (lambda () (when (eq major-mode 'python-mode)
					 (delete-trailing-whitespace))))

;; Binds isort to C-c i
(define-key python-mode-map (kbd "C-c i") #'myutils/call-isort-on-current-file)


;; An hydra for python :)
(defun my/install-elpy-pip-requirements ()
    (interactive)
    (async-shell-command (concat "pip install --upgrade pip jedi flake8"
                                 " autopep8 rope yapf black")))

(defun my/which-python ()
  (interactive)
  (async-shell-command "bash -x -c 'which python'"))

(defun my/setup-hydra/python-hydra ()
  (defhydra my/python-hydra (:color blue)
    "An hydra for python!\n"
    ("v" #'myutils/python-activate-venv "Activate venv (default to venv/.venv)\n")
    ("r" #'run-python "Run python\n")
    ("p" #'my/install-elpy-pip-requirements "Install elpy pip dependencies\n")
    ("w" #'my/which-python "Which python")))


;; -----------------------------------------------------------------------------
;; Scala
;; -----------------------------------------------------------------------------
(use-package ensime
  :ensure t
  :pin melpa-stable)

(use-package sbt-mode
  :pin melpa-stable)

(use-package scala-mode
  :pin melpa-stable)

;; -----------------------------------------------------------------------------
;; Elm
;; -----------------------------------------------------------------------------
;; Source: https://www.lambdacat.com/post-modern-emacs-setup-for-elm/
(use-package elm-mode :ensure
  :config (add-hook 'elm-mode-hook
		    (lambda ()
		      (add-to-list (make-local-variable 'company-backends)
				   '(company-elm company-dabbrev-code))
		      (elm-oracle-setup-completion))))

(use-package flycheck-elm :ensure
  :config (progn
	    (add-hook 'flycheck-mode-hook #'flycheck-elm-setup) ;Add elm flycheck
	    (add-hook 'elm-mode-hook #'flycheck-mode)))		;Calls flycheck-mode on elm

;; Also depends one external elm-oracle

;; -----------------------------------------------------------------------------
;; OpenWith
;; -----------------------------------------------------------------------------
(use-package openwith
  :config
  (progn
    (openwith-mode 1)
    (setq openwith-associations
	  '(("\\.pdf\\'" "evince" (file))
	    ("\\.xls\\'" "libreoffice5.3" (file))
	    ("\\.xlsx\\'" "libreoffice5.3" (file))
	    ("\\.html\\'" "firefox" (file))
	    ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file))
	    ("\\.mp3\\'" "xmms" (file))))))

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
;; Flymake
;; -----------------------------------------------------------------------------
;; We use compilation a lot and we don't want flymake to be prevented from running
(setq-default flymake-compilation-prevents-syntax-check nil)

;; -----------------------------------------------------------------------------
;; Web Development
;; -----------------------------------------------------------------------------
(use-package web-mode :ensure
  :init
  (progn
    (dolist (regxp (list "\\.html?\\'" "\\.css?\\'"))
      (add-to-list 'auto-mode-alist (cons regxp 'web-mode)))))

;;
;; js2-mode for javascript
;;
(defun my/add-jest-errors-to-compilation-regexp ()
  "Make compilation aware of how to find the path to jest errors"
  (interactive)
  (push 'npm-jest-errors compilation-error-regexp-alist)
  (push '(npm-jest-errors
          "^[ ]*at.*[ ](?\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2)
        compilation-error-regexp-alist-alist))

(use-package js2-mode
  :ensure
  :after (:all flycheck)
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (dolist (fun (list #'js2-imenu-extras-mode #'yas-minor-mode-on
                       #'flymake-mode-on))
      (add-hook 'js2-mode-hook fun))
    ;; Force C-c d to duplicate buffer, overriding existing bind
    (define-key js2-mode-map (kbd "C-c d") #'myutils/duplicate-buffer)
    ;; Same with C-c C-j
    (define-key js2-mode-map (kbd "C-c C-j") #'org-journal-new-entry)
    ;; Bound yas to shit + TAB to avoid conflict
    (define-key js2-mode-map (kbd "<backtab>") 'yas-expand)
    (setq js2-basic-offset 2)
    (my/add-jest-errors-to-compilation-regexp)
    ;; Also use the derived mode for jsx
    (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
    (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))))

;; 
;; For development with nodejs
;; 
(use-package indium
  :ensure
  :after js2-mode
  :init   (add-hook 'js2-mode-hook #'indium-interaction-mode)
  ;; Force C-c d to duplicate buffer, overriding existing bind
  :config
  (progn
    (define-key indium-interaction-mode-map (kbd "C-c d") #'myutils/duplicate-buffer)))

;; 
;; For jinaj2 template engine
;; 
(use-package jinja2-mode :ensure)

;; -----------------------------------------------------------------------------
;; Ansi colors
;; -----------------------------------------------------------------------------
(require 'ansi-color)

(defun my/ansi-colorize-buffer ()
  " Colorize a buffer "
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
;; Loads custom welcome script
;; ----------------------------------------------------------------------------
(when my/custom-welcome-script
  (setq inhibit-startup-screen t)		;Don't show me the ugly emacs start
  (add-hook 'after-init-hook
	    (lambda () (load my/custom-welcome-script))))

;; -----------------------------------------------------------------------------
;; Elisp and evaluation
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/eval-elisp-hydra ()
  "An hydra to evaluate elisp code quickly"
  (defhydra my/eval-elisp-hydra (:color blue)
    "Evaluate elisp code!"
    ("b" #'eval-buffer "Eval buffer\n")
    ("f" #'eval-defun "Eval defun\n")
    ("r" #'eval-region "Eval region\n")))

;; -----------------------------------------------------------------------------
;; Shell, sh, bash. Scripting.
;; -----------------------------------------------------------------------------
(defun my/setup-hydra/shell-hydra ()
  (defhydra my/shell-hydra (:color blue)
    "Shell, sh, bash etc!\n"
    ("s" #'shell "A new shell for you\n")
    ("c" #'myutils/call-shell-command
     "Calls a shell command, shows the result, and asks you if kill the buffer\n")))

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
    (define-key projectile-mode-map (kbd "C-c C-f") 'projectile-find-file)

    ;; And use ivy (S2) for completion
    (setq projectile-completion-system 'ivy)))

(defun my/setup-hydra/projectile-hydra ()
  "Prepares an hydra for projectile"
  (defhydra my/projectile-hydra (:color blue)
    "An hydra with projectile functionalities =D\n"
    ("r" #'projectile-dired "Dired at to project root\n")
    ("R" #'projectile-dired-other-window "Dired at to project root (other window)\n")
    ("t" #'projectile-toggle-between-implementation-and-test
     "Toggle between implementation and test\n")))

;; -----------------------------------------------------------------------------
;; fzf
;; -----------------------------------------------------------------------------
;; Demands fzf to be installed (pacman -S fzf)
(use-package fzf
  :ensure
  :bind (("C-c C-v" . #'fzf-projectile)))

;; -----------------------------------------------------------------------------
;; My Hydra!
;; -----------------------------------------------------------------------------
(defun my/hydras-setup ()
  " My custom setup for hydras "
  (my/setup-hydra/compile-hydra)
  (my/setup-hydra/typing-hydra)
  (my/setup-hydra/eval-elisp-hydra)
  (my/setup-hydra/files-hydra)
  (my/setup-hydra/python-hydra)
  (my/setup-hydra/shell-hydra)
  (my/setup-hydra/projectile-hydra)
  (my/setup-hydra/flymake-hydra)
  (my/setup-hydra/journal-hydra)
  (my/setup-hydra/register-hydra)

  (defhydra my/ag-hydra (:color blue)
    "An hydra for ag!\n"
    ("a" #'ag "Simply ag\n")
    ("r" #'ag-regexp "Ag with regexp\n"))

  (defhydra myhydra (:color blue)
    ("0" #'my/register-hydra/body "Register Hydra\n")
    ("a" #'my/ag-hydra/body "Ag Hydra\n")
    ("d" #'my-show-definitions "Show definitions\n")
    ("e" #'my/eval-elisp-hydra/body "Evaluate Elisp hydra\n")
    ("f" #'my/files-hydra/body "Files hydra!\n")
    ("i" #'counsel-imenu "Imenu (find definitions)!\n")
    ("h" #'highlight-symbol-at-point "Highlights symbol at point.\n")
    ("j" #'my/journal-hydra/body "Hydra for org-journal\n")
    ("r" #'my/projectile-hydra/body "Projectile hydra \n")
    ("m" #'my/flymake-hydra/body "Flymake hydra\n")
    ("o" #'my/occur-symbol-at-point "Occur with current symbol.\n")
    ("p" #'myutils/copy-file-path-to-clipboard "Copy file path.\n")
    ("s" #'my/shell-hydra/body "A shell, sh, bash hydra!.\n")
    ("g" #'push-mark-and-avy-goto-char "Avy go to char (tree)\n")
    ("l" #'goto-line "Go to a specific line\n")
    ("k" #'my/compile-hydra/body "Kompile dude\n")
    ("t" #'my/typing-hydra/body "Ttping hydra!\n")
    ("y" #'my/python-hydra/body "pYthon Hydra!\n")))

(use-package hydra :ensure
  :config (my/hydras-setup)
  :bind ("C-." . (lambda () (interactive) (myhydra/body))))

;; -----------------------------------------------------------------------------
;; Ivy - Counsell - Swipe
;; -----------------------------------------------------------------------------
;; Ivy configuration from https://www.reddit.com/r/emacs/comments/910pga/tip_how_to_use_ivy_and_its_utilities_in_your/
(use-package counsel
  :ensure
  :after ivy
  :config (counsel-mode)
  :bind (("C-x C-f" . counsel-find-file)))

(use-package ivy
  :ensure
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-rich
  :ensure
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package ivy-hydra
  :ensure
  :after ivy)

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
(global-set-key (kbd "<next>")    (myutils/li (scroll-up 6)))

;; We got too used with undo on C-M-q.
;; bind-key, shipped with use-package, does that for us
(bind-key* "C-M-q" #'undo)

(defun my/occur-symbol-at-point ()
  (interactive)
  (occur (symbol-name (symbol-at-point))))

;; Very cool search package!
(use-package ag
  :ensure
  :config
  (progn
    (setq ag-highlight-search t)
    ;; Automatically goes to error when selected
    (add-hook 'ag-search-finished-hook 'next-error-follow-minor-mode)
    (global-set-key (kbd "C-c a") 'ag)))

;; Allows you to jump to text on the screen!
;; Jumps to text
(use-package avy :ensure)

(defun push-mark-and-avy-goto-char ()
  " Calls avy-goto-char, BUT push-mark before so we can go back "
  (interactive)
  (push-mark)
  (call-interactively #'avy-goto-char))

;; Expand-region is the best package ever. We love it.
(defun my/mark-org-example-block ()
  "In org-mode, marks an example block at point (if any). 
   Usefull with expand-region."
  (-if-let (el (org-element-at-point))
      (when (-any? (-partial #'equal (car el)) '(example-block src-block))
        (-let* ((block-begin (plist-get (car (cdr el)) :begin))
                (block-end (plist-get (car (cdr el)) :end))
                ;; we want 1 line after begin and 2 before end
                (begin (save-excursion
                         (goto-char block-begin)
                         (next-line)
                         (beginning-of-line)
                         (point)))
                (end (save-excursion
                       (goto-char block-end)
                       (-dotimes 3 #'previous-line)
                       (end-of-line)
                       (+ 1 (point)))))
          (goto-char begin)
          (set-mark end)))))

(use-package expand-region
  :ensure
  :config (progn
            (global-set-key (kbd "C--") 'er/contract-region)
            (global-set-key (kbd "C-=") 'er/expand-region)
            ;; Only jump lines between headers if 3 empty lines
            (custom-set-variables '(org-cycle-separator-lines 3 t))
            ;; Adds org example blocks
            (add-hook 'org-mode-hook
                      (lambda ()
                        (make-variable-buffer-local 'er/try-expand-list)
                        (cl-pushnew #'my/mark-org-example-block er/try-expand-list)))))

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
 backup-directory-alist `(("." . , "/home/vitor/emacs-backups"))
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
;; Kotlin
;; -----------------------------------------------------------------------------
(use-package kotlin-mode :ensure)

;; -----------------------------------------------------------------------------
;; Flyspell
;; -----------------------------------------------------------------------------
;; We use C-., so lets make flyspell forget about it
(use-package flyspell
  :config (progn (define-key flyspell-mode-map (kbd "C-.") nil)))

;; -----------------------------------------------------------------------------
;; Browser (firefox)
;; -----------------------------------------------------------------------------
;; Use firefox for browsing!
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

;; -----------------------------------------------------------------------------
;; Computer specific hooks
;; -----------------------------------------------------------------------------
;; Tries to load computer-specific hooks
(defun my/run-profile-hook ()
    (-some--> (or load-file-name buffer-file-name)
              (file-name-directory it)
              (concat it "profile-hooks/")
              (concat it (-> my-current-profile symbol-name (substring 1)) ".el")
              (and (file-exists-p it) it)
              (load it)))
(my/run-profile-hook)
