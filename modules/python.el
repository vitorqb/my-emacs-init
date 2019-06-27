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
  (myutils/call-shell-command "bash -x -c 'which python'"))

(defun my/setup-hydra/python-hydra ()
  (defhydra my/python-hydra (:color blue)
    "An hydra for python!\n"
    ("v" #'myutils/python-activate-venv "Activate venv (default to venv/.venv)\n")
    ("r" #'run-python "Run python\n")
    ("p" #'my/install-elpy-pip-requirements "Install elpy pip dependencies\n")
    ("w" #'my/which-python "Which python")))

;; Adds commans do mfcs
(mfcs-add-command
 :description "Python Deactivate Venv [pyvenv-deactivate] [Deactivate Python]"
 :command #'pyvenv-deactivate)

;; For jinaj2 template engine
(use-package jinja2-mode :ensure)
