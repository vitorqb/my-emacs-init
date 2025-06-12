(use-package gptel
  :ensure
  :config (setq
           gptel-model   'sonar
           gptel-backend (gptel-make-perplexity "Perplexity" :key (lambda () (getenv "PERPLEXITY_API_KEY")))))
