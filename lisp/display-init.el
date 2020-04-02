(use-package smartparens
  :hook ((after-init . smartparens-global-mode)
	 (after-init . show-smartparens-global-mode))
  :bind (("C-S-a" . sp-beginning-of-sexp)
	 ("C-S-e" . sp-end-of-sexp)
	 ("C-(" . sp-backward-sexp)
	 ("C-)" . sp-forward-sexp))
  :config
  (require 'smartparens-config))
