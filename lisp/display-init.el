(use-package smartparens
  :bind (("C-S-a" . sp-beginning-of-sexp)
	 ("C-S-e" . sp-end-of-sexp)
	 ("C-(" . sp-backward-sexp)
	 ("C-)" . sp-forward-sexp))
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode))
