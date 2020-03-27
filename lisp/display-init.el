;; Highlights hexcolors, like #aabbcc and Red.
(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package smartparens-config
  :ensure smartparens
  :bind (("C-S-a" . sp-beginning-of-sexp)
	 ("C-S-e" . sp-end-of-sexp))
  :config (progn (show-smartparens-global-mode t)))
