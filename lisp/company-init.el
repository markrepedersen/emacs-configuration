(use-package company
  :hook ((prog-mode LaTeX-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 1
	company-tooltip-align-annotations t
	company-begin-commands '(self-insert-command)
	company-require-match 'never
	company-idle-delay 0.1))

(use-package company-lsp
  :defer t
  :config
  (setq company-lsp-cache-candidates 'auto))
