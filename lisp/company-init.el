(use-package company
  :hook ((prog-mode LaTeX-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 1
	company-tooltip-align-annotations t
	company-require-match 'never
	company-idle-delay 0))
