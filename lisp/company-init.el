(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.1
        company-echo-delay 0.1
        company-dabbrev-downcase nil
        company-dabbrev-code-everywhere t
        company-dabbrev-code-modes t
        company-dabbrev-code-ignore-case t
        company-tooltip-align-annotations t
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance)
	company-backends '((
			    company-capf
			    company-files
			    company-keywords
			    company-dabbrev-code
			    company-dabbrev))
	))
