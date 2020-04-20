(use-package auctex-latexmk
  :defer t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package reftex
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

(use-package company-auctex
  :defer t
  :config (company-auctex-init))

(use-package tex
  :ensure auctex
  :defer t
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
	    (setq TeX-source-correlate-mode t
		  TeX-source-correlate-method 'synctex
		  TeX-auto-save t
		  TeX-parse-self t
		  reftex-plug-into-AUCTeX t
		  TeX-view-program-selection '((output-pdf "PDF Tools") TeX-source-correlate-start-server t))
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
	    (add-hook 'LaTeX-mode-hook
		      (lambda ()
			(reftex-mode t)
			(flyspell-mode t)))))
