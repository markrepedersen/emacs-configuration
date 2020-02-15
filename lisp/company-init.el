(use-package company
  :ensure t
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-length 1)
  :hook (after-init . global-company-mode))

(use-package company-flx
  :requires company
  :config
  (company-flx-mode +1))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))
