(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :custom
  (ccls-executable (executable-find "ccls"))
  (ccls-sem-highlight-method 'font-lock)
  (ccls-enable-skipped-ranges nil))

(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))
