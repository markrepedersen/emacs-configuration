(use-package fish-completion
  :config
  (when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode)))
