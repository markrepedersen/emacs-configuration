;; Fish shell
(use-package fish-mode
  :defer t
  :hook (fish-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 #'fish_indent-before-save))))

(use-package fish-completion
  :defer t
  :config
  (when (and (executable-find "fish")
           (require 'fish-completion nil t))
  (global-fish-completion-mode)))
