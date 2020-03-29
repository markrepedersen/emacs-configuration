(use-package cmake-mode
  :defer t)

(use-package cmake-font-lock
  :defer t
  :config
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

(use-package modern-cpp-font-lock
  :defer t
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package ccls
  :defer t
  :init
  (setq ccls-executable (executable-find "ccls"))
  :config
  (setq ccls-sem-highlight-method 'font-lock))
