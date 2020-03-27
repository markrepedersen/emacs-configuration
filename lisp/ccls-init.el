;; (use-package cmake-mode)

;; (use-package cmake-font-lock
;;   :config
;;   (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
;;   (add-hook 'cmake-mode-hook 'cmake-font-lock-activate))

;; (use-package modern-cpp-font-lock
;;   :hook (c++-mode . modern-c++-font-lock-mode))

;; (use-package ccls
;;   :init
;;   (setq ccls-executable (executable-find "ccls"))
;;   :config
;;   (setq ccls-sem-highlight-method 'font-lock))
