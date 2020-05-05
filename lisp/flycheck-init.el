(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-display-errors-delay 0.1)
  (setq flycheck-checker-error-threshold 2000)
  (progn
    (add-hook 'c++-mode-hook
              (lambda ()
                (progn
                  (setq flycheck-clang-language-standard "c++17"
                        flycheck-clang-standard-library "libc++"
                        flycheck-gcc-language-standard "c++17"
                        flycheck-cppcheck-standards '("c++17")
                        flycheck-cppcheck-inconclusive t
                        flycheck-cppcheck-checks '("all")
                        flycheck-cppcheck-suppressions '("noExplicitConstructor")))))
    (setq-default flycheck-check-syntax-automatically '(save mode-enabled)
		  flycheck-disabled-checkers '(emacs-lisp-checkdoc c/c++-clang c/c++-gcc))))

(if (display-graphic-p)
    (use-package flycheck-posframe
      :hook (flycheck-mode . flycheck-posframe-mode)
      :after flycheck
      :config
      (flycheck-posframe-configure-pretty-defaults)
      (setq flycheck-posframe-position 'window-top-left-corner)))
