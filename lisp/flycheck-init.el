(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-display-errors-delay 0.1))

(if (display-graphic-p)
    (use-package flycheck-posframe
      :hook (flycheck-mode . flycheck-posframe-mode)
      :after flycheck
      :config
      (flycheck-posframe-configure-pretty-defaults)
      (setq flycheck-posframe-position 'window-top-left-corner)))
