(use-package shell-pop
  :ensure t
  :bind ("C-t" . shell-pop)
  :config
  (setq shell-pop-autocd-to-working-dir nil
        shell-pop-shell-type 'eshell
        shell-pop-window-position "right"
        shell-pop-window-size 50))
