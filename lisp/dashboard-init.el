(use-package dashboard
    :ensure t
    :diminish dashboard-mode
    :config
    (setq dashboard-banner-logo-title "your custom text")
    (setq dashboard-startup-banner "/path/to/image")
    (setq dashboard-items '((recents  . 10)
                            (bookmarks . 10)))
    (dashboard-setup-startup-hook))
