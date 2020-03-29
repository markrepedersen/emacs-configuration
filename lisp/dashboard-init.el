(use-package dashboard
  :config
  (setq show-week-agenda-p t
	dashboard-set-init-info t
	dashboard-banner-logo-title "hi"
	dashboard-set-heading-icons t
	dashboard-set-file-icons t)
  (dashboard-setup-startup-hook))
