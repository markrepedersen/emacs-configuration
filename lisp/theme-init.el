(use-package all-the-icons
  :if window-system
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-outrun-electric t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package theme-looper
  :after hydra doom-themes
  :bind (:map global-map ("C-c C-c t" . hydra-theme-looper/body))
  :hydra (hydra-theme-looper (:color blue :hint nil)
			     "toggle themes"
			     ("n" theme-looper-enable-next-theme "next" :column "Switch")
			     ("p" theme-looper-enable-previous-theme "previous"))
  :config
  (theme-looper-set-favorite-themes '(doom-one
				      doom-acario-dark
				      doom-city-lights
				      doom-challenger-deep
				      doom-manegarm
				      doom-outrun-electric
				      doom-spacegrey
				      doom-tomorrow-day)))
