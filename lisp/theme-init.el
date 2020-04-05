(use-package all-the-icons
  :if window-system
  :config
  (when (not (member "all-the-icons" (font-family-list)))
    (all-the-icons-install-fonts t)))
(use-package doom-modeline :init (doom-modeline-mode 1))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t))
(use-package theme-looper
  :bind (:map global-map ("C-c C-c t" . theme-looper-hydra/body))
  :pretty-hydra
  ("Update"
   (("n" theme-looper-enable-next-theme "next")
    ("p" theme-looper-enable-previous-theme "previous")))
  :config
  (theme-looper-set-favorite-themes '(doom-one
				      doom-acario-dark
				      doom-city-lights
				      doom-challenger-deep
				      doom-manegarm
				      doom-outrun-electric
				      doom-spacegrey
				      doom-tomorrow-day)))
