(use-package smartparens
  :hook ((after-init . smartparens-global-mode)
	 (after-init . show-smartparens-global-mode))
  :bind (("C-S-a" . sp-beginning-of-sexp)
	 ("C-S-e" . sp-end-of-sexp)
	 ("C-(" . sp-backward-sexp)
	 ("C-)" . sp-forward-sexp))
  :config
  (require 'smartparens-config))

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

(pretty-hydra-define toggle-functions
  (:title (with-faicon "toggle-on" "Toggles" 1 -0.05))
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("t" fic-mode "todos" :toggle t)
    ("h" highlight-indent-guides-mode "indentation" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("C-n" theme-looper-enable-next-theme "next")
    ("C-p" theme-looper-enable-previous-theme "previous"))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(global-set-key (kbd "C-c t") 'toggle-functions/body)
