(use-package smartparens
  :hook ((after-init . smartparens-global-mode)
	 (after-init . show-smartparens-global-mode))
  :bind (("C-S-a" . sp-beginning-of-sexp)
	 ("C-S-e" . sp-end-of-sexp)
	 ("C-(" . sp-backward-sexp)
	 ("C-)" . sp-forward-sexp))
  :config
  (require 'smartparens-config))

(defvar toggles-title (with-faicon "toggle-on" "Toggles" 1 -0.05))

(pretty-hydra-define toggle-functions
  (:color amaranth :quit-key "q" :title toggles-title)
  ("Basic"
   (("n" linum-mode "line number" :toggle t)
    ("w" whitespace-mode "whitespace" :toggle t)
    ("W" whitespace-cleanup-mode "whitespace cleanup" :toggle t)
    ("r" rainbow-mode "rainbow" :toggle t)
    ("L" page-break-lines-mode "page break lines" :toggle t))
   "Highlight"
   (("s" symbol-overlay-mode "symbol" :toggle t)
    ("l" hl-line-mode "line" :toggle t)
    ("x" highlight-sexp-mode "sexp" :toggle t)
    ("t" hl-todo-mode "todo" :toggle t))
   "UI"
   (("d" jp-themes-toggle-light-dark "dark theme" :toggle jp-current-theme-dark-p))
   "Coding"
   (("p" smartparens-mode "smartparens" :toggle t)
    ("P" smartparens-strict-mode "smartparens strict" :toggle t)
    ("S" show-smartparens-mode "show smartparens" :toggle t)
    ("f" flycheck-mode "flycheck" :toggle t))
   "Emacs"
   (("D" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
    ("X" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit)))))

(global-set-key (kbd "C-c t") 'toggle-functions/body)
