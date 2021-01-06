(use-package flycheck-posframe
  :after flycheck
  :custom-face
  (flycheck-posframe-face ((t (:foreground ,(face-foreground 'success)))))
  (flycheck-posframe-info-face ((t (:foreground ,(face-foreground 'success)))))
  :hook (flycheck-mode . flycheck-posframe-mode)
  :init (setq flycheck-posframe-border-width 4
              flycheck-posframe-inhibit-functions
              '((lambda (&rest _) (bound-and-true-p company-backend)))))

(use-package flycheck-rust
  :hook (flycheck-rust-setup))

(use-package flycheck
  :after hydra
  :diminish
  :commands flycheck-redefine-standard-error-levels
  :hook (after-init . global-flycheck-mode)
  :bind (("C-c f" . flycheck-hydra/body))
  :pretty-hydra
  ((:color teal :title "Flycheck")
   ("Errors"
    (("l" list-flycheck-errors "list"))))
  :init
  (setq flycheck-global-modes '(not text-mode
				    outline-mode
				    fundamental-mode
				    lisp-interaction-mode
				    org-mode
				    diff-mode
				    shell-mode
				    eshell-mode
				    term-mode
				    vterm-mode)
	flycheck-display-errors-delay 0.1
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-indication-mode (if (display-graphic-p)
                                     'right-fringe
                                   'right-margin)
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (flycheck-redefine-standard-error-levels "⏴" 'flycheck-fringe-bitmap-arrow))
