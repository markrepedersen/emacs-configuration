(use-package ibuffer
  :defer t
  :config
  (progn
    (global-set-key (kbd "C-x C-b") 'ibuffer)
    (setq ibuffer-expert t)
    (setq ibuffer-show-empty-filter-groups nil)
    (add-hook 'ibuffer-mode-hook
	      '(lambda ()
		 (ibuffer-auto-mode 1)
		 (ibuffer-switch-to-saved-filter-groups "home")))
    (setq ibuffer-default-sorting-mode 'major-mode)
    (setq ibuffer-show-empty-filter-groups nil)
    (use-package ibuffer-projectile
      :config
      (progn
        (defun pedersen/ibuffer-customization ()
          "My customization for `ibuffer'."
          (ibuffer-projectile-set-filter-groups)
          (unless (eq ibuffer-sorting-mode 'alphabetic)
            (ibuffer-do-sort-by-alphabetic)
            (ibuffer-do-sort-by-major-mode)))))
    (add-hook 'ibuffer-hook #'pedersen/ibuffer-customization)))
