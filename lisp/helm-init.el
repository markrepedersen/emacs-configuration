(use-package helm
  :config
  (setq helm-candidate-number-limit 100
	completion-styles '(flex)
        helm-display-source-at-screen-top t
        helm-exit-idle-delay 0
        helm-full-frame nil
        helm-apropos-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-ff-fuzzy-matching t
        helm-ff-file-name-history-use-recentf t
        helm-split-window-default-side (quote below)
        helm-reuse-last-window-split-state nil
        helm-split-window-in-side-p t ;; split in same window
	helm-autoresize-min-height 10
	helm-autoresize-max-height 40
        helm-quick-update t) ;; don't show invisible candidates
  (helm-autoresize-mode t))

(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop-without-pre-input)
   ("C-S-s" . helm-swoop))
  :config (setq
	   helm-swoop-split-direction 'split-window-horizontally
	   helm-swoop-split-with-multiple-windows t
	   helm-swoop-pre-input-function
	   (lambda ()
             (if mark-active
		 (buffer-substring-no-properties (mark) (point))
               ""))))
