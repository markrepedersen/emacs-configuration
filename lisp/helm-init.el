(use-package helm-xref
  :defer t
  :requires helm
  :preface
  (defadvice dired-do-find-regexp-and-replace
      (around netrom-no-helm-dired-do-find-regexp-and-replace activate)
    (let ((xref-show-xrefs-function 'xref--show-xref-buffer))
      ad-do-it))
  (defun netrom/xref-find-apropos-at-point (pattern)
    "Xref find apropos at point, if anything, and show prompt for PATTERN."
    (interactive
     (list
      (read-string "Xref find apropos of: " (thing-at-point 'symbol))))
    (xref-find-apropos pattern))
  :config
  (setq-default xref-show-xrefs-function 'helm-xref-show-xrefs)
  (setq helm-xref-candidate-formatting-function 'helm-xref-format-candidate-long))

(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini)
	 ("C-x C-b" . helm-buffers-list))
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

(use-package helm-ag
  :after hydra
  :preface
  (defun helm-do-ag-project-root-with-flag (flag)
    (let ((helm-ag-command-option (concat helm-ag-command-option " " flag)))
      (helm-do-ag-project-root)))
  :bind (("C-c h" . helm-ag-hydra/body))
  :pretty-hydra
  ("Search"
   (("d" helm-do-ag "current directory" :exit t)
    ("p" helm-do-ag-project-root "project root" :exit t)
    ("f" helm-do-ag-this-file "this file" :exit t)
    ("l" (lambda () (interactive) (helm-do-ag-project-root-with-flag "-l")) "for files" :exit t))
   "Navigation"
   (("m" helm-ag-clear-stack "clear stack" :exit t)
    ("." helm-ag-pop-stack "back" :exit t)))
  :config
  (setq helm-ag-fuzzy-match t))

(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop)
   ("M-g" . helm-swoop-back-to-last-point))
  :config
  (setq
   helm-swoop-split-direction 'split-window-vertically
   helm-swoop-use-line-number-face t
   helm-swoop-split-with-multiple-windows t))
