(use-package helm
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x b" . helm-mini))
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
  :bind (("C-c h" . helm-ag-hydra/body))
  :config
  (setq helm-ag-use-grep-ignore-list t)
  (add-to-list 'grep-find-ignored-directories "target")
  (add-to-list 'grep-find-ignored-directories "node_modules")
  (add-to-list 'grep-find-ignored-directories ".git")
  (add-to-list 'grep-find-ignored-directories "elpa")
  :hydra (helm-ag-hydra (:hint nil :color "beige")
			"Silver searcher"
			("d" helm-ag "Search current directory" :column "Search")
			("p" helm-ag-project-root "Search project root")
			("f" helm-ag-this-file "Search current buffer")
			("f" helm-ag-clear-stack "Clear AG markers" :column "Mark")
			("." helm-ag-pop-stack "Go back")))
			
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
