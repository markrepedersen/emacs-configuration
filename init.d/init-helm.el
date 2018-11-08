(require 'req-package)

(req-package helm
  :require recentf
  :config
  (setq helm-candidate-number-limit 100
        helm-display-source-at-screen-top t
        helm-exit-idle-delay 0
        helm-full-frame nil
        helm-buffers-fuzzy-matching t
        helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-apropos-fuzzy-match t
        helm-ff-file-name-history-use-recentf t
        helm-split-window-default-side (quote below)
        helm-reuse-last-window-split-state nil
        helm-split-window-in-side-p t ;; split in same window
        helm-quick-update t) ;; don't show invisible candidates

  ;; Resize helm according to number of results within min/max height.
  (setq helm-autoresize-min-height 10
        helm-autoresize-max-height 40)
  (helm-autoresize-mode t)

  ;; Aliases for viewing packages.
  (defalias 'lp 'helm-list-elisp-packages)
  (defalias 'lpn 'helm-list-elisp-packages-no-fetch)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)

  ;; Enhance the help menu using helm functionality.
  (define-key 'help-command (kbd "a") 'helm-apropos)
  (define-key 'help-command (kbd "r") 'helm-info-emacs)
  (define-key 'help-command (kbd "C-l") 'helm-locate-library)
  (define-key 'help-command (kbd "SPC") 'helm-all-mark-rings))

(req-package helm-swoop
  :require helm
  :config
  (setq helm-swoop-split-direction (quote split-window-vertically)
        helm-swoop-split-with-multiple-windows t
        helm-swoop-candidate-number-limit 1000)

  ;; Use isearch bindings to move up and down.
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

  ;; Activate helm-swoop on isearch results.
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key isearch-mode-map (kbd "M-I") 'helm-multi-swoop-all-from-isearch))

(req-package helm-gtags
  :require helm
  :config
  (setq helm-gtags-maximum-candidates 1000)

  ;; Enable helm-gtags-mode
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  ;; Set key bindings
  (eval-after-load "helm-gtags"
    '(progn
       (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
       (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
       (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol))))

(req-package helm-flx
  :require (helm flx)
  :config
  ;; Use flx for better search results.
  (helm-flx-mode +1))

(req-package helm-ag
  :require helm
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case"))


(provide 'init-helm)
