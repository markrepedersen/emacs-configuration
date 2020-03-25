(use-package helm
  :defer t
  :config
  ;; To get the best fuzzy completion style via helm: (via helm docs)
  ;; > For a better experience, if you don't know what to use, set
  ;; > completion-styles to '(flex) if you are using emacs-27 or to
  ;; > '(helm-flex) if you are using emacs-26 and keep 'emacs as default
  ;; > value for helm-completion-style.
  ;; Default: (setq completion-styles '(basic partial-completion emacs22))
  (cond
   ;; 27+
   ((version<= "27.0" emacs-version)
    (setq completion-styles '(flex)))

   ;; 26.x
   ((and (version<= "26.0" emacs-version)
         (version< emacs-version "27.0"))
    (setq completion-styles '(helm-flex))))

  (setq helm-candidate-number-limit 100
        helm-display-source-at-screen-top t
        helm-exit-idle-delay 0
        helm-full-frame nil

        ;; Fuzzy matching.
        helm-apropos-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-ff-fuzzy-matching t

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

(use-package helm-swoop
  :bind
  (("C-s" . helm-swoop-without-pre-input)
   ("C-S-s" . helm-swoop)))

(use-package helm-flx
  :defer t
  :requires (helm flx)
  :config
  ;; Use flx for better search results.
  (helm-flx-mode +1))

(use-package helm-ag
  :defer t
  :requires helm
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --smart-case"
        helm-ag-ignore-patterns '("*.min.js" "*.min.css")))
