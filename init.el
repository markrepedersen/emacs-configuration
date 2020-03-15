(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Packages setup.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-hydra)

(setq load-prefer-newer t)

(setq enable-recursive-minibuffers t)

(minibuffer-depth-indicate-mode)

(defun helper/kill-minibuffer ()
  "Exit the minibuffer if it is active."
  (when (and (>= (recursion-depth) 1)
           (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook #'mouse-leave-buffer-hook #'helper/kill-minibuffer)

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-splash-screen t) ;; no splash screen
(menu-bar-mode -1) ;; minimal chrome
(scroll-bar-mode -1) ;; disable scroll bars
(setq-default truncate-lines 1) ;; no wordwrap

(column-number-mode 1) ;; To know which column stack trace refers to.

(blink-cursor-mode 0)    ;; Reduce visual noise

(add-hook 'prog-mode-hook 'linum-mode) ;; Enable line numbers when programming.

;; Since I duplicate lines a lot.
(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))
(global-set-key (kbd "C-d") 'duplicate-line-or-region)

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Go back to previous mark.
(global-set-key (kbd "C-.") 'pop-to-mark-command)

;; Create shortcut of fullscreening Emacs.
(global-set-key (kbd "C-c f s") 'toggle-frame-fullscreen)

;; Intelligent line opening that also places cursor on new line.
(global-set-key (kbd "M-o") 'open-line) ;; Default way.

(global-set-key (kbd "C-c h b") 'describe-personal-keybindings)

(defun load-all-config-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

(load-all-config-in-directory "~/.emacs.d/lisp/")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(emms-info-mediainfo emms zenburn-theme zenburn xterm-color vterm use-package-hydra treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toml-mode theme-looper smartparens shell-pop rainbow-mode rainbow-delimiters qml-mode pretty-mode poet-theme org-pdfview notmuch multiple-cursors mu4e-alert modern-cpp-font-lock lsp-ui lsp-python-ms lsp-java indent-guide ibuffer-projectile highlight-thing highlight-numbers highlight-doxygen helm-xref helm-swoop helm-projectile helm-lsp helm-gtags helm-flx helm-c-yasnippet helm-ag gscholar-bibtex golden-ratio-scroll-screen golden-ratio go-tag go-playground go-impl go-gen-test go-fill-struct go-dlv flycheck-rust flycheck-pycheckers flycheck-inline flycheck-golangci-lint fish-mode fish-completion fic-mode expand-region exec-path-from-shell engine-mode easy-kill-extras drag-stuff doom-themes doom-modeline dashboard dap-mode company-lsp company-flx company-auctex cmake-font-lock ccls cargo calfw-org calfw-ical calfw bibclean-format beacon auto-dictionary auctex-latexmk anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
