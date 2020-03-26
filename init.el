(eval-and-compile
  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil
	inhibit-startup-screen t
	initial-scratch-message nil
	gc-cons-threshold 402653184
        gc-cons-percentage 0.6
	byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
	enable-recursive-minibuffers t
	load-path (append load-path (directory-files package-user-dir t "^[^.]" t))))

(eval-when-compile
  (require 'package)
  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))
  (unless (assoc-default "org" package-archives)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package-hydra)
  (setq use-package-always-ensure t)
  (add-to-list 'load-path "~/.emacs.d/lisp")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/"))

(defun init-settings ()
  "Initialize good-to-have settings for all modes."
  (desktop-save-mode 1) ;; Make initial screen whatever I was doing last.
  (setq user-full-name "Mark Pedersen"
	user-mail-address "markrepedersen@gmail.com")
  (minibuffer-depth-indicate-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq inhibit-splash-screen t) ;; no splash screen
  (menu-bar-mode -1) ;; minimal chrome
  (tool-bar-mode -1) ;; MINIMALISTIC BABY
  (scroll-bar-mode -1) ;; disable scroll bars
  (setq-default truncate-lines 1) ;; no wordwrap
  (set-face-attribute 'region nil :background "#0000ff" :foreground "#ffffff")
  (column-number-mode 1) ;; To know which column stack trace refers to.
  (blink-cursor-mode 0)    ;; Reduce visual noise
  (add-hook 'prog-mode-hook 'linum-mode)) ;; Enable line numbers when programming.

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
(init-settings)
