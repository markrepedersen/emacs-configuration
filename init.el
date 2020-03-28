(defun init-settings ()
  "Initialize good-to-have settings for all modes."
  (setq-default frame-title-format '("markrepedersen - %b"))
  (defvar file-name-handler-alist-original file-name-handler-alist)
  (setq gc-cons-threshold 100000000
	package-enable-at-startup nil
	file-name-handler-alist nil
	site-run-file nil
	scroll-preserve-screen-position 'always
	user-full-name "Mark Pedersen"
	user-mail-address "markrepedersen@gmail.com"
	inhibit-splash-screen t
	package-user-dir (expand-file-name "elpa" user-emacs-directory)
	package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
		           ("org" . "http://orgmode.org/elpa/")
			   ("melpa" . "https://melpa.org/packages/")))
  (define-constants)
  (desktop-save-mode 1)
  (minibuffer-depth-indicate-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1) 
  (scroll-bar-mode -1)
  (set-face-attribute 'region nil :background "dark magenta")
  (column-number-mode 1)
  (blink-cursor-mode 0)
  (display-time-mode 1)
  (display-battery-mode 1)
  (add-hook 'prog-mode-hook 'linum-mode))

(defun define-constants ()
  "Define some global constants"
  (defconst *sys/gui*
    (display-graphic-p)
    "Are we running on a GUI Emacs?")

  (defconst *sys/win32*
    (eq system-type 'windows-nt)
    "Are we running on a WinTel system?")

  (defconst *sys/linux*
    (eq system-type 'gnu/linux)
    "Are we running on a GNU/Linux system?")

  (defconst *sys/mac*
    (eq system-type 'darwin)
    "Are we running on a Mac system?")

  (defconst *sys/root*
    (string-equal "root" (getenv "USER"))
    "Are you a ROOT user?")

  (defconst *rg*
    (executable-find "rg")
    "Do we have ripgrep?")

  (defconst *find*
    (executable-find "find")
    "Do we have GNU find?")

  (defconst *python*
    (or (executable-find "python3")
	(and (executable-find "python")
             (> (length (shell-command-to-string "python --version | grep 'Python 3'")) 0)))
    "Do we have python3?")

  (defconst *pip*
    (or (executable-find "pip3")
	(and (executable-find "pip")
             (> (length (shell-command-to-string "pip --version | grep 'python 3'")) 0)))
    "Do we have pip3?")

  (defconst *tr*
    (executable-find "tr")
    "Do we have tr?")

  (defconst *mvn*
    (executable-find "mvn")
    "Do we have Maven?")

  (defconst *gcc*
    (executable-find "gcc")
    "Do we have gcc?")

  (defconst *git*
    (executable-find "git")
    "Do we have git?")

  (defvar better-gc-cons-threshold 67108864)

  (defvar my-load-file-dir (expand-file-name "lisp" user-emacs-directory)))

(defun update-to-load-path (folder)
  "Update FOLDER and its subdirectories to `load-path'."
  (let ((base folder))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name)))))))

(defun load-directory (dir)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(init-settings)
(update-to-load-path my-load-file-dir)

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package gnu-elpa-keyring-update)
(use-package use-package-hydra)

(eval-and-compile
  (setq use-package-verbose (not (bound-and-true-p byte-compile-current-file))
	use-package-always-ensure t
	use-package-expand-minimally t
	use-package-compute-statistics t
	use-package-enable-imenu-support t
	load-prefer-newer t
        package--init-file-ensured t
        package-enable-at-startup nil
	inhibit-startup-screen t
	initial-scratch-message nil
	byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
	enable-recursive-minibuffers t))

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold)
            (setq file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function :after after-focus-change-function
			      (lambda ()
                                (unless (frame-focus-state)
                                  (garbage-collect))))
	      (add-hook 'after-focus-change-function 'garbage-collect))
            (defun gc-minibuffer-setup-hook ()
	      (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

            (defun gc-minibuffer-exit-hook ()
	      (garbage-collect)
	      (setq gc-cons-threshold better-gc-cons-threshold))

            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)))

(load-directory my-load-file-dir)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 3)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)
 '(company-begin-commands '(self-insert-command))
 '(company-box-backends-colors nil t)
 '(company-box-doc-delay 0.3)
 '(company-box-max-candidates 50)
 '(company-box-show-single-candidate t)
 '(company-global-modes '(not shell-mode eaf-mode))
 '(company-idle-delay 0.1)
 '(company-lsp-cache-candidates 'auto)
 '(company-minimum-prefix-length 1)
 '(company-require-match 'never)
 '(company-show-numbers t)
 '(company-tabnine-max-num-results 9 t)
 '(company-tooltip-align-annotations t)
 '(dired-dwim-target t)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(global-auto-revert-non-file-buffers t)
 '(highlight-indent-guides-auto-character-face-perc 7)
 '(highlight-indent-guides-delay 0)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive 'top)
 '(load-prefer-newer t)
 '(package-selected-packages
   '(gnu-elpa-keyring-update helm-c-yasnippet undo-tree theme-looper xterm-color use-package-hydra typescript-mode smartparens shell-pop repl-toggle rainbow-mode rainbow-delimiters org-pdfview multiple-cursors magit lsp-ui lsp-python-ms lsp-java iedit ibuffer-projectile highlight-indent-guides helm-xref helm-swoop helm-projectile helm-lsp helm-ag groovy-mode graphql golden-ratio go-mode format-all flycheck-rust flycheck-pycheckers flycheck-inline fish-mode fish-completion fic-mode expand-region exec-path-from-shell ewal-doom-themes esup engine-mode emms elpy easy-kill-extras drag-stuff doom-modeline disk-usage dashboard dap-mode company-tabnine company-lsp company-box company-auctex cargo beacon auctex-latexmk anzu aio))
 '(rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (python-mode . elpy-shell-switch-to-shell)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts)) t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
