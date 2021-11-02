(defun define-constants ()
  "Define some global constants"
  (progn
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

    (defvar markrepedersen/website-dir "~/work/markrepedersen.github.io")

    (defvar my-load-file-dir (expand-file-name "lisp" user-emacs-directory))))

(defun delete-intermediate-dired-buffers ()
  (use-package dired
    :ensure nil
    :config
    (setq delete-by-moving-to-trash t)
    (eval-after-load "dired"
      #'(lambda ()
          (put 'dired-find-alternate-file 'disabled nil)
          (define-key dired-mode-map (kbd "RET") #'dired-find-alternate-file)))))

(defun dump-custom ()
  "Dump custom-set-variables to a garbage file and don’t load it"
  (use-package cus-edit
    :ensure nil
    :config
    (setq custom-file (concat user-emacs-directory "to-be-dumped.el"))))

(defun match-parens ()
  (use-package elec-pair
    :ensure nil
    :hook (prog-mode . electric-pair-mode)))

(defun change-frame-size-and-font ()
  "Changes the frame to be maximized on startup and changes the font"
  (use-package frame
    :preface
    (defun markrepedersen/set-default-font ()
      (interactive)
      (when (member "Consolas" (font-family-list))
	(set-face-attribute 'default nil :family "Consolas"))
      (set-face-attribute 'default nil
                          :height 120
                          :weight 'normal))
    :ensure nil
    :config
    (setq initial-frame-alist '((fullscreen . maximized)))
    (markrepedersen/set-default-font))
  )

(defun change-scrolling-speed ()
  "The default scrolling speed is quite fast; tone it down"
  (use-package mwheel
    :ensure nil
    :config (setq mouse-wheel-scroll-amount '(2 ((shift) . 1))
                  mouse-wheel-progressive-speed nil)))

(defun sync-buffer-external ()
  "In case of external (outside of Emacs) changes, auto refresh the buffer."
  (use-package autorevert
    :ensure nil
    :config
    (global-auto-revert-mode +1)
    (setq auto-revert-interval 2
          auto-revert-check-vc-info t
          global-auto-revert-non-file-buffers t
          auto-revert-verbose nil)))

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
  (let ((load-it (lambda (f) (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun init-startup-optimizations ()
  (setq gc-cons-threshold-original gc-cons-threshold
	gc-cons-threshold (* 1024 1024 100)
	file-name-handler-alist-original file-name-handler-alist
	file-name-handler-alist nil)
  (run-with-idle-timer
   5 nil
   (lambda ()
     (setq gc-cons-threshold gc-cons-threshold-original)
     (setq file-name-handler-alist file-name-handler-alist-original)
     (makunbound 'gc-cons-threshold-original)
     (makunbound 'file-name-handler-alist-original)
     (message "gc-cons-threshold and file-name-handler-alist restored"))))

(defun init-settings ()
  "Initialize good-to-have settings for all modes."
  (progn
    (setq package-enable-at-startup nil
	  message-log-max 16384
	  auto-window-vscroll nil
	  site-run-file nil
	  frame-title-format '("markrepedersen - %b")
	  user-full-name "Mark Pedersen"
	  user-mail-address "markrepedersen@gmail.com"
	  package-user-dir (expand-file-name "elpa" user-emacs-directory)
	  package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			     ("org" . "http://orgmode.org/elpa/")
			     ("melpa" . "https://melpa.org/packages/"))
	  custom-file "~/.emacs.d/custom.el")
    (load custom-file 'noerror)
    (define-constants)
    (minibuffer-depth-indicate-mode)
    (fset 'yes-or-no-p 'y-or-n-p)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (set-face-attribute 'region nil :background "dark magenta")
    (column-number-mode 1)
    (blink-cursor-mode 0)
    (display-time-mode 1)
    (display-battery-mode 1))

  (add-hook 'prog-mode-hook 'linum-mode)

  (unless (bound-and-true-p package--initialized)
    (setq package-enable-at-startup nil)
    (package-initialize))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

  (setq-default create-lockfiles nil)
  (setq-default compilation-always-kill t) ; kill compilation process before starting another
  (setq-default compilation-ask-about-save nil) ; save all buffers on `compile'
  (setq-default compilation-scroll-output t)

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
	  initial-major-mode 'text-mode
	  initial-scratch-message "Hello, Mark.\n"
	  byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local)
	  enable-recursive-minibuffers t))

  (update-to-load-path my-load-file-dir))

(init-settings)
(delete-intermediate-dired-buffers)
(dump-custom)
(match-parens)
(change-frame-size-and-font)
(change-scrolling-speed)
(sync-buffer-external)

(use-package hydra
  :demand
  :after all-the-icons
  :config
  (use-package major-mode-hydra
    :after hydra
    :demand
    :bind
    ("M-SPC" . major-mode-hydra))

  (require 'all-the-icons)

  (defun with-faicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-fileicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-octicon (icon str &optional height v-adjust)
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-material (icon str &optional height v-adjust)
    (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

  (defun with-mode-icon (mode str &optional height nospace face)
    (let* ((v-adjust (if (eq major-mode 'emacs-lisp-mode) 0.0 0.05))
           (args     `(:height ,(or height 1) :v-adjust ,v-adjust))
           (_         (when face
			(lax-plist-put args :face face)))
           (icon     (apply #'all-the-icons-icon-for-mode mode args))
           (icon     (if (symbolp icon)
			 (apply #'all-the-icons-octicon "file-text" args)
                       icon)))
      (s-concat icon (if nospace "" " ") str))))

(load-directory my-load-file-dir)
