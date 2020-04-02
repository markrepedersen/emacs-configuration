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

    (defvar my-load-file-dir (expand-file-name "lisp" user-emacs-directory))))

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

(defun init-settings ()
  "Initialize good-to-have settings for all modes."
  (progn
    (add-hook 'after-init-hook
              `(lambda ()
		 (setq gc-cons-threshold 800000
                       gc-cons-percentage 0.1)
		 (garbage-collect)) t)
    (setq package-enable-at-startup nil
	  message-log-max 16384
	  gc-cons-threshold 402653184
	  gc-cons-percentage 0.6
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
    (display-battery-mode 1)
    (toggle-frame-maximized)
    (toggle-frame-fullscreen)
    (add-hook 'prog-mode-hook 'linum-mode))
  (unless (bound-and-true-p package--initialized)
    (setq package-enable-at-startup nil)
    (package-initialize))

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

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

  (update-to-load-path my-load-file-dir)
  (require 'use-package))

(init-settings)
(load-directory my-load-file-dir)
