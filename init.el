(eval-and-compile
  (unless (file-directory-p package-user-dir)
    (make-directory package-user-dir t))
  (setq load-prefer-newer t
        package-user-dir "~/.emacs.d/elpa"
        package--init-file-ensured t
        package-enable-at-startup nil
	inhibit-startup-screen t
	initial-scratch-message nil
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

(defvar better-gc-cons-threshold 67108864 ; 64mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing, decrease this.  If you experience stuttering, increase this.")
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

  (defconst *clangd*
    (or (executable-find "clangd")  ;; usually
	(executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
    "Do we have clangd?")

  (defconst *gcc*
    (executable-find "gcc")
    "Do we have gcc?")

  (defconst *git*
    (executable-find "git")
    "Do we have git?")

  (defconst *pdflatex*
    (executable-find "pdflatex")
    "Do we have pdflatex?")

  (defconst *eaf-env*
    (and *sys/linux* *sys/gui* *python* *pip*
	 (not (equal (shell-command-to-string "pip freeze | grep '^PyQt\\|PyQtWebEngine'") "")))
    "Check basic requirements for EAF to run."))

(defun init-settings ()
  "Initialize good-to-have settings for all modes."
  (define-constants)
  (setq-default frame-title-format '("M-EMACS - " user-login-name "@" system-name " - %b"))
  (setq scroll-preserve-screen-position 'always
	user-full-name "Mark Pedersen"
	user-mail-address "markrepedersen@gmail.com"
	inhibit-splash-screen t)
  (desktop-save-mode 1) ;; Make initial screen whatever I was doing last.
  (minibuffer-depth-indicate-mode)
  (fset 'yes-or-no-p 'y-or-n-p)
  (menu-bar-mode -1) ;; minimal chrome
  (tool-bar-mode -1) ;; MINIMALISTIC BABY
  (scroll-bar-mode -1) ;; disable scroll bars
  (set-face-attribute 'region nil :background "#0000ff" :foreground "#ffffff")
  (column-number-mode 1) ;; To know which column stack trace refers to.
  (blink-cursor-mode 0)    ;; Reduce visual noise
  (display-time-mode 1)
  (display-battery-mode 1)
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

(init-settings)
(load-all-config-in-directory "~/.emacs.d/lisp/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 3)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)
 '(ccls-enable-skipped-ranges nil t)
 '(ccls-executable nil t)
 '(ccls-sem-highlight-method 'font-lock t)
 '(company-begin-commands '(self-insert-command))
 '(company-box-backends-colors nil t)
 '(company-box-doc-delay 0.3)
 '(company-box-max-candidates 50)
 '(company-box-show-single-candidate t)
 '(company-global-modes '(not shell-mode eaf-mode))
 '(company-idle-delay 0.1)
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
 '(highlight-indent-guides-method 'fill)
 '(highlight-indent-guides-responsive 'top)
 '(leetcode-prefer-language "c++" t)
 '(load-prefer-newer t)
 '(package-selected-packages
   '(leetcode iedit zenburn-theme zenburn vterm use-package-hydra typescript-mode treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil toml-mode theme-looper smartparens shell-pop rustic repl-toggle rainbow-mode rainbow-delimiters racer qml-mode pretty-mode poet-theme org-pdfview org-gcal notmuch multiple-cursors mu4e-alert modern-cpp-font-lock lsp-ui lsp-python-ms lsp-java indent-guide ibuffer-projectile highlight-thing highlight-numbers highlight-indent-guides highlight-doxygen helm-xref helm-swoop helm-projectile helm-lsp helm-gtags helm-flx helm-c-yasnippet helm-ag gscholar-bibtex groovy-mode golden-ratio-scroll-screen golden-ratio go-tag go-playground go-impl go-gen-test go-fill-struct go-dlv ggtags format-all flycheck-rust flycheck-pycheckers flycheck-inline flycheck-golangci-lint fish-mode fish-completion fic-mode expand-region exec-path-from-shell esup engine-mode emms-info-mediainfo elpy easy-kill-extras drag-stuff doom-themes doom-modeline disk-usage dashboard dap-mode company-tabnine company-racer company-lsp company-flx company-box company-auctex cmake-font-lock ccls cargo calfw-org calfw-ical calfw bibclean-format beacon auto-dictionary auctex-latexmk anzu))
 '(rtog/mode-repl-alist
   '((emacs-lisp-mode . ielm)
     (python-mode . elpy-shell-switch-to-shell)
     (js-mode . nodejs-repl)
     (typescript-mode . run-ts)) t)
 '(url-debug t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
