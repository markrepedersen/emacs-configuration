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

(server-start)

;; Change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(column-number-mode 1) ;; To know which column stack trace refers to.

(blink-cursor-mode 0)    ;; Reduce visual noise

(add-hook 'prog-mode-hook 'linum-mode) ;; Enable line numbers when programming.

;; key bindings

(global-set-key (kbd "RET") 'newline-and-indent) ;; indent when going to new line

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Go back to previous mark.
(global-set-key (kbd "C-.") 'pop-to-mark-command)

;; Create shortcut of fullscreening Emacs.
(global-set-key (kbd "C-c f s") 'toggle-frame-fullscreen)

;; Intelligent line opening that also places cursor on new line.
(global-set-key (kbd "C-o") 'prelude-smart-open-line)
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
 '(custom-safe-themes
   '("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" "777a3a89c0b7436e37f6fa8f350cbbff80bcc1255f0c16ab7c1e82041b06fccd" "d5f8099d98174116cba9912fe2a0c3196a7cd405d12fa6b9375c55fc510988b5" "7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" "ac2ca460db1668a48c35c4d0fd842e5d2ce2d4e8567a7903b76438f2750826cd" "6973f93f55e4a6ef99aa34e10cd476bc59e2f0c192b46ec00032fe5771afd9ad" "b11699e28cc2f6c34fa6336e67d443be89fadb6a9b60de0b1594f31340ea87e4" "c19e5291471680e72d8bd98f8d6e84f781754a9e8fc089536cda3f0b7c3550e3" default))
 '(package-selected-packages
   '(doom-themes zenburn-theme zenburn use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil smartparens shell-pop rainbow-mode rainbow-delimiters pretty-mode org-pdfview multiple-cursors lsp-ui lsp-treemacs lsp-java indent-guide highlight-thing highlight-numbers helm-xref helm-swoop helm-projectile helm-lsp helm-gtags helm-flx helm-c-yasnippet helm-ag golden-ratio-scroll-screen go-mode flycheck-rust flycheck-pycheckers flycheck-inline fic-mode exec-path-from-shell drag-stuff company-lsp company-flx cargo beacon anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
