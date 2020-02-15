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

;; key bindings

;; Make font bigger/smaller.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-0") 'text-scale-adjust)

;; Indent, untabify and clean whitespace of region or buffer.
(global-set-key (kbd "C-c c") 'cleanup-region-or-buffer)

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
 '(package-selected-packages
   '(doom-themes zenburn-theme zenburn use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-icons-dired treemacs-evil smartparens shell-pop rainbow-mode rainbow-delimiters pretty-mode org-pdfview multiple-cursors lsp-ui lsp-treemacs lsp-java indent-guide highlight-thing highlight-numbers helm-xref helm-swoop helm-projectile helm-lsp helm-gtags helm-flx helm-c-yasnippet helm-ag golden-ratio-scroll-screen go-mode flycheck-rust flycheck-pycheckers flycheck-inline fic-mode exec-path-from-shell drag-stuff company-lsp company-flx cargo beacon anzu)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
