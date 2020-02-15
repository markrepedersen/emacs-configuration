;; Highlights hexcolors, like #aabbcc and Red.
(use-package rainbow-mode
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

;; For programming modes, show delimiters with variying colors to easily
;; distinguish between them.
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlights numbers with another color so they are easier to spot.
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

;; Highlights thing at point.
(use-package highlight-thing
  :config
  (setq highlight-thing-delay-seconds 1.5)
  (setq highlight-thing-limit-to-defun t) ;; Limit to current function.
  (setq highlight-thing-what-thing 'symbol)
  (setq highlight-thing-case-sensitive-p t)

  ;; Enable highlight-thing-mode in prog modes except for modes using LSP because it has it's own
  ;; highlight feature.
  (add-hook 'prog-mode-hook
            '(lambda ()
               (when (not (member major-mode '(c++-mode c-mode python-mode rust-mode)))
                 (highlight-thing-mode)))))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)

  ;; Bindings
  (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
  (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

  (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
  (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
  (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
  (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

  (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
  (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

  (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
  (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

  (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
  (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

  (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

  (define-key smartparens-mode-map (kbd "C-M-S-k") 'sp-kill-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-S-t") 'sp-transpose-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-S-s") 'sp-slurp-hybrid-sexp)
  (define-key smartparens-mode-map (kbd "C-M-S-p") 'sp-push-hybrid-sexp)

  ;; General prog mode handling of "{}" to indent after hitting RET.
  (sp-with-modes '(c-mode c++-mode php-mode java-mode js-mode rust-mode sh-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET"))))

  ;; Same thing but with "[]" and match "|" with "|".
  (sp-with-modes '(rust-mode)
    (sp-local-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "|" "|"))

  ;; Markdown modes
  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "**" "**")
    (sp-local-pair "_" "_"))

  ;; Lisp modes
  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :wrap "C-(")
    (sp-local-pair "`" "'" :when '(sp-in-string-p)))

  (bind-key ";" 'sp-comment emacs-lisp-mode-map)

  ;; LaTeX modes
  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-pair "$" "$")
    (sp-local-pair "\\[" "\\]")
    (sp-local-pair "`" "'")
    (sp-local-tag "\"" "``" "''" :actions '(wrap))))

;; NSIS installer script file.
(use-package conf-mode
  :mode (("\\.nsi\\'" . conf-windows-mode)
         ("\\.nsis\\'" . conf-windows-mode)))

;; Show vertical lines to guide indentation.
(use-package indent-guide
  :config
  (setq indent-guide-char "|"
        indent-guide-delay 0.5)
  (add-hook 'prog-mode-hook 'indent-guide-mode))
