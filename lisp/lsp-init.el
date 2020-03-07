(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  :config
  (setq lsp-idle-delay 0.3000)
  (setq lsp-prefer-capf t)
  (setq lsp-prefer-flymake nil) ;; Prefer using lsp-ui (flycheck) 
  (setq lsp-enable-xref t)
  (setq lsp-keep-workspace-alive nil) ; Auto-kill LSP server
  (use-package helm-lsp
    :config
    (defun netrom/helm-lsp-workspace-symbol-at-point ()
      (interactive)
      (let ((current-prefix-arg t))
	(call-interactively #'helm-lsp-workspace-symbol)))

    (defun netrom/helm-lsp-global-workspace-symbol-at-point ()
      (interactive)
      (let ((current-prefix-arg t))
	(call-interactively #'helm-lsp-global-workspace-symbol))))

  (use-package lsp-ui
    :requires lsp-mode flycheck
    :init (setq lsp-ui-doc-enable t
                lsp-ui-doc-use-webkit nil
                lsp-ui-doc-delay 0.2
                lsp-ui-doc-include-signature t
                lsp-ui-doc-position 'at-point
                lsp-ui-doc-border (face-foreground 'default)
                lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer

                lsp-ui-sideline-enable t
                lsp-ui-sideline-show-hover nil
                lsp-ui-sideline-show-diagnostics nil
                lsp-ui-sideline-ignore-duplicate t

                lsp-ui-imenu-enable t
                lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                      ,(face-foreground 'font-lock-string-face)
                                      ,(face-foreground 'font-lock-constant-face)
                                      ,(face-foreground 'font-lock-variable-name-face)))
    :config
    (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))
    ;; `C-g'to close doc
    (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)))

(defhydra hydra-lsp (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(global-set-key (kbd "C-c l") 'hydra-lsp/body)

(use-package lsp-java
  :hook  (java-mode . lsp))

(use-package lsp-python-ms
  :hook (python-mode . lsp)
  :config
  (setq lsp-python-ms-executable
	"~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
