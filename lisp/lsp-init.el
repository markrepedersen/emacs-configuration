(use-package lsp-mode
  :requires hydra helm helm-lsp
  :bind ("C-." . lsp-find-definition)
  ;; reformat code and add missing (or remove old) imports
  :hook ((before-save . lsp-format-buffer)
         (go-mode . lsp-deferred)
         (before-save . lsp-organize-imports))
  :config
  (setq lsp-prefer-flymake nil ;; Prefer using lsp-ui (flycheck) over flymake.
        lsp-enable-xref t)

  ;; Let clangd use half of the logical cores but one as minimum.
  ;; `-background-index' requires clangd v8+!
  (setq lsp-clients-clangd-args `(,(format "-j=%d" (max 1 (/ (system-cores :logical) 2)))
                                  "-background-index" "-log=error"))

  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'php-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp))

(use-package lsp-java :ensure t :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp))

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
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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

(global-set-key (kbd "C-c C-c l") 'hydra-lsp/body)


