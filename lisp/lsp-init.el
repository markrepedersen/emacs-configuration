(use-package modern-cpp-font-lock
  :diminish t
  :init (modern-c++-font-lock-global-mode t))

(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable (executable-find "ccls")
	ccls-sem-highlight-method 'font-lock
	ccls-enable-skipped-ranges nil)
  (lsp-register-client
   (make-lsp-client
    :priority 1
    :new-connection (lsp-tramp-connection (cons ccls-executable ccls-args))
    :major-modes '(c-mode c++-mode cuda-mode objc-mode)
    :server-id 'ccls-remote
    :multi-root nil
    :remote? t
    :notification-handlers
    (lsp-ht ("$ccls/publishSkippedRanges" #'ccls--publish-skipped-ranges)
            ("$ccls/publishSemanticHighlight" #'ccls--publish-semantic-highlight))
    :initialization-options (lambda () ccls-initialization-options)
    :library-folders-fn nil)))

(use-package lsp-mode
  :hook ((rust-mode . lsp)
	 (sh-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (java-mode . lsp)
	 (groovy-mode . lsp)
	 (python-mode . lsp))
  :mode-hydra
  ((:color teal :quit-key "q" :title (with-mode-icon 'lsp-mode "LSP mode"))
   ("Find"
    (("d" lsp-goto-implementation         "Goto implementation")
     ("r" lsp-find-references             "Find references")
     ("o" lsp-describe-thing-at-point     "Describe thing"))

    "Peek"
    (("D" lsp-ui-peek-find-implementation "Peek implementation")
     ("R" lsp-ui-peek-find-references     "Peek references"))

    "Fix/Refactor"
    (("n" lsp-rename                     "Rename symbol")
     ("f" lsp-format-region              "Format region")
     ("x" lsp-execute-code-action        "Execute code action"))))
  :init
  (setq lsp-rust-server 'rust-analyzer
	gc-cons-threshold 100000000
	read-process-output-max (* 1024 1024)
	company-minimum-prefix-length 1
	company-idle-delay 0.2)
  :config
  (unbind-key "M-n" lsp-signature-mode-map)
  (unbind-key "M-p" lsp-signature-mode-map)
  (setq lsp-idle-delay 0.1000
	lsp-prefer-capf t
	lsp-prefer-flymake nil
	lsp-enable-xref t
	lsp-keep-workspace-alive nil))

(use-package helm-lsp
  :defer t
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
  :defer t
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
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package dap-mode
  :defer t
  :bind (:map lsp-mode-map ("C-c C-d" . dap-hydra))
  :hook ((after-init . dap-mode)
         (dap-mode . dap-ui-mode)
         (dap-session-created . (lambda (_args) (dap-hydra)))
         (dap-stopped . (lambda (_args) (dap-hydra)))

         (python-mode . (lambda () (require 'dap-python)))
         (ruby-mode . (lambda () (require 'dap-ruby)))
         (go-mode . (lambda () (require 'dap-go)))
         (java-mode . (lambda () (require 'dap-java)))
         ((c-mode c++-mode objc-mode swift-mode) . (lambda () (require 'dap-lldb)))
         (php-mode . (lambda () (require 'dap-php)))
         (elixir-mode . (lambda () (require 'dap-elixir)))
         ((js-mode js2-mode) . (lambda () (require 'dap-chrome)))))
