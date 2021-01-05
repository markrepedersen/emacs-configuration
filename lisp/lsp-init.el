(use-package ccls
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  (setq ccls-executable (executable-find "ccls")
	lsp-prefer-flymake nil
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
  :hook ((rustic-mode . lsp)
	 (sh-mode . lsp)
	 (html-mode . lsp)
	 (c-mode . lsp)
	 (c++-mode . lsp)
	 (java-mode . lsp)
	 (javascript-mode . lsp)
	 (typescript-mode . lsp)
	 (go-mode . lsp)
	 (python-mode . lsp)
	 (linum-mode))
  :pretty-hydra
  ((:color teal :quit-key "q" :title (with-mode-icon 'lsp-mode "LSP mode"))
   ("Find"
    (("i" lsp-goto-implementation         "Goto implementation")
     ("r" lsp-find-references             "Find references")
     ("o" lsp-describe-thing-at-point     "Describe thing"))

    "Debugging"
    (("d" dap-debug "Debug")
     ("e" dap-debug-edit-template "Edit debug template"))

    "Peek"
    (("I" lsp-ui-peek-find-implementation "Peek implementation")
     ("R" lsp-ui-peek-find-references     "Peek references")
     ("F" lsp-ui-doc-frame-focus          "Focus doc")
     ("U" lsp-ui-doc-frame-unfocus        "Unfocus doc")
     )

    "Fix/Refactor"
    (("n" lsp-rename                     "Rename symbol")
     ("f" lsp-format-buffer              "Format buffer")
     ("x" lsp-execute-code-action        "Execute code action"))))
  :bind (("C-c l" . lsp-mode-hydra/body))
  :init
  (setq read-process-output-max (* 1024 1024 10)
	lsp-rust-server 'rust-analyzer
	lsp-disabled-clients '(rls))
  :config
  (unbind-key "M-n" lsp-signature-mode-map)
  (unbind-key "M-p" lsp-signature-mode-map)
  (push "[/\\\\][^/\\\\]*\\.\\(json\\|html\\|jade\\)$" lsp-file-watch-ignored)
  (push "[/\\\\]build$" lsp-file-watch-ignored)
  (push "[/\\\\]target$" lsp-file-watch-ignored)
  (setq lsp-idle-delay 0.1000
	lsp-enable-file-watchers nil
	lsp-enable-indentation t
	lsp-file-watch-threshold 500
	lsp-prefer-flymake nil
	lsp-log-io nil
	lsp-enable-xref t
	lsp-signature-auto-activate nil
	lsp-eldoc-hook nil
	lsp-keep-workspace-alive nil))


;; Refer to https://code.visualstudio.com/docs/cpp/launch-json-reference for C++ dap-mode launch.json configuration arguments.
(use-package dap-mode
  :commands dap-mode
  :hook (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (dap-mode 1)
  (require 'dap-ui)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (dap-ui-mode 1)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (require 'dap-cpptools))

(use-package company-lsp
  :defer t
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package helm-lsp
  :defer t
  :after lsp-mode
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
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-doc-enable t
	      lsp-ui-doc-use-webkit nil
	      lsp-ui-doc-delay 0.1
	      lsp-ui-doc-include-signature t
	      lsp-ui-doc-position 'top
	      lsp-ui-doc-border (face-foreground 'default)
	      lsp-ui-doc-border "cyan"
	      lsp-eldoc-enable-hover t
	      lsp-ui-sideline-enable nil
	      lsp-ui-doc-use-childframe t
	      lsp-ui-imenu-enable t
	      lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face)))
  :config
  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 20))
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))
