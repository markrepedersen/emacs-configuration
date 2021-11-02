(use-package ccls
  :defer t
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
  :commands lsp
  :after hydra
  :hook ((linum-mode))
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
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
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
	lsp-keep-workspace-alive nil)

  ;; ------------ Tramp remote LSP config ------------
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote)))

;; Refer to https://code.visualstudio.com/docs/cpp/launch-json-reference for C++ dap-mode launch.json configuration arguments.
(use-package dap-mode
  :defer t
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

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
