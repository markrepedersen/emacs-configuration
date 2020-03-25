(use-package elpy
  :defer t
  :init
  (elpy-enable))

(use-package lsp-python-ms
  :defer t
  :hook (python-mode . lsp)
  :config
  (setq lsp-python-ms-executable
	"~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
