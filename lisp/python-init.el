(use-package lsp-python-ms
  :hook (python-mode . lsp)
  :config
  (setq lsp-python-ms-executable
	"~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
