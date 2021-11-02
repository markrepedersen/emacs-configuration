(use-package lsp-java
  :hook (java-mode . lsp)
  :bind (:map java-mode-map
	      ("C-d" . nil)))
