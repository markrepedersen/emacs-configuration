(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (lsp-clients-typescript-server-args '("--stdio" "--tsserver-log-file" "/dev/stderr"))
  :commands (typescript-mode))
