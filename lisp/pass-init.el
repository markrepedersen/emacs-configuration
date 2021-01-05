(use-package pass
  :defer t
  :commands pass
  :config
  (require 'auth-source-pass)
  (auth-source-pass-enable))
