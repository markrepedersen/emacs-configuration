(use-package exec-path-from-shell
  :defer t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))
