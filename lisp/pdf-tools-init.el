(use-package pdf-tools
  :bind (:map pdf-view-mode-map ("C-c p" . hydra-pdf-tools/body))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (use-package org-pdfview))
