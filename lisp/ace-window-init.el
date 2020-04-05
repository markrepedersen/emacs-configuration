(use-package ace-window
  :bind (("C-c a" . ace-window-hydra/body)
	 ("C-x o" . ace-window))
  :pretty-hydra
  ((:color teal)
   ("Actions"
    (("TAB" other-window "switch")
     ("x" ace-delete-window "delete")
     ("m" ace-delete-other-windows "maximize")
     ("s" ace-swap-window "swap")
     ("a" ace-select-window "select"))

    "Resize"
    (("h" move-border-left "←")
     ("j" move-border-down "↓")
     ("k" move-border-up "↑")
     ("l" move-border-right "→")
     ("n" balance-windows "balance")
     ("f" toggle-frame-fullscreen "toggle fullscreen"))

    "Split"
    (("b" split-window-right "horizontally")
     ("B" split-window-horizontally-instead "horizontally instead")
     ("v" split-window-below "vertically")
     ("V" split-window-vertically-instead "vertically instead"))

    "Zoom"
    (("+" zoom-in "in")
     ("-" zoom-out "out")
     ("0" jp-zoom-default "reset"))))
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
