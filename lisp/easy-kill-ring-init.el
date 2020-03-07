(use-package easy-kill)
;; M-w w: save word at point
;; M-w s: save sexp at point
;; M-w l: save list at point (enclosing sexp)
;; M-w d: save defun2 at point
;; M-w D: save current defun’s name
;; M-w f: save filename at point
;; M-w b: save buffer-file-name (the name of the file a buffer is currently visiting) or default-directory (the current directory of a buffer, in case it’s not visiting a file).
(use-package easy-kill-extras
  :bind (([remap kill-ring-save] . easy-kill)
         ([remap mark-sexp] . easy-mark-sexp)
         ([remap mark-word] . easy-mark-word)

         ;; Integrate `zap-to-char'
         ([remap zap-to-char] . easy-mark-to-char)
         ([remap zap-up-to-char] . easy-mark-up-to-char)

         ;; Integrate `expand-region'
         :map easy-kill-base-map
         ("o" . easy-kill-er-expand)
         ("i" . easy-kill-er-unexpand))
  :init (setq kill-ring-max 200
              save-interprogram-paste-before-kill t ; Save clipboard contents before replacement
              easy-kill-alist '((?w word           " ")
                                (?s sexp           "\n")
                                (?l list           "\n")
                                (?f filename       "\n")
                                (?d defun          "\n\n")
                                (?D defun-name     " ")
                                (?e line           "\n")
                                (?b buffer-file-name)

                                (?^ backward-line-edge "")
                                (?$ forward-line-edge "")
                                (?h buffer "")
                                (?< buffer-before-point "")
                                (?> buffer-after-point "")
                                (?f string-to-char-forward "")
                                (?F string-up-to-char-forward "")
                                (?t string-to-char-backward "")
                                (?T string-up-to-char-backward ""))))
