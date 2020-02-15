(use-package multiple-cursors
  :bind (("C-<"     . mc/mark-previous-like-this)
         ("C->"     . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  ;; From active region to multiple cursors:
  (general-define-key
   :preface "C-c m"
   "r" 'set=rectangular-region-anchor
   "c" 'mc/edit-lines
   "e" 'mc/edit-ends-of-lines
   "a" 'mc/edit-beginnings-of-lines))
