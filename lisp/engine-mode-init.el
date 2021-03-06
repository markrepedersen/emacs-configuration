(use-package engine-mode
  :after hydra
  :defer t
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Engine"
    (("h" engine/search-github "github")
     ("g" engine/search-google        "google")
     ("s" engine/search-stack-overflow "stackoverflow")
     ("w" engine/search-wikipedia     "wikipedia")
     ("e" engine/search-emacswiki "emacs-wiki"))))
  :bind (("C-c e" . engine-mode-hydra/body))
  :config
  (defengine github "https://github.com/search?ref=simplesearch&q=%s")
  (defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s")
  (defengine stack-overflow "https://stackoverflow.com/search?q=%s")
  (defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s")
  (defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s")
  (defengine emacswiki "http://google.com/search?q=site:emacswiki.org+%s"))
