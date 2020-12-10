(package! org-reverse-datetree)
;; (package! golden-ratio)
(package! ox-ipynb :recipe (:host github :repo "jkitchin/ox-ipynb"))
(package! ob-mermaid)
;; (package! vuiet :recipe (:host github :repo "mihaiolteanu/vuiet"))
(package! evil-cleverparens)
(package! aggressive-indent)
(package! tree-sitter
  :recipe (:host github :repo "ubolonton/emacs-tree-sitter"
           :files ("lisp/*.el")))
(package! tsc :recipe (:host github
            :repo "ubolonton/emacs-tree-sitter"
            :files ("core/*.el")))
(package! tree-sitter-langs
  :recipe (:host github :repo "ubolonton/emacs-tree-sitter"
           :files ("langs/*.el" "langs/queries")))
(package! pipenv :disable t)
(package! pyvenv :disable t)
(package! centered-cursor-mode)
;; (package! evil-lisp-state)
;; (unpin! dap-mode lsp-mode treemacs)
;; (package! solaire-mode :disable t)
;; (unpin! vterm)
(package! jupyter :recipe (:no-native-compile t))
(package! zmq :recipe (:no-byte-compile t))
;; (package! zmq-channel :recipe (:no-byte-compile t))
;; (package! miracle :recipe (:host github :repo "Saikyun/miracle"))
(package! cider :pin "bfcf9157a970c9423fe27c5021f445b509e71280") ; TODO pinned because of arcadia
;; (package! elegant-emacs
;;   :recipe (:host github :repo "rougier/elegant-emacs"
;;            :files ("*.el")))
; TODO cant build gtk
;; (package! webkit
;;   :recipe (:host github :repo "akirakyle/emacs-webkit"
;;            :branch "main"
;;            :files (:defaults "*.js" "*.css")
;;            :build ("make")))
;; (package! prescient)
;; (package! company-prescient)
;; (package! selectrum-prescient)
;; (package! consult :recipe (:host github :repo "minad/consult"))

(package! org-tree-slide :pin "18034c476038adcc1c4697168b8068f4d0ce62fe")
(package! org-re-reveal :pin "2035217ae9f9dbd20bf054daa8dabf7c6aa3938d")
(package! revealjs
  :recipe (:host github :repo "hakimel/reveal.js"
           :files ("css" "dist" "js" "plugin"))
  :pin "0582f57517c97a4c7bfeb58762138c78883f94c5")
