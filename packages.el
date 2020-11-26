(package! org-reverse-datetree)
;; (package! kubernetes)
;; (package! kubernetes-evil)
;; (package! kubel)
;; (package! kubel-evil)
;; (package! ivy-posframe)
;; (package! px :recipe (:host github :repo "aaptel/preview-latex"))
;; (package! golden-ratio)
(package! ox-ipynb :recipe (:host github :repo "jkitchin/ox-ipynb"))
(package! ob-mermaid)
;; (package! kubel)
;; (package! kubel-evil)
;; (package! pfuture)
;; (package! company-statistics)
(package! company-tabnine)
  ;; :recipe (:host github :repo "TommyX12/company-tabnine"
  ;;                :files ("company-tabnine.el" "fetch-binaries.sh")) :pin "e986a4ad0d")
  ;;
;; (package! wttrin :pin "df5427ce2a")
;; (package! org-super-agenda :pin "dd0d104c26")
;; (package! doct
;;   :recipe (:host github :repo "progfolio/doct")
;;   :pin "fb798eb3a2")
;; (package! org-pretty-table-mode
;;   :recipe (:host github :repo "Fuco1/org-pretty-table") :pin "88380f865a")
;; (package! org-fragtog :pin "8eca8084cc")
;; (package! org-fancy-priorities :pin "819bb993b7")
;; (package! org-pretty-tags :pin "40fd72f3e7")
;; (package! vuiet :recipe (:host github :repo "mihaiolteanu/vuiet"))
;; (package! howdoyou)
(package! evil-cleverparens)
(package! aggressive-indent)
;; (package! sayid)
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
(unpin! dap-mode lsp-mode treemacs)
;; (package! solaire-mode :disable t)
;; (package! modus-operandi-theme :recipe (:no-native-compile t))
;; (package! modus-vivendi-theme :recipe (:no-native-compile t))
(unpin! vterm)
(package! jupyter :recipe (:no-native-compile t))
(package! miracle :recipe (:host github :repo "Saikyun/miracle"))

(package! cider :pin "bfcf9157a970c9423fe27c5021f445b509e71280")
(package! elegant-emacs
  :recipe (:host github :repo "rougier/elegant-emacs"
           :files ("*.el")))
;; (package! webkit
;;   :recipe (:host github :repo "akirakyle/emacs-webkit"
;;            :branch "main"
;;            :files (:defaults "*.js" "*.css")
;;            :build ("make")))
