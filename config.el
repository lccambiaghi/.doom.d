(setq user-full-name "Luca Cambiaghi"
      user-mail-address "luca.cambiaghi@me.com")

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)

(general-auto-unbind-keys)

(map! :leader
      :desc "M-x"                   :n "SPC" #'counsel-M-x
      :desc "Async shell command"   :n "!"   #'async-shell-command
      :desc "Toggle eshell"         :n "'"   #'+eshell/toggle

      (:desc "windows" :prefix "w"
        :desc "Cycle focus to other window(s)" :n "TAB" #'other-window )

      (:desc "open" :prefix "o"
        :desc "Terminal"              :n  "t" #'+term/toggle
        :desc "Eshell"                :n  "e" #'+eshell/toggle )
      (:desc "project" :prefix "p"
        :desc "Eshell"               :n "'" #'projectile-run-eshell )
)

(setq display-line-numbers-type nil)

(setq doom-font (font-spec :family "Menlo" :size 14))

;transparent adjustment
(set-frame-parameter (selected-frame)'alpha '(94 . 94))
(add-to-list 'default-frame-alist'(alpha . (94 . 94)))

(setq doom-theme 'doom-vibrant)

(after! centaur-tabs
    (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-close-button nil)
    (centaur-tabs-group-by-projectile-project)
    (map! :n "g t" #'centaur-tabs-forward
          :n "g T" #'centaur-tabs-backward)
    (add-hook! dired-mode #'centaur-tabs-local-mode)
)

(after! winum
  ;; (defun winum-assign-0-to-treemacs ()
  ;;   (when (string-match-p (buffer-name) "*Treemacs*") 10))

  ;; (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)

    (map! (:when (featurep! :ui window-select)
            :leader
            :n "0" #'treemacs-select-windows
            :n "1" #'winum-select-window-1
            :n "2" #'winum-select-window-2
            :n "3" #'winum-select-window-3
        )))

(setq +pretty-code-enabled-modes '(org-mode))

;; add to ~/.doom.d/config.el
;; (use-package! golden-ratio
;;   :after-call pre-command-hook
;;   :config
;;   (golden-ratio-mode +1)
;;   ;; Using this hook for resizing windows is less precise than
;;   ;; `doom-switch-window-hook'.
;;   (remove-hook 'window-configuration-change-hook #'golden-ratio)
;;   (add-hook 'doom-switch-window-hook #'golden-ratio) )

;; (after! ivy-posframe
;;     (setq ivy-posframe-display-functions-alist
;;             '((swiper          . nil)
;;             (complete-symbol . ivy-posframe-display-at-point)
;;             (t               . ivy-posframe-display-at-frame-top-center)))
;;     (setq ivy-posframe-min-width 110)
;;     (setq ivy-posframe-width 110)
;; (setq ivy-posframe-parameters '((alpha . 85)))
;;     (setq ivy-posframe-height-alist '((t . 20))))
;; (ivy-posframe-mode)

(setq magit-repository-directories '(("~/git" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit
      magit-inhibit-save-previous-winconf t)

(setq org-directory "~/git/org-notes/"
      org-image-actual-width nil
      +org-export-directory "~/git/org-notes/export/"
      org-default-notes-file "~/git/org-notes/inbox.org"
      org-id-locations-file "~/git/org-notes/.orgids"
      )

(load! "modules/ox-ravel")

(after! org

  (setq org-capture-templates
                  '(("d" "Diary")
                    ("u" "URL")))

  (add-to-list 'org-capture-templates
             '("dn" "New Diary Entry" entry(file+olp+datetree"~/git/org-notes/personal/diary.org" "Daily Logs")
"* %^{thought for the day}
:PROPERTIES:
:CATEGORY: %^{category}
:SUBJECT:  %^{subject}
:MOOD:     %^{mood}
:END:
:RESOURCES:
:END:

\*What was one good thing you learned today?*:
- %^{whatilearnedtoday}

\*List one thing you could have done better*:
- %^{onethingdobetter}

\*Describe in your own words how your day was*:
- %?"))

  (add-to-list 'org-capture-templates
      '("un" "New URL Entry" entry(file+function "~/git/org-notes/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
            "* [[%^{URL}][%^{Description}]] %^g %?")))

(setq org-bullets-bullet-list '("✖" "✚")
      org-ellipsis "▼")

(after! org (set-popup-rule! "^Capture.*\\.org$" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(after! org (set-popup-rule! "*org agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3))

(after! evil-org
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                        (:pandoc t)
                                                        (:kernel . "python3"))))

;; (add-hook! '+org-babel-load-functions
  ;;   (λ! ()
  ;;       (require 'ob-jupyter  "/Users/luca/.emacs.d/.local/straight/repos/emacs-jupyter/ob-jupyter.el" nil)
  ;;       (org-babel-jupyter-override-src-block "python"))
  ;; )
;; (org-babel-jupyter-restore-src-block "python")

;; (after! org
;;   (set-company-backend! 'org-mode
;;     '(company-capf)))

;; (defun add-pcomplete-to-capf ()
;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)

;;(:when (featurep! :tools +jupyter)
(map! :after jupyter
    :map evil-org-mode-map
    :n "gR" #'jupyter-org-execute-subtree
    :localleader
    :desc "Hydra" :n "," #'jupyter-org-hydra/body
    :desc "Inspect at point" :n "?" #'jupyter-inspect-at-point
    :desc "Execute and step" :n "RET" #'jupyter-org-execute-and-next-block
    :desc "Delete code block" :n "x" #'jupyter-org-kill-block-and-results
    :desc "New code block above" :n "+" #'jupyter-org-insert-src-block
    :desc "New code block below" :n "=" (λ! () (interactive) (jupyter-org-insert-src-block t nil))
    :desc "Merge code blocks" :n "m" #'jupyter-org-merge-blocks
    :desc "Split code block" :n "-" #'jupyter-org-split-src-block
    )

(after! jupyter (set-popup-rule! "*jupyter-pager*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(after! jupyter (set-popup-rule! "^\\*Org Src*" :side 'right :size .40 :select t :vslot 2 :ttl 3))

(require 'ox-ipynb)

(defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "ipython")))
        (apply orig-fn args))
    (apply orig-fn args)))

(set-popup-rule! "^\\*Python*" :ignore t)

(after! python
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl :persist t))

(map!
 :map python-mode-map
 :localleader
 :desc "Eval region" :v "r" #'python-shell-send-region
)

(setq python-shell-prompt-detect-failure-warning nil)

(set-popup-rule! "^\\*R:" :ignore t)

(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))

(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

(after! eshell
  (set-eshell-alias!
   "fd" "+eshell/fd $1"
   "fo" "find-file-other-window $1"))
