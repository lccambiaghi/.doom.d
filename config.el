(setq user-full-name "Luca Cambiaghi"
      user-mail-address "luca.cambiaghi@me.com")

(setq-default
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      ;; auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      inhibit-compacting-font-caches t)           ; When there are lots of glyphs, keep them in memory

(delete-selection-mode 1)                         ; Replace selection when inserting text
;; (global-subword-mode 1)                           ; Iterate through CamelCase words
(setq +evil-want-o/O-to-continue-comments nil)

(general-auto-unbind-keys)

(map! :leader
      :desc "M-x"                   :n "SPC" #'counsel-M-x

      :desc "ivy resume" :n ":" #'ivy-resume
      :desc "Async shell command"   :n "!"   #'async-shell-command
      :desc "Toggle eshell"         :n "'"   #'+eshell/toggle
      :desc "Open dir in iTerm" :n "oi" #'+macos/open-in-iterm

      (:desc "windows" :prefix "w"
        :desc "popup raise" :n "p" #'+popup/raise)

      (:desc "project" :prefix "p"
        :desc "Eshell"               :n "'" #'projectile-run-eshell
        :desc "Terminal" :n "t" #'projectile-run-vterm ))

(define-key evil-normal-state-map (kbd "s-d") #'evil-multiedit-match-symbol-and-next)

(after! evil-multiedit
  (setq evil-multiedit-follow-matches t)
  (map! :v "zD" #'evil-multiedit-toggle-or-restrict-region))

(setq display-line-numbers-type nil)

(setq doom-font (font-spec :family "Menlo" :size 16)
      doom-big-font (font-spec :family "Menlo" :size 20))

(after! which-key
    (setq which-key-idle-delay 0.5))

(defmacro modus-themes-format-sexp (sexp &rest objects)
  `(eval (read (format ,(format "%S" sexp) ,@objects))))

(dolist (theme '("operandi" "vivendi"))
  (modus-themes-format-sexp
   (defun modus-%1$s-theme-load ()
     (setq modus-%1$s-theme-slanted-constructs t
           modus-%1$s-theme-bold-constructs t
           modus-%1$s-theme-fringes 'subtle ; {nil,'subtle,'intense}
           modus-%1$s-theme-mode-line '3d ; {nil,'3d,'moody}
           modus-%1$s-theme-faint-syntax nil
           modus-%1$s-theme-intense-hl-line nil
           modus-%1$s-theme-intense-paren-match nil
           modus-%1$s-theme-no-link-underline t
           modus-%1$s-theme-no-mixed-fonts nil
           modus-%1$s-theme-prompts nil ; {nil,'subtle,'intense}
           modus-%1$s-theme-completions 'moderate ; {nil,'moderate,'opinionated}
           modus-%1$s-theme-diffs nil ; {nil,'desaturated,'fg-only}
           modus-%1$s-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
           modus-%1$s-theme-headings  ; Read further below in the manual for this one
           '((1 . line)
             (t . rainbow-line-no-bold))
           modus-%1$s-theme-variable-pitch-headings t
           modus-%1$s-theme-scale-headings t
           modus-%1$s-theme-scale-1 1.1
           modus-%1$s-theme-scale-2 1.15
           modus-%1$s-theme-scale-3 1.21
           modus-%1$s-theme-scale-4 1.27
           modus-%1$s-theme-scale-5 1.33)
     (setq doom-theme 'modus-%1$s)
     (doom/reload-theme))
   theme))

(setq modus-operandi-theme-override-colors-alist
            '(("bg-main" . "#fefcf4")
              ("bg-dim" . "#faf6ef")
              ("bg-alt" . "#f7efe5")
              ("bg-hl-line" . "#f4f0e3")
              ("bg-active" . "#e8dfd1")
              ("bg-inactive" . "#f6ece5")
              ("bg-region" . "#c6bab1")
              ("bg-header" . "#ede3e0")
              ("bg-tab-bar" . "#dcd3d3")
              ("bg-tab-active" . "#fdf6eb")
              ("bg-tab-inactive" . "#c8bab8")
              ("fg-unfocused" . "#55556f"))
            modus-vivendi-theme-override-colors-alist
            '(("bg-main" . "#100b17")
              ("bg-dim" . "#161129")
              ("bg-alt" . "#181732")
              ("bg-hl-line" . "#191628")
              ("bg-active" . "#282e46")
              ("bg-inactive" . "#1a1e39")
              ("bg-region" . "#393a53")
              ("bg-header" . "#202037")
              ("bg-tab-bar" . "#262b41")
              ("bg-tab-active" . "#120f18")
              ("bg-tab-inactive" . "#3a3a5a")
              ("fg-unfocused" . "#9a9aab"))
            modus-operandi-theme-intense-paren-match t
            modus-operandi-theme-distinct-org-blocks t)

;;Light for the day
(run-at-time "07:00" (* 60 60 24)
             (lambda ()
               (modus-operandi-theme-load)))

;; Dark for the night
(run-at-time "15:00" (* 60 60 24)
             (lambda ()
               (modus-vivendi-theme-load)))

(setq +doom-dashboard-banner-file
      (expand-file-name "splash-images/black-hole2.png" doom-private-dir))

(after! centaur-tabs
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-close-button nil)
  (centaur-tabs-group-by-projectile-project))

(after! winum
    (map! (:when (featurep! :ui window-select)
            :leader
            :n "1" #'winum-select-window-1
            :n "2" #'winum-select-window-2
            :n "3" #'winum-select-window-3
        )))

(setq +ligatures-extras-in-modes
      '(not special-mode comint-mode eshell-mode term-mode vterm-mode python-mode))
;; (setq +ligatures-in-modes '(org-mode))

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python nil))

(after! lsp-mode
  (setq lsp-modeline-diagnostics-enable nil))

(use-package! centered-cursor-mode
  :defer t
  :config
  (map! :leader
        :desc "toggle centered cursor"                   :n "t-" (λ! () (interactive) (centered-cursor-mode 'toggle))
        ))

(after! magit
  ;; (magit-wip-mode)
  (setq magit-repository-directories '(("~/git" . 2))
        magit-save-repository-buffers nil
        ;; Don't restore the wconf after quitting magit
        magit-inhibit-save-previous-winconf t
        magit-log-arguments '("--graph" "--decorate" "--color")
        ;; magit-delete-by-moving-to-trash nil
        git-commit-summary-max-length 120))

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-dabbrev-code-everywhere t
        company-dabbrev-code-other-buffers 'all))
;; company-quickhelp-delay 0.4)

(after! company
  (define-key! company-active-map
    "TAB"       nil
    [tab]       nil))

(after! company
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

(after! org
(setq org-directory "~/Dropbox/org"
      org-image-actual-width nil
      +org-export-directory "~/Dropbox/org/export"
      org-default-notes-file "~/Dropbox/org/personal/inbox.org"
      org-id-locations-file "~/Dropbox/org/.orgids"
      ;; org-agenda-files (directory-files-recursively "~/dropbox/org/" "\\.org$")
      org-agenda-files '("~/dropbox/org/personal/inbox.org" "~/dropbox/org/personal/tasks.org" "~/dropbox/org/personal/birthdays.org")
      ;; org-export-in-background t
      org-catch-invisible-edits 'smart))

(after! org

  (setq org-capture-templates
        `(("a" "Article to write" entry
           (file+headline "personal/tasks.org" "Writing list")
           ,(concat "* WRITE %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n:END:\n\n"
                    "%i%?"))
          ("b" "Basic task for future review" entry
           (file+headline "personal/tasks.org" "Basic tasks that need to be reviewed")
           ,(concat "* %^{Title}\n"
                    ":PROPERTIES:\n"
                    ":CAPTURED: %U\n"
                    ":END:\n\n"
                    "%i%l"))
          ("w" "Task or assignment" entry
           (file+headline "personal/tasks.org" "Work tasks")
           ,(concat "* TODO [#A] %^{Title} :@work:\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))
          ("t" "Task with a due date" entry
           (file+headline "personal/tasks.org" "Task list with a date")
           ,(concat "* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\n"
                    "SCHEDULED: %^t\n"
                    ":PROPERTIES:\n:CAPTURED: %U\n:END:\n\n"
                    "%i%?"))
          ("r" "Reply to an email" entry
           (file+headline "tasks.org" "Mail correspondence")
           ,(concat "* TODO [#B] %:subject :mail:\n"
                    "SCHEDULED: %t\n:"
                    "PROPERTIES:\n:CONTEXT: %a\n:END:\n\n"
                    "%i%?"))))

  (add-to-list 'org-capture-templates
             '("d" "New Diary Entry" entry(file+olp+datetree"~/Dropbox/org/personal/diary.org" "Daily Logs")
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
      '("u" "New URL Entry" entry(file+function "~/Dropbox/org/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
            "* [[%^{URL}][%^{Description}]] %^g %?")))

(after! org-superstar
    (setq org-superstar-headline-bullets-list '("✖" "✚" "◆" "▶" "○")
        org-ellipsis "▼"))

(set-popup-rule! "*org agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3)

(after! org
  (require 'ox-ipynb))

(after! evil-org
  (setq org-babel-clojure-backend 'cider))

(use-package! org-re-reveal
  :after ox
  :config
  ;; (setq org-re-reveal-root (expand-file-name "../../" (locate-library "dist/reveal.js" t))
  ;;       org-re-reveal-revealjs-version "4")
  (setq org-re-reveal-root "./reveal.js"
        org-re-reveal-revealjs-version "3.8"
        org-re-reveal-external-plugins  '((progress . "{ src: '%s/plugin/toc-progress/toc-progress.js', async: true, callback: function() { toc_progress.initialize(); toc_progress.create();} }"))
        ))

(after! evil-org
    (use-package ox-moderncv
        :load-path "/Users/luca/git/org-cv/"
        :init (require 'ox-altacv))
        ;; :init (require 'ox-moderncv))
    )

(after! latex
    (setq org-latex-compiler "xelatex"))

(after! org
  (defun html-body-id-filter (output backend info)
    "Remove random ID attributes generated by Org."
    (when (eq backend 'html)
      (replace-regexp-in-string
       " id=\"[[:alpha:]-]*org[[:alnum:]]\\{7\\}\""
       ""
       output t)))

  (add-to-list 'org-export-filter-final-output-functions 'html-body-id-filter))

(after! org
  (map! :leader :n "t p" #'org-tree-slide-mode))

;; (defun remap-faces-for-present ()

;;         )

(use-package! org-tree-slide
  :after org
  :defer t
  :commands org-tree-slide-mode
  :hook ((org-tree-slide-play . (lambda ()
                                  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                                                     (org-verbatim (:height 1.75) org-verbatim)
                                                                     (org-block (:height 1.25) org-block)))
                                  (hide-mode-line-mode +1)
                                  (centaur-tabs-mode -1)
                                  ))
         (org-tree-slide-stop . (lambda ()
                                  (setq-local face-remapping-alist '((default variable-pitch default)))
                                  (hide-mode-line-mode -1)
                                  (centaur-tabs-mode +1)
                                  )))
  :config
  (org-tree-slide-presentation-profile)
  ;; (org-tree-slide-simple-profile)
  (setq ;; org-tree-slide-skip-outline-level 0
   org-tree-slide-activate-message " "
   org-tree-slide-deactivate-message " "
   ;; org-tree-slide-modeline-display nil
   ;; org-tree-slide-heading-emphasis  t
   org-tree-slide-slide-in-effect nil
   ;; text-scale-mode-amount 5
   )



  ;; always toggle inline images
  (add-hook 'org-tree-slide-mode-after-narrow-hook #'org-display-inline-images)

  ;; (defun +org-present-hide-blocks-h ()
  ;;   "Hide org #+ constructs."
  ;;   (save-excursion
  ;;     (goto-char (point-min))
  ;;     (while (re-search-forward "^[[:space:]]*\\(#\\+\\)\\(\\(?:BEGIN\\|END\\|ATTR\\)[^[:space:]]+\\).*" nil t)
  ;;       (+org-present--make-invisible
  ;;        (match-beginning 1)
  ;;        (match-end 0)))))

  ;; (add-hook! 'org-tree-slide-mode-hook
  ;;            #'+org-present-hide-blocks-h
  ;;            #'+org-present-prettify-slide-h
  ;;            )

  (map! :map org-tree-slide-mode-map
        :n "C-j" #'org-tree-slide-move-next-tree
        :n "C-k"  #'org-tree-slide-move-previous-tree)

  (add-hook 'org-tree-slide-mode-hook #'evil-normalize-keymaps)
  )

(after! ox
  (add-to-list 'org-export-backends 'beamer))

(after! evil-org
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       ;; (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '(;; (:pandoc t)
                                                  (:kernel . "ir"))))

;; (:when (featurep! :lang +jupyter)
(map! :after evil-org
      :map evil-org-mode-map
      :leader
      :desc "tangle" :n "ct" #'org-babel-tangle
      :localleader
      :desc "Hydra" :n "," #'jupyter-org-hydra/body
      :desc "Inspect at point" :n "?" #'jupyter-inspect-at-point
      :desc "Execute and step" :n "RET" #'jupyter-org-execute-and-next-block
      :desc "Delete code block" :n "x" #'jupyter-org-kill-block-and-results
      :desc "New code block above" :n "+" #'jupyter-org-insert-src-block
      :desc "New code block below" :n "=" (λ! () (interactive) (jupyter-org-insert-src-block t nil))
      :desc "Merge code blocks" :n "m" #'jupyter-org-merge-blocks
      :desc "Split code block" :n "-" #'jupyter-org-split-src-block
      :desc "Fold results" :n "z" #'org-babel-hide-result-toggle

      :map org-src-mode-map
      :localleader
      :desc "Exit edit" :n "'" #'org-edit-src-exit)

(map! :after python
      :map python-mode-map
      :localleader
      (:desc "eval" :prefix "e"
       :desc "line or region" :n "e" #'jupyter-eval-line-or-region
        :desc "defun" :n "d" #'jupyter-eval-defun
       :desc "buffer" :n "b" #'jupyter-eval-buffer))

(set-popup-rule! "*jupyter-pager*" :side 'right :size .40 :select t :vslot 2 :ttl 3)
(set-popup-rule! "^\\*Org Src*" :side 'right :size .60 :select t :vslot 2 :ttl 3 :quit nil)
(set-popup-rule! "*jupyter-repl*" :side 'bottom :size .30 :vslot 2 :ttl 3)

;; (after! evil-org
;;   (org-babel-lob-ingest "/Users/luca/git/experiments/literate/ml/rpy2.org"))

(after! ob-jupyter
  (set-eval-handler! 'jupyter-repl-interaction-mode #'jupyter-eval-line-or-region))

(add-hook! python-mode
  (set-repl-handler! 'python-mode #'jupyter-repl-pop-to-buffer))

;; (after! ob-jupyter
;;   (setq jupyter-eval-use-overlays t))

(after! ob-jupyter
  (cl-defmethod jupyter-org--insert-result (_req context result)
    (let ((str
           (org-element-interpret-data
            (jupyter-org--wrap-result-maybe
             context (if (jupyter-org--stream-result-p result)
                         (thread-last result
                           jupyter-org-strip-last-newline
                           jupyter-org-scalar)
                       result)))))
      (if (< (length str) 100000)
          (insert str)
        (insert (format ": Result was too long! Length was %d" (length str)))))
    (when (/= (point) (line-beginning-position))
      ;; Org objects such as file links do not have a newline added when
      ;; converting to their string representation by
      ;; `org-element-interpret-data' so insert one in these cases.
      (insert "\n"))))

(defadvice! fixed-zmq-start-process (orig-fn &rest args)
  :around #'zmq-start-process
  (letf! (defun make-process (&rest plist)
           (plist-put! plist :coding (plist-get plist :coding-system))
           (plist-delete! plist :coding-system)
           (apply make-process plist))
    (apply orig-fn args)))

;; (map! :after org-evil
;;         :map evil-org-mode-map
;;       :n "M-<down>" nil
;;       :n "M-j" nil
;;       )

(after! jupyter
  (defun jupyter-run-repl-or-pop-to-buffer-dwim ()
    "If a buffer is already associated with a jupyter buffer,
then pop to it. Otherwise start a jupyter kernel."
    (interactive)
    (if (bound-and-true-p jupyter-current-client)
        (jupyter-repl-pop-to-buffer)
      (call-interactively #'jupyter-run-repl)))

  ;; * eldoc integration
  (defun scimax-jupyter-signature ()
    "Try to return a function signature for the thing at point."
    (when (and (eql major-mode 'org-mode)
               (string= (or (get-text-property (point) 'lang) "") "jupyter-python"))
      (save-window-excursion
     ;;; Essentially copied from (jupyter-inspect-at-point).
        (jupyter-org-with-src-block-client
         (cl-destructuring-bind (code pos)
             (jupyter-code-context 'inspect)
           (jupyter-inspect code pos nil 0)))
        (when (get-buffer "*Help*")
          (with-current-buffer "*Help*"
            (goto-char (point-min))
            (prog1
                (cond
                 ((re-search-forward "Signature:" nil t 1)
                  (buffer-substring (line-beginning-position) (line-end-position)))
                 ((re-search-forward "Docstring:" nil t 1)
                  (forward-line)
                  (buffer-substring (line-beginning-position) (line-end-position)))
                 (t
                  nil))
              ;; get rid of this so we don't accidentally show old results later
              (with-current-buffer "*Help*"
                (toggle-read-only)
                (erase-buffer))))))))

  (defun scimax-jupyter-eldoc-advice (orig-func &rest args)
    "Advice function to get eldoc signatures in blocks in org-mode."
    (or (scimax-jupyter-signature) (apply orig-func args)))


  (defun scimax-jupyter-turn-on-eldoc ()
    "Turn on eldoc signatures."
    (interactive)
    (advice-add 'org-eldoc-documentation-function :around #'scimax-jupyter-eldoc-advice))

  ( scimax-jupyter-turn-on-eldoc )
  )

(defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "ipython")))
        (apply orig-fn args))
    (apply orig-fn args)))

(setq python-shell-prompt-detect-failure-warning nil)

(set-popup-rule! "^\\*Python*" :ignore t)

(after! python
  (setq python-shell-completion-native-enable nil))

(after! lsp-python-ms
  (set-lsp-priority! 'pyright 1))

(after! lsp-mode
  (setq lsp-auto-guess-root nil))

(after! projectile
  (setq projectile-project-root-files '("Dockerfile" "pyproject.toml" "project.clj")))

(setq read-process-output-max (* 1024 1024))

(set-popup-rule! "^\\*lsp-help" :side 'right :size .50 :select t :vslot 1)

(after! lsp-mode
  (setq lsp-diagnostic-package :none))

(after! flycheck
    (add-hook 'pyhon-mode-local-vars-hook
            (lambda ()
                (when (flycheck-may-enable-checker 'python-flake8)
                (flycheck-select-checker 'python-flake8)))))
  ;; (setq flycheck-disabled-checkers 'lsp)

(after! lsp-mode
  (setq lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;; lsp-enable-on-type-formatting nil
        ;; lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil))

(after! lsp-mode
  (setq lsp-restart 'ignore))

(after! python-pytest
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (evil-set-initial-state 'python-pytest-mode 'normal))

(set-popup-rule! "^\\*pytest*" :side 'right :size .50)

(after! dap-mode
  ;; (setq dap-auto-show-output t)
  (setq dap-output-window-max-height 50)
  (setq dap-output-window-min-height 50)
  (setq dap-auto-configure-features '(locals))

  (setq dap-ui-buffer-configurations
        `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
          (,"*dap-ui-repl*" . ((side . right) (slot . 1) (window-width . 0.50))) ;; added this! TODO enable when release on MELPA
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.20)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.20)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))

;; (set-popup-rule! "^\\*dap-debug-script*" :side 'bottom :size .30)


  (defun my/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun my/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (my/window-visible dap-ui--repl-buffer)
          (dap-ui-repl)))))

  (add-hook 'dap-stopped-hook 'my/show-debug-windows)

  (defun my/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--repl-buffer)
           (kill-buffer dap-ui--repl-buffer)
           (get-buffer dap-ui--debug-window-buffer)
           (kill-buffer dap-ui--debug-window-buffer))))

  (add-hook 'dap-terminated-hook 'my/hide-debug-windows)

  )

;; (setq dap-auto-configure-features '(locals))
;; (after! dap-mode
;;   (setq dap-overlays-use-overlays nil)
;;   )
(remove-hook 'dap-mode-hook #'dap-tooltip-mode)
(remove-hook 'dap-ui-mode-hook #'dap-ui-controls-mode)

(after! dap-python
  (dap-register-debug-template "dap-debug-script"
                               (list :type "python"
                                     :args []
                                     :cwd "${workspaceFolder}"
                                     ;; :cwd (lsp-workspace-root)
                                     ;; :justMyCode :json-false
                                     ;; :debugOptions ["DebugStdLib" "ShowReturnValue" "RedirectOutput"]
                                     ;; :program nil ; (expand-file-name "~/git/blabla")
                                     :request "launch"
                                     ;; :debugger 'ptvsd
                                     :debugger 'debugpy
                                     :name "dap-debug-script"))

  ;; (dap-register-debug-template "Python :: Run pytest (at point), ptvsd"
  ;;                              (list :type "python-test-at-point"
  ;;                                    :args ""
  ;;                                    :module "pytest"
  ;;                                    :request "launch"
  ;;                                    :debugger 'ptvsd
  ;;                                    :name "Python :: Run pytest (at point)"))

  ;; (dap-register-debug-template "Python :: Run pytest (at point), debugpy"
  ;;                              (list :type "python-test-at-point"
  ;;                                    :args ["/Users/luca/git/wondercast/caf/test/customer_allocation/summarize_historical/summarize_historical_test.py::test_summarize"]
  ;;                                    ;; :module "pytest"
  ;;                                    :request "launch"
  ;;                                    :debugger 'debugpy
  ;;                                    :name "Python :: Run pytest (at point)"))

  )

;; (after! dap-python
;;   (require 'python-pytest)

;;   (defun dap-python-test-method-at-point-debugpy ()
;;     (interactive
;;        (dap-debug
;;         (list :type "python"
;;               ;; :args []
;;               ;; :args "py.test /Users/luca/git/wondercast/caf/test/customer_allocation/summarize_historical/summarize_historical_test.py"
;;               :args (concat (buffer-file-name) ":" ":" (python-pytest--current-defun))
;;               ;; :program (concat (buffer-file-name) ":" ":" (python-pytest--current-defun))
;;               ;; :program "/Users/luca/git/wondercast/caf/test/customer_allocation/summarize_historical/summarize_historical_test.py"
;;               ;; :module "pytest"
;;               :debugger 'debugpy
;;               :request "launch"
;;               :name "dap-debug-test-function-debugpy"))))

;;   (defun dap-python-test-method-at-point ()
;;     (interactive
;;        (dap-debug
;;         (list :type "python"
;;               :args ""
;;               ;; :args []
;;               :cwd (lsp-workspace-root)
;;               :program (concat (buffer-file-name) ":" ":" (python-pytest--current-defun))
;;               :module "pytest"
;;               :debugger 'ptvsd
;;               ;; :debugger 'debugpy
;;               :request "launch"
;;               :name "dap-debug-test-function")))))

(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))
;; (after! dap-python
;;   (defun dap-python--pyenv-executable-find (command)
;;     (concat (getenv "VIRTUAL_ENV") "/bin/python")))

(map! :localleader
        :map +dap-running-session-mode-map
      "d" nil)

;; (map! :after dap-mode
;;     :map dap-mode-map
;;     :localleader "d" nil)

(map! :after dap-mode
    :map python-mode-map
    :localleader
    ;; "d" nil
    (:desc "debug" :prefix "d"
      :desc "Hydra" :n "h" #'dap-hydra
      :desc "Run debug configuration" :n "d" #'dap-debug
      :desc "dap-ui REPL" :n "r" #'dap-ui-repl
      :desc "Debug test function" :n "t" #'dap-python-debug-test-at-point
      :desc "Run last debug configuration" :n "l" #'dap-debug-last
      :desc "Toggle breakpoint" :n "b" #'dap-breakpoint-toggle
      :desc "dap continue" :n "c" #'dap-continue
      :desc "dap next" :n "n" #'dap-next
      :desc "Debug script" :n "s" #'dap-python-script
      :desc "dap step in" :n "i" #'dap-step-in
      :desc "dap eval at point" :n "e" #'dap-eval-thing-at-point
      :desc "Disconnect" :n "q" #'dap-disconnect ))

(after! dap-mode
  (setq dap-python-debugger 'debugpy))

(after! ein
  (set-popup-rule! "^\\*ein" :ignore t))

(map! (:when (featurep! :tools ein)
        (:map ein:notebook-mode-map
          :nmvo doom-localleader-key nil ;; remove binding to local-leader

          ;; :desc "Execute" :ni "S-RET" #'ein:worksheet-execute-cell

          :localleader
          :desc "Show Hydra" :n "?" #'+ein/hydra/body
          :desc "Execute and step" :n "RET" #'ein:worksheet-execute-cell-and-goto-next
          :desc "Yank cell" :n "y" #'ein:worksheet-copy-cell
          :desc "Paste cell" :n "p" #'ein:worksheet-yank-cell
          :desc "Delete cell" :n "d" #'ein:worksheet-kill-cell
          :desc "Insert cell below" :n "o" #'ein:worksheet-insert-cell-below
          :desc "Insert cell above" :n "O" #'ein:worksheet-insert-cell-above
          :desc "Next cell" :n "j" #'ein:worksheet-goto-next-input
          :desc "Previous cell" :n "k" #'ein:worksheet-goto-prev-input
          :desc "Save notebook" :n "fs" #'ein:notebook-save-notebook-command
      )))

(set-popup-rule! "*eww*" :side 'right :size .50 :select t :vslot 2 :ttl 3)

(after! dash-docs
  ;; (setq dash-docs-docsets-path "/Users/luca/Library/Application Support/Dash/DocSets")
  ;; (setq counsel-dash-docsets-path "/Users/luca/Library/Application Support/Dash/DocSets")
  ;; (expand-file-name "~/Library/Application Support/Dash/DocSets")
  ;; (set-docsets! 'python-mode "NumPy" "Pandas" "scikit-learn"))
  (setq counsel-dash-docsets '("Pandas" "scikit-learn"))
  (setq dash-docs-docsets '("Pandas" "scikit-learn")))

(set-popup-rule! "*compilation*" :side 'right :size .50 :select t :vslot 2 :quit 'current)

(after! pyvenv
  (setq pyvenv-mode-line-indicator nil))

(set-popup-rule! "^\\*R:" :ignore t)

(after! ess
  (setq ess-eval-visibly 'nowait))

(after! ess
  (setq ess-R-font-lock-keywords '((ess-R-fl-keyword:keywords . t)
                                   (ess-R-fl-keyword:constants . t)
                                   (ess-R-fl-keyword:modifiers . t)
                                   (ess-R-fl-keyword:fun-defs . t)
                                   (ess-R-fl-keyword:assign-ops . t)
                                   (ess-R-fl-keyword:%op% . t)
                                   (ess-fl-keyword:fun-calls . t)
                                   (ess-fl-keyword:numbers . t)
                                   (ess-fl-keyword:operators . t)
                                   (ess-fl-keyword:delimiters . t)
                                   (ess-fl-keyword:= . t)
                                   (ess-R-fl-keyword:F&T . t))))

(after! cider
  (add-hook 'company-completion-started-hook 'custom/set-company-maps)
  (add-hook 'company-completion-finished-hook 'custom/unset-company-maps)
  (add-hook 'company-completion-cancelled-hook 'custom/unset-company-maps)

  (defun custom/unset-company-maps (&rest unused)
    "Set default mappings (outside of company).
    Arguments (UNUSED) are ignored."
    (general-def
      :states 'insert
      :keymaps 'override
      "<down>" nil
      "<up>"   nil
      "RET"    nil
      [return] nil
      "C-n"    nil
      "C-p"    nil
      "C-j"    nil
      "C-k"    nil
      "C-h"    nil
      "C-u"    nil
      "C-d"    nil
      "C-s"    nil
      "C-S-s"   (cond ((featurep! :completion helm) nil)
                      ((featurep! :completion ivy)  nil))
      "C-SPC"   nil
      "TAB"     nil
      [tab]     nil
      [backtab] nil))

  (defun custom/set-company-maps (&rest unused)
    "Set maps for when you're inside company completion.
    Arguments (UNUSED) are ignored."
    (general-def
      :states 'insert
      :keymaps 'override
      "<down>" #'company-select-next
      "<up>" #'company-select-previous
      "RET" #'company-complete
      [return] #'company-complete
      "C-w"     nil           ; don't interfere with `evil-delete-backward-word'
      "C-n"     #'company-select-next
      "C-p"     #'company-select-previous
      "C-j"     #'company-select-next
      "C-k"     #'company-select-previous
      "C-h"     #'company-show-doc-buffer
      "C-u"     #'company-previous-page
      "C-d"     #'company-next-page
      "C-s"     #'company-filter-candidates
      "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
                      ((featurep! :completion ivy)  #'counsel-company))
      "C-SPC"   #'company-complete-common
      ;; "TAB"     #'company-complete-common-or-cycle
      ;; [tab]     #'company-complete-common-or-cycle
      [backtab] #'company-select-previous    ))
  )

(add-hook! cider-repl-mode #'evil-normalize-keymaps)

(use-package! evil-cleverparens
  :defer t
  :after smartparens
  :init
  (setq evil-move-beyond-eol t
        evil-cleverparens-use-additional-bindings nil
        evil-cleverparens-use-s-and-S nil
        ;; evil-cleverparens-swap-move-by-word-and-symbol t
        ;; evil-cleverparens-use-regular-insert t
        )
  :config
  (add-hook! clojure-mode #'evil-cleverparens-mode)
  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

(use-package! aggressive-indent
  :after clojure-mode
  :defer t
  :config (add-hook! clojure-mode (aggressive-indent-mode 1)))

(map! :after evil-cleverparens
      :map clojure-mode-map
      :localleader
      (:desc "Wrap round" :n "(" #'sp-wrap-round
       :desc "Wrap square" :n "[" #'sp-wrap-square
       :desc "Wrap curly" :n "{" #'sp-wrap-curly
       :desc "Unwrap sexp" :n "u" #'sp-unwrap-sexp
       ))

(after! cider
 (setq nrepl-sync-request-timeout nil))

(after! clojure-mode
  (setq clojure-align-forms-automatically t))

(map! :after cider
      :map clojure-mode-map
      :localleader
      (:desc "eval" :prefix "e"
       :desc "sexp in comment" :n "E" #'cider-pprint-eval-last-sexp-to-comment
       :desc "defun in comment" :n "D" #'cider-pprint-eval-defun-to-comment
       ))

(after! cider
  (set-lookup-handlers! 'clojure-mode
    :documentation #'cider-doc))

(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))

(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

  (set-popup-rule! "*Async Shell Command*" :side 'bottom :size .40)
  (set-popup-rule! "vterm" :side 'right :size .40 :quit 'current :ttl 3)

(set-popup-rule! "*eshell*" :side 'right :size .50)
