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

(setq display-line-numbers-type nil)

(setq doom-font (font-spec :family "Menlo" :size 16)
      doom-big-font (font-spec :family "Menlo" :size 20))
;; (setq doom-font (font-spec :family "Fira Code" :size 14)
;;       doom-big-font (font-spec :family "Fira Code" :size 22)
;; doom-variable-pitch-font (font-spec :family "Overpass" :size 16))

(after! which-key
    (setq which-key-idle-delay 0.5))

;; (set-frame-parameter (selected-frame)'alpha '(98 . 98))
;; (add-to-list 'default-frame-alist'(alpha . (98 . 98)))

(setq doom-theme 'doom-one)

(setq +doom-dashboard-banner-file
      (expand-file-name "splash-images/black-hole2.png" doom-private-dir))

(after! centaur-tabs
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-close-button nil)
  (centaur-tabs-group-by-projectile-project))

(after! winum
  ;; (defun winum-assign-0-to-treemacs ()
  ;;   (when (string-match-p (buffer-name) "*Treemacs*") 10))

  ;; (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)
  ;; (set-face-attribute 'winum-face nil :weight 'bold)

    (map! (:when (featurep! :ui window-select)
            :leader
            :n "1" #'winum-select-window-1
            :n "2" #'winum-select-window-2
            :n "3" #'winum-select-window-3
        )))

(setq +pretty-code-enabled-modes '(org-mode))

(after! doom-modeline
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-enable-python nil))
;; (setq doom-modeline-env-python-executable (executable-find "python"))

(after! doom-modeline
    (setq display-time-default-load-average nil)      ; don't show load average
    (display-time-mode 1)                             ; Enable time in the mode-line
    (display-battery-mode 1))                          ; On laptops it's nice to know how much power you have

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(use-package tree-sitter :after python-mode)

(after! tree-sitter
  (require 'tree-sitter)
  (require 'tree-sitter-langs)
  (require 'tree-sitter-hl)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

(map! :leader
      :desc "toggle centered cursor"                   :n "t-" (λ! () (interactive) (centered-cursor-mode 'toggle))
      )

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
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 1
  company-dabbrev-code-everywhere t
  company-dabbrev-code-other-buffers 'all))
        ;; company-quickhelp-delay 0.4)

(after! prescient
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000))

;; (add-hook 'after-init-hook 'company-statistics-mode))

;; (set-company-backend! 'org-mode
;;   '(company-capf company-files company-dabbrev-code))
(after! company
  (use-package company-tabnine :ensure t)
  (setq company-backends '(company-tabnine company-capf company-yasnippet)))

;; (after! company
;;   (add-to-list 'company-backends 'company-tabnine))

;; (after! company
;;   (defun company-yasnippet-or-completion ()
;;     (interactive)
;;     (let ((yas-fallback-behavior nil))
;;       (unless (yas-expand)
;;         (call-interactively #'company-complete-common))))

;;   (add-hook 'company-mode-hook (lambda ()
;;                                  (substitute-key-definition 'company-complete-common
;;                                                             'company-yasnippet-or-completion
;;                                                             company-active-map))))

(setq org-directory "~/Dropbox/org"
      org-image-actual-width nil
      +org-export-directory "~/Dropbox/org/export"
      org-default-notes-file "~/Dropbox/org/personal/inbox.org"
      org-id-locations-file "~/Dropbox/org/.orgids"
      org-agenda-files (directory-files-recursively "~/Dropbox/org/" "\\.org$")
      ;; org-export-in-background t
      org-catch-invisible-edits 'smart)

;; (load! "modules/ox-ravel")

(after! org

  (setq org-capture-templates
                  '(("d" "Diary")
                    ("u" "URL")))

  (add-to-list 'org-capture-templates
             '("dn" "New Diary Entry" entry(file+olp+datetree"~/git/Dropbox/org/personal/diary.org" "Daily Logs")
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
      '("un" "New URL Entry" entry(file+function "~/Dropbox/org/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
            "* [[%^{URL}][%^{Description}]] %^g %?")))

(after! org-superstar
    (setq org-superstar-headline-bullets-list '("✖" "✚" "◆" "▶" "○")
        org-ellipsis "▼"))

;; (setq global-org-pretty-table-mode t)

;; (add-hook! 'org-mode-hook #'+org-pretty-mode)

(set-popup-rule! "*org agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3)

(require 'ox-ipynb)

(after! evil-org
  (setq org-babel-clojure-backend 'cider))

(after! org-re-reveal
(setq org-re-reveal-root "./reveal.js")
  )

(after! evil-org
    (use-package ox-moderncv
        :load-path "/Users/luca/git/org-cv/"
        :init (require 'ox-altacv))
        ;; :init (require 'ox-moderncv))
    )

(after! latex
    (setq org-latex-compiler "xelatex"))

(after! evil-org
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '((:pandoc t)
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

(after! evil-org
  (org-babel-lob-ingest "/Users/luca/git/experiments/literate/ml/rpy2.org"))

(after! jupyter
  (set-eval-handler! 'jupyter-repl-interaction-mode #'jupyter-eval-line-or-region))

(add-hook! python-mode
  (set-repl-handler! 'python-mode #'jupyter-repl-pop-to-buffer))

(after! jupyter
  (setq jupyter-eval-use-overlays t))

(after! jupyter
  (cl-defmethod jupyter-org--insert-result (_req context result)
    (let ((str
           (org-element-interpret-data
            (jupyter-org--wrap-result-maybe
             context (if (jupyter-org--stream-result-p result)
                         (thread-last result
                           jupyter-org-strip-last-newline
                           jupyter-org-scalar)
                       result)))))
      (if (< (length str) 10000)
          (insert str)
        (insert (format ": Result was too long! Length was %d" (length str)))))
    (when (/= (point) (line-beginning-position))
      ;; Org objects such as file links do not have a newline added when
      ;; converting to their string representation by
      ;; `org-element-interpret-data' so insert one in these cases.
      (insert "\n"))))

;; (setq org-image-actual-width t)

(defadvice! fixed-zmq-start-process (orig-fn &rest args)
  :around #'zmq-start-process
  (letf! (defun make-process (&rest plist)
           (plist-put! plist :coding (plist-get plist :coding-system))
           (plist-delete! plist :coding-system)
           (apply make-process plist))
    (apply orig-fn args)))

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
  (set-lsp-priority! 'mspyls 1))

;; (setq lsp-pyright-server-cmd '("pyright-langserver"
;;                                "--stdio"))

;; (after! lsp-mode
;;   (lsp-register-client
;;    (make-lsp-client
;;     :new-connection (lsp-stdio-connection
;;                      (lambda () lsp-pyright-server-cmd)
;;                      (lambda ()
;;                        (and (cl-first lsp-pyright-server-cmd)
;;                             (executable-find (cl-first lsp-pyright-server-cmd)))))
;;     :major-modes '(python-mode)
;;     :server-id 'mspyright
;;     :priority 1
;;     :initialized-fn (lambda (workspace)
;;                       (with-lsp-workspace workspace
;;                         (lsp--set-configuration (lsp-configuration-section "python"))))
;;     :notification-handlers (lsp-ht ("pyright/beginProgress" 'ignore)
;;                                    ("pyright/reportProgress" 'ignore)
;;                                    ("pyright/endProgress" 'ignore))))
;;   )

(after! lsp-mode
  (setq lsp-auto-guess-root nil))

(after! projectile
  (setq projectile-project-root-files '("Dockerfile" "pyproject.toml" "project.clj")))

(setq read-process-output-max (* 1024 1024))

;; (after! lsp-mode
;;   (setq lsp-idle-delay 0.500))

;; (setq +lsp-company-backend 'company-capf)

(set-popup-rule! "^\\*lsp-help" :side 'right :size .50 :select t :vslot 1)

;; (after! pyimport
;;   (setq pyimport-pyflakes-path "~/git/experiments/.venv/bin/pyflakes"))

;; (after! lsp-mode
;;   (setq lsp-diagnostic-package :flymake))

;; (after! lsp-mode
;;   (add-hook! python-mode (setq lsp-diagnostic-package :flymake)))

;; (after! python
;;   (setq python-flymake-command  "~/git/experiments/.venv/bin/pyflakes"))

;; (after! flycheck
;;   (setq-default flycheck-checker 'python-pylint))

(after! lsp-mode
  (setq lsp-diagnostic-package :none))
  ;; (setq flycheck-disabled-checkers 'lsp)

(after! lsp-mode
  (setq lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;; lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil))
        ;; lsp-enable-file-watchers nil))

(after! lsp-mode
  (setq lsp-restart 'ignore))

(after! python-pytest
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (evil-set-initial-state 'python-pytest-mode 'normal))

(set-popup-rule! "^\\*pytest*" :side 'right :size .50)

(after! dap-python
  (setq dap-auto-show-output nil)

  (setq dap-auto-configure-features '(locals))

  (setq dap-ui-buffer-configurations
        `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.20)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.20)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))


  (defun my/window-visible (b-name)
    "Return whether B-NAME is visible."
    (-> (-compose 'buffer-name 'window-buffer)
        (-map (window-list))
        (-contains? b-name)))

  (defun my/show-debug-windows (session)
    "Show debug windows."
    (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
      (save-excursion
        (unless (my/window-visible dap-ui--locals-buffer)
          (dap-ui-locals)))))

  (add-hook 'dap-stopped-hook 'my/show-debug-windows)

  (defun my/hide-debug-windows (session)
    "Hide debug windows when all debug sessions are dead."
    (unless (-filter 'dap--session-running (dap--get-sessions))
      (and (get-buffer dap-ui--locals-buffer)
           (kill-buffer dap-ui--locals-buffer))))

  (add-hook 'dap-terminated-hook 'my/hide-debug-windows)

  )

;; (after! dap-mode


;;   ;; (set-popup-rule! "*dap-debug-.*" :side 'bottom :size .20 :slot 1)
;;   ;; (set-popup-rule! "*dap-ui-repl*" :side 'right :size .50 :select t :vslot 2)
;;   ;; (set-popup-rule! "*dap-ui-locals*" :side 'right :size .50)

;;   )

(after! dap-python
  (dap-register-debug-template "dap-debug-script"
                               (list :type "python"
                                     :args "-i"
                                     :cwd (lsp-workspace-root)
                                     :program nil ; (expand-file-name "~/git/blabla")
                                     :request "launch"
                                     :name "dap-debug-script"))

  (dap-register-debug-template "dap-debug-test"
                               (list :type "python"
                                     :cwd (lsp-workspace-root)
                                     ;; :environment-variables '(("PYTHONPATH" . "src"))
                                     :module "pytest"
                                     :request "launch"
                                     :name "dap-debug-test-file"))

  (dap-register-debug-template "dap-debug-bokeh"
                               (list :type "python"
                                     :args "--show crewrelief --log-level info"
                                     :cwd (expand-file-name "~/git/crewrelief/src")
                                     :program "serve"
                                     :module "bokeh"
                                     :request "launch"
                                     :name "dap-debug-bokeh"))


  )

(after! dap-python
  (defun dap-python-script ()
    (interactive
     (dap-debug
      (list :type "python"
            :args "-i"
            :cwd (lsp-workspace-root)
            :program nil
            :request "launch"
            :name "dap-debug-script")))))

(after! dap-python
  (require 'python-pytest)

  (defun dap-python-test-method-at-point ()
    (interactive
       (dap-debug
        (list :type "python"
              :args ""
              :cwd (lsp-workspace-root)
              :program (concat (buffer-file-name) ":" ":" (python-pytest--current-defun))
              :module "pytest"
              :request "launch"
              :name "dap-debug-test-function")))))

(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))
;; (after! dap-python
;;   (defun dap-python--pyenv-executable-find (command)
;;     (concat (getenv "VIRTUAL_ENV") "/bin/python")))

  ;; (set-company-backend! 'dap-ui-repl-mode 'company-capf)

;; (after! dap-mode

;;   (add-hook 'dap-ui-repl-mode-hook
;;             (lambda ()
;;               (setq-local company-minimum-prefix-length 0))))

(map! :after dap-python
    :map python-mode-map
    :localleader
    (:desc "debug" :prefix "d"
      :desc "Hydra" :n "h" #'dap-hydra
      :desc "Run debug configuration" :n "d" #'dap-debug
      :desc "dap-ui REPL" :n "r" #'dap-ui-repl
      :desc "Debug test function" :n "t" #'dap-python-test-method-at-point
      :desc "Run last debug configuration" :n "l" #'dap-debug-last
      :desc "Toggle breakpoint" :n "b" #'dap-breakpoint-toggle
      :desc "dap continue" :n "c" #'dap-continue
      :desc "dap next" :n "n" #'dap-next
      :desc "Debug script" :n "s" #'dap-python-script
      :desc "dap step in" :n "i" #'dap-step-in
      :desc "dap eval at point" :n "e" #'dap-eval-thing-at-point
      :desc "Disconnect" :n "q" #'dap-disconnect ))

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

(after! ess-r-mode
  (appendq! +pretty-code-symbols
            '(:assign "⟵"
              :multiply "×"))
  (set-pretty-symbols! 'ess-r-mode
    ;; Functional
    :def "function"
    ;; Types
    :null "NULL"
    :true "TRUE"
    :false "FALSE"
    :int "int"
    :floar "float"
    :bool "bool"
    ;; Flow
    :not "!"
    :and "&&" :or "||"
    :for "for"
    :in "%in%"
    :return "return"
    ;; Other
    :assign "<-"
    :multiply "%*%"))

;; (after! lsp-mode
;;   (add-hook! clojure-mode (setq lsp-diagnostic-package :flycheck)))

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
      "TAB"     #'company-complete-common-or-cycle
      [tab]     #'company-complete-common-or-cycle
      [backtab] #'company-select-previous    ))
  )

(add-hook! cider-repl-mode #'evil-normalize-keymaps)

(after! smartparens
  ;; (add-hook! clojure-mode #'smartparens-strict-mode)

  (setq evil-cleverparens-use-s-and-S nil)

  (use-package! evil-cleverparens
    :init
    (setq evil-move-beyond-eol t
          evil-cleverparens-use-additional-bindings nil
          ;; evil-cleverparens-swap-move-by-word-and-symbol t
          ;; evil-cleverparens-use-regular-insert t
          )

    (add-hook! clojure-mode #'evil-cleverparens-mode)
    ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
    ))

(after! clojure-mode
  (use-package! aggressive-indent
    :config (add-hook! clojure-mode (aggressive-indent-mode 1))))

(map! :after evil-cleverparens
      :map clojure-mode-map
      :localleader
      (:desc "Wrap round" :n "(" #'sp-wrap-round
       :desc "Wrap square" :n "[" #'sp-wrap-square
       :desc "Wrap curly" :n "{" #'sp-wrap-curly
       :desc "Unwrap sexp" :n "u" #'sp-unwrap-sexp
       ))

;; (after! lispyville
;;   (setq lispyville-key-theme
;;         '((operators normal)
;;           ;; c-w
;;           (prettify insert)
;;           (atom-movement normal visual)
;;           slurp/barf-lispy
;;           additional
;;           ;; additional-insert
;;           additional-wrap
;;           additional-motions))

;;   ;; (setq lispyville-motions-put-into-special t)

;;   (map! :mode lispy-mode
;;         :after lispyville
;;         ;; :i "M-[" #'lispy-brackets
;;         :n "[" #'lispyville-previous-opening
;;         :n "]" #'lispyville-next-opening)

;;   (map! :map lispy-mode-map
;;         :after lispyville
;;         :i "[" #'lispy-brackets)

;;   ;; (map! :map evil-motion-state-map
;;   ;;         :n "[[" #'lispyville-previous-opening)
;;   )

;; (after! cider
;;   (use-package! vega-view
;;     :init
;;     (setq vega-view-prefer-png t)))

(after! cider
 (setq nrepl-sync-request-timeout nil))

(after! clojure-mode
  (setq clojure-align-forms-automatically t))

;; (after! cider
;;   (eval-after-load 'clojure-mode
;;     '(sayid-setup-package)))

;; (use-package sayid
;;     :defer t
;;     :init
;;     (progn
;;       (setq sayid--key-binding-prefixes
;;             '(("mdt" . "trace")))
;;       (spacemacs|forall-clojure-modes m
;;         (mapc (lambda (x) (spacemacs/declare-prefix-for-mode m
;;                             (car x) (cdr x)))
;;               sayid--key-binding-prefixes)
;;         (spacemacs/set-leader-keys-for-major-mode m
;;           ;;These keybindings mostly preserved from the default sayid bindings
;;           "d!" 'sayid-load-enable-clear
;;           "dE" 'sayid-eval-last-sexp ;in default sayid bindings this is lowercase e, but that was already used in clojure mode
;;           "dc" 'sayid-clear-log
;;           "df" 'sayid-query-form-at-point
;;           "dh" 'sayid-show-help
;;           "ds" 'sayid-show-traced
;;           "dS" 'sayid-show-traced-ns
;;           "dtb" 'sayid-trace-ns-in-file
;;           "dtd" 'sayid-trace-fn-disable
;;           "dtD" 'sayid-trace-disable-all
;;           "dte" 'sayid-trace-fn-enable
;;           "dtE" 'sayid-trace-enable-all
;;           "dtK" 'sayid-kill-all-traces
;;           "dtn" 'sayid-inner-trace-fn
;;           "dto" 'sayid-outer-trace-fn
;;           "dtp" 'sayid-trace-ns-by-pattern
;;           "dtr" 'sayid-remove-trace-fn
;;           "dty" 'sayid-trace-all-ns-in-dir
;;           "dV" 'sayid-set-view
;;           "dw" 'sayid-get-workspace
;;           "dx" 'sayid-reset-workspace
;;           ))

;;       (evilified-state-evilify sayid-mode sayid-mode-map
;;         (kbd "H") 'sayid-buf-show-help
;;         (kbd "n") 'sayid-buffer-nav-to-next
;;         (kbd "N") 'sayid-buffer-nav-to-prev
;;         (kbd "C-s v") 'sayid-toggle-view
;;         (kbd "C-s V") 'sayid-set-view
;;         (kbd "L") 'sayid-buf-back
;;         (kbd "e") 'sayid-gen-instance-expr) ;Originally this was bound to 'g', but I feel this is still mnemonic and doesn't overlap with evil

;;       (evilified-state-evilify sayid-pprint-mode sayid-pprint-mode-map
;;         (kbd "h") 'sayid-pprint-buf-show-help
;;         (kbd "n") 'sayid-pprint-buf-next
;;         (kbd "N") 'sayid-pprint-buf-prev
;;         (kbd "l") 'sayid-pprint-buf-exit)

;;       (evilified-state-evilify sayid-traced-mode sayid-traced-mode-map
;;         (kbd "l") 'sayid-show-traced
;;         (kbd "h") 'sayid-traced-buf-show-help)))

(map! :after cider
      :map clojure-mode-map
      :localleader
      (:desc "eval" :prefix "e"
       :desc "sexp in comment" :n "E" #'cider-pprint-eval-last-sexp-to-comment
       :desc "defun in comment" :n "D" #'cider-pprint-eval-defun-to-comment
       ))

(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))

(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

  (set-popup-rule! "*Async Shell Command*" :side 'bottom :size .40)
  (set-popup-rule! "vterm" :side 'right :size .40 :quit 'current)

;; (after! counsel
  ;; :config
  ;; Thanks to https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-counsel.el
  ;; (setq counsel-rg-base-command "rg --with-filename --no-heading --line-number --hidden --color never %s"))
  ;; (setq counsel-rg-base-command (concat counsel-rg-base-command " --hidden")))
