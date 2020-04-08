(setq user-full-name "Luca Cambiaghi"
      user-mail-address "luca.cambiaghi@me.com")

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

(setq doom-font (font-spec :family "Menlo" :size 14))

;transparent adjustment
;; (set-frame-parameter (selected-frame)'alpha '(94 . 94))
;; (add-to-list 'default-frame-alist'(alpha . (94 . 94)))

(setq doom-theme 'doom-one)

(after! centaur-tabs
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs
        centaur-tabs-set-close-button nil)
  (centaur-tabs-group-by-projectile-project)
  (add-hook 'dired-mode-hook 'centaur-tabs-local-mode))

(after! winum
  ;; (defun winum-assign-0-to-treemacs ()
  ;;   (when (string-match-p (buffer-name) "*Treemacs*") 10))

  ;; (add-to-list 'winum-assign-functions #'winum-assign-0-to-treemacs)

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

;; (setq inhibit-compacting-font-caches t)

(setq magit-repository-directories '(("~/git" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit
      magit-inhibit-save-previous-winconf t)

(after! company
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-quickhelp-delay 0.4)
    (add-hook 'after-init-hook 'company-statistics-mode))

(set-company-backend! 'org-mode
  'company-capf
  'company-files
  'company-dabbrev-code)

(setq org-directory "~/git/org/"
      org-image-actual-width nil
      +org-export-directory "~/git/org/export/"
      org-default-notes-file "~/git/org/inbox.org"
      org-id-locations-file "~/git/org/.orgids"
      )

(load! "modules/ox-ravel")

(after! org

  (setq org-capture-templates
                  '(("d" "Diary")
                    ("u" "URL")))

  (add-to-list 'org-capture-templates
             '("dn" "New Diary Entry" entry(file+olp+datetree"~/git/org/personal/diary.org" "Daily Logs")
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
      '("un" "New URL Entry" entry(file+function "~/git/org/personal/dailies.org" org-reverse-datetree-goto-date-in-file)
            "* [[%^{URL}][%^{Description}]] %^g %?")))

(setq org-bullets-bullet-list '("✖" "✚")
      org-ellipsis "▼")
(set-pretty-symbols! 'org-mode
  :src_block "#+begin_src"
  :src_block_end "#+end_src")

(set-popup-rule! "*org agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3)

(after! evil-org
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                       (:pandoc t)
                                                       (:kernel . "python3")))
  (setq org-babel-default-header-args:jupyter-R '((:pandoc t)
                                                  (:kernel . "ir"))))

;; (:when (featurep! :lang +jupyter)
(map! :after evil-org
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
 :desc "Fold results" :n "z" #'org-babel-hide-result-toggle
 )

(set-popup-rule! "*jupyter-pager*" :side 'right :size .40 :select t :vslot 2 :ttl 3)
;; (after! jupyter (set-popup-rule! "^\\*Org Src*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(set-popup-rule! "^\\*Org Src*" :ignore t)

;; (setq org-image-actual-width t)

(require 'ox-ipynb)

(defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "ipython")))
        (apply orig-fn args))
    (apply orig-fn args)))

(add-hook! python-mode
  ;; (set-repl-handler! 'python-mode #'jupyter-repl-pop-to-buffer)
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl)
  )

(setq python-shell-prompt-detect-failure-warning nil)

(set-popup-rule! "^\\*Python*" :ignore t)

(after! python
  (setq python-shell-completion-native-enable nil))

(after! lsp-mode
  (setq lsp-auto-guess-root nil))

(setq read-process-output-max (* 1024 1024))

;; (after! lsp-mode
;;   (setq lsp-idle-delay 0.500))

(setq +lsp-company-backend 'company-capf)

;; (after! lsp-mode
;;   (setq lsp-prefer-capf t))

(set-popup-rule! "^\\*lsp-help" :side 'right :size .50 :select t :vslot 1)

(after! pyimport
  (setq pyimport-pyflakes-path "~/git/experiments/.venv/bin/pyflakes"))

(after! lsp-mode
  (setq lsp-diagnostic-package :flymake))

(after! python
  (setq python-flymake-command  "~/git/experiments/.venv/bin/pyflakes"))

(after! lsp-mode
  (setq lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;; lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil))
        ;; lsp-enable-file-watchers nil))

(after! python-pytest
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (set-popup-rule! "^\\*pytest*" :side 'right :size .50))

(after! dap-mode
  (setq dap-auto-show-output nil)
  ;; (set-popup-rule! "*dap-ui-locals*" :side 'right :size .50 :vslot 1)
  (set-popup-rule! "*dap-debug-.*" :side 'bottom :size .20 :slot 1)
  (set-popup-rule! "*dap-ui-repl*" :side 'right :size .40 :select t :slot 1)

  ;; (defun my/window-visible (b-name)
  ;;   "Return whether B-NAME is visible."
  ;;   (-> (-compose 'buffer-name 'window-buffer)
  ;;       (-map (window-list))
  ;;       (-contains? b-name)))

  ;; (defun my/show-debug-windows (session)
  ;;   "Show debug windows."
  ;;   (let ((lsp--cur-workspace (dap--debug-session-workspace session)))
  ;;       (save-excursion
  ;;       (unless (my/window-visible dap-ui--locals-buffer)
  ;;           (dap-ui-locals)))))

  ;;   (add-hook 'dap-stopped-hook 'my/show-debug-windows)

  ;;   (defun my/hide-debug-windows (session)
  ;;   "Hide debug windows when all debug sessions are dead."
  ;;   (unless (-filter 'dap--session-running (dap--get-sessions))
  ;;       (and (get-buffer dap-ui--locals-buffer)
  ;;           (kill-buffer dap-ui--locals-buffer))))

  ;;   (add-hook 'dap-terminated-hook 'my/hide-debug-windows)
  )

(map! :after dap-python
    :map python-mode-map
    :localleader
    (:desc "debug" :prefix "d"
      :desc "Hydra" :n "h" #'dap-hydra
      :desc "Run debug configuration" :n "d" #'dap-debug
      :desc "dap-ui REPL" :n "r" #'dap-ui-repl
      :desc "Edit debug template" :n "t" #'dap-debug-edit-template
      :desc "Run last debug configuration" :n "l" #'dap-debug-last
      :desc "Toggle breakpoint" :n "b" #'dap-breakpoint-toggle
      :desc "dap continue" :n "c" #'dap-continue
      :desc "dap next" :n "n" #'dap-next
      :desc "dap step in" :n "s" #'dap-step-in
      :desc "dap eval at point" :n "e" #'dap-eval-thing-at-point
      :desc "Disconnect" :n "q" #'dap-disconnect ))

(after! dap-python
    (dap-register-debug-template "dap-debug-script"
                            (list :type "python"
                                :args "-i"
                                :cwd (lsp-workspace-root)
                                :program nil ; (expand-file-name "~/git/blabla")
                                :environment-variables '(("PYTHONPATH" . "src"))
                                :request "launch"
                                :name "dap-debug-script"))

    (dap-register-debug-template "dap-debug-test"
                            (list :type "python"
                                :cwd (lsp-workspace-root)
                                :environment-variables '(("PYTHONPATH" . "src"))
                                :module "pytest"
                                :request "launch"
                                :name "dap-debug-test")))

(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))
;; (after! dap-python
;;   (defun dap-python--pyenv-executable-find (command)
;;     (concat (getenv "VIRTUAL_ENV") "/bin/python")))

(after! dap-mode
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl)

  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1))))

(after! dap-mode
  (dap-tooltip-mode 1)
  (tooltip-mode 1))

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

(set-popup-rule! "*compilation*" :side 'right :size .50 :select t :vslot 2 :ttl 3)

(set-popup-rule! "^\\*R:" :ignore t)

(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))

(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

(set-popup-rule! "*Async Shell Command*" :side 'bottom :size .40 :ttl 3)
(set-popup-rule! "vterm" :side 'right :size .40 :ttl 3)
