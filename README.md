# Preamble

This is my private DOOM emacs configuration. It is tangled by the
`config.org` and exported to markdown to produce this README.

``` bash
git clone https://github.com/lccambiaghi/doom.d.git ~/.doom.d/
```

Username and e-mail:

``` commonlisp
(setq user-full-name "Luca Cambiaghi"
      user-mail-address "luca.cambiaghi@me.com")
```

Scratch buffer major mode:

``` commonlisp
(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode)
```

# Keybindings

Let's have `general` auto-unbind keys:

``` commonlisp
(general-auto-unbind-keys)
```

We then remap some of the bindings (inspired by
[bindings.el](https://github.com/jsmestad/dfiles/blob/master/.doom.d/%2Bbindings.el#L496-L854)).

``` commonlisp
(map! :leader
      :desc "M-x"                   :n "SPC" #'counsel-M-x
      :desc "Async shell command"   :n "!"   #'async-shell-command
      :desc "Toggle eshell"         :n "'"   #'+eshell/toggle

      (:desc "windows" :prefix "w"
        :desc "Cycle focus to other window(s)" :n "TAB" #'other-window
        :desc "popup raise" :n "p" #'+popup/raise)

      (:desc "open" :prefix "o"
        :desc "Terminal"              :n  "t" #'+term/toggle
        :desc "Eshell"                :n  "e" #'+eshell/toggle )
      (:desc "project" :prefix "p"
        :desc "Eshell"               :n "'" #'projectile-run-eshell )
)
```

# User Interface

## Turn off line numbers

``` commonlisp
(setq display-line-numbers-type nil)
```

## Font and font size:

``` commonlisp
(setq doom-font (font-spec :family "Menlo" :size 14))
```

## Transparency

``` commonlisp
;transparent adjustment
(set-frame-parameter (selected-frame)'alpha '(94 . 94))
(add-to-list 'default-frame-alist'(alpha . (94 . 94)))
```

## Theme:

``` commonlisp
(setq doom-theme 'doom-vibrant)
```

## Centaur-tabs

``` commonlisp
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
```

## Winum

``` commonlisp
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
```

## Pretty code

``` commonlisp
(setq +pretty-code-enabled-modes '(org-mode))
```

## <span class="todo TODO">TODO</span> Golden ratio

``` commonlisp
;; add to ~/.doom.d/config.el
;; (use-package! golden-ratio
;;   :after-call pre-command-hook
;;   :config
;;   (golden-ratio-mode +1)
;;   ;; Using this hook for resizing windows is less precise than
;;   ;; `doom-switch-window-hook'.
;;   (remove-hook 'window-configuration-change-hook #'golden-ratio)
;;   (add-hook 'doom-switch-window-hook #'golden-ratio) )
```

## <span class="todo TODO">TODO</span> Ivy posframe

``` commonlisp
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
```

# Magit

``` commonlisp
(setq magit-repository-directories '(("~/git" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit
      magit-inhibit-save-previous-winconf t)
```

# Org

## Directories:

``` commonlisp
(setq org-directory "~/git/org-notes/"
      org-image-actual-width nil
      +org-export-directory "~/git/org-notes/export/"
      org-default-notes-file "~/git/org-notes/inbox.org"
      org-id-locations-file "~/git/org-notes/.orgids"
      )
```

## Export

Load `ox-ravel`:

``` commonlisp
(load! "modules/ox-ravel")
```

This allows to export from `.org` to `.Rmd`

## Capture

``` commonlisp
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
```

## Prettify

``` commonlisp
(setq org-bullets-bullet-list '("✖" "✚")
      org-ellipsis "▼")
```

## Popups: capture and agenda

``` commonlisp
(after! org (set-popup-rule! "^Capture.*\\.org$" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(after! org (set-popup-rule! "*org agenda*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
```

## org-babel

### Default header arguments for `jupyter-python`:

``` commonlisp
(after! evil-org
  (setq org-babel-default-header-args:jupyter-python '((:async . "yes")
                                                        (:pandoc t)
                                                        (:kernel . "python3"))))
```

### <span class="todo TODO">TODO</span> Override default python src block

``` commonlisp
  ;; (add-hook! '+org-babel-load-functions
  ;;   (λ! ()
  ;;       (require 'ob-jupyter  "/Users/luca/.emacs.d/.local/straight/repos/emacs-jupyter/ob-jupyter.el" nil)
  ;;       (org-babel-jupyter-override-src-block "python"))
  ;; )
;; (org-babel-jupyter-restore-src-block "python")
```

### <span class="todo TODO">TODO</span> Company backend

``` commonlisp
;; (after! org
;;   (set-company-backend! 'org-mode
;;     '(company-capf)))

;; (defun add-pcomplete-to-capf ()
;;   (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

;; (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
```

### Key bindings:

``` commonlisp
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
```

### Popups: pager and Org Src

``` commonlisp
(after! jupyter (set-popup-rule! "*jupyter-pager*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(after! jupyter (set-popup-rule! "^\\*Org Src*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
```

## ox-ipynb

``` commonlisp
(require 'ox-ipynb)
```

# Python

## iPython REPL

### virtualenv executable

``` commonlisp
(defadvice! +python-poetry-open-repl-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'+python/open-repl
  (if (getenv "VIRTUAL_ENV")
      (let ((python-shell-interpreter (executable-find "ipython")))
        (apply orig-fn args))
    (apply orig-fn args)))
```

### Set REPL handler

On a scratch buffer, first run `jupyter-associate-buffer`. Then, hitting
`SPC o r` allows use to hit the REPL buffer with the lines/regions of
code we send with `g r`.

``` commonlisp
(add-hook! python-mode
  ;; (set-repl-handler! 'python-mode #'jupyter-repl-pop-to-buffer)
  (set-repl-handler! 'python-mode #'+python/open-ipython-repl)
  )
```

### Silence warnings when opening REPL

``` commonlisp
(setq python-shell-prompt-detect-failure-warning nil)
```

### Ignore popup rule

``` emacs-lisp
(set-popup-rule! "^\\*Python*" :ignore t)
```

### <span class="todo TODO">TODO</span> iPython

``` emacs-lisp
;; (setq python-shell-interpreter "ipython")
```

### <span class="todo TODO">TODO</span> PYTHONPATH

``` emacs-lisp
;; (add-hook! python-mode
;;     (add-to-list python-shell-extra-pythonpaths (list (getenv "PYTHONPATH"))))
```

## LSP

### lsp-ui

``` emacs-lisp
(after! lsp-mode
  (setq lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil)
  )
```

### lsp-describe help popup

``` emacs-lisp
(set-popup-rule! "*lsp-help*" :side 'right :size .50 :select t :vslot 1)
```

### Missing imports

In python mode, use `, i i` to add missing imports

``` emacs-lisp
(after! pyimport
  (setq pyimport-pyflakes-path "~/git/experiments/.venv/bin/pyflakes"))
```

### Bindings

``` emacs-lisp
(map! :after lsp
      :map python-mode-map
      :localleader
      :desc "doc" :n "d" #'lsp-describe-thing-at-point
      :desc "rename" :n "r" #'lsp-rename
        )
```

### <span class="todo TODO">TODO</span> direnv

``` emacs-lisp
;; (after! direnv
;;   (add-hook! python-mode #'direnv-update-directory-environment ))
```

### <span class="todo TODO">TODO</span> remote python

Add in `.dir-locals.el`:

``` emacs-lisp
;; ((nil . ((ssh-deploy-root-remote . "/ssh:luca@ricko-ds.westeurope.cloudapp.azure.com:/mnt/data/luca/emptiesforecast"))))
```

``` emacs-lisp
;; (after! lsp
;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection "~/.pyenv/shims/pyls")
;;                     :major-modes '(python-mode)
;;                     :remote? t
;;                     :server-id 'pyls-remote)))
```

## Pytest

``` emacs-lisp
(after! python-pytest
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (set-popup-rule! "^\\*pytest*" :side 'right :size .50))
```

## dap-mode

### dap-ui windows

``` emacs-lisp
(after! dap-mode
  (setq dap-auto-show-output nil)
  (set-popup-rule! "*dap-ui-locals*" :side 'right :width .50 :vslot 1)
  (set-popup-rule! "*dap-debug-.*" :side 'bottom :size .30 :slot 1)
  (set-popup-rule! "*dap-ui-repl*" :side 'bottom :size .30 :select t :slot 1)

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
```

### Bindings

``` emacs-lisp
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
    ))
```

### Debug templates:

``` emacs-lisp
(after! dap-python
    (dap-register-debug-template "dap-debug-script"
                            (list :type "python"
                                :args "-i"
                                :cwd (lsp-workspace-root)
                                :program nil
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
```

### <span class="todo TODO">TODO</span> debug provider

Custom debug provider which prepends `PYTHONPATH`

``` emacs-lisp
;; (after! dap-mode
  ;; (defun my/dap-python--pyenv-executable-find (command)
  ;;   (concat (getenv "VIRTUAL_ENV") "/bin/python"))

    ;; (defun my/dap-python--populate-start-file-args (conf)
    ;;     "Populate CONF with the required arguments."
    ;;     (let* ((host "localhost")
    ;;             (debug-port (dap--find-available-port))
    ;;             (python-executable (my/dap-python--pyenv-executable-find dap-python-executable))
    ;;             (python-args (or (plist-get conf :args) ""))
    ;;             (program (or (plist-get conf :target-module)
    ;;                         (plist-get conf :program)
    ;;                         (buffer-file-name)))
    ;;             (module (plist-get conf :module)))

    ;;         (plist-put conf :program-to-start
    ;;                 (format "%s %s%s -m ptvsd --wait --host %s --port %s %s %s %s"
    ;;                         (concat "PYTHONPATH=" (getenv "PYTHONPATH"))
    ;;                         (or dap-python-terminal "")
    ;;                         (shell-quote-argument python-executable)
    ;;                         host
    ;;                         debug-port
    ;;                         (if module (concat "-m " (shell-quote-argument module)) "")
    ;;                         (shell-quote-argument program)
    ;;                         python-args))
    ;;         (plist-put conf :program program)
    ;;         (plist-put conf :debugServer debug-port)
    ;;         (plist-put conf :port debug-port)
    ;;         (plist-put conf :hostName host)
    ;;         (plist-put conf :host host)
    ;;         conf))

    ;; (dap-register-debug-provider "my/python" 'my/dap-python--populate-start-file-args)

    ;; (dap-register-debug-template "my/python"
    ;;                          (list :type "my/python"
    ;;                                ;; :cwd "/Users/luca/git/emptiesforecast"
    ;;                                :cwd (poetry-find-project-root)
    ;;                                :request "launch"
    ;;                                :name "Python :: Run Configuration")))
```

### virtualenv executable

``` emacs-lisp
(defadvice! +dap-python-poetry-executable-find-a (orig-fn &rest args)
  "Use the Python binary from the current virtual environment."
  :around #'dap-python--pyenv-executable-find
  (if (getenv "VIRTUAL_ENV")
      (executable-find (car args))
    (apply orig-fn args)))
;; (after! dap-python
;;   (defun dap-python--pyenv-executable-find (command)
;;     (concat (getenv "VIRTUAL_ENV") "/bin/python")))
```

### completion

``` emacs-lisp
(after! dap-mode
  (set-company-backend! 'dap-ui-repl-mode 'company-dap-ui-repl)

  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 1)))
  )
```

## Jupyter Notebook

### Don't ignore `ein` buffers

``` emacs-lisp
(after! ein
  (set-popup-rule! "^\\*ein" :ignore t))
```

### Bindings

Bindings, inspired by[
this](https://github.com/millejoh/emacs-ipython-notebook/wiki/Spacemacs-Evil-Bindings).

``` emacs-lisp

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
```

## docsets

``` emacs-lisp
(set-docsets! 'python-mode "Python 3" "NumPy" "Pandas")
```

# R

## R console in a buffer

Disable popup for ESS:

``` emacs-lisp
(set-popup-rule! "^\\*R:" :ignore t)
```

# Shell

## Async Shell command

``` emacs-lisp
(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))

(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)
```

## Eshell aliases

``` emacs-lisp
(after! eshell
  (set-eshell-alias!
   "fd" "+eshell/fd $1"
   "fo" "find-file-other-window $1"))
```
