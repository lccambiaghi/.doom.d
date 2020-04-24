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

(setq doom-font (font-spec :family "Menlo" :size 14)
      doom-big-font (font-spec :family "Menlo" :size 19))
;; (setq doom-font (font-spec :family "Fira Code" :size 14)
;;       doom-big-font (font-spec :family "Fira Code" :size 22)
;; doom-variable-pitch-font (font-spec :family "Overpass" :size 16))

(after! which-key
    (setq which-key-idle-delay 0.5))

;transparent adjustment
;; (set-frame-parameter (selected-frame)'alpha '(94 . 94))
;; (add-to-list 'default-frame-alist'(alpha . (94 . 94)))

(setq doom-theme 'doom-one)

(after! doom-themes
  (custom-theme-set-faces! 'doom-one-theme
    ;; `(swiper-line-face            ((,c (:background ,blue-d))))
    ;; `(swiper-match-face-1         ((,c (:background ,grey-d :foreground ,black))))
    ;; `(swiper-match-face-2         ((,c (:background ,green :foreground ,black))))
    ;; `(swiper-match-face-3         ((,c (:background ,orange :foreground ,black))))
    ;; `(swiper-match-face-4         ((,c (:background ,magenta :foreground ,black))))

    ;; `(swiper-background-match-face-1         ((,c (:background ,black :foreground ,light-grey))))
    ;; `(swiper-background-match-face-2         ((,c (:background ,green :foreground ,black))))
    ;; `(swiper-background-match-face-3         ((,c (:background ,orange :foreground ,black))))
    ;; `(swiper-background-match-face-4         ((,c (:background ,magenta :foreground ,black))))

    ))

(setq +doom-dashboard-banner-file
      (expand-file-name "splash-images/black-hole2.png" doom-private-dir))

;; (defvar fancy-splash-image-template
;;   (expand-file-name "splash-images/blackhole-lines-template.svg" doom-private-dir)
;;   "Default template svg used for the splash image, with substitutions from ")
;; (defvar fancy-splash-image-nil
;;   (expand-file-name "splash-images/transparent-pixel.png" doom-private-dir)
;;   "An image to use at minimum size, usually a transparent pixel")

;; (setq fancy-splash-sizes
;;   `((:height 500 :min-height 50 :padding (0 . 2) :template ,(expand-file-name "splash-images/blackhole-lines-0.svg" doom-private-dir))
;;     (:height 440 :min-height 42 :padding (1 . 4) :template ,(expand-file-name "splash-images/blackhole-lines-0.svg" doom-private-dir))
;;     (:height 400 :min-height 38 :padding (1 . 4) :template ,(expand-file-name "splash-images/blackhole-lines-1.svg" doom-private-dir))
;;     (:height 350 :min-height 36 :padding (1 . 3) :template ,(expand-file-name "splash-images/blackhole-lines-2.svg" doom-private-dir))
;;     (:height 300 :min-height 34 :padding (1 . 3) :template ,(expand-file-name "splash-images/blackhole-lines-3.svg" doom-private-dir))
;;     (:height 250 :min-height 32 :padding (1 . 2) :template ,(expand-file-name "splash-images/blackhole-lines-4.svg" doom-private-dir))
;;     (:height 200 :min-height 30 :padding (1 . 2) :template ,(expand-file-name "splash-images/blackhole-lines-5.svg" doom-private-dir))
;;     (:height 100 :min-height 24 :padding (1 . 2) :template ,(expand-file-name "splash-images/emacs-e-template.svg" doom-private-dir))
;;     (:height 0   :min-height 0  :padding (0 . 0) :file ,fancy-splash-image-nil)))

;; (defvar fancy-splash-sizes
;;   `((:height 500 :min-height 50 :padding (0 . 2))
;;     (:height 440 :min-height 42 :padding (1 . 4))
;;     (:height 330 :min-height 35 :padding (1 . 3))
;;     (:height 200 :min-height 30 :padding (1 . 2))
;;     (:height 0   :min-height 0  :padding (0 . 0) :file ,fancy-splash-image-nil))
;;   "list of plists with the following properties
;;   :height the height of the image
;;   :min-height minimum `frame-height' for image
;;   :padding `+doom-dashboard-banner-padding' to apply
;;   :template non-default template file
;;   :file file to use instead of template")

;; (defvar fancy-splash-template-colours
;;   '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
;;   "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

;; (unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
;;   (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

;; (defun fancy-splash-filename (theme-name height)
;;   (expand-file-name (concat (file-name-as-directory "theme-splashes")
;;                             (symbol-name doom-theme)
;;                             "-" (number-to-string height) ".svg")
;;                     doom-cache-dir))

;; (defun fancy-splash-clear-cache ()
;;   "Delete all cached fancy splash images"
;;   (interactive)
;;   (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
;;   (message "Cache cleared!"))

;; (defun fancy-splash-generate-image (template height)
;;   "Read TEMPLATE and create an image if HEIGHT with colour substitutions as  ;described by `fancy-splash-template-colours' for the current theme"
;;     (with-temp-buffer
;;       (insert-file-contents template)
;;       (re-search-forward "$height" nil t)
;;       (replace-match (number-to-string height) nil nil)
;;       (dolist (substitution fancy-splash-template-colours)
;;         (beginning-of-buffer)
;;         (while (re-search-forward (car substitution) nil t)
;;           (replace-match (doom-color (cdr substitution)) nil nil)))
;;       (write-region nil nil
;;                     (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

;; (defun fancy-splash-generate-images ()
;;   "Perform `fancy-splash-generate-image' in bulk"
;;   (dolist (size fancy-splash-sizes)
;;     (unless (plist-get size :file)
;;       (fancy-splash-generate-image (or (plist-get size :file)
;;                                        (plist-get size :template)
;;                                        fancy-splash-image-template)
;;                                    (plist-get size :height)))))

;; (defun ensure-theme-splash-images-exist (&optional height)
;;   (unless (file-exists-p (fancy-splash-filename
;;                           (symbol-name doom-theme)
;;                           (or height
;;                               (plist-get (car fancy-splash-sizes) :height))))
;;     (fancy-splash-generate-images)))

;; (defun get-appropriate-splash ()
;;   (let ((height (frame-height)))
;;     (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
;;              fancy-splash-sizes)))

;; (setq fancy-splash-last-size nil)
;; (setq fancy-splash-last-theme nil)
;; (defun set-appropriate-splash (&optional frame)
;;   (let ((appropriate-image (get-appropriate-splash)))
;;     (unless (and (equal appropriate-image fancy-splash-last-size)
;;                  (equal doom-theme fancy-splash-last-theme)))
;;     (unless (plist-get appropriate-image :file)
;;       (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
;;     (setq fancy-splash-image
;;           (or (plist-get appropriate-image :file)
;;               (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
;;     (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
;;     (setq fancy-splash-last-size appropriate-image)
;;     (setq fancy-splash-last-theme doom-theme)
;;     (+doom-dashboard-reload)))

;; (add-hook 'window-size-change-functions #'set-appropriate-splash)
;; (add-hook 'doom-load-theme-hook #'set-appropriate-splash)

;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

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

(after! doom-modeline
    (setq display-time-default-load-average nil)      ; don't show load average
    (display-time-mode 1)                             ; Enable time in the mode-line
    (display-battery-mode 1))                          ; On laptops it's nice to know how much power you have

(setq evil-split-window-below t
      evil-vsplit-window-right t)

(setq magit-repository-directories '(("~/git" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit
      magit-inhibit-save-previous-winconf t)

(after! company
  (setq company-idle-delay 0
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
  (setq company-backends '(company-tabnine company-capf)))

;; (after! company
;;   (add-to-list 'company-backends 'company-tabnine))

(setq org-directory "~/git/org/"
      org-image-actual-width nil
      +org-export-directory "~/git/org/export/"
      org-default-notes-file "~/git/org/inbox.org"
      org-id-locations-file "~/git/org/.orgids"
      org-export-in-background t
      org-catch-invisible-edits 'smart)

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

(use-package! doct
  :commands (doct))

;; (after! org-capture
;;   (defun org-capture-select-template-prettier (&optional keys)
;;     "Select a capture template, in a prettier way than default
;;   Lisp programs can force the template by setting KEYS to a string."
;;     (let ((org-capture-templates
;;            (or (org-contextualize-keys
;;                 (org-capture-upgrade-templates org-capture-templates)
;;                 org-capture-templates-contexts)
;;                '(("t" "Task" entry (file+headline "" "Tasks")
;;                   "* TODO %?\n  %u\n  %a")))))
;;       (if keys
;;           (or (assoc keys org-capture-templates)
;;               (error "No capture template referred to by \"%s\" keys" keys))
;;         (org-mks org-capture-templates
;;                  "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
;;                  "Template key: "
;;                  `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))
;;   (advice-add 'org-capture-select-template :override #'org-capture-select-template-prettier)

;;   (defun org-mks-pretty (table title &optional prompt specials)
;;     "Select a member of an alist with multiple keys. Prettified.

;;   TABLE is the alist which should contain entries where the car is a string.
;;   There should be two types of entries.

;;   1. prefix descriptions like (\"a\" \"Description\")
;;      This indicates that `a' is a prefix key for multi-letter selection, and
;;      that there are entries following with keys like \"ab\", \"ax\"…

;;   2. Select-able members must have more than two elements, with the first
;;      being the string of keys that lead to selecting it, and the second a
;;      short description string of the item.

;;   The command will then make a temporary buffer listing all entries
;;   that can be selected with a single key, and all the single key
;;   prefixes.  When you press the key for a single-letter entry, it is selected.
;;   When you press a prefix key, the commands (and maybe further prefixes)
;;   under this key will be shown and offered for selection.

;;   TITLE will be placed over the selection in the temporary buffer,
;;   PROMPT will be used when prompting for a key.  SPECIALS is an
;;   alist with (\"key\" \"description\") entries.  When one of these
;;   is selected, only the bare key is returned."
;;     (save-window-excursion
;;       (let ((inhibit-quit t)
;;       (buffer (org-switch-to-buffer-other-window "*Org Select*"))
;;       (prompt (or prompt "Select: "))
;;       case-fold-search
;;       current)
;;         (unwind-protect
;;       (catch 'exit
;;         (while t
;;           (setq-local evil-normal-state-cursor (list nil))
;;           (erase-buffer)
;;           (insert title "\n\n")
;;           (let ((des-keys nil)
;;           (allowed-keys '("\C-g"))
;;           (tab-alternatives '("\s" "\t" "\r"))
;;           (cursor-type nil))
;;       ;; Populate allowed keys and descriptions keys
;;       ;; available with CURRENT selector.
;;       (let ((re (format "\\`%s\\(.\\)\\'"
;;             (if current (regexp-quote current) "")))
;;             (prefix (if current (concat current " ") "")))
;;         (dolist (entry table)
;;           (pcase entry
;;             ;; Description.
;;             (`(,(and key (pred (string-match re))) ,desc)
;;              (let ((k (match-string 1 key)))
;;          (push k des-keys)
;;          ;; Keys ending in tab, space or RET are equivalent.
;;          (if (member k tab-alternatives)
;;              (push "\t" allowed-keys)
;;            (push k allowed-keys))
;;          (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
;;             ;; Usable entry.
;;             (`(,(and key (pred (string-match re))) ,desc . ,_)
;;              (let ((k (match-string 1 key)))
;;          (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
;;          (push k allowed-keys)))
;;             (_ nil))))
;;       ;; Insert special entries, if any.
;;       (when specials
;;         (insert "─────────────────────────\n")
;;         (pcase-dolist (`(,key ,description) specials)
;;           (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
;;           (push key allowed-keys)))
;;       ;; Display UI and let user select an entry or
;;       ;; a sub-level prefix.
;;       (goto-char (point-min))
;;       (unless (pos-visible-in-window-p (point-max))
;;         (org-fit-window-to-buffer))
;;       (let ((pressed (org--mks-read-key allowed-keys prompt)))
;;         (setq current (concat current pressed))
;;         (cond
;;          ((equal pressed "\C-g") (user-error "Abort"))
;;          ;; Selection is a prefix: open a new menu.
;;          ((member pressed des-keys))
;;          ;; Selection matches an association: return it.
;;          ((let ((entry (assoc current table)))
;;             (and entry (throw 'exit entry))))
;;          ;; Selection matches a special entry: return the
;;          ;; selection prefix.
;;          ((assoc current specials) (throw 'exit current))
;;          (t (error "No entry available")))))))
;;     (when buffer (kill-buffer buffer))))))
;;   (advice-add 'org-mks :override #'org-mks-pretty)
;;   (setq +org-capture-uni-units (split-string (f-read-text "~/.org/.uni-units")))
;;   (add-transient-hook! 'org-capture-select-template
;;     (setq org-capture-templates
;;           (doct `((,(format "%s\tPersonal todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
;;                    :keys "t"
;;                    :file +org-capture-todo-file
;;                    :prepend t
;;                    :headline "Inbox"
;;                    :type entry
;;                    :template ("* TODO %?"
;;                               "%i %a")
;;                    )
;;                   (,(format "%s\tPersonal note" (all-the-icons-faicon "sticky-note-o" :face 'all-the-icons-green :v-adjust 0.01))
;;                    :keys "n"
;;                    :file +org-capture-todo-file
;;                    :prepend t
;;                    :headline "Inbox"
;;                    :type entry
;;                    :template ("* %?"
;;                               "%i %a")
;;                    )
;;                   (,(format "%s\tUniversity" (all-the-icons-faicon "graduation-cap" :face 'all-the-icons-purple :v-adjust 0.01))
;;                    :keys "u"
;;                    :file +org-capture-todo-file
;;                    :headline "University"
;;                    :unit-prompt ,(format "%%^{Unit|%s}" (string-join +org-capture-uni-units "|"))
;;                    :prepend t
;;                    :type entry
;;                    :children ((,(format "%s\tTest" (all-the-icons-material "timer" :face 'all-the-icons-red :v-adjust 0.01))
;;                                :keys "t"
;;                                :template ("* TODO [#C] %{unit-prompt} %? :uni:tests:"
;;                                           "SCHEDULED: %^{Test date:}T"
;;                                           "%i %a"))
;;                               (,(format "%s\tAssignment" (all-the-icons-material "library_books" :face 'all-the-icons-orange :v-adjust 0.01))
;;                                :keys "a"
;;                                :template ("* TODO [#B] %{unit-prompt} %? :uni:assignments:"
;;                                           "DEADLINE: %^{Due date:}T"
;;                                           "%i %a"))
;;                               (,(format "%s\tLecture" (all-the-icons-fileicon "keynote" :face 'all-the-icons-orange :v-adjust 0.01))
;;                                :keys "l"
;;                                :template ("* TODO [#C] %{unit-prompt} %? :uni:lecture:"
;;                                           "%i %a"))
;;                               (,(format "%s\tMiscellaneous task" (all-the-icons-faicon "list" :face 'all-the-icons-yellow :v-adjust 0.01))
;;                                :keys "u"
;;                                :template ("* TODO [#D] %{unit-prompt} %? :uni:"
;;                                           "%i %a"))))
;;                   (,(format "%s\tEmail" (all-the-icons-faicon "envelope" :face 'all-the-icons-blue :v-adjust 0.01))
;;                    :keys "e"
;;                    :file +org-capture-todo-file
;;                    :prepend t
;;                    :headline "Inbox"
;;                    :type entry
;;                    :template ("* TODO %^{type|reply to|contact} %\\3 %? :email:"
;;                               "Send an email %^{urgancy|soon|ASAP|anon|at some point|eventually} to %^{recipiant}"
;;                               "about %^{topic}"
;;                               "%U %i %a"))
;;                   (,(format "%s\tInteresting" (all-the-icons-faicon "eye" :face 'all-the-icons-lcyan :v-adjust 0.01))
;;                    :keys "i"
;;                    :file +org-capture-todo-file
;;                    :prepend t
;;                    :headline "Interesting"
;;                    :type entry
;;                    :template ("* [ ] %{desc}%? :%{i-type}:"
;;                               "%i %a")
;;                    :children ((,(format "%s\tWebpage" (all-the-icons-faicon "globe" :face 'all-the-icons-green :v-adjust 0.01))
;;                                :keys "w"
;;                                :desc "%(org-cliplink-capture) "
;;                                :i-type "read:web"
;;                                )
;;                               (,(format "%s\tArticle" (all-the-icons-octicon "file-text" :face 'all-the-icons-yellow :v-adjust 0.01))
;;                                :keys "a"
;;                                :desc ""
;;                                :i-type "read:reaserch"
;;                                )
;;                               (,(format "%s\tInformation" (all-the-icons-faicon "info-circle" :face 'all-the-icons-blue :v-adjust 0.01))
;;                                :keys "i"
;;                                :desc ""
;;                                :i-type "read:info"
;;                                )
;;                               (,(format "%s\tIdea" (all-the-icons-material "bubble_chart" :face 'all-the-icons-silver :v-adjust 0.01))
;;                                :keys "I"
;;                                :desc ""
;;                                :i-type "idea"
;;                                )))
;;                   (,(format "%s\tTasks" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
;;                    :keys "k"
;;                    :file +org-capture-todo-file
;;                    :prepend t
;;                    :headline "Tasks"
;;                    :type entry
;;                    :template ("* TODO %? %^G%{extra}"
;;                               "%i")
;;                    :children ((,(format "%s\tGeneral Task" (all-the-icons-octicon "inbox" :face 'all-the-icons-yellow :v-adjust 0.01))
;;                                :keys "k"
;;                                :extra ""
;;                                )
;;                               (,(format "%s\tTask with deadline" (all-the-icons-material "timer" :face 'all-the-icons-orange :v-adjust -0.1))
;;                                :keys "d"
;;                                :extra "\nDEADLINE: %^{Deadline:}t"
;;                                )
;;                               (,(format "%s\tScheduled Task" (all-the-icons-octicon "calendar" :face 'all-the-icons-orange :v-adjust 0.01))
;;                                :keys "s"
;;                                :extra "\nSCHEDULED: %^{Start time:}t"
;;                                )
;;                               ))
;;                   (,(format "%s\tProject" (all-the-icons-octicon "repo" :face 'all-the-icons-silver :v-adjust 0.01))
;;                    :keys "p"
;;                    :prepend t
;;                    :type entry
;;                    :headline "Inbox"
;;                    :template ("* %{time-or-todo} %?"
;;                               "%i"
;;                               "%a")
;;                    :file ""
;;                    :custom (:time-or-todo "")
;;                    :children ((,(format "%s\tProject-local todo" (all-the-icons-octicon "checklist" :face 'all-the-icons-green :v-adjust 0.01))
;;                                :keys "t"
;;                                :time-or-todo "TODO"
;;                                :file +org-capture-project-todo-file)
;;                               (,(format "%s\tProject-local note" (all-the-icons-faicon "sticky-note" :face 'all-the-icons-yellow :v-adjust 0.01))
;;                                :keys "n"
;;                                :time-or-todo "%U"
;;                                :file +org-capture-project-notes-file)
;;                               (,(format "%s\tProject-local changelog" (all-the-icons-faicon "list" :face 'all-the-icons-blue :v-adjust 0.01))
;;                                :keys "c"
;;                                :time-or-todo "%U"
;;                                :heading "Unreleased"
;;                                :file +org-capture-project-changelog-file))
;;                    )
;;                   ("\tCentralised project templates"
;;                    :keys "o"
;;                    :type entry
;;                    :prepend t
;;                    :template ("* %{time-or-todo} %?"
;;                               "%i"
;;                               "%a")
;;                    :children (("Project todo"
;;                                :keys "t"
;;                                :prepend nil
;;                                :time-or-todo "TODO"
;;                                :heading "Tasks"
;;                                :file +org-capture-central-project-todo-file)
;;                               ("Project note"
;;                                :keys "n"
;;                                :time-or-todo "%U"
;;                                :heading "Notes"
;;                                :file +org-capture-central-project-notes-file)
;;                               ("Project changelog"
;;                                :keys "c"
;;                                :time-or-todo "%U"
;;                                :heading "Unreleased"
;;                                :file +org-capture-central-project-changelog-file))
;;                    ))))))

(after! org-superstar
    (setq org-superstar-headline-bullets-list '("✖" "✚" "◆" "▶" "○")
        org-ellipsis "▼"))

;; (setq global-org-pretty-table-mode t)

;; (add-hook! 'org-mode-hook #'+org-pretty-mode)

;; (add-hook 'org-mode-hook 'org-fragtog-mode)

;; (setq org-format-latex-header "\\documentclass{article}
;; \\usepackage[usenames]{color}

;; \\usepackage[T1]{fontenc}
;; \\usepackage{mathtools}
;; \\usepackage{textcomp,amssymb}
;; \\usepackage[makeroom]{cancel}

;; \\pagestyle{empty}             % do not remove
;; % The settings below are copied from fullpage.sty
;; \\setlength{\\textwidth}{\\paperwidth}
;; \\addtolength{\\textwidth}{-3cm}
;; \\setlength{\\oddsidemargin}{1.5cm}
;; \\addtolength{\\oddsidemargin}{-2.54cm}
;; \\setlength{\\evensidemargin}{\\oddsidemargin}
;; \\setlength{\\textheight}{\\paperheight}
;; \\addtolength{\\textheight}{-\\headheight}
;; \\addtolength{\\textheight}{-\\headsep}
;; \\addtolength{\\textheight}{-\\footskip}
;; \\addtolength{\\textheight}{-3cm}
;; \\setlength{\\topmargin}{1.5cm}
;; \\addtolength{\\topmargin}{-2.54cm}
;; % my custom stuff
;; \\usepackage{arev}
;; \\usepackage{arevmath}")

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
      )

(set-popup-rule! "*jupyter-pager*" :side 'right :size .40 :select t :vslot 2 :ttl 3)
;; (after! jupyter (set-popup-rule! "^\\*Org Src*" :side 'right :size .40 :select t :vslot 2 :ttl 3))
(set-popup-rule! "^\\*Org Src*" :ignore t)

(after! evil-org
  (org-babel-lob-ingest "/Users/luca/git/experiments/literate/ml/rpy2.org"))

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

;; (after! python
;;   (setq python-flymake-command  "~/git/experiments/.venv/bin/pyflakes"))

;; (after! flycheck
;;   (setq-default flycheck-disabled-checkers '(python-flake8)))

(after! lsp-mode
  (setq lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        ;; lsp-enable-on-type-formatting nil
        lsp-enable-symbol-highlighting nil))
        ;; lsp-enable-file-watchers nil))

(after! python-pytest
  (setq python-pytest-arguments '("--color" "--failed-first"))
  (evil-set-initial-state 'python-pytest-mode 'normal))

(set-popup-rule! "^\\*pytest*" :side 'right :size .50)

(after! dap-mode
  (setq dap-auto-show-output nil)

  (setq dap-ui-buffer-configurations
        `((,"*dap-ui-locals*"  . ((side . right) (slot . 1) (window-width . 0.50))) ;; changed this to 0.50
          (,"*dap-ui-repl*" . ((side . bottom) (slot . 2) (window-width . 0.50)))
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 0.20)))
          (,"*dap-ui-sessions*" . ((side . right) (slot . 3) (window-width . 0.20)))
          (,"*dap-ui-breakpoints*" . ((side . left) (slot . 2) (window-width . , 0.20)))
          (,"*debug-window*" . ((side . bottom) (slot . 3) (window-width . 0.20)))))

  ;; (set-popup-rule! "*dap-debug-.*" :side 'bottom :size .20 :slot 1)
  ;; (set-popup-rule! "*dap-ui-repl*" :side 'right :size .50 :select t :vslot 2)
  ;; (set-popup-rule! "*dap-ui-locals*" :side 'right :size .50)

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
                                :name "dap-debug-test-file")))

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

(after! dap-mode
  ;; (set-company-backend! 'dap-ui-repl-mode 'company-capf)

  (add-hook 'dap-ui-repl-mode-hook
            (lambda ()
              (setq-local company-minimum-prefix-length 0))))

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

(set-popup-rule! "*compilation*" :side 'right :size .50 :select t :vslot 2 :ttl nil)

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

(defun shell-command-print-separator ()
  (overlay-put (make-overlay (point-max) (point-max))
               'before-string
               (propertize "!" 'display
                           (list 'left-fringe
                                 'right-triangle))))

(advice-add 'shell-command--save-pos-or-erase :after 'shell-command-print-separator)

(set-popup-rule! "*Async Shell Command*" :side 'bottom :size .40)
(set-popup-rule! "vterm" :side 'right :size .40 :ttl nil)

(after! counsel
  ;; :config
  ;; Thanks to https://github.com/kaushalmodi/.emacs.d/blob/master/setup-files/setup-counsel.el
  ;; (setq counsel-rg-base-command "rg --with-filename --no-heading --line-number --hidden --color never %s"))
  (setq counsel-rg-base-command (concat counsel-rg-base-command " --hidden")))
