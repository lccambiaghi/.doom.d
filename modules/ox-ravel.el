;;; ox-ravel.el --- Sweave/knit/brew document maker for orgmode
;; Copyright (C) 2012---2016  Charles C. Berry

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;              Several exporters are provided for translating from
;;              Org to formats for reproducible research using
;;              document generating engines such as Sweave, brew,
;;              knitr, rmarkdown, et cetera. Typically, R src blocks
;;              are converted to `code chunks' in the desired format
;;              and the rest of the Org document is translated to
;;              latex, html, markdown, or some other document format.
;;
;;              See ox-ravel.org in the orgmode-accessories archive on
;;              github for details.  Also see demos.org and other
;;              *.org files for examples of usage.

;;; Code:
;;

;;; Requisites and Declarations
(eval-when-compile (require 'cl-lib))
(require 'ox)

(declare-function org-babel-expand-body:R "ob-R.el" )

;; defconst-org-babel-header-args:ravel
;; #+NAME: defconst-org-babel-header-args-ravel

(defconst org-babel-header-args:ravel
  '(
    (ravel               . :any)
    (ravel-style         . :any)
    (engine              . :any))
  "Ravel-specific header arguments.")

;; org-lint org-lint needs these
(eval-after-load 'ob-core
  '(mapc (lambda (x)
	   (add-to-list
	    'org-babel-common-header-args-w-values x))
	 org-babel-header-args:ravel))

(eval-after-load 'ob-core
  '(mapc (lambda (x)
	   (add-to-list
	    'org-babel-header-arg-names (car x)))
	 org-babel-header-args:ravel))

;; defvar-org-ravel-style

;; #+NAME: defvar-org-ravel-style

(defvar org-ravel-style nil
  "The default style to use for constructing chunks.
Can be buffer-local, and is usually set by the export dispatcher.")

(make-variable-buffer-local 'org-ravel-style)

;; defvar-org-ravel-run

;; #+NAME: defvar-org-ravel-run

(defvar-local org-ravel-run nil
  "If ravel is to be run on src blocks, this will be a list like

       '(\"R\") or '(\"R\" \"python\" \"awk\")

and usually set (by the export dispatcher) to `org-ravel-engines'.

Set this as buffer/file local for demos or debugging.")

;; defcustom-org-ravel-engines

;; #+NAME: defcustom-org-ravel-engines

(defcustom org-ravel-engines '(("R"))
  "Use these engines in forming ravel chunks.

Typically, `org-ravel-run' will default to these.  It can be
buffer-local.  These engines are recognized by `knitr':

`R' `python' `awk' `ruby' `haskell' `bash' `perl' `dot'
 `tikz' `sas' `coffeescript', `c', `Rcpp', and `polyglot'.

Each alist CONS cell has the language (as a string) for the CAR and
any cdr is cons-ed to the ravel attributes.

Buffer local values are allowed."

 :group 'org-export-ravel

 :type '(set :greedy t
             (const :tag "   R" ("R") )
             (const :tag "   c" ("c" . "engine='c'"))
	     (const :tag "   rcpp" ("c++" . "engine='Rcpp'"))
             (const :tag "   C" ("C" . "engine='c'"))
	     (const :tag "   Rcpp" ("C++" . "engine='Rcpp'"))
             (const :tag "   Python" ("python" . "engine='python'"))
             (const :tag "   AWK" ("awk" . "engine='awk'"))
             (const :tag "   Ruby" ("ruby" . "engine='ruby'"))
             (const :tag "   Haskell" ("haskell" . "engine='haskell'"))
             (const :tag "   bash" ("bash" . "engine='bash'"))
             (const :tag "   perl" ("perl" . "engine='perl'"))
             (const :tag "   dot" ("dot" . "engine='dot'"))
             (const :tag "   TikZ" ("tikz" . "engine='tikz'"))
             (const :tag "   SAS" ("sas" . "engine='sas'"))
             (const :tag "   CoffeeScript"
                    ("coffeescript" . "engine='coffeescript'"))
             (const :tag "   Polyglot" ("polyglot" . "engine='polyglot'"))
             (cons  :tag "   Other"  string  string)))


(make-variable-buffer-local 'org-ravel-engines)

;; defvar-org-ravel-style-alist


;; #+NAME: defcustom-org-ravel-style-alist

(defgroup org-export-ravel nil
    "Options for exporting Org mode files via Ravel."
    :tag "Org Export Ravel"
    :group 'org-export)

(defcustom org-ravel-style-alist
  '((rnw . (org-ravel-block-rnw org-ravel-inline-rnw ".Rnw"))
    (brew . (org-ravel-block-brew org-ravel-inline-brew ".Rbrew"))
    (tex  . (org-ravel-block-tex org-ravel-inline-tex ".Rtex"))
    (html . (org-ravel-block-html org-ravel-inline-html ".Rhtml"))
    (md   . (org-ravel-block-md org-ravel-inline-md ".Rmd"))
    (braces   . (org-ravel-block-braces org-ravel-inline-braces ".Rtmpl"))
    (rst  . (org-ravel-block-rst org-ravel-inline-rst ".Rrst")))
  "The Chunk Style Alist to use in formatting Ravel output.

The key of each element is matched by the `:ravel-style' property
of a document, if specified, or by the default `:ravel-style' of
the exporter selected.

The value of each pair is a list of three elements:
  - the function that formats src blocks
  - the function that formats inline src blocks
  - a string giving the file extension. "
  :group 'org-export-ravel
  :type '(alist
          :key-type (symbol :tag "Ravel Style")
          :value--type (list :tag "Chunk Defn"
                             (function :tag "block coder")
                             (function :tag "inline coder")
                             (string :tag "File extension"))))

;; defvar-org-ravel-backend-parent 


(defvar org-ravel-backend-parent nil
  "If ravel is running, this variable will contain the name of the parent.")

;; defun-org-babel-expand-body:ravel



(defun org-babel-expand-body:ravel (body params &optional var-lines)
  "Use native `org-babel-expand-body' for src-block engine if
  there is one to format BODY as per PARAMS."
  (let*
((engine-cdr (cdr (assq :engine params)))
 (engine (and engine-cdr
		     (replace-regexp-in-string
		"engine='\\([^']+\\)'" "\\1" engine-cdr)))
 (expand-cmd
	(intern (concat "org-babel-expand-body:" engine))))
    (cond
     ((and engine (fboundp expand-cmd))
(funcall expand-cmd body params))
     (engine (org-babel-expand-body:generic body params))
     (t (org-babel-expand-body:R body params)))))

;; defun-org-ravel-rewrap

;; Wrap the results of `org-babel-execute:ravel' in a
;; :#+BEGIN_EXPORT RAVEL ... #+END_EXPORT block.

;; #+NAME: defun-org-ravel-rewrap

(defun org-ravel-rewrap (retval &optional inline engine-cdr)
  "(Re)Set `:wrap', `:results', `:exports', amd `:engine'
   header args to values ravel uses. INLINE settings
   differ. ENGINE-CDR gives the engine string, if any.

Argument RETVAL is the vslue of `org-babel-get-src-block-info'..

The original header args `:exports', `:wrap', `:file', `:file-ext', and
`:results' get suffixed with `-arg'. Block/snippet style
functions can find them in `R-HEADERS-ATTR'. "
  (let ((n2r (nth 2 retval)))
    (cl-loop
     for carname in
     '(:exports :results :wrap :file :file-ext) do
     (let ((elt (assq carname n2r)))
 (if elt
	   (setcar elt (intern (format "%S-arg" carname))))))
    ;; end do
    (setf (nth 2 retval)
		(append
		 `((:results . "replace")
		   (:wrap . ,(if inline "ravel" "EXPORT RAVEL"))
		   (:exports . "results")			 
		   (:engine . ,engine-cdr)) 			 
		 n2r))))

;; defvar-org-ravel-no-confirm-for-ravel

;; Confirmation of ravel `execution' is a nuisance --- and no code is
;; actually run --- so disable confirmations for `ravel' src blocks.
;; This can be overridden by `(setq org-ravel-no-confirm-for-ravel t)' if
;; ever needed.

;; Maybe need to add check if (functionp org-confirm-babel-evaluate) is
;; nil in which case, I do not reset it.

;; #+NAME: defvar-org-ravel-no-confirm-for-ravel

(defvar org-ravel-no-confirm-for-ravel
  (lambda (language body)
    (if (string= language "ravel") nil t))
  "Do not confirm if LANGUAGE is `ravel'.")

(defun org-ravel-reset-confirm (value)
  "Revert `org-confirm-babel-evaluate' as buffer local VALUE."
  (when org-confirm-babel-evaluate
    (setf org-confirm-babel-evaluate
          value)))

;; defun-org-babel-execute:ravel

;; `org-babel-execute:ravel' calls formatting functions for the code. No
;; actual code is run. Also need to add some kind of alias for edit modes
;; if Rcpp is to be supported. Like `(defalias 'Rcpp-mode 'c++-mode)'

;; #+NAME: defun-org-babel-execute-ravel

(defun org-babel-execute:ravel (body params)
  "Format BODY as ravel according to PARAMS."
   (save-excursion
     (if (string= "none" (cdr (assoc :exports params)))
         ""
 (let*
           ((oec (org-element-context))
            (ravel-attr (org-element-property :attr_ravel oec))
            (type (org-element-type oec))
            ;; Need (org-babel-params-from-properties "ravel") here as
            ;; parsing was done on "R" or other language.
            (headers  (apply #'org-babel-merge-params
                             (append
			(org-babel-params-from-properties "ravel")
			(list params))))
            (ravelarg (cdr (assoc :ravel headers)))
            (engine (cdr (assoc :engine headers)))
            (ravelstyle (cdr (assoc :ravel-style headers)))
            (label (org-element-property :name oec))
            (non-ravelargs (assq-delete-all :ravel headers))
            (chunk-style
             (org-ravel-get-style ravelstyle))
	    (body (org-remove-indentation body))
            (full-body
             (org-babel-expand-body:ravel body params)))
	 (when engine
	   (setq ravel-attr
		 (cons engine
		 ravel-attr)))
         (if (memq type '(inline-src-block inline-babel-call))
             (org-ravel-snippetize chunk-style ravelarg non-ravelargs full-body)
           (org-ravel-blockify chunk-style label ravelarg ravel-attr
			 non-ravelargs full-body))))))

;; defun-org-ravel-snippetize/blockify

;;    Call the chunk-style functions to format the code.

;; #+NAME: defun-org-ravel-snippetize

(defun org-ravel-snippetize (chunk-style ravelarg r-headers-attr body)
  "Format an inline src block.

Use CHUNK-STYLE, RAVELARG, and R-HEADERS-ATTR (often ignored) to
format BODY, then wrap it inside an export snippet."
   (funcall (nth 1 chunk-style)
	    ravelarg r-headers-attr body))

(defun  org-ravel-blockify
  (chunk-style label ravelarg ravel-attr non-ravelargs body)
   "Format a src block.

Use CHUNK-STYLE, LABEL, RAVELARG, RAVEL-ATTR and
NON-RAVELARGS (typically ignored) to format BODY and wrap it
inside an export block."
           (funcall (nth 0 chunk-style) label ravelarg
		    ravel-attr non-ravelargs body))

;; defun-org-ravel-get-style
;; #+NAME: defun-org-ravel-get-style

(defun org-ravel-get-style (style-from-header)
  "Return the chunk style for STYLE-FROM-HEADER.

Possibly find it in properties or use `org-ravel-style' by
  default."
  (or
   (assoc-default
    (or style-from-header
        (cdr (assoc
              :ravel-style
              (org-babel-parse-header-arguments
               (org-entry-get (point)
                              "header-args:ravel"
                              'inherit))))
        org-ravel-style)
    org-ravel-style-alist 'string=)
   (user-error "Ravel-style: %S not found -- Consult `org-ravel-style-alist'"
               style-from-header)))

;; defun-org-ravel-attr-plus-header
;; #+NAME: defun-org-ravel-attr-plus-header

(defun org-ravel-attr-plus-header
  (label ravelarg ravel-attr)
  "Separate LABEL, RAVELARG, and RAVEL-ATTR by commas."
  (mapconcat #'identity
             (delete nil
                     (cons label
                           (cons ravelarg ravel-attr))) ", "))

;; defmacro-org-ravel-style-x
;; #+NAME: defmacro-org-ravel-style-x

(defmacro org-ravel-style-x (x xblock xinline &optional xcode)
   "Make style functions.

The functions are `org-ravel-block-X' and `org-ravel-inline-X'
where X names the style, XBLOCK gives the block format, XINLINE gives the
inline format, and XCODE is an optional line prefix.

 `org-ravel-block-X' defines the Chunk code style.  It's arguments are

     LABEL - the chunk name (which will be sanitized by
substituting `_' for any character not allowed as a
chunk label by Sweave),

     RAVEL - header args as a string,
     ATTR-RAVEL - attributes to be combined with RAVEL,
     R-HEADERS-ATTR - other headers from Babel as a string parseable
by `org-babel-parse-header-arguments',
     SRC-CODE is the code from the block.

 `org-ravel-inline-X' defines the inline code style.  It's arguments
     are RAVEL, R-HEADERS-ATTR, SRC-CODE as above.  Note that only SRC-CODE is
     used in this macro, but other arguments may be used in hand tooled inline
     style functions."
   (let ((blk-args
          '(label ravel attr-ravel r-headers-attr src-code))
         (inline-args '(ravel r-headers-attr src-code))
         (blk-body
          `(let* ((label
		   (if label
		 (replace-regexp-in-string "[^[:alnum:]#+-_.]" "_" label)))
		  (ravel  (org-ravel-attr-plus-header label ravel attr-ravel)))
             ,(if xcode
                  `(format ,xblock ravel
                           (replace-regexp-in-string "^" ,xcode src-code))
                `(format ,xblock ravel src-code))))
         (inline-body `(format ,xinline src-code))
         (bname (concat "org-ravel-block-" x))
         (iname (concat "org-ravel-inline-" x)))
     (defalias (intern bname)
 (list 'lambda blk-args blk-body)
 (concat "Run this:\n\n" (pp-to-string blk-body)))
     (defalias (intern iname)
 (list 'lambda inline-args inline-body)
 (concat "Run this:\n\n" (pp-to-string inline-body)))
     (format "Functions: %s and %s" bname iname)))

;; defun-org-ravel-format-brew-spec
;; #+NAME: defun-org-ravel-format-brew-spec

(defun org-ravel-format-brew-spec (&optional spec)
  "Check a brew SPEC, escape % signs, and add a %s spec."
  (let
      ((spec (or spec "<% %>")))
    (if (string-match
         "<\\(%+\\)\\([=]?\\)\\(.+?\\)\\([{}]?[ ]*-?\\)\\(%+\\)>"
         spec)
        (let (
              (opct (match-string 1 spec))
              (eqsign (match-string 2 spec))
              (filler (match-string 3 spec))
              (enddash (match-string 4 spec))
              (clpct (match-string 5 spec)))
          (if (string= opct clpct)
              (concat "<" opct opct eqsign " %s " enddash clpct clpct ">")
            (error "Percent signs do not balance:%s" spec)))
      (error "Invalid spec:%s" spec))))

;; defun-org-ravel-block-brew
;; #+NAME: defun-org-ravel-block-brew

(defun org-ravel-block-brew (label ravel attr_ravel r-headers-attr src-code)
  "Define the chunk style for brew.

LABEL is the chunk name, RAVEL is the collection of ravel args as
a string, ATTR_RAVEL and R-HEADERS-ATTR are ignored here,
SRC-CODE is the code from the block."
  (format (org-ravel-format-brew-spec ravel) src-code))

(defun org-ravel-inline-brew (ravel r-headers-attr src-code)
  "Define the inline-src style for brew.

RAVEL is the collection of ravel args as a string, R-HEADERS-ATTR
is the collection of headers from Babel as a string parseable by
`org-babel-parse-header-arguments', SRC-CODE is the code from the
block."
  (format (org-ravel-format-brew-spec
           (or ravel "<%= code -%>"))
          src-code))

;; org-ravel-style-x-rnw
;; #+NAME: org-ravel-style-x-rnw

(org-ravel-style-x "rnw"
"<<%s>>=\n%s\n@ %%def"
"\\Sexpr{ %s }")

;; org-ravel-style-x-tex
;; #+NAME: org-ravel-style-x-tex

(org-ravel-style-x "tex"
                   "%% begin.rcode( %s )\n%s\n%% end.code"
                   "\\rinline{ %s }"
                   "%")

;; org-ravel-style-x-html
;; #+NAME: org-ravel-style-x-html

(org-ravel-style-x "html"
"<!--begin.rcode  %s \n%s\nend.rcode-->"
"<!--rinline  %s  -->")

;; org-ravel-style-x-md
;; #+NAME: org-ravel-style-x-md

(org-ravel-style-x "md"
"```{r  %s }\n%s \n```"
"`r  %s `")

;; org-ravel-style-x-braces
;; #+NAME: org-ravel-style-x-braces

(org-ravel-style-x "braces"
"{{%0.0s%s}}"
"{{%s}}")

;; org-ravel-style-x-rst

;; #+NAME: org-ravel-style-x-rst

(org-ravel-style-x "rst"
		   "..{r %s}\n%s\n.. .."
		   ":r:`%s`"
		   "%")

;; defun-org-ravel-export-block 


;; #+NAME: defun-org-ravel-export-block

(defun org-ravel-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to ravel.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (equal (org-element-property :type export-block) "RAVEL")
(org-unescape-code-in-string
 (org-element-property :value export-block))
    (org-export-with-backend
           org-ravel-backend-parent export-block contents info)))

;; defun-org-ravel-export-snippet

;; #+NAME: defun-org-ravel-export-snippet

(defun org-ravel-export-snippet (export-snippet contents info)
  "Transcode a EXPORT-SNIPPET element from Org to ravel.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (eq (org-export-snippet-backend export-snippet) 'ravel)
(org-element-property :value export-snippet)
    (org-export-with-backend org-ravel-backend-parent export-snippet contents info)))

;; defun-org-ravel-create-backend

(defun  org-ravel-create-backend (parent &optional style)
  "Create a ravel-compliant backend from PARENT using STYLE.
Hence, (org-ravel-create-backend 'ascii \"md\") creates a backend
whose parent is ascii and default style is \"md\"."
  (org-export-create-backend
   :parent parent
   :transcoders '((export-snippet . org-ravel-export-snippet)
                  (export-block . org-ravel-export-block))
   :options `((:ravel-style "RAVEL_STYLE" nil ,style t))
   :blocks    '("RAVEL")))

;; defmacro-org-ravel-export-wrapper

;; See [[*defun-org-ravel-export-string-as][defun-org-ravel-export-string-as]] as an example of how this
;; macro is used.


(defmacro org-ravel-export-wrapper (&rest body)
  "Set up the preliminaries for the BODY of an export function.

`org-ravel-export-to-file' and similar actions need to redefine
 `org-babel-get-src-block-info' and restore the
 function to its original value on exit, set values for
 `org-ravel-run' and for `org-ravel-style', force the `backend'
 to be ravel compliant and let-bind its parent as
 `org-ravel-backend-parent', and (by default) turn off
 confirmation for the evaluation of ravel blocks.

`(org-ravel-export-wrapper BODY)' when used inside a `defun' will
take care of these issues.

Use of this macro outside of ravel export functions is
discouraged as it can corrupt the cache used by the
`org-element-*' functions.  In case of these issues,
`org-element-cache-reset' will straighten things out."
  (declare (indent 1) (debug (form body)))
  `(let* ((org-ravel-get-s-b-info
	   ;; avoid recursive redefinition
	   (or (bound-and-true-p org-ravel-get-s-b-info)
	 (symbol-function
		'org-babel-get-src-block-info)))
	  (org-ravel-lob-get-info
	   ;; avoid recursive redefinition
	   (or (bound-and-true-p org-ravel-lob-get-info)
	 (symbol-function
		'org-babel-lob-get-info)))
	  ;; set ravel variables
	  (org-ravel-run
	   (or  engines org-ravel-run org-ravel-engines))
	  (bk-orig
	   (if (symbolp backend)
	 (org-export-get-backend backend) backend))
	  (ravel-style-option
	   (assq :ravel-style
		 (org-export-backend-options bk-orig)))
	  (backend (if ravel-style-option bk-orig
		     (unless style
		 (message "Non ravel BACKEND might need STYLE."))
		     (org-ravel-create-backend
		(org-export-backend-name bk-orig) style)))
	  (org-ravel-backend-parent (org-export-backend-parent backend))
	  (org-ravel-style
	   (or style org-ravel-style
	 (nth 3
		    (assoc :ravel-style
			   (org-export-backend-options
			    backend)))))
	  (org-confirm-babel-evaluate org-confirm-babel-evaluate))
     ;; org-babel-get-src-block-info will modify info for ravel blocks

     (cl-letf
	 (((symbol-function 'org-babel-get-src-block-info)
	   (lambda (&optional light datum)
	     (let* ((dat (or datum (org-element-context)))
		    (lang (org-element-property :language dat))
		    (ravel-it (assoc lang org-ravel-run))
		    (inline (eq 'inline-src-block (org-element-type datum)))
		    (engine-cdr (and ravel-it (cdr ravel-it))))
	 (if ravel-it
		   (setf (nth 1 dat)
			 (plist-put (nth 1 dat) :language "ravel")))
	 (let* ((info (funcall org-ravel-get-s-b-info light dat))
		(nth-2-info (nth 2 info)))

		 (unless (or (not ravel-it)
			     (member '(:exports . "none") (nth 2 info)))
		   ;; revise headers of RAVEL src-blocks
		   (org-ravel-rewrap info inline engine-cdr))
		 ;; return info for all src-blocks
		 info))))
	  ((symbol-function 'org-babel-lob-get-info)
	   (lambda (&optional datum)
	     (let*
		 ((datum (or datum (org-element-context)))
		  (info  (funcall org-ravel-lob-get-info datum))
		  (lang (car info))
		  (ravel-it  (string= lang "ravel"))
		  (inline (eq 'inline-babel-call (org-element-type datum))))
	 (unless (or (not ravel-it)
			   (member '(:exports . "none") (nth 2 info)))
		    ;; revise headers of RAVEL src-blocks
		 (org-ravel-rewrap info inline))
	 info))))
 ,@body)))

;; defun-org-ravel-export-string-as
;; #+NAME: defun-org-ravel-export-string-as

(defun org-ravel-export-string-as
  (string backend &optional body-only ext-plist engines style)
  "Export STRING as a string.

 Use BACKEND with BODY-ONLY and EXT-PLIST, all as per
`org-export-string-as'.  If non-nil, ENGINES will set
`org-ravel-run' locally.  Otherwise, an attempt will be made to
replace it with `org-ravel-run' or `org-ravel-engines'.  STYLE
will set `org-ravel-style' if non-nil, otherwise
`org-ravel-style' or the default for BACKEND will be used.  

This function can be run by Babel to produce a string that is
used in a Babel src block.

It can run arbitrary backends if STYLE is supplied or if STRING
supplies valid values for src blocks and inline src blocks in it."


  (org-ravel-export-wrapper
   (org-ravel-reset-confirm
    org-ravel-no-confirm-for-ravel)
   (org-export-string-as string backend body-only ext-plist)))

;; defun-org-ravel-export-to-file

;; #+NAME: defun-org-ravel-export-to-file

(defun org-ravel-export-to-file
  (backend &optional file async subtreep visible-only
           body-only ext-plist post-process engines style)
  "Export invoking ravel with BACKEND to FILE.

ASYNC must be nil, but SUBTREEP, VISIBLE-ONLY, BODY-ONLY,
EXT-PLIST, and POST-PROCESS are passed to `org-export-to-file'.
ENGINES supplies a value for `org-ravel-run' and STYLE for
`org-ravel-style'.  If a backend is used that is not set up for
ravel, it usually best to use, e.g.

     `(org-ravel-export-to-file
 (org-ravel-create-backend 'ascii \"md\") ... )'

  to create a ravel-compliant backend.

Note that `org-babel-confirm-evaluate' is set locally by `let*'
to `org-ravel-no-confirm-for-ravel', which holds a `lambda'
function.  To override this, create a variable with that name."

  (org-ravel-export-wrapper
(let ((file (or file
		(org-export-output-file-name
		 (org-ravel-extension org-ravel-style) subtreep))))
	(when async (user-error "ASYNC not allow for ravel"))
	(org-ravel-reset-confirm org-ravel-no-confirm-for-ravel)
	(org-export-to-file backend file async subtreep visible-only
			    body-only ext-plist post-process))))

;; defun-org-ravel-export-to-buffer

;; #+NAME: defun-org-ravel-export-to-buffer

(defun org-ravel-export-to-buffer
    (backend &optional buffer async subtreep visible-only
             body-only ext-plist post-process engines style)
  "Export invoking ravel using BACKEND to BUFFER.

ASYNC must be nil, but SUBTREEP, VISIBLE-ONLY, BODY-ONLY,
EXT-PLIST, and POST-PROCESS are passed to `org-export-to-buffer'.
ENGINES supplies a value for `org-ravel-run' and STYLE for
`org-ravel-style'.  If a backend is used that is not set up for
ravel, it usually best to use, e.g.

   `(org-ravel-export-to-buffer
     (org-ravel-create-backend 'ascii \"md\") ... )'

to create a ravel-compliant backend.

Note that `org-babel-confirm-evaluate' is set locally by `let*'
to `org-ravel-no-confirm-for-ravel', which holds a `lambda'
function.  To override this, create a variable with that name."

  (org-ravel-export-wrapper
(let ((buffer (or buffer
			(format "* %S Output *"
				(org-export-backend-name backend)))))
	(when async (user-error "ASYNC not allow for ravel"))
	(org-ravel-reset-confirm org-ravel-no-confirm-for-ravel)
	(org-export-to-buffer backend buffer async subtreep visible-only
			body-only ext-plist post-process))))

;; defun-org-ravel-extension
;; #+NAME: defun-org-ravel-extension

(defun org-ravel-extension (style)
  "Get the file extension for STYLE."
  (nth 3 (assoc-string style org-ravel-style-alist)))

;; defmacro-ravel-define-exporter

;; #+NAME: defmacro-ravel-define-exporter

(defmacro org-ravel-define-exporter
  (ravel-backend parent menu-key menu-label style-default
                 &optional fileout bufferout post-proc filters)
  "Define ravel backends.

The arguments are:

 RAVEL-BACKEND is a symbol naming the backend derived from

 PARENT is a registered backend,

 MENU-KEY should be an integer code for a lower-case
 character like `?a' to refer to file dispatch,

 MENU-LABEL tells how to label the backend in the
 dispatch menu,

 STYLE-DEFAULT is the style to use if not specified as a
        `:ravel-style' attribute,

 FILEOUT is usually nil which allows
 `org-ravel-export-to-file' to assign the file name

 BUFFEROUT is usually `t' - if non-nil create menu
 entry `(upcase MENU-KEY)' that will be used for menu
 dispatch) or nil for no buffer dispatcher, and

 POST-PROC is a post-export hook function or nil

 FILTERS is an alist of filters that will overwrite or
 complete filters defined in PARENT back-end.  See
 `org-export-filters-alist' for a list of allowed filters."

  `(org-export-define-derived-backend
 ,ravel-backend
 ,parent
 :translate-alist '(
                          (export-snippet . org-ravel-export-snippet)
                          (export-block . org-ravel-export-block))
 :options-alist '((:ravel-style "RAVEL_STYLE"
				nil ,style-default t))
 :filters-alist ,filters
 :menu-entry
 '(?r "Ravel"
            ,(remq nil
                   `((,menu-key ,(concat menu-label " file")
                                (lambda (a s v b)
                                  (org-ravel-export-to-file
                                 ,ravel-backend ,fileout a s v b nil 
                                 nil nil ,style-default)))
                     ,(if bufferout
                          `(,(upcase menu-key) ,(concat menu-label " buffer")
                            (lambda (a s v b)
			(org-ravel-export-to-buffer
			 ,ravel-backend nil a s v b nil ,post-proc
			 nil ,style-default)))))))))

;; LaTeX (Rnw) and HTML (Rhtml) flavored exports

;; The `(eval-after-load FILE FORM)' forms seems to work. i.e. FORM is
;; executed if the backend specified in FILE (e.g. 'ox-latex) is already loaded.
;; If not, then when FILE is loaded, FORM is run.

;; The variable `org-export-backends' can be customized to (de-)list
;; parent backends. The `ravel' backends that depend on those parents are
;; (de-)activated when the parent is (de-)listed.

;; A ravel backend whose parent is not in `org-export-backends' will need
;; to `require' or `load' that parent.

;; #+NAME: run-org-ravel-define-exporters

(eval-after-load 'ox-latex
  '(org-ravel-define-exporter
    'ravel-latex
    'latex ?l "Ravel-LaTeX" "rnw" nil t (lambda () (LaTeX-mode))))

(eval-after-load 'ox-beamer
  '(org-ravel-define-exporter
    'ravel-beamer
    'beamer ?b "Ravel-beamer" "rnw" nil t (lambda () (LaTeX-mode)))
  )
(eval-after-load 'ox-html
  '(org-ravel-define-exporter
    'ravel-html
    'html ?h "Ravel-html" "html" nil t ))

;; R markdown exports

;; The Markdown flavored exporters have a template that inserts a =YAML=
;; header at the top of the exported document. The ='ravel-markdown=
;; exporter is intended for the user.  It is derived from the ='rmd=
;; backend that adds options and a transcoder to the ='md= exporter. This
;; extra step is necessary since the macro ='org-ravel-define-backend'
;; does not add options or transcoders on its own.


;; The =`ravel-markdown= backend will extract all `YAML' export blocks
;; and combine their contents to form the yaml header.  The
;; `:with-auto-yaml-header' option controls automatic inclusion of the
;; author, title and date (from the corresponding =org= options) in that
;; block. Setting `rmd_yaml:header' in an options line will insert a yaml
;; header with title, author, and date before any other yaml lines.
;; Setting `rmd_yaml:footer' puts them after those lines. Since yaml
;; obeys `left-join' rules, the latter allows yaml export blocks to
;; override those settings.  Setting `rmd_yaml:nil' prevents those lines
;; from being inserted and if there are no yaml export blocks will result
;; in no yaml header being included.


;; By default ='ravel-markdown= (and ='rmd=) does (do) not produce a table
;; of contents as some output formats produce their own.  Setting
;; `:with-toc' to `t' will produce a table of contents in the =*.Rmd= file.

;; LaTeX style =\cite= directives are translated to Pandoc format in
;; ='ravel-markdown=.

;; The option =:with-biblinks= allows bibliography links to be processed
;; if such are defined and will allow citation links to be rendered by
;; =org-ref= if it is loaded.  Often one will want to place the
;; bibliography directives in a =YAML= export block, and the option
;; =biblinks:nil= will prevent superfluous insertion of a bibliography in
;; the =*.Rmd= file. Also, with =biblinks:nil= and =org-ref= loaded,
;; citation links will be rendered in =pandoc= =[@mycite]= style.

;; #+NAME: run-org-ravel-define-rmarkdown

(eval-after-load 'ox-md
  '(progn
     (org-export-define-derived-backend
	 'rmd
	 'md
 :translate-alist '((template . org-ravel-rmd-template)
			  (link . org-rmd-link))
 :options-alist '((:with-auto-yaml-header nil "rmd_yaml" 'footer t)
			(:with-toc nil "toc" nil t)
			(:with-biblinks nil "biblinks" t t)))
     (org-ravel-define-exporter
'ravel-markdown
'rmd ?m "Ravel-markdown" "md" nil t nil 
'((:filter-latex-fragment . org-ravel-filter-cite-as-pandoc)))))

;; Markdown helpers

;; A filter for LaTeX citations:


(defun org-ravel--fix-cite (str)
  "Fix a \\cite{...} entry in STR."
  (let
((newstr
	(replace-regexp-in-string ",[\s-]*" "; @" str)))
    (setq newstr (replace-regexp-in-string "^\\\\cite{" "[@" newstr))
    (replace-regexp-in-string "}$" "]" newstr)))

(defun org-ravel-filter-cite-as-pandoc (text back-end info)
  "Translate citations in latex format (i.e. \cite{id}) into
citations in pandoc format (i.e. [@id]). 

Note, loading `ox-bibtex' transforms all latex/bibtex citations
into html links, so do not load it if this format is desired."
  (replace-regexp-in-string "\\\\cite{[^}]*}" #'org-ravel--fix-cite
			    text nil t))



;; Set up a =YAML= header:


(defun org-ravel--yaml-header (info)
  "A minimal header/footer is CONSed to value in 
    :with-auto-yaml-header from INFO."
  (let ((rmd_yaml (plist-get info :with-auto-yaml-header)))
    (when rmd_yaml 
(let* ((with-title  (and (plist-get info :with-title)
			 (plist-get info :title)))
	     (with-author  (and (plist-get info :with-author)
				(plist-get info :author)))
	     (with-date  (and (plist-get info :with-date)
			(plist-get info :date)))
	     (title  (and with-title
			  (format "title: %s\n"
				  (org-element-interpret-data with-title))))	
	     (author  (and with-author
			   (format "author: %s\n"
				   (org-element-interpret-data with-author))))
	     (date  (and with-date
			 (format "date: %s\n"
				 (org-element-interpret-data with-date))))
	     )
	(cons rmd_yaml (concat title author date))))))

(defun org-ravel-rmd-template (contents info)
  "Return complete document string after Markdown conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
used as a communication channel. A YAML block is added as a
header consisting of all YAML export blocks and (optionally)
the title, author and date as determined from their options."
  (let* ((rmd_yaml (org-ravel--yaml-header info))
	 (auto-first (eq (car rmd_yaml) 'header))
	 (auto-last (eq (car rmd_yaml) 'footer))
	 (auto-content (cdr rmd_yaml))	 
	 (parsed (plist-get info :parse-tree))
	 (yaml-export-blocks
	  (apply 'concat (org-element-map parsed 'export-block
			   (lambda (exb) (and
					  (equal (org-element-property :type exb)
						 "YAML")
					  (org-element-property :value exb)))))))
    (concat
     (when (or (< 0 (length auto-content)) (<  0 (length yaml-export-blocks)))
 (concat "---\n" "# YAML header created by ox-ravel\n"
	 (when auto-first auto-content)
	 yaml-export-blocks
	 (when auto-last auto-content)
	 "---\n"))
     contents)))



;; =org-rmd-link= is a kludge to enable =[@mycite]= style citations to
;; override =org-ref= processing of =cite= (et cetera) links without
;; having to rewrite a bunch of code or advise a lot of functions.
;; =org-ref= provides the right format under =pandoc= exports, so this
;; works. However, custom link types that refer to the backend could
;; trigger a failure if the pandoc backend is not loaded.


(defun org-rmd-link (link contents info)
  "Transcode LINK object into Markdown format.
  CONTENTS is the link's description.  INFO is a plist used as
  a communication channel."
  (let ((biblinks (plist-get info :with-biblinks)))
    (if (or biblinks (not (featurep 'org-ref)))
	(org-export-with-backend 'md link contents info)
(unless 
	  (string= (org-element-property :type link) "bibliography")
	(org-export-custom-protocol-maybe link contents 'pandoc)))))

;; provide ravel							   :noexport:


(provide 'ox-ravel)

;;; ox-ravel.el ends here
