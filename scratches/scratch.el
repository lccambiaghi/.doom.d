
(defun my/open-ipython-repl ()
  "Open an IPython REPL."
  (interactive)
  (let ((python-shell-interpreter (or (+python-executable-find "ipython") "ipython"))
        (python-shell-interpreter-args (string-join +python-ipython-repl-args " ")))
    (my/open-repl)))

(defun my/open-repl ()
  "Open the Python REPL."
  (interactive)
  (unless python-shell-interpreter
    (user-error "`python-shell-interpreter' isn't set"))
  (pop-to-buffer
   (process-buffer
    (if-let* ((poetry (+python-executable-find "poetry"))
              (poetry-project (poetry-find-project-root)))
        (let ((default-directory poetry-project)
              (python-shell-interpreter-args
               (format "run %s %s"
                       python-shell-interpreter
                       python-shell-interpreter-args))
              (python-shell-interpreter poetry))
          (run-python nil nil t))))))
