;;; ob-fennel.el --- Org Babel Functions for Fennel evaluation -*- lexical-binding: t; -*-

;; Copyright © 2023 Phil Hagelberg and contributors

;; Author: Andrey Listopadov
;; URL: https://git.sr.ht/~technomancy/fennel-mode
;; Package-Requires: ((emacs "26.1") (fennel-mode "0.8.1"))
;; Keywords: literate programming, reproducible research
;; Prefix: ob-fennel
;; Version: 0.0.9

;;; Commentary:

;; Support for evaluating fennel code

;; Requirements:
;;
;; - fennel (at least 1.4.0)
;; - fennel-mode

;;; Code:

(require 'ob)
(require 'ansi-color)
(require 'fennel-proto-repl)

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("fennel" . "fnl"))

(defvar org-babel-default-header-args:fennel '())

(defvar ob-fennel--hline-to "(setmetatable [] {:__fennelview #:hline})"
  "Replace hlines in incoming tables with this when translating to Fennel.")

(defvar ob-fennel--buffer-fmt "*OB Fennel REPL:%s*"
  "Template for naming session buffers.")

(defun ob-fennel--check-fennel-proc (buffer)
  "Check if BUFFER has a running `inferior-lisp-proc'."
  (car (memq (get-buffer buffer) (fennel-proto-repl-live-repls))))

(defun ob-fennel--initialize-repl (name params)
  "Create a Fennel REPL buffer with given NAME according to PARAMS."
  (let* ((cmd (or (cdr (assq :fennel-cmd params))
                  fennel-program))
         (buffer (get-buffer-create name)))
    (save-window-excursion
      (with-current-buffer buffer
        (fennel-proto-repl cmd buffer)
        (rename-buffer name)))
    buffer))

(defun ob-fennel--get-create-repl-buffer (session params)
  "Get or create Fennel REPL buffer for SESSION according to PARAMS.

Raises a `user-error' in case there was no REPL buffer."
  (cond ((and (or (string= "none" session)
                  (null session))
              (ob-fennel--check-fennel-proc
               (format ob-fennel--buffer-fmt "default")))
         (get-buffer (format ob-fennel--buffer-fmt "default")))
        ((or (string= "none" session)
             (null session))
         (ob-fennel--initialize-repl
          (format ob-fennel--buffer-fmt "default") params))
        ((ob-fennel--check-fennel-proc (format ob-fennel--buffer-fmt session))
         (get-buffer (format ob-fennel--buffer-fmt session)))
        (t (ob-fennel--initialize-repl
            (format ob-fennel--buffer-fmt session)
            params))))

(defun ob-fennel--eval-to-string (body params)
  "Evaluate BODY according to PARAMS."
  (let* ((result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
         (pretty? (or (member "code" result-params)
                      (member "pp" result-params)))
         string done)
    (pcase result-type
      (`output
       (fennel-proto-repl-send-message
        :eval body
        (lambda (values)
          (setq done t))
        (lambda (type message traceback)
          (setq done t)
          (setq string (string-join (list message (or traceback "")) "\n")))
        (lambda (data)
          (setq string (concat (or string "") data)))))
      (`value
       (fennel-proto-repl-send-message
        :eval body
        (lambda (values)
          (setq done t)
          (setq string
                (if pretty? (string-join values "\n")
                  (fennel-proto-repl--minify-body (string-join values "\t") t))))
        (lambda (type message traceback)
          (setq done t)
          (setq string (string-join (list message (or traceback "")) "\n"))))))
    (while (not done)
      (accept-process-output nil 0.01))
    (ansi-color-apply (or string ""))))

(defun ob-fennel--send-to-repl (repl-buffer body params)
  "Send BODY to the REPL-BUFFER and retrieve the result."
  (with-current-buffer repl-buffer
    (ob-fennel--eval-to-string body params)))

(defun ob-fennel-var-to-fennel (var)
  "Convert an elisp value to a fennel variable.
Convert an elisp value, VAR, into a string of fennel source code
specifying a variable of the same value."
  (cond ((listp var)
         (concat "[" (mapconcat #'ob-fennel-var-to-fennel var " ") "]"))
        ((eq var 'hline)
         ob-fennel--hline-to)
        (t (format
            (if (stringp var) "%S" "%s")
            (if (stringp var) (substring-no-properties var) var)))))

(defun ob-fennel-table-or-string (results)
  "Convert RESULTS into an appropriate elisp value.
If the results look like a list or tuple, then convert them into an
Emacs-lisp table, otherwise return the result as a string."
  (org-babel-script-escape results))

(defun org-babel-variable-assignments:fennel (params)
  "Return a list of Fennel let bindings assigning the block's PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s %s"
	     (car pair)
	     (ob-fennel-var-to-fennel (cdr pair))))
   (org-babel--get-vars params)))

(defun org-babel-expand-body:fennel (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let* ((vars (org-babel-variable-assignments:fennel params))
         (body
          (with-temp-buffer
            (insert (if (null vars) body
                      (format "(let [%s]\n %s)" (string-join vars "\n") body)))
            (buffer-substring-no-properties (point-min) (point-max)))))
    (string-trim-right body)))

(defun org-babel-execute:fennel (body params)
  "Evaluate a block of Fennel code with Babel.

Sends BODY to the `fennel-repl' process according to PARAMS.
Supports `:session' parameter, and automatically creates a new
REPL buffer, named after the session.  Includes an additional
parameter, exclusive to Fennel src blocks `:fennel-cmd' used to
specify how to start the REPL process.

For example:

#+begin_src fennel :session foo :fennel-cmd \"fennel --lua luajit --repl\"
(+ 1 2 3)
#+end_src

automatically creates buffer \"foo\", starts Fennel REPL in it,
by using \"fennel --lua luajit --repl\" as a command.  Sessions
are isolated, and repl-local variables with the same names can
co-exist in different sessions, since they're different
processes."
  (let* ((repl-buffer (ob-fennel--get-create-repl-buffer
                       (cdr (assq :session params)) params))
         (body (org-babel-expand-body:fennel body params))
         (eval-result (ob-fennel--send-to-repl
                       repl-buffer body params)))
    (org-babel-result-cond (cdr (assq :result-params params))
      eval-result
      (ob-fennel-table-or-string (org-trim eval-result)))))

(provide 'ob-fennel)
;;; ob-fennel.el ends here
