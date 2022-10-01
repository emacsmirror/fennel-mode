;;; fennel-eldoc.el --- Eldoc support for Fennel -*- lexical-binding: t -*-

;; Copyright © 2018-2021 Phil Hagelberg and contributors
;;
;; Author: Andrey Listopadov

;;; Commentary:

;; Support for Eldoc and documentation popup for company-quickhelp and
;; corfu-doc.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'eldoc)
(require 'inf-lisp)

(declare-function markdown-mode "ext:markdown-mode")
(declare-function fennel-repl-redirect-one "ext:fennel-mode")

(defcustom fennel-eldoc-fontify-markdown nil
  "Fontify doc buffer as Markdown.
Requires `markdown-mode' package."
  :group 'fennel-mode
  :type 'boolean
  :package-version '(fennel-mode "0.5.0"))

(defvar fennel-eldoc--doc-buffer " *fennel-doc*"
  "Last Fennel documentation buffer.")

(defun fennel-eldoc-arglist-query-command
    (symbol &optional separator message one-line)
  "Construct a fennel command to query SYMBOL argument list.

Arguments are separated with the SEPARATOR, and an optional
MESSAGE is prepended.  If ONE-LINE is non-nil, make best effort
to oneline he arglist.

Multi-syms (symbols that contain a dot) are queried as is as
those are fully qualified.

Non-multi-syms are first queried in the specials field of
Fennel's scope.  If not found, then ___replLocals___ is tried.
Finally _G is queried.  This should roughly match the symbol
lookup that Fennel does in the REPL."
  (let* ((multisym (and (string-match-p "[.:]" symbol)
                        (not (member symbol '("." ".." "?." ":")))))
         (symbol (replace-regexp-in-string
                  ":" "."
                  (if (and multisym (string-match-p "[.:]$" symbol))
                      (substring symbol 0 -1)
                    symbol))))
    (format
     "%s"
     `(let [fennel (require :fennel) scope (fennel.scope)]
        ,(when message
           `(io.write ,(format "\"%s\"" message)))
        (->> ,(if multisym
                  `(-> ,symbol
                       (fennel.metadata:get :fnl/arglist)
                       (or [,(format "\"no arglist available for %s\"" symbol)])
                       (table.concat ,(format "\"%s\"" (or separator " ")))
                       ,(if one-line
                            `(string.gsub "\"\\n%s+\"" "\"\"")
                          `(string.gsub "\"\"" "\"\"")))
                `(-> ,(format "(. scope.specials \"%s\")" symbol)
                     (or ,(format "(. scope.macros \"%s\")" symbol))
                     (or ,(format "(. _G.___replLocals___ \"%s\")" symbol))
                     (or ,(format "(. _G \"%s\")" symbol))
                     (fennel.metadata:get :fnl/arglist)
                     (or [,(format "\"no arglist available for %s.\"" symbol)])
                     (table.concat ,(format "\"%s\"" (or separator " ")))
                     ,(if one-line
                          `(string.gsub "\"\\n%s+\"" "\"\"")
                        `(string.gsub "\"\"" "\"\""))))
             (pick-values 1)
             print)))))

(defun fennel-eldoc--valid-buffer ()
  "Check whether buffer doesn't contain common errors."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (not (search-forward-regexp
            (rx (or "#<undocumented>"
                    (seq bol "Repl error:")
                    (seq bol "Compile error:")
                    "no arglist available for"
                    (seq "not found" eol)))
            nil t)))))

(defun fennel-eldoc--format-variable ()
  "Format eldoc message for a Fennel variable."
  (with-current-buffer fennel-eldoc--doc-buffer
    (when (fennel-eldoc--valid-buffer)
      (goto-char (point-min))
      (end-of-line)
      (let ((name (string-trim (buffer-substring-no-properties (point-min) (point))))
            (doc (string-trim (buffer-substring-no-properties (point) (point-max)))))
        (format "%s: %s"
                (propertize name 'face 'font-lock-variable-name-face)
                doc)))))

;; taken from elisp-mode.el
(defun fennel-eldoc--num-skipped-sexps ()
  "Find the amount of inner sexps from sexp start to point."
  (let ((parse-sexp-ignore-comments t)
	(num-skipped-sexps 0))
    (condition-case _
	(progn
	  (condition-case _
	      (let ((p (point)))
		(forward-sexp -1)
		(forward-sexp 1)
		(when (< (point) p)
		  (setq num-skipped-sexps 1)))
	    (error))
	  (while
	      (let ((p (point)))
		(forward-sexp -1)
		(when (< (point) p)
		  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

(defun fennel-eldoc--fn-in-current-sexp ()
  "Obtain function name and position in argument list."
  (save-excursion
    (unless (nth 8 (syntax-ppss))
      (let ((argument-index (1- (fennel-eldoc--num-skipped-sexps))))
        (when (< argument-index 0)
          (setq argument-index 0))
        (cons (thing-at-point 'symbol) argument-index)))))

(defun fennel-eldoc--format-function (pos)
  "Format eldoc message for a Fennel function.

POS ia a position in argument list."
  (with-current-buffer fennel-eldoc--doc-buffer
    (goto-char (point-min))
    (end-of-line)
    (when (fennel-eldoc--valid-buffer)
      (let* ((signature (split-string (buffer-substring-no-properties (point-min) (point)) "\t"))
             (name (car signature))
             (method? (string-match-p ":" name))
             (args (if method?
                       (cddr signature)
                     (cdr signature)))
             (pos (min (1- pos) (1- (length args)))))
        (when (>= pos 0)
          (setcar (nthcdr pos args)
                  (propertize (nth pos args) 'face 'eldoc-highlight-function-argument)))
        (format "%s: (%s)"
                (propertize name 'face 'font-lock-function-name-face)
                (mapconcat 'identity args " "))))))

(defun fennel-eldoc--font-lock-doc-buffer ()
  "Apply Markdown font lock."
  (when (and fennel-eldoc-fontify-markdown
             (fboundp 'markdown-mode))
    (setq-local delay-mode-hooks t)
    (setq-local delayed-mode-hooks nil)
    (markdown-mode)
    (font-lock-fontify-region (point-min) (point-max))))

(defun fennel-eldoc--pre-format-doc ()
  "Preformat doc buffer.
Removes 2 leading spaces after the first expression.  If
`fennel-eldoc-fontify-markdown' is t wraps the expression in a
code block."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (if (not fennel-eldoc-fontify-markdown)
          (forward-sexp)
        (insert "```fennel\n")
        (forward-sexp)
        (insert "\n```"))
      (newline)
      (while (not (eobp))
        (when (re-search-forward "^  " nil t)
          (replace-match ""))
        (end-of-line)
        (forward-line))
      (when (re-search-backward (format "\\(^%s\\|^%s\\)"
                                        fennel-mode-repl-prompt-regexp
                                        fennel-mode-repl-subprompt-regexp)
                                nil t)
        (replace-match ""))
      (fennel-eldoc--font-lock-doc-buffer))))

(defun fennel-eldoc--prepare-doc-buffer (sym &optional fn)
  "Prepare documentation buffer for a SYM.

If FN is passed, formats buffer for function documentation."
  (when-let ((buffer (get-buffer fennel-eldoc--doc-buffer)))
    (with-current-buffer buffer
      (erase-buffer)))
  (when sym
    (condition-case nil
        (let* ((sym (substring-no-properties sym))
               (command (if fn
                            (fennel-eldoc-arglist-query-command
                             sym "\t" (format "%s\t" sym) t)
                          (format ",doc %s" sym)))
               (proc (inferior-lisp-proc))
               (buffer (get-buffer-create fennel-eldoc--doc-buffer)))

          (fennel-repl-redirect-one proc command buffer))
      (error nil))))

(defun fennel-eldoc--format-message (&optional pos fn)
  "Format message for eldoc.

Takes optional POS for current position in the argument list and
FN indicating that message should be formatted for a fynction
call."
  (when (get-buffer fennel-eldoc--doc-buffer)
    (with-current-buffer fennel-eldoc--doc-buffer
      (goto-char (point-min))
      (unless (save-match-data
                (search-forward-regexp "[^[:space:]]+ not found$" nil t))
        (if fn
            (fennel-eldoc--format-function pos)
          (fennel-eldoc--format-variable))))))

(defun fennel-eldoc-get-doc-buffer (symbol)
  "Get a valid documentation buffer for SYMBOL."
  (when-let ((buf (fennel-eldoc--prepare-doc-buffer symbol)))
    (with-current-buffer buf
      (when (fennel-eldoc--valid-buffer)
        (fennel-eldoc--pre-format-doc)
        buf))))

(defun fennel-eldoc-function (&rest _)
  "Document thing at point.
Intended for the `eldoc-documentation-functions'."
  (let* ((fn-info (fennel-eldoc--fn-in-current-sexp))
         (fn (car fn-info))
         (pos (cdr fn-info)))
    (fennel-eldoc--prepare-doc-buffer (or fn (thing-at-point 'symbol)) fn)
    (fennel-eldoc--format-message pos fn)))

(provide 'fennel-eldoc)
;;; fennel-eldoc.el ends here
