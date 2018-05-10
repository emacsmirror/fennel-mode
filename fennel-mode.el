;;; fennel-mode.el --- a major-mode for editing Fennel code

;; Copyright © 2018 Phil Hagelberg and contributors

;; Author: Phil Hagelberg
;; URL: https://gitlab.com/technomancy/fennel-mode
;; Version: 0.0.1
;; Created: 2018-02-18
;;
;; Keywords: languages, tools

;;; Commentary:

;; Provides font-lock and indentation for editing Fennel code.

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

(defun fennel-paredit-setup ()
  (define-key fennel-mode-map "{" #'paredit-open-curly)
  (define-key fennel-mode-map "}" #'paredit-close-curly))

(defvar fennel-module-name nil
  "Buffer-local value for storing the module name.")

(defvar fennel-mode-syntax-table
  (let ((table (copy-syntax-table lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    table))

(defvar fennel-keywords
  '("require-macros" "eval-compiler"
    "do" "values" "if" "when" "each" "for" "fn" "lambda" "λ" "partial" "while"
    "set" "global" "var" "local" "let" "tset"
    "or" "and" "true" "false" "nil"
    "." "+" ".." "^" "-" "*" "%" "/" ">" "<" ">=" "<=" "=" "~=" "#" "..." ":"
    "defn" "->" "->>"))

(defvar fennel-builtins
  '("_G" "_VERSION" "arg" "assert" "bit32" "collectgarbage" "coroutine" "debug"
    "dofile" "error" "getfenv" "getmetatable" "io" "ipairs" "load" "loadfile"
    "loadstring" "math" "next" "os" "package" "pairs" "pcall" "print" "rawequal"
    "rawget" "rawlen" "rawset" "require" "select" "setfenv" "setmetatable"
    "string" "table" "tonumber" "tostring" "type" "unpack" "xpcall"))

(defvar fennel-local-fn-pattern
  (rx (syntax open-parenthesis)
      (or "global" "var" "set" "local") (1+ space)
      (group (1+ (or (syntax word) (syntax symbol) "-" "_")))
      (0+ (syntax whitespace)) ;; newline will cause this to not match
      (syntax open-parenthesis) (or "fn" "lambda" "λ")))

(defvar fennel-defn-pattern
  (rx (syntax open-parenthesis) "defn" (1+ space)
      (group (1+ (or (syntax word) (syntax symbol) "-" "_")))))

(defvar fennel-font-lock-keywords
  (eval-when-compile
    `((,fennel-local-fn-pattern 1 font-lock-variable-name-face)
      (,fennel-defn-pattern 1 font-lock-variable-name-face)
      (,(rx (syntax open-parenthesis)
            (or "fn" "lambda" "λ") (1+ space)
            (group (and (not (any "["))
                        (1+ (or (syntax word) (syntax symbol))))))
       1 font-lock-variable-name-face)
      (,(regexp-opt fennel-keywords 'symbols) . font-lock-keyword-face)
      (,(regexp-opt fennel-builtins 'symbols) . font-lock-builtin-face)
      (,(rx (group ":" (1+ word))) 0 font-lock-builtin-face)
      (,(rx (group (1+ word) "." (1+ word))) 0 font-lock-type-face))))

(defun fennel-font-lock-setup ()
  (setq font-lock-defaults
        '(fennel-font-lock-keywords nil nil (("+-*/.<>=!?$%_&:" . "w")))))

;; simplified version of lisp-indent-function
(defun fennel-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (let* ((fn (buffer-substring (point) (progn (forward-sexp 1) (point))))
           (open-paren (elt state 1))
           (method (get (intern-soft fn) 'fennel-indent-function)))
      (cond ((member (char-after open-paren) '(?\[ ?\{))
             (goto-char open-paren)
             (1+ (current-column)))
            ((eq method 'defun)
             (lisp-indent-defform state indent-point))
            ((integerp method)
             (lisp-indent-specform method state indent-point normal-indent))
            (method
             (funcall method indent-point state))))))

;;;###autoload
(define-derived-mode fennel-mode lisp-mode "Fennel"
  "Major mode for editing Fennel code.

\\{fennel-mode-map}"
  ;; TODO: completion using inferior-lisp
  (add-to-list 'imenu-generic-expression `(nil ,fennel-local-fn-pattern 1))
  (add-to-list 'imenu-generic-expression `(nil ,fennel-defn-pattern 1))
  (make-local-variable 'fennel-module-name)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'lisp-indent-function) 'fennel-indent-function)
  (set (make-local-variable 'inferior-lisp-program) "fennel --repl")
  (set-syntax-table fennel-mode-syntax-table)
  (fennel-font-lock-setup)
  (add-hook 'paredit-mode-hook #'fennel-paredit-setup))

(defun fennel-get-module (ask? last-module)
  "Ask for the name of a module for the current file; returns keyword."
  (let ((module (if (or ask? (not last-module))
                    (read-string "Module: " (or last-module (file-name-base)))
                  last-module)))
    (setq fennel-module-name module) ; remember for next time
    (intern (concat ":" module))))

(defun fennel-reload-form (module-keyword)
  "Return a string of the code to reload the `module-keyword' module."
  (format "%s\n" `(let [old (require ,module-keyword)
                            _ (tset package.loaded ,module-keyword nil)
                            new (require ,module-keyword)]
                    ;; if the module isn't a table then we can't make
                    ;; changes which affect already-loaded code, but if
                    ;; it is then we should splice new values into the
                    ;; existing table and remove values that are gone.
                    (when (= (type new) :table)
                      (each [k v (pairs new)]
                            (tset old k v))
                      (each [k (pairs old)]
                            ;; the elisp reader is picky about where . can be
                            (when (not (,"." new k))
                              (tset old k nil)))
                      (tset package.loaded ,module-keyword old)))))

(defun fennel-reload (ask?)
  "Reload the module for the current file.

Tries to reload in a way that makes it retroactively visible; if
the module returns a table, then existing references to the same
module will have their contents updated with the new
value. Requires installing `fennel.searcher'.

Queries the user for a module name upon first run for a given
buffer, or when given a prefix arg."
  (interactive "P")
  (comint-check-source buffer-file-name)
  (let* ((module (fennel-get-module ask? fennel-module-name)))
    (when (and (file-exists-p (concat (file-name-base) ".lua"))
               (yes-or-no-p "Lua file for module exists; delete it first?"))
      (delete-file (concat (file-name-base) ".lua")))
    (comint-send-string (inferior-lisp-proc) (fennel-reload-form module)))
  (switch-to-lisp t))

(defun fennel-find-definition-go (location)
  (when (string-match "^@\\(.+\\)!\\(.+\\)" location)
    (let ((file (match-string 1 location))
          (line (string-to-number (match-string 2 location))))
      (message "found file, line %s %s" file line)
      (when file (find-file file))
      (when line
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun fennel-find-definition-for (identifier)
  (let ((tempfile (make-temp-file "fennel-completions-")))
    (comint-send-string
     (inferior-lisp-proc)
     (format "%s\n"
             `(let [f (io.open ,(format "\"%s\"" tempfile) :w)
                      info (debug.getinfo ,identifier)]
                (when (= :Lua info.what)
                  (: f :write info.source :! info.linedefined))
                (: f :close))))
    (sit-for 0.1)
    (unwind-protect
        (when (file-exists-p tempfile)
          (with-temp-buffer
            (insert-file-contents tempfile)
            (delete-file tempfile)
            (buffer-substring-no-properties (point-min) (point-max)))))))

(defun fennel-find-definition ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (fennel-find-definition-go (fennel-find-definition-for (symbol-at-point))))

(defun fennel-find-definition-pop ()
  "Return point to previous position in previous buffer."
  (interactive)
  (defvar find-tag-marker-ring) ;; etags.el
  (require 'etags)
  (let ((marker (ring-remove find-tag-marker-ring 0)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(define-key fennel-mode-map (kbd "M-.") 'fennel-find-definition)
(define-key fennel-mode-map (kbd "M-,") 'fennel-find-definition-pop)
(define-key fennel-mode-map (kbd "C-c C-k") 'fennel-reload)

(put 'lambda 'fennel-indent-function 'defun)
(put 'λ 'fennel-indent-function 'defun)
(put 'fn 'fennel-indent-function 'defun)
(put 'defn 'fennel-indent-function 'defun)
(put 'while 'fennel-indent-function 'defun)
(put 'defn 'fennel-indent-function 'defun)
(put 'do 'fennel-indent-function 0)
(put 'let 'fennel-indent-function 1)
(put 'when 'fennel-indent-function 1)
(put 'for 'fennel-indent-function 1)
(put 'each 'fennel-indent-function 1)
(put 'eval-compiler 'fennel-indent-function 'defun)
(put 'macro 'fennel-indent-function 'defun)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(provide 'fennel-mode) ;;; fennel-mode.el ends here
