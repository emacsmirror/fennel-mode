;;; fennel-mode.el --- A major-mode for editing Fennel code -*- lexical-binding: t -*-

;; Copyright © 2018-2021 Phil Hagelberg and contributors

;; Author: Phil Hagelberg
;; URL: https://gitlab.com/technomancy/fennel-mode
;; Version: 0.4.1
;; Created: 2018-02-18
;; Package-Requires: ((emacs "26.1"))
;;
;; Keywords: languages, tools

;;; Commentary:

;; Provides font-lock, indentation, navigation, and repl for Fennel code.

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

(require 'lisp-mode)
(require 'prog-mode)
(require 'inf-lisp)
(require 'thingatpt)
(require 'xref)

(eval-when-compile
  (defvar paredit-space-for-delimiter-predicates))

(declare-function paredit-open-curly "ext:paredit")
(declare-function paredit-close-curly "ext:paredit")
(declare-function lua-mode "ext:lua-mode")

(defcustom fennel-mode-switch-to-repl-after-reload t
  "If the focus should switch to the repl after a module reload."
  :group 'fennel-mode
  :type 'boolean
  :package-version '(fennel-mode "0.10.0"))

(defcustom fennel-program "fennel --repl"
  "Command to run the fennel REPL."
  :group 'fennel-mode
  :type 'string
  :package-version '(fennel-mode "0.10.0"))

(defcustom fennel-mode-annotate-completion t
  "Whether or not to show kind of completion candidates."
  :group 'fennel-mode
  :type 'boolean
  :package-version '(fennel-mode "0.10.0"))

(defvar-local fennel-repl--last-fennel-buffer nil
  "Variable that holds last fennel buffer for switching from the REPL.")

(defun fennel-show-documentation (symbol)
  "Show SYMBOL documentation in the REPL."
  (interactive (lisp-symprompt "Documentation" (lisp-fn-called-at-pt)))
  (comint-proc-query
   (inferior-lisp-proc)
   (format ",doc %s\n" symbol)))

(defun fennel-show-variable-documentation (symbol)
  "Show SYMBOL documentation in the REPL.

Main difference from `fennel-show-documentation' is that rather
than a function, a variable at point is picked automatically."
  (interactive (lisp-symprompt "Documentation" (lisp-var-at-pt)))
  (fennel-show-documentation symbol))

(defun fennel-show-arglist (symbol)
  "Query SYMBOL in the scope for arglist.

Multi-syms are queried as is as those are fully qualified.

Non-multi-syms are first queried in the specials field of
Fennel's scope.  If not found, then ___replLocals___ is tried.
Finally _G is queried.  This should roughly match the symbol
lookup that Fennel does in the REPL."
  (interactive (lisp-symprompt "Arglist" (lisp-fn-called-at-pt)))
  (comint-proc-query
   (inferior-lisp-proc)
   (format "%s\n" (if (string-match-p "\\." (format "%s" symbol))
                      `(let [fennel (require :fennel)]
                         (print ,(format "\"Arglist for %s:\"" symbol))
                         (-> ,symbol
                           (fennel.metadata:get :fnl/arglist)
                           (or [,(format "\"no arglist available for %s\"" symbol)])
                           (table.concat "\" \"")
                           print))
                    `(let [fennel (require :fennel)
                                  scope (fennel.scope)
                                  str ,(format "\"%s\"" symbol)]
                       (print ,(format "\"Arglist for %s:\"" symbol))
                       (-> ("." scope.specials str)
                         (or ("." scope.macros str))
                         (or ("." ___replLocals___ str))
                         (or ("." _G str))
                         (fennel.metadata:get :fnl/arglist)
                         (or [,(format "\"no arglist available for %s.\"" symbol)])
                         (table.concat "\" \"")
                         print))))))

(defvar fennel-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Initialize ASCII charset as symbol syntax
    (modify-syntax-entry '(0 . 127) "_" table)

    ;; Word syntax
    (modify-syntax-entry '(?0 . ?9) "w" table)
    (modify-syntax-entry '(?a . ?z) "w" table)
    (modify-syntax-entry '(?A . ?Z) "w" table)

    ;; Whitespace
    (modify-syntax-entry ?\s " " table)
    (modify-syntax-entry ?\xa0 " " table) ; non-breaking space
    (modify-syntax-entry ?\t " " table)
    (modify-syntax-entry ?\f " " table)

    ;; Delimiters
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?\{ "(}" table)
    (modify-syntax-entry ?\} "){" table)

    ;; Prefix chars
    (modify-syntax-entry ?` "'" table)
    (modify-syntax-entry ?~ "'" table)
    (modify-syntax-entry ?^ "'" table)
    (modify-syntax-entry ?@ "'" table)
    (modify-syntax-entry ?? "_ p" table)
    (modify-syntax-entry ?# "_ p" table)

    ;; Others
    (modify-syntax-entry ?\; "<" table)  ; comment start
    (modify-syntax-entry ?\n ">" table)  ; comment end
    (modify-syntax-entry ?\" "\"" table) ; string
    (modify-syntax-entry ?\\ "\\" table) ; escape

    table)
  "Syntax table for Fennel mode.")

;;;###autoload
(define-derived-mode fennel-repl-mode inferior-lisp-mode "Fennel REPL"
  "Major mode for Fennel REPL."
  (setq-local inferior-lisp-prompt ">> ")
  (setq-local lisp-indent-function 'fennel-indent-function)
  (setq-local lisp-doc-string-elt-property 'fennel-doc-string-elt)
  (setq-local comment-end "")
  (setq-local comint-prompt-read-only t)
  (fennel-font-lock-setup)
  (set-syntax-table fennel-mode-syntax-table)
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions 'fennel-complete)
  (add-hook 'paredit-mode-hook #'fennel-repl-paredit-setup nil t))


(define-key fennel-repl-mode-map (kbd "TAB") 'completion-at-point)
(define-key fennel-repl-mode-map (kbd "C-c C-z") 'fennel-repl)
(define-key fennel-repl-mode-map (kbd "C-c C-f") 'fennel-show-documentation)
(define-key fennel-repl-mode-map (kbd "C-c C-d") 'fennel-show-documentation)
(define-key fennel-repl-mode-map (kbd "C-c C-v") 'fennel-show-variable-documentation)
(define-key fennel-repl-mode-map (kbd "C-c C-a") 'fennel-show-arglist)
(define-key fennel-repl-mode-map (kbd "M-.") 'fennel-find-definition)
(define-key fennel-repl-mode-map (kbd "<return>") 'fennel-repl-send-input)
(define-key fennel-repl-mode-map (kbd "C-j") 'newline-and-indent)
(define-key fennel-repl-mode-map (kbd "RET") 'fennel-repl-send-input)
(define-key fennel-repl-mode-map (kbd "C-c C-q") 'fennel-repl-quit)

(defvar fennel-repl--buffer "*Fennel REPL*")

(defun fennel-repl--start (&optional ask-for-command?)
  "Launch the Fennel REPL.

If ASK-FOR-COMMAND? is supplied, asks for the command to use via
the prompt."
  (if (not (comint-check-proc fennel-repl--buffer))
      (let* ((cmd (or (and ask-for-command? (read-from-minibuffer "Command: "))
                      fennel-program))
             (cmdlist (split-string cmd)))
        (set-buffer (apply #'make-comint "Fennel REPL" (car cmdlist) nil (cdr cmdlist)))
        (fennel-repl-mode)
        (setq-local fennel-program cmd)
        (setq inferior-lisp-buffer fennel-repl--buffer)))
  (get-buffer fennel-repl--buffer))

(defun fennel-repl-quit ()
  "Kill the Fennel REPL buffer."
  (interactive)
  (let ((kill-buffer-query-functions
         (delq 'process-kill-buffer-query-function kill-buffer-query-functions)))
    (kill-buffer fennel-repl--buffer)))

(defun fennel-repl--current-input-balanced-p ()
  "Get current input from the REPL and check if it is balanced."
  (save-restriction
    (save-mark-and-excursion
      (goto-char (point-max))
      (when (search-backward-regexp (format "^%s" inferior-lisp-prompt) nil t)
        (forward-char (length inferior-lisp-prompt))
        (ignore-errors
          (let ((parse-sexp-ignore-comments t))
            (while (not (eobp))
              ;; the essence of `forward-sexp'
              (goto-char (or (scan-sexps (point) 1)
                             (buffer-end 1))))
            t))))))

(defun fennel-repl-send-input (&optional no-newline artificial)
  "Send input to the REPL process if the expression is balanced.
Otherwise insert a literal newline.

Passes NO-NEWLINE and ARTIFICIAL to `comint-send-input' function."
  (interactive)
  (if (fennel-repl--current-input-balanced-p)
      (comint-send-input no-newline artificial)
    (newline-and-indent)
    (message "[Fennel REPL] Input not complete")))

(defvar-local fennel-module-name nil
  "Buffer-local value for storing the current file's module name.")

;; see syntax.fnl to generate these next two forms:
(defvar fennel-keywords
  '("#" "%" "*" "+" "-" "->" "->>" "-?>" "-?>>" "." ".." "/" "//" ":" "<"
    "<=" "=" ">" ">=" "?." "^" "accumulate" "and" "band" "bnot" "bor" "bxor"
    "collect" "comment" "do" "doto" "each" "eval-compiler" "fn" "for"
    "global" "hashfn" "icollect" "if" "import-macros" "include" "lambda"
    "length" "let" "local" "lshift" "lua" "macro" "macrodebug" "macros"
    "match" "not" "not=" "or" "partial" "pick-args" "pick-values" "quote"
    "require-macros" "rshift" "set" "set-forcibly!" "tset" "values" "var"
    "when" "while" "with-open" "~=" "λ"))

(defvar fennel-builtin-modules
  '("_G" "arg" "bit32" "coroutine" "debug" "io" "math" "os" "package"
    "string" "table" "utf8"))

(defvar fennel-builtin-functions
  '("assert" "collectgarbage" "dofile" "error" "getmetatable" "ipairs" "load"
    "loadfile" "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen"
    "rawset" "require" "select" "setmetatable" "tonumber" "tostring" "type"
    "xpcall"))

(defvar fennel-module-functions
  '("bit32.arshift" "bit32.band" "bit32.bnot" "bit32.bor" "bit32.btest"
    "bit32.bxor" "bit32.extract" "bit32.lrotate" "bit32.lshift"
    "bit32.replace" "bit32.rrotate" "bit32.rshift" "coroutine.create"
    "coroutine.isyieldable" "coroutine.resume" "coroutine.running"
    "coroutine.status" "coroutine.wrap" "coroutine.yield" "debug.debug"
    "debug.gethook" "debug.getinfo" "debug.getlocal" "debug.getmetatable"
    "debug.getregistry" "debug.getupvalue" "debug.getuservalue"
    "debug.sethook" "debug.setlocal" "debug.setmetatable" "debug.setupvalue"
    "debug.setuservalue" "debug.traceback" "debug.upvalueid"
    "debug.upvaluejoin" "io.close" "io.flush" "io.input" "io.lines" "io.open"
    "io.output" "io.popen" "io.read" "io.tmpfile" "io.type" "io.write"
    "math.abs" "math.acos" "math.asin" "math.atan" "math.atan2" "math.ceil"
    "math.cos" "math.cosh" "math.deg" "math.exp" "math.floor" "math.fmod"
    "math.frexp" "math.ldexp" "math.log" "math.log10" "math.max" "math.min"
    "math.modf" "math.pow" "math.rad" "math.random" "math.randomseed"
    "math.sin" "math.sinh" "math.sqrt" "math.tan" "math.tanh"
    "math.tointeger" "math.type" "math.ult" "os.clock" "os.date"
    "os.difftime" "os.execute" "os.exit" "os.getenv" "os.remove" "os.rename"
    "os.setlocale" "os.time" "os.tmpname" "package.loadlib"
    "package.searchpath" "string.byte" "string.char" "string.dump"
    "string.find" "string.format" "string.gmatch" "string.gsub" "string.len"
    "string.lower" "string.match" "string.pack" "string.packsize"
    "string.rep" "string.reverse" "string.sub" "string.unpack" "string.upper"
    "table.concat" "table.insert" "table.move" "table.pack" "table.remove"
    "table.sort" "table.unpack" "utf8.char" "utf8.codepoint" "utf8.codes"
    "utf8.len" "utf8.offset"))

(defvar fennel-local-fn-pattern
  (rx (syntax open-parenthesis)
      (or "fn" "lambda" "λ" "macro") (1+ space)
      (group (1+ (or (syntax word) (syntax symbol) "-" "_")))))

(defvar fennel-local-var-pattern
  (rx (syntax open-parenthesis)
      (or "global" "var" "local") (1+ space)
      (group (1+ (or (syntax word) (syntax symbol))))))

(defvar fennel-font-lock-keywords
  `((,fennel-local-fn-pattern 1 font-lock-function-name-face)
    (,fennel-local-var-pattern 1 font-lock-variable-name-face)
    (,(regexp-opt fennel-keywords 'symbols) . font-lock-keyword-face)
    (,(regexp-opt fennel-builtin-functions 'symbols) . font-lock-builtin-face)
    (,(rx bow "$" (optional digit) eow) . font-lock-keyword-face)
    (,(rx (group ":" (1+ word))) 0 font-lock-builtin-face)
    (,(rx (group letter (0+ word) "." (1+ word))) 0 font-lock-type-face)
    (,(rx bow "&" (optional "as") eow) . font-lock-keyword-face)))

(defvar fennel-doc-string-elt-property 'doc-string-elt
  "The symbol property that holds the docstring position info.")

(defun fennel-string-in-doc-position-p (listbeg startpos)
  "Return non-nil if a doc string may occur at STARTPOS inside a list.
LISTBEG is the position of the start of the innermost list
containing STARTPOS."
  (let* ((firstsym (and listbeg
                        (save-excursion
                          (goto-char listbeg)
                          (and (looking-at "([ \t\n]*\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)")
                               (match-string 1)))))
         (docelt (and firstsym
                      (function-get (intern-soft firstsym)
                                    lisp-doc-string-elt-property))))
    (and docelt
         (save-excursion
           (when (functionp docelt)
             (goto-char (match-end 1))
             (setq docelt (funcall docelt)))
           (goto-char listbeg)
           (forward-char 1)
           (condition-case nil
               (while (and (> docelt 0) (< (point) startpos)
                           (progn (forward-sexp 1) t))
                 (setq docelt (1- docelt)))
             (error nil))
           (and (zerop docelt) (<= (point) startpos)
                (progn (forward-comment (point-max)) t)
                (= (point) startpos))))))

(defun fennel-font-lock-syntactic-face-function (state)
  "Return syntactic face function for the position represented by STATE.
STATE is a `parse-partial-sexp' state, and the returned function is the
Lisp font lock syntactic face function."
  (if (nth 3 state)
      (let ((startpos (nth 8 state)))
        (let ((listbeg (nth 1 state)))
          (if (fennel-string-in-doc-position-p listbeg startpos)
              font-lock-doc-face
            font-lock-string-face)))
    font-lock-comment-face))

(defun fennel-font-lock-setup ()
  "Setup font lock for keywords."
  (setq font-lock-defaults
        '(fennel-font-lock-keywords
          nil nil
          (("+-*/.<>=!?$%_&:" . "w"))
          nil
          (font-lock-mark-block-function . mark-defun)
          (font-lock-syntactic-face-function
           . fennel-font-lock-syntactic-face-function))))

(defvar calculate-lisp-indent-last-sexp)

(defun fennel-indent-function (indent-point state)
  "Simplified version of function `lisp-indent-function'.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (let* ((fn (buffer-substring (point) (progn (forward-sexp 1) (point))))
           (open-paren (elt state 1))
           (method (get (intern-soft fn) 'fennel-indent-function)))
      (cond ((member (char-after open-paren) '(?\[ ?\{))
             (goto-char open-paren)
             (skip-chars-forward (string (char-after open-paren)))
             (if (looking-at-p "[ \t\n]")
                 (progn
                   (skip-chars-forward "[ \t\n]")
                   (current-column))
               (goto-char open-paren)
               (+ 1 (current-column))))
            ((eq method 'defun)
             (lisp-indent-defform state indent-point))
            ((integerp method)
             (lisp-indent-specform method state indent-point normal-indent))
            (method
             (funcall method indent-point state))))))

(defun fennel-space-for-delimiter-p (endp _delim)
  "Prevent paredit from inserting useless spaces.
See `paredit-space-for-delimiter-predicates' for the meaning of
ENDP and DELIM."
  (and (not endp)
       ;; don't insert after opening quotes, auto-gensym syntax
       (not (looking-back "\\_<#" (point-at-bol)))))

;;;###autoload
(define-derived-mode fennel-mode prog-mode "Fennel"
  "Major mode for editing Fennel code.

\\{fennel-mode-map}"
  (setq-local comment-add 1)       ; default to `;;' in comment-region
  (setq-local comment-column 40)
  (setq-local comment-end "")
  (setq-local comment-start ";")
  (setq-local comment-start-skip ";+ *")
  (setq-local comment-use-syntax t)
  (setq-local electric-pair-open-newline-between-pairs nil)
  (setq-local electric-pair-skip-whitespace 'chomp)
  (setq-local fill-paragraph-function #'lisp-fill-paragraph)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local lisp-doc-string-elt-property 'fennel-doc-string-elt)
  (setq-local lisp-indent-function #'fennel-indent-function)
  (setq-local multibyte-syntax-as-symbol t)
  (setq-local normal-auto-fill-function #'do-auto-fill)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local outline-level 'lisp-outline-level)
  (setq-local outline-regexp ";;;;* ")
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local inferior-lisp-program fennel-program)
  ;; NOTE: won't work if the fennel module name has changed but beats nothing
  (setq-local inferior-lisp-load-command "((. (require :fennel) :dofile) %s)")
  (when (version<= "26.1" emacs-version)
    (setq-local adaptive-fill-function #'lisp-adaptive-fill))
  (add-to-list 'imenu-generic-expression `(nil ,fennel-local-fn-pattern 1))
  (make-local-variable 'completion-at-point-functions)
  (add-to-list 'completion-at-point-functions 'fennel-complete)
  (set-syntax-table fennel-mode-syntax-table)
  (fennel-font-lock-setup)
  (add-hook 'paredit-mode-hook #'fennel-paredit-setup nil t))

(put 'fn 'fennel-doc-string-elt 3)
(put 'lambda 'fennel-doc-string-elt 3)
(put 'λ 'fennel-doc-string-elt 3)
(put 'macro 'fennel-doc-string-elt 3)

(defun fennel--paredit-setup (mode-map)
  "Setup paredit keys for given MODE-MAP."
  (define-key mode-map "{" #'paredit-open-curly)
  (define-key mode-map "}" #'paredit-close-curly)
  (make-local-variable 'paredit-space-for-delimiter-predicates)
  (add-to-list 'paredit-space-for-delimiter-predicates #'fennel-space-for-delimiter-p))

(defun fennel-paredit-setup ()
  "Setup paredit keys."
  (fennel--paredit-setup fennel-mode-map))

(defun fennel-repl-paredit-setup ()
  "Setup paredit keys in `fennel-repl-mode'."
  (fennel--paredit-setup fennel-repl-mode-map))

(defun fennel-get-module (ask? last-module)
  "Ask for the name of a module for the current file; return keyword.

If ASK? or LAST-MODULE were not supplied, asks for the name of a module."
  (let ((module (if (or ask? (not last-module))
                    (read-string "Module: " (or last-module (file-name-base nil)))
                  last-module)))
    (setq fennel-module-name module) ; remember for next time
    (intern (concat ":" module))))

(defun fennel-reload-form (module-keyword)
  "Return a string of the code to reload the MODULE-KEYWORD module."
  (format "%s\n" `(let [old (require ,module-keyword)
                            _ (tset package.loaded ,module-keyword nil)
                            (ok new) (pcall require ,module-keyword)
                            ;; keep the old module if reload failed
                            new (if (not ok) (do (print new) old) new)]
                    ;; if the module isn't a table then we can't make
                    ;; changes which affect already-loaded code, but if
                    ;; it is then we should splice new values into the
                    ;; existing table and remove values that are gone.
                    (when (and (= (type old) :table) (= (type new) :table))
                      (each [k v (pairs new)]
                            (tset old k v))
                      (each [k (pairs old)]
                            ;; the elisp reader is picky about where . can be
                            (when (= nil ("." new k))
                              (tset old k nil)))
                      (tset package.loaded ,module-keyword old)))))

(defun fennel-reload (ask?)
  "Reload the module for the current file.

ASK? forces module name prompt.

Tries to reload in a way that makes it retroactively visible; if
the module returns a table, then existing references to the same
module will have their contents updated with the new
value.  Requires installing `fennel.searcher'.

Queries the user for a module name upon first run for a given
buffer, or when given a prefix arg."
  (interactive "P")
  (comint-check-source buffer-file-name)
  (let* ((module (fennel-get-module ask? fennel-module-name)))
    (when (and (file-exists-p (concat (file-name-base nil) ".lua"))
               (yes-or-no-p "Lua file for module exists; delete it first?"))
      (delete-file (concat (file-name-base nil) ".lua")))
    (comint-send-string (inferior-lisp-proc) (fennel-reload-form module)))
  (when fennel-mode-switch-to-repl-after-reload
    (switch-to-lisp t)))


(defun fennel-completion--candidate-kind (item)
  "Annotate completion kind of the ITEM for company mode.

Annotations are based on ITEM appearing either in
`fennel-keywords', `fennel-builtins'  or `fennel-functions'."
  (cond ((member item fennel-keywords) 'keyword)
        ((member item fennel-builtin-modules) 'module)
        ((or (member item fennel-builtin-functions)
             (member item fennel-module-functions)
             (string-match-p ":" item))
         'function)
        ((string-match-p "\\." item) 'field)
        (t 'variable)))

(defun fennel-completion--annotate (item)
  "Annotate completion kind of the ITEM.

Annotations are based on ITEM appearing either in
`fennel-keywords', `fennel-builtins' or `fennel-functions'.  Uses
`fennel-completion--candidate-kind' to obtain kind name, but
ignores variable because it's a bit loose definition here."
  (let ((kind (fennel-completion--candidate-kind item)))
    (if (and fennel-mode-annotate-completion
             (not (eq kind 'variable)))
        (format " %s" kind)
      "")))

(defun fennel-completions (input)
  "Query completions for the INPUT from the `inferior-lisp-proc'."
  (condition-case nil
      (when input
        (let ((command (format ",complete %s" input))
              (buf (get-buffer-create "*fennel-completion*"))
              (prompt inferior-lisp-prompt) ; need to cache before with-current-buffer
              (proc (inferior-lisp-proc)))
          (with-current-buffer buf
            (delete-region (point-min) (point-max))
            (let ((comint-prompt-regexp prompt))
              (comint-redirect-send-command-to-process command buf proc nil t))
            (accept-process-output proc 0.01)
            (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
              ;; strip ansi escape codes added by readline
              (split-string (ansi-color-apply contents))))))
    (error nil)))

(defun fennel-complete ()
  "Return a list of completion data for `completion-at-point'.

Requires Fennel 0.9.3+."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (completions (fennel-completions (symbol-at-point))))
    (when completions
      (list (car bounds)
            (cdr bounds)
            completions
            :annotation-function #'fennel-completion--annotate
            :company-kind #'fennel-completion--candidate-kind))))

(defun fennel-find-definition-go (location)
  "Go to the definition LOCATION."
  (when (string-match "^@\\(.+\\)!\\(.+\\)" location)
    (let ((file (match-string 1 location))
          (line (string-to-number (match-string 2 location))))
      (when file (find-file file))
      (when line
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun fennel-find-definition-for (identifier)
  "Find the definition of IDENTIFIER."
  (let ((tempfile (make-temp-file "fennel-find-")))
    (comint-send-string
     (inferior-lisp-proc)
     (format "%s\n"
             `(with-open [f (io.open ,(format "\"%s\"" tempfile) :w)]
                         (match (-?> ,identifier (debug.getinfo))
                                {:what :Lua
                                : source : linedefined} (f:write source :! linedefined)))))
    (sit-for 0.1)
    (unwind-protect
        (when (file-exists-p tempfile)
          (with-temp-buffer
            (insert-file-contents tempfile)
            (delete-file tempfile)
            (buffer-substring-no-properties (point-min) (point-max)))))))

(defun fennel-find-definition (identifier)
  "Jump to the definition of the function IDENTIFIER at point.
This will only work when the reference to the function is in scope for the repl;
for instance if you have already entered (local foo (require :foo)) then foo.bar
can be resolved.  It also requires line number correlation."
  (interactive (list (let* ((default-value (symbol-at-point))
                            (prompt (if default-value
                                        (format "Find definition (default %s): "
                                                default-value)
                                      "Find definition: ")))
                       (read-string prompt nil nil default-value))))
  (xref-push-marker-stack (point-marker))
  (fennel-find-definition-go (fennel-find-definition-for identifier)))

(defvar fennel-module-history nil)
(defvar fennel-field-history nil)

(defun fennel-find-module-field (module fields-string)
  "Find FIELDS-STRING in the MODULE."
  (let ((tempfile (make-temp-file "fennel-module-"))
        (fields (mapcar (apply-partially 'concat ":")
                        (split-string fields-string "\\."))))
    (comint-send-string
     (inferior-lisp-proc)
     (format "%s\n"
             `(with-open [f (io.open ,(format "\"%s\"" tempfile) :w)]
                         (match (-?> ("." (require ,(format "\"%s\"" module)) ,@fields)
                                     (debug.getinfo))
                                {:what :Lua
                                : source : linedefined} (f:write source :! linedefined)))))
    (sit-for 0.1)
    (unwind-protect
        (when (file-exists-p tempfile)
          (with-temp-buffer
            (insert-file-contents tempfile)
            (delete-file tempfile)
            (buffer-substring-no-properties (point-min) (point-max)))))))

(defun fennel-find-module-definition ()
  "Ask for the module and the definition to find in that module."
  (interactive)
  (let* ((module (read-from-minibuffer "Find in module: " nil nil nil
                                       'fennel-module-history
                                       (car fennel-module-history)))
         (fields (read-from-minibuffer "Find module field: " nil nil nil
                                       'fennel-field-history
                                       (car fennel-field-history))))
    (xref-push-marker-stack (point-marker))
    (fennel-find-definition-go (fennel-find-module-field module fields))))

(defun fennel-find-definition-pop ()
  "Return point to previous position in previous buffer."
  (interactive)
  (require 'etags)
  (let ((marker (xref-pop-marker-stack)))
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun fennel-view-compilation ()
  "Compile the current buffer and view the output."
  (interactive)
  (let ((compile-command (format "fennel --compile %s" (buffer-file-name))))
    (switch-to-buffer (format "*fennel %s*" (buffer-name)))
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert (shell-command-to-string compile-command))
    (lua-mode)
    (read-only-mode)
    (goto-char (point-min))))

;;;###autoload
(defun fennel-repl (ask-for-command? &optional buffer)
  "Switch to the fennel repl BUFFER, or start a new one if needed.

If there was a REPL buffer but its REPL process is dead,
a new one is started in the same buffer.

If ASK-FOR-COMMAND? was supplied, asks for command to start the
REPL.  If optional BUFFER is supplied it is used as the last
buffer before starting the REPL.

The command is persisted as a buffer-local variable, the REPL
buffer remembers the command that was used to start it.
Resetting the command to another value can be done by invoking
setting ASK-FOR-COMMAND? to non-nil, i.e.  by using a prefix
argument.

Return this buffer."
  (interactive "P")
  (if (eq major-mode 'fennel-repl-mode)
      (when fennel-repl--last-fennel-buffer
        (switch-to-buffer-other-window fennel-repl--last-fennel-buffer))
    (let ((last-buf (or buffer (current-buffer)))
          (repl-buf (or (get-buffer fennel-repl--buffer)
                        (fennel-repl--start ask-for-command?))))
      (with-current-buffer repl-buf
        (setq fennel-repl--last-fennel-buffer last-buf))
      (pop-to-buffer repl-buf)))
  (unless (comint-check-proc fennel-repl--buffer)
    (fennel-repl--start ask-for-command?)))

(defun fennel-format ()
  "Run fnlfmt on the current buffer."
  (interactive)
  (if (executable-find "fnlfmt")
      (shell-command-on-region (point-min) (point-max) "fnlfmt -" nil t)
    (message "fnlfmt not found")))

(define-key fennel-mode-map (kbd "M-.") 'fennel-find-definition)
(define-key fennel-mode-map (kbd "M-,") 'fennel-find-definition-pop)
(define-key fennel-mode-map (kbd "M-'") 'fennel-find-module-definition)
(define-key fennel-mode-map (kbd "C-c C-k") 'fennel-reload)
(define-key fennel-mode-map (kbd "C-c C-l") 'fennel-view-compilation)
(define-key fennel-mode-map (kbd "C-c C-z") 'fennel-repl)
(define-key fennel-mode-map (kbd "C-c C-t") 'fennel-format)
(define-key fennel-mode-map (kbd "C-c C-f") 'fennel-show-documentation)
(define-key fennel-mode-map (kbd "C-c C-d") 'fennel-show-documentation)
(define-key fennel-mode-map (kbd "C-c C-v") 'fennel-show-variable-documentation)
(define-key fennel-mode-map (kbd "C-c C-a") 'fennel-show-arglist)
;; lisp-mode functions
(define-key fennel-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
(define-key fennel-mode-map (kbd "C-c C-e") 'lisp-eval-defun)
(define-key fennel-mode-map (kbd "C-M-x") 'lisp-eval-defun)
(define-key fennel-mode-map (kbd "C-c C-n") 'lisp-eval-form-and-next)
(define-key fennel-mode-map (kbd "C-c C-p") 'lisp-eval-paragraph)
(define-key fennel-mode-map (kbd "C-c C-r") 'lisp-eval-region)

(put 'lambda 'fennel-indent-function 'defun)
(put 'λ 'fennel-indent-function 'defun)
(put 'fn 'fennel-indent-function 'defun)
(put 'while 'fennel-indent-function 'defun)
(put 'do 'fennel-indent-function 0)
(put 'let 'fennel-indent-function 1)
(put 'when 'fennel-indent-function 1)
(put 'for 'fennel-indent-function 1)
(put 'each 'fennel-indent-function 1)
(put 'eval-compiler 'fennel-indent-function 'defun)
(put 'macro 'fennel-indent-function 'defun)
(put 'doto 'fennel-indent-function 1)
(put 'match 'fennel-indent-function 1)
(put 'with-open 'fennel-indent-function 1)
(put 'collect 'fennel-indent-function 1)
(put 'icollect 'fennel-indent-function 1)
(put 'accumulate 'fennel-indent-function 1)
(put 'pick-values 'fennel-indent-function 1)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))

(provide 'fennel-mode)
;;; fennel-mode.el ends here
