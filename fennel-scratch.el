;;; fennel-scratch.el --- A scratch buffer for Fennel -*- lexical-binding: t -*-

;; Copyright © 2018-2021 Phil Hagelberg and contributors
;;
;; Author: Andrey Listopadov
;; URL: https://git.sr.ht/~technomancy/fennel-mode
;; Version: 0.1.1
;; Created: 2021-11-22
;; Package-Requires: ((emacs "26.1") (fennel-mode "0.7.0"))
;;
;; Keywords: languages, tools

;;; Commentary:

;; Provides a scratch buffer for Fennel evaluation, much like
;; `lisp-interaction-mode' does.

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

(require 'fennel-mode)

(declare-function fennel-proto-repl "ext:fennel-proto-repl")
(declare-function fennel-proto-repl-send-message-sync "ext:fennel-proto-repl")

(defcustom fennel-scratch-message ";; This buffer is for Fennel evaluation.
;; Use \\[fennel-scratch-eval-print-last-sexp] after the expression to evaluate it and insert the result.

"
  "Initial documentation displayed in *fennel-scratch* buffer.
If this is nil, no message will be displayed."
  :group 'fennel-mode
  :package-version '(fennel-mode "0.4.1")
  :type '(choice (text :tag "Message")
		 (const :tag "none" nil)))

(defcustom fennel-scratch-use-proto-repl nil
  "Whether to use the `fennel-proto-repl' as a backend for the `fennel-scratch'."
  :group 'fennel-mode
  :package-version '(fennel-mode "0.8.1")
  :type 'boolean)

(defvar fennel-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fennel-mode-map)
    (define-key map "\C-j" 'fennel-scratch-eval-print-last-sexp)
    map)
  "Keymap for *fennel-scratch* buffer.
All commands in `fennel-mode-map' are inherited by this map.")

(defun fennel-scratch--get-buffer (cmd)
  "Return the scratch buffer, create it if necessary.
CMD is used to start the REPL process."
  (or (get-buffer "*fennel-scratch*")
      (with-current-buffer (get-buffer-create "*fennel-scratch*")
        (prog1 (current-buffer)
	  (fennel-mode)
	  (use-local-map fennel-scratch-mode-map)
          (when fennel-scratch-message
            (insert (substitute-command-keys fennel-scratch-message)))
	  (if fennel-scratch-use-proto-repl
              (fennel-proto-repl cmd)
            (fennel-repl--start cmd))))))

(defun fennel-scratch--eval-to-string (sexp)
  "Send SEXP to the Fennel process, return result as a string."
  (let ((sexp (string-trim (substring-no-properties sexp))))
    (if fennel-scratch-use-proto-repl
        (let (string)
          (if-let ((res (fennel-proto-repl-send-message-sync
                         :eval sexp
                         (lambda (_ message traceback)
                           (setq string (string-join (list message (or traceback "")) "\n"))))))
              (string-join res "\t")
              (or string "")))
      (let ((buf (get-buffer-create " *fennel-eval*"))
            (proc (inferior-lisp-proc)))
        (fennel-repl-redirect-one proc sexp buf)
        (with-current-buffer buf
          (string-trim (buffer-substring-no-properties (point-min) (point-max))))))))

(defun fennel-scratch-eval-print-last-sexp ()
  "Evaluate s-expression before point and print value into current buffer."
  (interactive)
  (let* ((sexp (buffer-substring (save-excursion (backward-sexp) (point)) (point)))
         (result (fennel-scratch--eval-to-string sexp)))
    (newline)
    (unless (string-empty-p result)
      (insert result)
      (newline))))

;;;###autoload
(defun fennel-scratch (&optional ask-for-command?)
  "Create or open an existing scratch buffer for Fennel evaluation.
With prefix argument ASK-FOR-COMMAND? asks for the command to
start the REPL process."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Fennel command: " fennel-program)
	   fennel-program)))
  (set-buffer (fennel-scratch--get-buffer ask-for-command?))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(provide 'fennel-scratch)
;;; fennel-scratch.el ends here
