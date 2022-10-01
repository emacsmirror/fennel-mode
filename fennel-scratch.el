;;; fennel-scratch.el --- A scratch buffer for Fennel -*- lexical-binding: t -*-

;; Copyright © 2018-2021 Phil Hagelberg and contributors
;;
;; Author: Andrey Listopadov

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

(defcustom fennel-scratch-message ";; This buffer is for Fennel evaluation.
;; Use \\[fennel-scratch-eval-print-last-sexp] after the expression to evaluate it and insert the result.

"
  "Initial documentation displayed in *fennel-scratch* buffer.
If this is nil, no message will be displayed."
  :group 'fennel-mode
  :package-version '(fennel-mode "0.4.1")
  :type '(choice (text :tag "Message")
		 (const :tag "none" nil)))

(defvar fennel-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map fennel-mode-map)
    (define-key map "\C-j" 'fennel-scratch-eval-print-last-sexp)
    map)
  "Keymap for *fennel-scratch* buffer.
All commands in `fennel-mode-map' are inherited by this map.")

(defun fennel-scratch--get-buffer (cmd)
  "Return the scratch buffer, create it if necessary."
  (or (get-buffer "*fennel-scratch*")
      (with-current-buffer (get-buffer-create "*fennel-scratch*")
        (prog1 (current-buffer)
	  (fennel-mode)
	  (use-local-map fennel-scratch-mode-map)
          (when fennel-scratch-message
            (insert (substitute-command-keys fennel-scratch-message)))
	  (fennel-repl--start cmd)))))

(defun fennel-scratch--eval-to-string (sexp)
  "Send SEXP to the inferior lisp process, return result as a string."
  (let ((sexp (string-trim (substring-no-properties sexp)))
        (buf (get-buffer-create " *fennel-eval*"))
        (prompt inferior-lisp-prompt)
        (proc (inferior-lisp-proc)))
    (fennel-repl-redirect-one proc sexp buf)
    (with-current-buffer buf
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

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
  "Create or open an existing scratch buffer for Fennel evaluation."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Fennel command: " fennel-program)
	   fennel-program)))
  (set-buffer (fennel-scratch--get-buffer ask-for-command?))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(provide 'fennel-scratch)
;;; fennel-scratch.el ends here
