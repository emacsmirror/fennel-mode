;;; antifennel.el --- Turn Lua code into Fennel -*- lexical-binding: t -*-

;; Copyright © 2022 Phil Hagelberg and contributors

;; Author: Phil Hagelberg
;; URL: https://git.sr.ht/~technomancy/fennel-mode
;; Version: 0.0.1
;; Created: 2022-03-27
;; Package-Requires: ((fennel-mode "0.1.0"))
;;
;; Keywords: languages, tools

;;; Commentary:

;; Turn Lua code into Fennel code from Emacs.
;; Requires install of antifennel: https://git.sr.ht/~technomancy/antifennel

;; Recommended usage:

;; (add-hook 'lua-mode-hook 'antifennel-mode)

;; This will make it so that C-c C-f in a lua-mode buffer will display
;; the Fennel equivalent.  You can also invoke it with M-x antifennel-buffer.

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

(defvar antifennel-program "antifennel")

(defvar antifennel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-f") 'antifennel-buffer)
    map))

;;;###autoload
(define-minor-mode antifennel-mode
  "Quickly turn Lua code into Fennel."
  :keymap antifennel-mode-map)

;;;###autoload
(defun antifennel-buffer ()
  "Compile the contents of the current buffer's file from Lua to Fennel."
  (interactive)
  (let ((compile-command (concat antifennel-program " " (buffer-file-name))))
    (switch-to-buffer (format "*antifennel %s*" (buffer-name)))
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert (shell-command-to-string compile-command))
    (fennel-mode)
    (read-only-mode)
    (goto-char (point-min))))

;;;###autoload
(defun antifennel-region (beg end)
  "Compile the region of the current buffer's file from Lua to Fennel."
  (interactive "r")
  (save-excursion
    (let* ((temp-file (make-temp-file "antifennel-"))
           (compile-command (concat antifennel-program " " temp-file)))
      (kill-ring-save beg end)
      (write-region (substring-no-properties (car kill-ring)) nil temp-file 'append)
      (switch-to-buffer (get-buffer-create "*antifennel*"))
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (insert (shell-command-to-string compile-command))
      (delete-file temp-file)
      (fennel-mode)
      (read-only-mode)
      (goto-char (point-min)))))

(provide 'antifennel)
;;; antifennel.el ends here
