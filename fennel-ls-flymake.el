;;; fennel-ls-flymake.el --- Flymake backend for the Fennel language server -*- lexical-binding: t; -*-

;; Copyright © 2023 Phil Hagelberg and contributors

;; Author: Andrey Listopadov
;; URL: https://git.sr.ht/~technomancy/fennel-mode
;; Version: 0.1.0
;; Created: 2025-03-05
;; Package-Requires: ((emacs "26.1"))
;; Keywords: languages, tools

;;; Commentary:
;;
;; This package uses fennel-ls --lint as a flymake backend.

;;; Code:

(require 'flymake)

(defconst fennel-ls-flymake--diagnositc-regexp
  "^.+:\\([[:digit:]]+\\):\\([[:digit:]]+\\):[[:space:]]*\\([[:space:][:alpha:]]+\\):[[:space:]]+\\(.*\\)$"
  "A regex for matching fennel-ls diagnostics.")

(defvar-local fennel-ls-flymake--proc nil
  "Active fennel-ls process.")

(defun fennel-ls-flymake-backend (report-fn &rest _args)
  "Build the Flymake backend for fennel-ls linter with REPORT-FN."
  (unless (executable-find "fennel-ls")
    (user-error "Executable fennel-ls not found on PATH"))

  (when (process-live-p fennel-ls-flymake--proc)
    (kill-process fennel-ls-flymake--proc))

  (let ((default-directory
         (or (locate-dominating-file (buffer-file-name) "flsproject.fnl")
             (file-name-directory (buffer-file-name))))
        (source (current-buffer)))
    (save-restriction
      (widen)
      (setq
       fennel-ls-flymake--proc
       (make-process
        :name "fennel-ls-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *fennel-ls-flymake*")
        :command '("fennel-ls" "--lint" "-")
        :sentinel
        (lambda (proc _event)
          (when (memq (process-status proc) '(exit signal))
            (unwind-protect
                (if (with-current-buffer source (eq proc fennel-ls-flymake--proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop while (search-forward-regexp fennel-ls-flymake--diagnositc-regexp nil t)
                               for lnum = (string-to-number (match-string 1))
                               for lcol = (string-to-number (match-string 2))
                               for type = (let ((severity (match-string 3)))
                                            (pcase severity
                                              ("error" :error)
                                              ("warning" :warning)
                                              (_ :note)))
                               for msg = (match-string 4)
                               for (beg . end) = (flymake-diag-region source lnum lcol)
                               collect (flymake-make-diagnostic source beg end type msg)
                               into diags
                               finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s" proc))
              (kill-buffer (process-buffer proc)))))))
      (process-send-region fennel-ls-flymake--proc (point-min) (point-max))
      (process-send-eof fennel-ls-flymake--proc))))

;;;###autoload
(defun fennel-ls-flymake ()
  "Setup fennel-ls integration with Flymake."
  (interactive)
  (if (< emacs-major-version 26)
      (error "Fennel-ls-flymake requires Emacs 26 or later")
    (add-hook 'flymake-diagnostic-functions #'fennel-ls-flymake-backend nil t)
    (flymake-mode)))

(provide 'fennel-ls-flymake)
;;; fennel-ls-flymake.el ends here
