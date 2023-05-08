;;; -*- lexical-binding: t; -*-
(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el"
              (:send-report nil)
              (:report-format 'text)))

(require 'ert)
(require 'compat nil t)
(require 'fennel-proto-repl)

(defmacro with-fennel-proto-repl (&rest body)
  "Starts the Fennel Proto REPL and evals BODY in its context."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t))
     (with-current-buffer (fennel-proto-repl "fennel --repl")
       (unwind-protect
           (progn ,@body)
         (fennel-proto-repl-quit)
         (kill-buffer)))))

(defmacro fpr--send-input (string)
  (declare (debug t))
  `(progn
     (goto-char (point-max))
     (fennel-proto-repl--input-sender nil ,string)
     (while (fennel-proto-repl--callbacks-pending)
       (accept-process-output nil 0.01))
     (fennel-proto-repl--display-prompt)))

(defmacro fpr--with-mocks (bindings &rest body)
  (declare (indent 1))
  `(cl-letf ,(cl-loop for pair in bindings
                      collect (list `(symbol-function ',(car pair)) (cadr pair)))
     ,@body))

(ert-deftest fpr-start-repl-test ()
  "Starting the Proto REPL and checking its buffers and processes."
  (let* ((inhibit-message t)
         (repl (fennel-proto-repl "fennel --repl")))
    (should (bufferp repl))
    (should (bufferp fennel-proto-repl--buffer))
    (should (eq repl fennel-proto-repl--buffer))
    (should (process-live-p (get-buffer-process repl)))
    (with-current-buffer repl
      (should (bufferp fennel-proto-repl--process-buffer))
      (should (process-live-p (get-buffer-process fennel-proto-repl--process-buffer)))
      (fennel-proto-repl-quit)
      (should-not (buffer-live-p fennel-proto-repl--process-buffer))
      (should-not (process-live-p (get-buffer-process repl))))))

(ert-deftest fpr-start-repl-in-custom-buffer-test ()
  "Starting the Proto REPL and checking its buffers and processes."
  (let* ((inhibit-message t)
         (buffer (get-buffer-create "repl"))
         (repl (fennel-proto-repl "fennel --repl" buffer)))
    (unwind-protect
        (should (eq repl buffer))
      (with-current-buffer repl
        (fennel-proto-repl-quit)
        (kill-buffer)))))

(ert-deftest fpr-multiple-live-repls-test ()
  (with-temp-buffer
    (let* ((inhibit-message t)
           (repl1 (fennel-proto-repl "fennel --repl"))
           (repl2 (fennel-proto-repl "fennel --repl"))
           (repl3 (fennel-proto-repl "fennel --repl")))
      (should-not (or (eq repl1 repl2) (eq repl1 repl3) (eq repl2 repl3)))
      (with-current-buffer repl1 (rename-buffer "repl1"))
      (with-current-buffer repl2 (rename-buffer "repl2"))
      (with-current-buffer repl3 (rename-buffer "repl3"))
      (should (memq repl1 (fennel-proto-repl-live-repls)))
      (should (memq repl2 (fennel-proto-repl-live-repls)))
      (should (memq repl3 (fennel-proto-repl-live-repls)))
      (with-current-buffer repl2 (fennel-proto-repl-quit))
      (should-not (memq repl2 (fennel-proto-repl-live-repls)))
      (kill-buffer repl2)
      (with-current-buffer repl1 (fennel-proto-repl-quit))
      (should-not (memq repl1 (fennel-proto-repl-live-repls)))
      (kill-buffer repl1)
      (with-current-buffer repl3 (fennel-proto-repl-quit))
      (should-not (memq repl3 (fennel-proto-repl-live-repls)))
      (kill-buffer repl3)
      (should-not (fennel-proto-repl-live-repls)))))

(ert-deftest fpr-link-test ()
  "Linking current buffer to a specific REPL"
  (let* ((inhibit-message t)
         (repl1 (fennel-proto-repl "fennel --repl"))
         (repl2 (fennel-proto-repl "fennel --repl")))
    (unwind-protect
        (with-temp-buffer
          (should-not (eq repl1 repl2))
          (fennel-proto-repl-link-buffer repl1)
          (should (eq repl1 fennel-proto-repl--buffer))
          (should (eq (with-current-buffer repl1 fennel-proto-repl--process-buffer)
                      (fennel-proto-repl--process-buffer)))
          (fennel-proto-repl-link-buffer repl2)
          (should (eq repl2 fennel-proto-repl--buffer))
          (should (eq (with-current-buffer repl2 fennel-proto-repl--process-buffer)
                      (fennel-proto-repl--process-buffer))))
      (with-current-buffer repl1 (fennel-proto-repl-quit) (kill-buffer))
      (with-current-buffer repl2 (fennel-proto-repl-quit) (kill-buffer)))))

(ert-deftest fpr-interactive-link-test ()
  (let* ((inhibit-message t)
         (repl1 (fennel-proto-repl "fennel --repl"))
         (repl2 (fennel-proto-repl "fennel --repl")))
    (unwind-protect
        (with-temp-buffer
          (with-current-buffer repl1 (rename-buffer "repl-a"))
          (with-current-buffer repl2 (rename-buffer "repl-b"))
          (fpr--with-mocks ((completing-read (lambda (&rest _) "repl-a")))
            (fennel-proto-repl-link-buffer))
          (should (eq repl1 fennel-proto-repl--buffer))
          (should (eq (with-current-buffer repl1 fennel-proto-repl--process-buffer)
                      (fennel-proto-repl--process-buffer)))
          (fpr--with-mocks ((completing-read (lambda (&rest _) "repl-b")))
            (fennel-proto-repl-link-buffer))
          (should (eq repl2 fennel-proto-repl--buffer))
          (should (eq (with-current-buffer repl2 fennel-proto-repl--process-buffer)
                      (fennel-proto-repl--process-buffer))))
      (with-current-buffer repl1 (fennel-proto-repl-quit) (kill-buffer))
      (with-current-buffer repl2 (fennel-proto-repl-quit) (kill-buffer)))))

(ert-deftest fpr-send-message-test ()
  "Sending a successful asynchronous request."
  (with-fennel-proto-repl
    (let (res done)
      (fennel-proto-repl-send-message
       :eval "(+ 1 2 3)"
       (lambda (values) (setq done t) (setq res values)))
      (while (not done) (accept-process-output nil 0.01))
      (should (equal '("6") res)))))

(ert-deftest fpr-send-message-error-test ()
  "Sending a errornous asynchronous request."
  (with-fennel-proto-repl
    (should-error
     (fennel-proto-repl-send-message-sync :eval "x" (lambda (&rest _) (error "ok"))))))

(ert-deftest fpr-send-message-io-test ()
  "Sending a successful asynchronous request with IO."
  (with-fennel-proto-repl
    (let (res done)
      (fennel-proto-repl-send-message
       :eval "(print 1234)"
       (lambda (_) (setq done t))
       #'ignore
       (lambda (data) (setq res data)))
      (while (not done) (accept-process-output nil 0.01))
      (should (equal "1234\n" res)))))

(ert-deftest fpr-send-message-sync-test ()
  "Sending a successful synchronous request."
  (with-fennel-proto-repl
    (should
     (equal '("6") (fennel-proto-repl-send-message-sync :eval "(+ 1 2 3)")))))

(ert-deftest fpr-send-message-sync-error-test ()
  "Signaling an error from a synchronous request."
  (with-fennel-proto-repl
    (should-error
     (fennel-proto-repl-send-message-sync :eval "x" (lambda (&rest _) (error "ok"))))))

(ert-deftest fpr-send-message-sync-io-test ()
  "Sending a successful asynchronous request."
  (with-fennel-proto-repl
    (let (res)
      (fennel-proto-repl-send-message-sync
       :eval "(print 1234)"
       #'ignore
       (lambda (data) (setq res data)))
      (should (equal "1234\n" res)))))

(ert-deftest fpr-completion-test ()
  (with-fennel-proto-repl
    (let ((res (with-temp-buffer
                 (insert "f")
                 (fennel-proto-repl-complete))))
      (should (pcase res
                ((seq  1 2
		       _
		       :annotation-function fennel-proto-repl--completion-annotate
                       :company-kind fennel-proto-repl--completion-candidate-kind
                       :company-doc-buffer fennel-proto-repl--eldoc-get-doc-buffer)
                 t)))
      (let ((completions (funcall (nth 2 res) "f" #'identity t)))
        (should (listp completions))
        (should (equal '("faccumulate" "fcollect" "fn" "for")
                       (sort completions #'string<))))
      (should (equal " keyword" (funcall (nth 4 res) "fn")))
      (should (equal 'keyword (funcall (nth 6 res) "fn")))
      (should (equal 'field (funcall (nth 6 res) "a.b")))
      (should (equal 'variable (funcall (nth 6 res) "a"))))))

(ert-deftest fpr-no-live-repl-test ()
  (should-not (fennel-proto-repl-send-message-sync :eval "(+ 1 2)")))

(ert-deftest fpr-double-quit-test ()
  (with-temp-buffer
    (let* ((inhibit-message t)
           (repl (fennel-proto-repl "fennel --repl")))
      (fennel-proto-repl-quit)
      (should-error (fennel-proto-repl-quit)))))

(ert-deftest fpr-restarting-in-repl-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (fennel-proto-repl-quit)
      (should-not (memq repl (fennel-proto-repl-live-repls)))
      (fennel-proto-repl--check-for-repl)
      (should (memq repl (fennel-proto-repl-live-repls))))))

(ert-deftest fpr-restarting-outside-repl-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer)
        (fennel-proto-repl-quit)
        (should-not (memq repl (fennel-proto-repl-live-repls)))
        (fennel-proto-repl--check-for-repl)
        (should (memq repl (fennel-proto-repl-live-repls)))))))

(ert-deftest fpr-switching-to-repl-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer)
        (fennel-proto-repl-switch-to-repl)
        (should (eq repl (current-buffer)))))))

(ert-deftest fpr-switching-from-repl-test ()
  (let ((buf (get-buffer-create "test-buffer")))
    (unwind-protect
        (with-current-buffer buf
          (with-fennel-proto-repl
            (fennel-proto-repl-switch-to-repl)
            (should (eq buf (current-buffer)))))
      (kill-buffer buf))))

(ert-deftest fpr-switching-from-dead-repl-test ()
  (let ((buf (get-buffer-create "test-buffer")))
    (unwind-protect
        (with-current-buffer buf
          (with-fennel-proto-repl
            (let ((repl (current-buffer)))
              (fennel-proto-repl-quit)
              (fennel-proto-repl-switch-to-repl)
              (should (eq repl (current-buffer)))
              (should (memq repl (fennel-proto-repl-live-repls)))
              (should (eq buf fennel-proto-repl--last-buffer)))))
      (kill-buffer buf))))

(ert-deftest fpr-disabling-fpr-interaction-test ()
  (let ((buf (get-buffer-create "test-buffer")))
    (unwind-protect
        (with-current-buffer buf
          (with-fennel-proto-repl nil)
          (should fennel-proto-repl-minor-mode)
          (should (memq 'fennel-proto-repl-complete completion-at-point-functions))
          (should (memq 'fennel-proto-repl--xref-backend xref-backend-functions))
          (when (boundp 'eldoc-documentation-functions)
            (should (memq 'fennel-proto-repl-eldoc-fn-docstring eldoc-documentation-functions))
            (should (memq 'fennel-proto-repl-eldoc-var-docstring eldoc-documentation-functions)))
          (fennel-proto-repl-minor-mode -1)
          (should-not fennel-proto-repl-minor-mode)
          (should-not (memq 'fennel-proto-repl-complete completion-at-point-functions))
          (should-not (memq 'fennel-proto-repl--xref-backend xref-backend-functions))
          (when (boundp 'eldoc-documentation-functions)
            (should-not (memq 'fennel-proto-repl-eldoc-fn-docstring eldoc-documentation-functions))
            (should-not (memq 'fennel-proto-repl-eldoc-var-docstring eldoc-documentation-functions))))
      (kill-buffer buf))))

(ert-deftest fpr-output-test ()
  (with-fennel-proto-repl
    (fennel-proto-repl-clear-buffer)
    (should (equal ">> " (buffer-substring-no-properties (point-min) (point-max))))
    (fennel-proto-repl-send-message-sync :eval "(io.write 123)")
    (should (equal "123\n>> " (buffer-substring-no-properties (point-min) (point-max))))
    (fennel-proto-repl-send-message-sync :eval "(io.write 456)")
    (should (equal "123456\n>> " (buffer-substring-no-properties (point-min) (point-max))))
    (fennel-proto-repl-send-message-sync :eval "(print 789)")
    (should (equal "123456789\n\n>> " (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest fpr-clean-output-test ()
  (with-fennel-proto-repl
    (fennel-proto-repl-clear-output)
    (should (equal "*** output flushed ***\n>> " (buffer-substring-no-properties (point-min) (point-max))))
    (fennel-proto-repl-send-message-sync :eval "(print 123)")
    (should (equal "*** output flushed ***\n123\n>> " (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest fpr-eval-print-test ()
  (with-fennel-proto-repl
   (with-temp-buffer
     (fennel-proto-repl-link-buffer)
     (insert "[\"a\nb\" 1 2]")
     (goto-char (point-max))
     (fennel-proto-repl-eval-print-last-sexp)
     (while (fennel-proto-repl--callbacks-pending)
       (accept-process-output nil 0.01))
     (should (equal "[\"a\nb\" 1 2]\n[\"a\\nb\" 1 2]"
                    (buffer-substring-no-properties (point-min) (point-max))))))
  (with-fennel-proto-repl
   (with-temp-buffer
     (fennel-proto-repl-link-buffer)
     (insert "[\"a\nb\" 1 2]")
     (goto-char (point-max))
     (fennel-proto-repl-eval-print-last-sexp 'pp)
     (while (fennel-proto-repl--callbacks-pending)
       (accept-process-output nil 0.01))
     (should (equal "[\"a\nb\" 1 2]\n[\"a\nb\"\n 1\n 2]"
                    (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest fpr-eval-last-sexp-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer repl)
        (insert "(+ 1 2 3)")
        (goto-char (point-max))
        (let (res done)
          (fpr--with-mocks ((fennel-proto-repl--display-result
                             (lambda (values)
                               (setq done t)
                               (setq res values))))
            (fennel-proto-repl-eval-last-sexp)
            (while (not done) (accept-process-output nil 0.01))
            (should (equal '("6") res)))))))
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer)
        (insert "(+ 1 2 3)")
        (goto-char (point-max))
        (let (res done)
          (fpr--with-mocks ((fennel-proto-repl--display-result
                             (lambda (values)
                               (setq done t)
                               (setq res values))))
            (fennel-proto-repl-eval-last-sexp 'and-go)
            (should (eq repl (current-buffer)))
            (while (not done) (accept-process-output nil 0.01))
            (should (equal '("6") res))))))))

(ert-deftest fpr-eval-buffer-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer repl)
        (insert "(+ 1 2 3 4)")
        (goto-char (point-max))
        (let (res done)
          (fpr--with-mocks ((fennel-proto-repl--display-result
                             (lambda (values)
                               (setq done t)
                               (setq res values))))
            (fennel-proto-repl-eval-buffer)
            (while (not done) (accept-process-output nil 0.01))
            (should (equal '("10") res))))))))

(ert-deftest fpr-eval-defun-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer repl)
        (insert "(+ 1 2)")
        (goto-char (point-min))
        (forward-char 2)
        (let (res done)
          (fpr--with-mocks ((fennel-proto-repl--display-result
                             (lambda (values)
                               (setq done t)
                               (setq res values))))
            (fennel-proto-repl-eval-defun)
            (while (not done) (accept-process-output nil 0.01))
            (should (equal '("3") res))))))))

(ert-deftest fpr-eval-form-and-next-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer repl)
        (insert "(+ 1 2 3)\n(+ 4 5)")
        (goto-char (point-min))
        (forward-char 2)
        (let (res done)
          (fpr--with-mocks ((fennel-proto-repl--display-result
                             (lambda (values)
                               (setq done t)
                               (setq res values))))
            (fennel-proto-repl-eval-form-and-next)
            (while (not done) (accept-process-output nil 0.01))
            (should (equal '("6") res))
            (should (equal 18 (point)))))))))

(ert-deftest fpr-eval-paragraph-test ()
  (with-fennel-proto-repl
    (let ((repl (current-buffer)))
      (with-temp-buffer
        (fennel-proto-repl-link-buffer repl)
        (insert "(+ 1 2 3)\n(+ 4 5)")
        (goto-char (point-min))
        (forward-char 2)
        (let (res done)
          (fpr--with-mocks ((fennel-proto-repl--display-result
                             (lambda (values)
                               (setq done t)
                               (setq res values))))
            (fennel-proto-repl-eval-paragraph)
            (while (not done) (accept-process-output nil 0.01))
            (should (equal '("9") res))))))))

(ert-deftest fpr-sync-request-timeout-test ()
  (with-fennel-proto-repl
    (should-error (fennel-proto-repl-send-message-sync :eval "(while true nil)")
                  :type 'fennel-proto-repl-timeout)))

(ert-deftest fpr-user-input-test ()
  (with-fennel-proto-repl
    (fpr--with-mocks ((read-string (lambda (&rest _) "123")))
      (should (equal '("123") (fennel-proto-repl-send-message-sync :eval "(io.read :n)"))))))

(ert-deftest fpr-delete-output-test ()
  (with-fennel-proto-repl
    (fennel-proto-repl-clear-buffer)
    (should (equal ">> " (buffer-substring-no-properties (point-min) (point-max))))
    (fpr--send-input "(print 123)")
    (should (equal ">> 123\nnil\n>> " (buffer-substring-no-properties (point-min) (point-max))))
    (fennel-proto-repl-clear-output)
    (should (equal "*** output flushed ***\n>> " (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest fpr-comma-commands ()
  (should (equal '("apropos" "apropos-doc" "apropos-show-docs" "compile"
                   "complete" "doc" "exit" "find" "help" "reload" "reset")
                 (sort (fennel-proto-repl--available-comma-commands) #'string<)))
  (with-fennel-proto-repl
    (should-error
     (fpr--with-mocks ((completing-read (lambda (&rest _) "")))
       (fennel-proto-repl-comma-command))))
  (with-fennel-proto-repl
    (let (res)
      (fpr--with-mocks ((fennel-proto-repl--print (lambda (data) (setq res data)))
                        (completing-read (lambda (&rest _) "complete"))
                        (read-string (lambda (&rest _) "f")))
        (fennel-proto-repl-comma-command)
        (while (fennel-proto-repl--callbacks-pending)
          (accept-process-output nil 0.01)))
      (should (stringp res))
      (should (equal '("faccumulate" "fcollect" "fn" "for")
                     (sort (split-string res "\n" t) #'string<)))))
  (with-fennel-proto-repl
    (let (res)
      (fpr--with-mocks ((fennel-proto-repl--print (lambda (data) (setq res data)))
                        (completing-read (lambda (&rest _) "help")))
        (fennel-proto-repl-comma-command)
        (while (fennel-proto-repl--callbacks-pending)
          (accept-process-output nil 0.01)))
      (should (stringp res)))))
