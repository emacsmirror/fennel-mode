;;; -*- lexical-binding: t; -*-
(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el"
              (:send-report nil)
              (:report-format 'text)))

(require 'ert)
(require 'compat nil t)
(require 'fennel-mode)
(require 'outline)

(defmacro with-fennel-repl (&rest body)
  "Starts the Fennel REPL and evals BODY in its context."
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t))
     (with-current-buffer (fennel-repl "fennel --repl")
       (while (save-excursion
                (goto-char (point-min))
                (not (re-search-forward
                      (format "^%s" (regexp-quote fennel-mode-repl-prompt))
                      nil t)))
         (accept-process-output))
       (unwind-protect
           (progn ,@body)
         (fennel-repl-quit)))))

(ert-deftest fennel-mode-start-repl-test ()
  "Starting the Proto REPL and checking its buffers and processes."
  (with-temp-buffer
    (let* ((current (current-buffer))
           (inhibit-message t)
           (repl (fennel-repl "fennel --repl")))
      (unwind-protect
          (progn
            (should-not (eq current (current-buffer)))
            (should (bufferp repl))
            (should (eq repl (current-buffer)))
            (should (bufferp fennel-repl--last-buffer))
            (should (eq repl fennel-repl--buffer))
            (should (process-live-p (get-buffer-process repl)))
            (with-current-buffer repl
              (should (string= fennel-program "fennel --repl"))))
        (with-current-buffer repl
          (fennel-repl-quit))))))

(ert-deftest fennel-mode-kill-repl-error-test ()
  (with-fennel-repl
    (with-temp-buffer
      (should-error (fennel-repl-quit)))))

(ert-deftest fennel-mode-eval-test ()
  (with-fennel-repl
    (let ((proc (inferior-lisp-proc)))
      (with-temp-buffer
        (fennel-repl-redirect-one proc "(+ 1 2 3)" (current-buffer))
        (should (equal (string-trim (buffer-substring-no-properties (point-min) (point-max))) "6"))))))

(ert-deftest fennel-mode-view-compilation-test ()
  (let ((code "(+ 1 2 3)"))
    (with-temp-buffer
      (let ((name (buffer-name)))
        (insert code)
        (fennel-view-compilation)
        (with-current-buffer (format "*fennel %s*" name)
          (should (equal (string-trim (buffer-substring-no-properties (point-min) (point-max)))
                         (string-trim (shell-command-to-string (format "fennel -c <(echo '%s')" code))))))))))

(defun fennel-mode--remove-leading-spaces (s)
  (replace-regexp-in-string "^[[:space:]]+" "" s))

(ert-deftest fennel-mode-indentation-test ()
  (dolist (expected '("
(do 1
    2
    3)" "
(do
  1
  2
  3)" "
(do
  1
  2
  3
  )" "
(+
 1
 2 3
 )" "
(doto
    {}
  (tset :a 1)
  (tset :b 2))" "
(doto {}
  (tset :a 1)
  (tset :b 2))" "
(let [a 1
      b 2]
  (+
   a
   b))" "
(let
    [a 1
     b
     2]
  (+ a
     b))" "
[1
 2
 3]" "
[
 1
 2
 3
 ]" "
{:a
 1
 :b
 2}" "
{
 :a 1
 :b 2
 }" "
{:a
 [1
  2
  3
  ]
 :bcd [
       1
       2
       3
       ]}" "
(case {:a 1}
  {:a 1}
  1
  (where {:a 2})
  2)" "
(case
    {:a 1}
  {:a 1}
  1
  (where {:a 2})
  2)"
  ))
    (with-temp-buffer
      (let ((inhibit-message t)
            (code (fennel-mode--remove-leading-spaces expected)))
        (insert code)
        (fennel-mode)
        (should (indent-region (point-min) (point-max)))
        (should (equal (string-trim expected)
                       (string-trim (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest fennel-mode-outline-level-1-test ()
  (with-temp-buffer
    (insert "
;;; Heading A

(local x 1)

(fn f []
  (and 1 ; Any long body.
       2
       3
       4
       5))

;;; Heading B

(local y 1)

(fn g []
  (and 1 ; Any long body.
       2
       3
       4
       5))")
    (fennel-mode)
    (outline-cycle-buffer)
    (outline-headers-as-kill (point-min) (point-max))
    (should (equal (current-kill 0)
                   ";;; Heading A

;;; Heading B

"))))

(ert-deftest fennel-mode-outline-level-2-test ()
  (with-temp-buffer
    (insert "
;;; Heading A

(local x 1)

(fn f []
  (and 1 ; Any long body.
       2
       3
       4
       5))

;;; Heading B

(local y 1)

(fn g []
  (and 1 ; Any long body.
       2
       3
       4
       5))")
    (fennel-mode)
    (outline-cycle-buffer)
    (outline-cycle-buffer)
    (outline-headers-as-kill (point-min) (point-max))
    (should (equal (current-kill 0)
                   ";;; Heading A

(local x 1)

(fn f []

;;; Heading B

(local y 1)

(fn g []

"))))
