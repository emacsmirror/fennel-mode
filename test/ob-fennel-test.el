;;; -*- lexical-binding: t -*-
(when (require 'undercover nil t)
  (setq undercover-force-coverage t)
  (undercover "*.el"
              (:send-report nil)
              (:report-format 'text)))

(require 'ert)
(require 'compat nil t)
(require 'ob-fennel)

(defvar params
  '((:colname-names)
    (:rowname-names)
    (:result-params "replace")
    (:result-type . value)
    (:results . "replace")
    (:exports . "code")
    (:session . "none")
    (:cache . "no")
    (:noweb . "no")
    (:hlines . "no")
    (:tangle . "no")))

(defmacro with-session-teardown (name &rest body)
  "Teardown the Babel session NAME after executing BODY."
  (declare (indent 1))
  `(let ((inhibit-message t))
     (unwind-protect
         (progn ,@body)
       (with-current-buffer
           ,(format ob-fennel--buffer-fmt (or name "default"))
         (fennel-proto-repl-quit)
         (kill-buffer)))))

(ert-deftest ob-fennel-evaluation-test ()
  (with-session-teardown nil
    (should
     (equal 6 (org-babel-execute:fennel "(+ 1 2 3)" params)))
    (should
     (equal "foo" (org-babel-execute:fennel ":foo" params)))
    (should
     (equal '(1 2 3) (org-babel-execute:fennel "[1 2 3]" params)))))

(ert-deftest ob-fennel-session-test ()
  (with-session-teardown "a"
    (with-session-teardown "b"
      (should
       (equal "nil" (org-babel-execute:fennel
                     "(local a 10)"
                     (cons '(:session . "a") params))))
      (should
       (equal "nil" (org-babel-execute:fennel
                     "(local a 20)"
                     (cons '(:session . "b") params))))
      (should
       (equal 10 (org-babel-execute:fennel
                  "a"
                  (cons '(:session . "a") params))))
      (should
       (equal 20 (org-babel-execute:fennel
                  "a"
                  (cons '(:session . "b") params)))))))

(ert-deftest ob-fennel-variable-test ()
  (with-session-teardown nil
    (should
     (equal 30 (org-babel-execute:fennel
                "(+ x y)"
                (append '((:var x . 10) (:var y . 20)) params))))))

(ert-deftest ob-fennel-table-test ()
  (with-session-teardown nil
    (should
     (equal '(("low" "high" "average")
	      hline
	      (0 6 3.0)
	      (3 7 5.0)
	      (8 42 25.0))
            (org-babel-execute:fennel
             "(let [[column-names
                     separator
                     & rows] samples]
                 (table.insert column-names :average)
                 (icollect [_ row (ipairs rows)
                             :into [column-names separator]]
                          (doto row
                                (table.insert
                                 (/ (accumulate [res 0 _ val (ipairs row)]
                                                (+ res val))
                                    (length row))))))"
             (append '((:var samples ("low" "high") hline (0 6) (3 7) (8 42))
                       (:result-params replace table)
                       (:hlines . yes))
                     params))))))

(ert-deftest ob-fennel-output-test ()
  (with-session-teardown nil
    (should
     (equal "vaiv"
            (org-babel-execute:fennel
             "(print :vaiv) 42"
             (append '((:result-type . output)) params))))))

(ert-deftest ob-fennel-output-with-error-test ()
  (with-session-teardown nil
    (should
     (not (string-match-p
           "vaiv"
           (org-babel-execute:fennel
            "(print :vaiv) x"
            (append '((:result-type . output)) params)))))))

(ert-deftest ob-fennel-pretty-printing-test ()
  (with-session-teardown nil
    (should
     (equal '("a\\nb" 1 2)
            (org-babel-execute:fennel
             "[\"a\nb\" 1 2]"
             (append '((:result-paramss "replace" "scalar")) params))))
    (should
     (equal "[\"a\nb\"\n 1\n 2]"
            (org-babel-execute:fennel
             "[\"a\nb\" 1 2]"
             (append '((:result-params "replace" "scalar" "pp")) params))))))

(ert-deftest ob-fennel-error-test ()
  (with-session-teardown nil
    (should
     (string-match-p "error" (org-babel-execute:fennel "x" params)))))
