;; print fennel-keywords and fennel-built-in definitions for fennel-mode.el
;; requires fennel 0.9.3 or newer
(local fennel (require :fennel))

(var column 5)

(local keywords (doto (icollect [name {: global?} (pairs (fennel.syntax))]
                        (if (not global?) name))
                  (table.sort)))

(local builtin-modules (doto (icollect [name {: global? : function?} (pairs (fennel.syntax))]
                               (if (and global? (not function?))
                                   name))
                         (table.sort)))

(local builtin-functions (doto (icollect [name {: global? : function?} (pairs (fennel.syntax))]
                                 (if (and global? function? (not (name:match "%.")))
                                     name))
                           (table.sort)))

(local module-functions (doto (icollect [name {: global? : function?} (pairs (fennel.syntax))]
                                (if (and global? function? (name:match "%."))
                                    name))
                          (table.sort)))

(fn write-name [name]
  (let [out (string.format "%q" name)
        next-column (+ column 1 (length out))]
    (if (= column 5)
        (do (set column (- next-column 1))
            (io.write out))
        (< next-column 79)
        (do (set column next-column)
            (io.write (.. " " out)))
        (do (set column (+ 5 (length out)))
            (io.write (.. "\n    " out))))))

(io.write "(defvar fennel-keywords
  '(")

(each [_ name (pairs keywords)]
  (write-name name))

(set column 5)

(io.write "))\n
(defvar fennel-builtin-modules
  '(")

(each [_ name (pairs builtin-modules)]
  (write-name name))

(set column 5)

(io.write "))\n
(defvar fennel-builtin-functions
  '(")

(each [_ name (pairs builtin-functions)]
  (write-name name))

(set column 5)

(io.write "))\n
(defvar fennel-module-functions
  '(")

(each [_ name (pairs module-functions)]
  (write-name name))

(io.write "))\n")
