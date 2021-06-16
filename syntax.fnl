;; print fennel-keywords and fennel-built-in definitions for fennel-mode.el
;; requires fennel 0.9.3 or newer
(local fennel (require :fennel))

(var column 5)

(local keywords (doto (icollect [name {: global?} (pairs (fennel.syntax))]
                        (if (not global?) name))
                  (table.sort)))

(local builtins (doto (icollect [name {: global?} (pairs (fennel.syntax))]
                        (if global? name))
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
(defvar fennel-builtins
  '(")

(each [_ name (pairs builtins)]
  (write-name name))

(io.write "))\n")

