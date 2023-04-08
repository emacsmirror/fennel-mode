;;; fennel-proto-repl.el --- Protocol-based REPL for Fennel -*- lexical-binding: t -*-
;; Copyright © 2023 Phil Hagelberg and contributors
;; Author: Andrey Listopadov

;;; Commentary:
;;
;; A minor mode for in-buffer interaction and a client for the Fennel
;; REPL.  Implements some IDE-like features like completion-at-point,
;; Eldoc and Xref support, interactive evaluation, and more.
;;
;; Unlike the underlying REPL implementation used by the `fennel-repl'
;; command, which is mainly meant as a human interaction interface,
;; `fennel-proto-repl' implements a more machine-friendly protocol,
;; and upgrades a REPL to use this it for all communications.  As a
;; result, the client can utilize the protocol API, allowing
;; asynchronous communication, and more robust interactions without
;; needening to rely on parsing of the raw process outptut.
;;
;; The protocol allows the client to intercept IO operations, tightly
;; integrate into the editor with better error reporting, and in
;; general provides a clear mapping between user interactions and
;; process output.
;;
;; For detailed protocol description, see
;; https://gitlab.com/andreyorst/fennel-proto-repl-protocol

;;; Usage
;;
;; Upon starting the REPL with the `fennel-proto-repl' function a
;; `fennel-proto-repl-minor-mode' is enabled in the current buffer.
;; To interact with the already running REPL from some other buffer,
;; this minor mode can be enabled manually, or via a hook to
;; fennel-mode:
;;
;; (add-hook 'fennel-mode-hook 'fennel-proto-repl-minor-mode)
;;
;; The minor mode binds commands to usual interaction keys.  See the
;; `fennel-proto-repl-minor-mode-map' for details.
;;
;; If there are multiple REPL processes running,
;; `fennel-proto-repl-link-buffer' can be used to link the current
;; buffer with a specific REPL process.  This command provides a list
;; of running REPLs to choose from via `completing-read'.

;;; API interface
;;
;; The public API consists of two main functions, the asynchronous
;; `fennel-proto-repl-send-message' function and its synchronous
;; variant `fennel-proto-repl-send-message-sync'.  Both functions
;; accept the keyword of the operation to send and the data for that
;; operation.  The `fennel-proto-repl-send-message' accepts a callback
;; that will be called with the result of an operation.  For example:
;;
;; (fennel-proto-repl-send-message
;;  :eval "(+ 1 2 3)"
;;  (lambda (vals) (message "%S" vals)))
;;
;; Will print the ("6") in the echo area and in the *Messages* buffer.
;; The callback will not be called if an error occurs during
;; execution.
;;
;; The `fennel-proto-repl-send-message-sync' doesn't accept a callback
;; and instead returns the result of the operation instead but blocks
;; until the operation is completed:
;;
;; (fennel-proto-repl-send-message-sync
;;  :eval "(values 1 :foo [1 2] {:bar (fn [] :baz)})")
;; => ("1" "\"foo\"" "[1 2]" "{:bar #<function: 0x55b65e751990>}")
;;
;; The result is a list of strings, each of which represents a
;; serialized Fennel value.  If an error occurs during execution, this
;; function returns nil.
;;
;; Additionally, both functions accept optional callbacks for
;; overriding the protocol error handling and print handling OPs:
;;
;; (fennel-proto-repl-send-message-sync
;;   :eval "(io.write :foo) (error :bar) :never-reached"
;;   (lambda (_error-type message _traceback)
;;     "Error OP handler."
;;     (message "error: %s" message))
;;   (lambda (data)
;;     "Print OP handler."
;;     (message "print: %s" data)))
;;
;; Will print the "print: foo", "error: bar" messages to the
;; *Messages* buffer and return nil because an error happened during
;; the execution.

;;; Code:

(require 'compile)
(require 'xref)
(require 'cl-generic)
(require 'fennel-mode)

(defvar fennel-proto-repl--upgrade
  "(fn protocol [format-function]
     (let [{: view : eval : traceback : parser
            : version &as fennel} (require :fennel)
           {:concat t/concat} table
           InternalError {}
           protocol-env (collect [k v (pairs _G)]
                          (when (or (not= k :_G)
                                    (not= k :___repl___))
                            (values k v)))
           protocol* {:version \"0.1.0\"
                      :id -1
                      :op nil
                      :env protocol-env}
           protocol {}
           protocol (->> {:__index protocol*
                          :__newindex
                          (fn [self k v]
                            (if (. protocol* k)
                                (protocol.internal-error
                                 (: \"modification of the protocol.%s field is forbidden\" :format k))
                                (rawset self k v)))}
                         (setmetatable protocol))]
       (doto protocol*.env
         (tset :_G protocol-env)
         (tset :fennel fennel)
         (tset :protocol protocol))

       (var expr-count 0)
       (var upgraded? false)

       ;; Protocol methods
       (fn protocol.internal-error [cause message]
         (error {:type InternalError :cause cause :data message}))

       (set protocol*.internal-error protocol.internal-error)

       (set protocol.format #(format-function protocol*.env $))

       (fn tmpname []
         ;; Generate name for temporary file that will act as a named
         ;; FIFO pipe.
         (let [p (io.popen \"mktemp -u ${TMPDIR:-/tmp}/fennel-proto-repl.FIFO.XXXXXXXX\")
               name (p:read :l)
               (ok? msg code) (p:close)]
           (if ok? name (values ok? msg code))))

       (fn protocol.mkfifo []
         ;; Create a named FIFO pipe.  Continiously tries to create a
         ;; temporary name via `protocol.tmpname` and create a FIFO pipe with it.
         ;;
         ;; TODO: This is not portable, though a client can override this
         ;; if needed.
         (fn open-fifo [name]
           (: (io.popen (: \"mkfifo '%s' 2>/dev/null\" :format name)) :close))
         (var (i name) (values 0 (tmpname)))
         (while (and name (not (open-fifo name)))
           (when (> i 10)
             (protocol.internal-error
              \"too many retries\" \"can't open FIFO\"))
           (set i (+ i 1))
           (set name (tmpname)))
         name)

       (fn protocol.read [mode message op]
         ;; User input handling trhough FIFO.
         (case (protocol.mkfifo)
           fifo (let [_ (message [[:id {:sym protocol.id}]
                                  [:op {:string op}]
                                  [:data {:string fifo}]])
                      data (with-open [f (io.open fifo :r)]
                             (f:read mode))]
                  (: (io.popen (: \"rm -f '%s'\" :format fifo)) :close)
                  data)
           nil (protocol.internal-error \"unable to create FIFO pipe.\")))

       ;; Protocol initialization
       (case _G.___repl___
         {:onValues on-values :readChunk read-chunk
          :env env :onError on-error :pp pp &as ___repl___}
         (let [{:fennel fennel-ver :lua lua-ver} (fennel.runtime-version true)
               {:write io/write :read io/read
                : stdin : stdout : stderr} env.io
               {:write fd/write :read fd/read &as fd}
               (. (getmetatable env.io.stdin) :__index)
               lua-print print]
           (fn join [sep ...]
             ;; Concatenate multiple values into a string using `sep` as a
             ;; separator.
             (t/concat
              (fcollect [i 1 (select :# ...)]
                (tostring (select i ...))) sep))

           (fn set-io [env message]
             ;; Set up IO interceptors for current environment.  Message is
             ;; a callback that is used to send messsages to the REPL.
             (when upgraded?
               (fn env.print [...]
                 (env.io.write (.. (join \"\t\" ...) \"\n\"))
                 nil)
               (fn env.io.write [...]
                 (: (env.io.output) :write ...))
               (fn env.io.read [mode]
                 (let [input (env.io.input)]
                   (if (= input stdin)
                       (protocol.read mode message :read)
                       (input:read mode))))
               (fn fd.write [fd ...]
                 (if (or (= fd stdout) (= fd stderr))
                     (message [[:id {:sym protocol.id}]
                               [:op {:string :print}]
                               [:descr {:string (if (= fd stdout) :stdout :stderr)}]
                               [:data {:string (join \"\" ...)}]])
                     (fd/write fd ...))
                 fd)
               (fn fd.read [fd ...]
                 (if (= fd stdin)
                     (env.io.read ...)
                     (fd/read fd ...)))))

           (fn reset-io [env]
             ;; Resets IO to original handlers.
             (set env.print lua-print)
             (set env.io.wirte io/write)
             (set env.io.read io/read)
             (set fd.read fd/read)
             (set fd.write fd/write))

           (fn message [data]
             ;; General purpose way of sending messages to the REPL.
             (reset-io env)
             (on-values [(protocol.format data)])
             (io.flush)
             (set-io env message))

           (fn done [id]
             ;; Sends the message that processing the `id` is complete and
             ;; resets the `protocol.id`.
             (when (> id 0)
               (set protocol*.id -1)
               (set protocol*.op nil)
               (message [[:id {:sym id}]
                         [:op {:string :done}]])))

           (fn count-expressions [data]
             ;; Counts amount of expressions in the given string.  If the
             ;; string fails to parse, returns 1 as exprssion count,
             ;; because the expression will break down the line.
             (let [(ok? n)
                   (pcall #(accumulate [i 0 _ _ (parser data)] (+ i 1)))]
               (if ok? n 1)))

           (fn accept [id op msg callback]
             ;; Accept the message.  Sets the current ID to `id` and writes
             ;; back a message that the communication was successful.
             (when (not (= :number (type id)))
               (protocol.internal-error \"message ID must be a positive number\" (view id)))
             (when (< id 1)
               (protocol.internal-error \"message ID must be greater than 0\" id))
             (message [[:id {:sym id}]
                       [:op {:string :accept}]])
             (set protocol*.id id)
             (set protocol*.op op)
             (set expr-count 1)
             (case op
               :eval (set expr-count (count-expressions msg))
               :downgrade (callback)     ; downgrade passed as a callback
               :exit (done id))
             (when (= msg \"\") (done id))
             (.. msg \"\n\"))

           (fn data [id data]
             ;; Sends the data back to the process and completes the
             ;; communication.
             (when (not= protocol.op :nop)
               (message [[:id {:sym id}]
                         [:op {:string protocol.op}]
                         [:values {:list (icollect [_ v (ipairs data)] (view v))}]]))
             (done id))

           (fn err [id ?kind mesg ?trace]
             ;; Sends back the error information and completes the
             ;; communication.
             (message [[:id {:sym id}]
                       [:op {:string :error}]
                       [:type {:string (if ?kind ?kind :runtime)}]
                       [:data {:string mesg}]
                       (when ?trace
                         [:traceback {:string ?trace}])])
             (done id))

           (fn remove-locus [msg]
             ;; Removes error information from the message.
             (if (= :string (type msg))
                 (pick-values 1 (msg:gsub \"^[^:]*:%d+:%s+\" \"\"))
                 (view msg)))

           (fn downgrade []
             ;; Reset the REPL back to its original state.
             (set upgraded? false)
             (reset-io env)
             (doto ___repl___
               (tset :readChunk read-chunk)
               (tset :onValues on-values)
               (tset :onError on-error)
               (tset :pp pp)))

           (fn upgrade []
             ;; Upgrade the REPL to use the protocol-based communication.
             (set upgraded? true)
             (set-io env message)
             (fn ___repl___.readChunk [{: stack-size &as parser-state}]
               (if (> stack-size 0)
                   (error \"incomplete message\")
                   (let [msg (let [_ (reset-io env)
                                   mesg (read-chunk parser-state)
                                   _ (set-io env message)]
                               mesg)]
                     (case (and msg (eval msg {:env protocol.env}))
                       {: id :eval code} (accept id :eval code)
                       {: id :complete sym} (accept id :complete (.. \",complete \" sym))
                       {: id :doc sym} (accept id :doc (.. \",doc \" sym))
                       {: id :reload module} (accept id :reload (.. \",reload \" module))
                       {: id :find val} (accept id :find (.. \",find \" val))
                       {: id :compile expr} (accept id :compile (.. \",compile \" expr))
                       {: id :apropos re} (accept id :apropos (.. \",apropos \" re))
                       {: id :apropos-doc re} (accept id :apropos-doc (.. \",apropos-doc \" re))
                       {: id :apropos-show-docs re} (accept id :apropos-show-docs (.. \",apropos-show-docs \" re))
                       {: id :help \"\"} (accept id :help \",help\")
                       {: id :reset \"\"} (accept id :reset \",reset\")
                       {: id :exit \"\"} (accept id :exit \",exit\")
                       {: id :downgrade \"\"} (accept id :downgrade \"\" downgrade)
                       {: id :nop \"\"} (accept id :nop \"nil\")
                       _ (protocol.internal-error \"message did not conform to protocol\" (view msg))))))
             (fn ___repl___.onValues [xs]
               (set expr-count (- expr-count 1))
               (when (= 0 expr-count)
                 (data protocol.id xs)))
             (fn ___repl___.onError [type* msg source]
               (case (values type* msg)
                 (_ {:type InternalError : cause :data ?msg})
                 (err -1 :proto-repl (if ?msg (.. cause \": \" (remove-locus ?msg)) cause))
                 \"Lua Compile\"
                 (err protocol.id :lua
                      (.. \"Bad code generated - likely a bug with the compiler:\n\"
                          \"--- Generated Lua Start ---\n\"
                          source
                          \"\n--- Generated Lua End ---\n\"))
                 \"Runtime\"
                 (err protocol.id :runtime
                      (remove-locus msg)
                      (traceback nil 4))
                 _ (err protocol.id (string.lower type*)
                        (remove-locus msg))))
             (fn ___repl___.pp [x] (view x))
             (message [[:id {:sym 0}]
                       [:op {:string \"init\"}]
                       [:status {:string \"done\"}]
                       [:protocol {:string protocol*.version}]
                       [:fennel {:string (or fennel-ver \"unknown\")}]
                       [:lua {:string (or lua-ver \"unknown\")}]]))

           (upgrade))
         _
         ;; Bail out if the REPL doesn't expose the ___repl___ table or its
         ;; contents differ.  Fennelview is used to communicate back the
         ;; response in the protocol-based message format.
         (-> [[:id {:sym 0}]
              [:op {:string \"init\"}]
              [:status {:string \"fail\"}]
              [:data {:string (.. \"unsupported Fennel version: \" version)}]]
             (setmetatable {:__fennelview #(protocol.format $)})))))"
  "Upgrade code for the basic Fennel REPL.

This code is sent to the REPL as a part of the initialization process.
Once evaluated, the REPL starts accepting messages accordingly to the
defined protocol.")

(defvar fennel-proto-repl--format-plist
  "(fn format-plist [env data]
     ;; Format data as emacs-lisp plist.
     (:  \"(%s)\" :format
         (table.concat
          (icollect [_ [k v] (ipairs data)]
            (: \":%s %s\" :format k
               (: (case v
                    {:list data} (: \"(%s)\" :format (table.concat data \" \"))
                    {:string data} (env.fennel.view data)
                    {:sym data} (case data true :t false :nil _ (tostring data))
                    _ (env.protocol.internal-error \"wrong data kind\" (env.fennel.view v)))
                  :gsub \"\n\" \"\\\\n\"))) \" \")))"
  "Format function for the protocol that formats the messages as plists.")

(defvar fennel-proto-repl--message-id 0)
(defvar fennel-proto-repl--doc-buffer " *fennel-doc*")

(defvar-local fennel-proto-repl--buffer nil)
(defvar-local fennel-proto-repl--last-buffer nil)
(defvar-local fennel-proto-repl--message-buf nil)
(defvar-local fennel-proto-repl--print-marker nil)
(defvar-local fennel-proto-repl--process-buffer nil)
(defvar-local fennel-proto-repl--message-callbacks nil)

(defvar fennel-proto-repl--arglist-query-template
  "(let [fennel (require :fennel)
         scope (fennel.scope)]
     (-> (. scope.specials %S)
         (or (. scope.macros %S))
         (or (. _G.___replLocals___ %S))
         (or (. _G %S))
         (fennel.metadata:get :fnl/arglist)
         (or nil)))")

(defvar fennel-proto-repl--multisym-arglist-query-template
  "(let [fennel (require :fennel)
         scope (fennel.scope)]
     (-> %s
         (fennel.metadata:get :fnl/arglist)
         (or nil)))")

(defvar fennel-proto-repl--doc-query-template
  "(let [fennel (require :fennel)
         scope (fennel.scope)]
     (-> (. scope.specials %S)
         (or (. scope.macros %S))
         (or (. _G.___replLocals___ %S))
         (or (. _G %S))
         (fennel.metadata:get :fnl/docstring)
         (or nil)))")

(defvar fennel-proto-repl--multisym-doc-query-template
  "(let [fennel (require :fennel)
         scope (fennel.scope)]
     (-> %s
         (fennel.metadata:get :fnl/docstring)
         (or nil)))")

(defun fennel-proto-repl--method-to-sym (symbol)
  "Convert SYMBOL from a method call to an ordinary table lookup call.
E.g. foo:bar becomes foo.bar.  If SYMBOL is not a method call,
returns the symbol as is."
  (let* ((multisym (and (string-match-p "[.:]" symbol)
                        (not (member symbol '("." ".." "?." ":")))))
         (symbol (replace-regexp-in-string
                  ":" "."
                  (if (and multisym (string-match-p "[.:]$" symbol))
                      (substring symbol 0 -1)
                    symbol))))
    (cons symbol multisym)))

(defun fennel-proto-repl--generate-query-command
    (symbol template multisym-template)
  "Generate a command to obtain SYMBOL information based on a template.
If the symbol is a multisym use the MULTISYM-TEMPLATE, otherwise
the TEMPLATE is used."
  (let* ((symbol-info (fennel-proto-repl--method-to-sym symbol))
         (symbol (car symbol-info)))
    (fennel-proto-repl--minify-body
     (if (cdr symbol-info)
         (format multisym-template symbol)
       (format template symbol symbol symbol symbol))
     t)))

(cl-defstruct fennel-proto-repl--callback
  "Message callbacks."
  values error print)

(defcustom fennel-proto-repl-log-communication nil
  "Whether to log REPL communication.
Should only be used for protocol debugging and bug reporting, as
it can slow the REPL significantly."
  :group 'fennel-mode
  :type 'boolean
  :package-version '(fennel-mode "0.7.0"))

(defcustom fennel-proto-repl-show-welcome-message t
  "Whether to show a welcome message when starting the REPL."
  :group 'fennel-mode
  :type 'boolean
  :package-version '(fennel-mode "0.7.0"))

(defcustom fennel-proto-repl-sync-timeout 5
  "The maximum time for synchronous REPL requests."
  :group 'fennel-mode
  :type 'integer
  :package-version '(fennel-mode "0.7.0"))

(defcustom fennel-proto-repl-error-buffer-action 'show
  "What to do when an error occurs."
  :group 'fennel-mode
  :type '(choice
	  (const :tag "jump to buffer" jump)
	  (const :tag "show the buffer" show)
	  (const :tag "do nothing" nil))
  :package-version '(fennel-mode "0.7.0"))

(defcustom fennel-proto-repl-prompt ">> "
  "Prompt string for Fennel Proto REPL."
  :group 'fennel-mode
  :type 'string
  :package-version '(fennel-mode "0.7.0"))

;;; Input massaging

(defun fennel-proto-repl--remove-comments (buffer)
  "Remove comments in the BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (re-search-forward ";" nil 'noerror)
      (unless (or (nth 3 (syntax-ppss)) (looking-at-p "\""))
        (delete-region (1- (point)) (progn (end-of-line) (point)))
        (beginning-of-line)))))

(defun fennel-proto-repl--collapse-strings (buffer)
  "Replace literal newlines in BUFFER strings with escaped newlines."
  (with-current-buffer buffer
    (goto-char (point-min))
    (while (re-search-forward "\"" nil 'noerror)
      (when (nth 3 (syntax-ppss))
        (let* ((start (1- (point)))
               (end (progn (goto-char start)
                           (forward-sexp)
                           (point))))
          (save-match-data
            (save-restriction
              (save-excursion
                (narrow-to-region start end)
                (goto-char (point-min))
                (while (re-search-forward "\n" nil 'noerror)
                  (replace-match "\\\\n"))))))))))

(defun fennel-proto-repl--delete-indentation (buffer)
  "Delete all indentation in the BUFFER."
  (with-current-buffer buffer
    (delete-indentation nil (point-min) (point-max))))

(defun fennel-proto-repl--replace-literal-newlines (buffer)
  "Replace literal newlines in a BUFFER with control codes."
  (with-current-buffer buffer
    (save-match-data
      (goto-char (point-min))
      (while (re-search-forward "\n" nil 'noerror)
        (replace-match "")))))

(defun fennel-proto-repl--minify-body (body &optional no-newlines)
  "Minify BODY according to the `fennel-proto-repl--minify-code' setting.
If NO-NEWLINES is non-nil, delete all indentation."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (set-syntax-table fennel-mode-syntax-table)
      (insert body)
      (fennel-proto-repl--remove-comments buf)
      (fennel-proto-repl--collapse-strings buf)
      (if no-newlines
          (fennel-proto-repl--delete-indentation buf)
        (fennel-proto-repl--replace-literal-newlines buf))
      (string-trim
       (buffer-substring-no-properties
        (point-min) (point-max))))))

(defmacro fennel-proto-repl--log (message &optional to?)
  "Log the MESSAGE in the log buffer.
If TO? is a non-nil message treated as outcoming, otherwise message
was incoming."
  `(when fennel-proto-repl-log-communication
     (with-current-buffer (fennel-proto-repl--process-buffer)
       (goto-char (point-max))
       (if ,to?
           (insert "--> ")
         (insert "<-- "))
       (insert (string-trim-right ,message))
       (insert "\n"))))

(defmacro fennel-proto-repl--with-no-undo (&rest body)
  "Disable undo in the current buffer, execute BODY, enable undo."
  `(unwind-protect
       (progn
         (buffer-disable-undo)
         ,@body)
     (buffer-enable-undo)))

;;; Support for multiple REPL processes

(defun fennel-proto-repl--select-repl ()
  "Select a buffer out of all Fennel REPLs."
  (let (repls)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (eq major-mode 'fennel-proto-repl-mode)
                   (comint-check-proc buffer))
          (setq repls (cons (buffer-name buffer) repls)))))
    (pcase (length repls)
      (0 nil)
      (1 (get-buffer (car repls)))
      (_ (get-buffer
          (completing-read
           "Choose REPL buffer to link: "
           repls nil t))))))

(defun fennel-proto-repl-link-buffer (&optional repl-buffer)
  "Link the current buffer to s specific Fennel Proto REPL session.
Linking means that all buffer-based interaction commands will use
the specified REPL session.  Optionally accepts REPL-BUFFER to
use instead of the current one."
  (interactive)
  (unless (eq major-mode 'fennel-proto-repl-mode)
    (setq fennel-proto-repl--buffer
          (or (and repl-buffer (get-buffer repl-buffer))
              (fennel-proto-repl--select-repl)))))

(defun fennel-proto-repl--process-buffer ()
  "Get the process associated with the current REPL buffer."
  (if (eq major-mode 'fennel-proto-repl-mode)
      fennel-proto-repl--process-buffer
    (and fennel-proto-repl--buffer
         (buffer-live-p (get-buffer fennel-proto-repl--buffer))
         (buffer-local-value
          'fennel-proto-repl--process-buffer
          (get-buffer fennel-proto-repl--buffer)))))

;;; Process handling

(defun fennel-proto-repl--init-callbacks ()
  "Clear all pending callbacks.
This function is part of the REPL initialization process and
should not be used on its own."
  (setq fennel-proto-repl--message-id 0)
  (setq fennel-proto-repl--message-callbacks (make-hash-table :test 'eq)))

(defvar fennel-proto-repl--internal-callback
  (make-fennel-proto-repl--callback
   :values #'ignore :error #'fennel-proto-repl--display-error :print #'ignore)
  "REPL callback for internal errors.")

(defun fennel-proto-repl--get-callbacks (id)
  "Get a callback for a message with this ID.
Optional keyword argument KIND can be either :error or :print,
for getting callbacks for these actions."
  (when (and fennel-proto-repl--buffer
             (buffer-live-p (get-buffer fennel-proto-repl--buffer)))
    (with-current-buffer fennel-proto-repl--buffer
      (if (= id -1) fennel-proto-repl--internal-callback
        (gethash id fennel-proto-repl--message-callbacks)))))

(defun fennel-proto-repl--unassign-callbacks (id)
  "Remove the callback assigned to a message with this ID."
  (when (and fennel-proto-repl--buffer
             (buffer-live-p (get-buffer fennel-proto-repl--buffer)))
    (with-current-buffer fennel-proto-repl--buffer
      (remhash id fennel-proto-repl--message-callbacks))))

(defun fennel-proto-repl--callbacks-pending ()
  "Check if there are pending callbacks in the REPL."
  (when (and fennel-proto-repl--buffer
             (buffer-live-p (get-buffer fennel-proto-repl--buffer)))
    (with-current-buffer fennel-proto-repl--buffer
      (not (hash-table-empty-p fennel-proto-repl--message-callbacks)))))

(defun fennel-proto-repl--assign-callback
    (values-callback &optional error-callback print-callback)
  "Assign callbacks to ID and return the it.
VALUES-CALLBACK receives a list of values as a result of
evaluating the expression.  Optional ERROR-CALLBACK can override
the default callback for displaying errors.  Optional
PRINT-CALLBACK can override the default callback for printing to
the REPL window."
  (when (and fennel-proto-repl--buffer
             (buffer-live-p (get-buffer fennel-proto-repl--buffer)))
    (with-current-buffer fennel-proto-repl--buffer
      (let ((id fennel-proto-repl--message-id))
        (puthash id
                 (make-fennel-proto-repl--callback
                  :values values-callback
                  :error (or error-callback #'fennel-proto-repl--display-error)
                  :print (or print-callback #'fennel-proto-repl--print))
                 fennel-proto-repl--message-callbacks)
        (setq fennel-proto-repl--message-id (1+ id))
        id))))

(defun fennel-proto-repl--handle-protocol-op (message)
  "Handle protocol MESSAGE.
The message contains an id, operation to execute, and any
additional data related to the operation."
  (let ((id (plist-get message :id))
        (op (plist-get message :op)))
    (when-let ((callbacks (fennel-proto-repl--get-callbacks id)))
      (pcase op
        ("accept"
         (fennel-proto-repl--display-prompt))
        ("done"
         (fennel-proto-repl--unassign-callbacks id))
        ((or "eval" "complete" "doc" "reload"
             "apropos" "apropos-doc" "apropos-show-docs"
             "find" "help" "compile" "reset" "exit")
         (funcall (fennel-proto-repl--callback-values callbacks)
                  (plist-get message :values)))
        ("print"
         (funcall (fennel-proto-repl--callback-print callbacks)
                  (plist-get message :data)))
        ("error"
         (funcall (fennel-proto-repl--callback-error callbacks)
                  (plist-get message :type)
                  (plist-get message :data)
                  (plist-get message :traceback)))
        ("read"
         (let ((inhibit-message t))
           (write-region
            (read-string "stdin: ") nil
            (plist-get message :data))))
        ("init"
         (fennel-proto-repl--unassign-callbacks 0)
         (pcase (plist-get message :status)
           ("done"
            (funcall (fennel-proto-repl--callback-values callbacks)
                     (list 'ok
                           (plist-get message :protocol)
                           (plist-get message :fennel)
                           (plist-get message :lua))))
           ("fail"
            (funcall (fennel-proto-repl--callback-values callbacks)
                     (list (plist-get message :data))))))))))

(defun fennel-proto-repl--buffered-split-string (string)
  "Split STRING on newlines.
If the string doesn't end with a newline character, the last (or
the only) line of the string is buffered and excluded from the
result."
  (let ((strings (string-lines string t)))
    (when fennel-proto-repl--message-buf
      (setcar strings (concat fennel-proto-repl--message-buf (car strings)))
      (setq fennel-proto-repl--message-buf nil))
    (if (string-suffix-p "\n" string)
        strings
      (setq fennel-proto-repl--message-buf (car (last strings)))
      (nbutlast strings))))

(defun fennel-proto-repl--process-filter (process message)
  "Parse the MESSAGE and PROCESS it with the callback handler."
  (with-current-buffer (process-buffer process)
    (dolist (message (fennel-proto-repl--buffered-split-string message))
      (when-let ((idx (string-match-p "(:id " message)))
        (let* ((fennel-proto-repl--buffer (get-buffer fennel-proto-repl--buffer))
               (message (substring message idx)))
          (fennel-proto-repl--log message)
          (when-let ((data (condition-case nil
                               (car (read-from-string message))
                             (error nil))))
            (let ((len (proper-list-p data)))
              (when (and len (zerop (% len 2)))
                (fennel-proto-repl--handle-protocol-op data)))))))))

(defun fennel-proto-repl--send-string (process string)
  "Send STRING to Fennel PROCESS, chunking it if necessary."
  (let ((s (string-trim-right string)))
    (while (length> s 2048)
      (send-string process (substring s 0 2048))
      (setq s (substring s 2048)))
    (send-string process s)
    (send-string process "\n")))

(defun fennel-proto-repl--format-message (id op data)
  "Format the ID, OP, and DATA as a protocol message."
  (format "{:id %s %s %S}" id op
          (fennel-proto-repl--minify-body
           (substring-no-properties data))))

(defun fennel-proto-repl-send-message
    (op data callback &optional error-callback print-callback)
  "Send OP and DATA as a message to the REPL process.
Attaches CALLBACK to the message ID.  OP must be a keyword or
nil.  If the OP is nil sends the DATA as is without formatting it
as a message.

Passes a list of values to the CALLBACK, representing values from
the executed code.  Each value is a serialized string.

If the optional argument ERROR-CALLBACK is passed, REPL will use
it to handle errors.  The ERROR-CALLBACK must accept three
arguments: the error type, error message, and stack trace.

If the optional argument PRINT-CALLBACK is passed, REPL will use
it to handle print operations.  The PRINT-CALLBACK must accept at
least one argument, which is a text to be printed."
  (let ((proc-buffer (fennel-proto-repl--process-buffer)))
    (when-let ((proc (and proc-buffer (get-buffer-process proc-buffer))))
      (let* ((id (fennel-proto-repl--assign-callback
                  callback error-callback print-callback))
             (mesg (if op
                       (fennel-proto-repl--format-message id op data)
                     data)))
        (fennel-proto-repl--log mesg t)
        (fennel-proto-repl--send-string proc mesg)
        id))))

(define-error
 'fennel-proto-repl--timeout
 "Sync Proto REPL request timed out"
 'error)

(defun fennel-proto-repl-send-message-sync
    (op data &optional error-callback print-callback)
  "Send the message to the REPL process synchronously.
OP must be a keyword or nil.  OP and DATA are formatted as a
message unless OP is nil.

Return a list of strings, representing values from the executed
code.  If an error occurs during execution returns nil.  Accepts
optional ERROR-CALLBACK and PRINT-CALLBACK.  See
`fennel-proto-repl-send-message' for information on additional
callbacks."
  (let ((time0 (current-time))
        response done)
    (when-let ((id (fennel-proto-repl-send-message
                    op data
                    (lambda (res) (setq done t) (setq response res))
                    (lambda (kind message trace)
                      (setq done t)
                      (funcall (or error-callback
                                   #'fennel-proto-repl--display-error)
                               kind message trace))
                    print-callback)))
      (while (not done)
        (when (time-less-p fennel-proto-repl-sync-timeout
                           (time-subtract nil time0))
          (fennel-proto-repl--unassign-callbacks id)
          (signal 'fennel-proto-repl--timeout nil))
        (accept-process-output nil 0.01))
      response)))

(defun fennel-proto-repl--start-repl
    (status &optional protocol-version fennel-version lua-version)
  "Start the REPL.
If STATUS is ok, initialize the REPL with the given
PROTOCOL-VERSION FENNEL-VERSION, and LUA-VERSION.  Otherwise, the
status is the error message."
  (pcase status
    ('ok
     (message "Fennel Proto REPL initialized")
     (with-current-buffer fennel-proto-repl--buffer
       (unless (comint-check-proc (current-buffer))
         (let ((proc (start-process (buffer-name fennel-proto-repl--buffer)
                                    (current-buffer)
                                    nil))
               (repl-proc (get-buffer-process (fennel-proto-repl--process-buffer))))
           (add-hook 'kill-buffer-hook
                     (lambda () (delete-process repl-proc))
                     nil t)
           (fennel-proto-repl--with-no-undo
            (widen)
            (goto-char (point-max))
            (unless (= (point) (line-beginning-position))
              (insert "\n"))
            (when fennel-proto-repl-show-welcome-message
              (insert (format ";; Welcome to Fennel Proto REPL %s\n"
                              (or protocol-version "unknown")))
              (insert (format ";; Fennel version: %s\n;; Lua version: %s\n"
                              (or fennel-version "unknown")
                              (or lua-version "unknown"))))
            (set-marker (process-mark proc) (point))
            (fennel-proto-repl--display-prompt))))
       (pop-to-buffer (current-buffer))))
    (_
     (user-error "Unable to initialize Fennel Proto REPL: %s" status))))

(defun fennel-proto-repl--server-sentinel (_ _)
  "Fennel REPL process sentinel.
Terminates the REPL buffer process when the REPL server process
is terminated."
  (message "*Fennel Proto REPL is terminated*")
  (when-let ((proc (get-buffer-process fennel-proto-repl--buffer)))
    (delete-process proc)))

(defun fennel-proto-repl--start-server (command &optional repl-buffer)
  "Start the Fennel REPL process.
COMMAND is used to start the Fennel REPL.  If optional argument
REPL-BUFFER is provided, REPL is started in that buffer."
  (let* ((upgrade-code (fennel-proto-repl--minify-body
                        (format "(%s %s)"
                                fennel-proto-repl--upgrade
                                fennel-proto-repl--format-plist)
                        'delete-indentation))
         (command (if (fboundp 'split-string-shell-command)
                      (split-string-shell-command command)
                    (split-string command)))
         (proc (make-process
                :name (car command)
                :buffer (generate-new-buffer-name " fennel-proto-repl")
                :command command
                :connection-type 'pipe
                :filter #'fennel-proto-repl--process-filter
                :sentinel #'fennel-proto-repl--server-sentinel))
         (pid (process-id proc))
         (proc-buffer (process-buffer proc))
         (repl-name (format "*Fennel Proto REPL:%s*" pid))
         (repl-buffer (if (not repl-buffer)
                          (get-buffer-create repl-name)
                        (with-current-buffer repl-buffer
                          (rename-buffer repl-name))
                        (get-buffer repl-buffer))))
    (condition-case err
        (progn
          (fennel-proto-repl-link-buffer repl-buffer)
          (with-current-buffer proc-buffer
            (rename-buffer (format " *fennel-proto-repl:%s*" pid))
            (buffer-disable-undo)
            (setq mode-line-process '(":%s"))
            (setq fennel-proto-repl--buffer repl-buffer))
          (with-current-buffer repl-buffer
            (fennel-proto-repl-mode)
            (fennel-proto-repl--init-callbacks)
            (setq fennel-proto-repl--buffer repl-buffer)
            (setq fennel-proto-repl--process-buffer proc-buffer)
            (message "Waiting for Fennel REPL initialization...")
            (condition-case nil
                (apply #'fennel-proto-repl--start-repl
                       (fennel-proto-repl-send-message-sync
                        nil
                        (fennel-proto-repl--minify-body upgrade-code t)))
              (fennel-proto-repl--timeout
               (kill-buffer fennel-proto-repl--buffer)
               (user-error "Unable to initialize Fennel Proto REPL: timeout")))))
      (error (delete-process proc)
             (signal (car err) (cdr err))))))

;;; REPL

(defun fennel-proto-repl--display-prompt ()
  "Display prompt unless there's already a prompt."
  (with-current-buffer fennel-proto-repl--buffer
    (fennel-proto-repl--with-no-undo
     (unless (save-excursion
               (comint-goto-process-mark)
               (forward-char -1)
               (string-suffix-p fennel-proto-repl-prompt (field-string)))
       (let ((proc (get-buffer-process (current-buffer))))
         (setq fennel-proto-repl--print-marker
               (copy-marker (process-mark proc)))
         (comint-output-filter
          proc
          (if (= (marker-position fennel-proto-repl--print-marker)
                 (line-beginning-position))
              fennel-proto-repl-prompt
            (format "\n%s" fennel-proto-repl-prompt))))))))

(defun fennel-proto-repl--print (data)
  "Print DATA to the REPL buffer.
Handles printing with the respect to the prompt."
  (with-current-buffer fennel-proto-repl--buffer
    (fennel-proto-repl--with-no-undo
     (save-excursion
       (save-restriction
         (let ((inhibit-read-only t)
               (pos (marker-position fennel-proto-repl--print-marker))
               (input-end (marker-position comint-last-input-end))
               (prompt-start (progn
                               (comint-goto-process-mark)
                               (forward-char -1)
                               (field-beginning))))
           (goto-char pos)
           (narrow-to-region (point-min) pos)
           (cond ((string-suffix-p "\n" data)
                  (insert-before-markers data))
                 ((= pos prompt-start)
                  (insert-before-markers data)
                  (let ((pos (point)))
                    (insert-before-markers "\n")
                    (set-marker fennel-proto-repl--print-marker pos)))
                 (t (insert-before-markers data)))
           (when (= input-end pos)
             (set-marker comint-last-input-end pos))))))))

(defun fennel-proto-repl--add-comma-command (op hash &optional interactive?)
  "Add comma-command for the OP to the HASH.
If the INTERACTIVE? arg is non-nil, ask for user input to be
passed as an argument to the command."
  (puthash
   op
   (lambda ()
     (interactive)
     (fennel-proto-repl-send-message
      op
      (if interactive?
          (read-string
           (format "%s: " (substring (symbol-name op) 1)))
        "")
      (lambda (values)
        (fennel-proto-repl--print
         (format "%s\n" (string-trim-right
                         (if (listp values)
                             (string-join values "\n")
                           values)))))
      (lambda (_ message _)
        (fennel-proto-repl--print (format "%s\n" (string-trim-right message))))))
   hash)
  hash)

(defvar fennel-proto-repl--comma-commands
  (let ((hash (make-hash-table :test 'equal)))
    (fennel-proto-repl--add-comma-command :complete hash t)
    (fennel-proto-repl--add-comma-command :doc hash t)
    (fennel-proto-repl--add-comma-command :reload hash t)
    (fennel-proto-repl--add-comma-command :apropos-show-docs hash t)
    (fennel-proto-repl--add-comma-command :find hash t)
    (fennel-proto-repl--add-comma-command :apropos-doc hash t)
    (fennel-proto-repl--add-comma-command :help hash)
    (fennel-proto-repl--add-comma-command :compile hash t)
    (fennel-proto-repl--add-comma-command :apropos hash t)
    (fennel-proto-repl--add-comma-command :reset hash)
    (fennel-proto-repl--add-comma-command :exit hash))
  "Comma-commands for the Fennel Proto REPL.")

(defun fennel-proto-repl--available-comma-commands ()
  "List available comma-commands."
  (let ((keys '()))
    (maphash (lambda (k _v)
               (setq keys (cons (substring (symbol-name k) 1) keys)))
             fennel-proto-repl--comma-commands)
    keys))

(defun fennel-proto-repl-comma-command ()
  "Execute a comma-command or insert a comma.
If the point is right at the prompt, provide a list of available
comma commands to choose from via `completing-read'."
  (interactive)
  (if (> (point) (save-excursion (comint-goto-process-mark) (point)))
      (insert ",")
    (pcase (completing-read
            "Command: "
            (fennel-proto-repl--available-comma-commands)
            nil t)
      ("" (user-error "No command selected"))
      (command (call-interactively
                (gethash (intern (format ":%s" command))
                         fennel-proto-repl--comma-commands))))))

(defun fennel-proto-repl-clear-output ()
  "Delete the output inserted since the last input.
With a prefix argument CLEAR-REPL it will clear the entire REPL buffer instead."
  (interactive)
  (fennel-proto-repl--with-no-undo
   (comint-delete-output)))

(defun fennel-proto-repl-clear-buffer ()
  "Clear the currently visited REPL buffer completely.
See also the related command `fennel-proto-repl-clear-output'."
  (interactive)
  (fennel-proto-repl--with-no-undo
   (save-excursion
     (comint-goto-process-mark)
     (forward-line 0)
     (let ((inhibit-read-only t))
       (delete-region (point-min) (point))))))

(defun fennel-proto-repl-quit ()
  "Quit the Fennel Proto REPL."
  (interactive)
  (if (fennel-proto-repl--check-for-repl 'no-restart)
      (when-let ((proc (get-buffer-process (fennel-proto-repl--process-buffer))))
        (delete-process proc))
    (user-error "%s is not linked to any Fennel REPL" (buffer-name))))

(defun fennel-proto-repl--input-sender (_ input)
  "Sender for INPUT from the REPL buffer to the REPL process."
  (set-marker fennel-proto-repl--print-marker (point))
  (let ((fennel-proto-repl--buffer (current-buffer)))
    (fennel-proto-repl-send-message
     :eval input
     (lambda (values)
       (fennel-proto-repl--print
        (if (listp values) (string-join values "\t") values))))))

(defun fennel-proto-repl-switch-to-repl ()
  "Switch to the currently linked REPL buffer.
If invoked interactively with a prefix argument, asks for command
to start the REPL."
  (interactive)
  (if (and (eq major-mode 'fennel-proto-repl-mode)
           (not current-prefix-arg)
           (get-buffer-process (current-buffer)))
      (switch-to-buffer-other-window fennel-proto-repl--last-buffer)
    (when current-prefix-arg
      (setq-local fennel-program
                  (read-string "Fennel command: " fennel-program)))
    (when (fennel-proto-repl--check-for-repl)
      (let ((current (current-buffer)))
        (with-current-buffer fennel-proto-repl--buffer
          (setq fennel-proto-repl--last-buffer current)))
      (pop-to-buffer fennel-proto-repl--buffer))))

;;;###autoload
(defun fennel-proto-repl (command &optional repl-buffer)
  "Start a new Fennel Proto REPL.

If invoked interactively with a prefix argument, asks for COMMAND
to start the REPL.  If optional REPL-BUFFER is supplied it is
used as the buffer to start the REPL in.

The command is persisted as a buffer-local variable, the REPL
buffer remembers the command that was used to start it.
Resetting the command to another value can be done by invoking it
by using a prefix argument.

Multiple REPLs are possible and new sessions can be created by
simply calling this function.  Once the new REPL is created, the
current buffer is linked with it, and all in-buffer commands
start working in terms of the new REPL process.  To change what
REPL is used for the current buffer using the
`fennel-proto-repl-link-buffer' function.

Return the REPL buffer."
  (interactive
   (list (if current-prefix-arg
	     (read-string "Fennel command: " fennel-program)
	   fennel-program)))
  (setq-local fennel-program command)
  (fennel-proto-repl--start-server command repl-buffer)
  (fennel-proto-repl-minor-mode 1)
  (with-current-buffer fennel-proto-repl--buffer
    (setq fennel-proto-repl--last-buffer (current-buffer))
    (setq-local fennel-program command))
  (get-buffer fennel-proto-repl--buffer))

;;;###autoload
(define-derived-mode fennel-proto-repl-mode comint-mode "FPR"
  "Major mode for Fennel Proto REPL.

\\{fennel-proto-repl-mode-map}"
  :group 'fennel-mode
  (setq comint-prompt-regexp (format "^%s" fennel-proto-repl-prompt))
  (setq comint-prompt-read-only t)
  (setq comint-input-sender 'fennel-proto-repl--input-sender)
  (add-hook 'comint-input-filter-functions 'fennel-repl--input-filter nil t)
  (setq mode-line-process '(":%s"))
  (setq-local lisp-indent-function #'fennel-indent-function)
  (setq-local indent-line-function #'lisp-indent-line)
  (setq-local lisp-doc-string-elt-property 'fennel-doc-string-elt)
  (setq-local comment-end "")
  (fennel-font-lock-setup)
  (set-syntax-table fennel-mode-syntax-table)
  (add-hook 'completion-at-point-functions 'fennel-proto-repl-complete nil t)
  (add-hook 'paredit-mode-hook #'fennel-repl-paredit-setup nil t)
  (fennel-proto-repl--setup-eldoc))

(define-key fennel-proto-repl-mode-map (kbd "C-c C-z") #'fennel-proto-repl-switch-to-repl)
(define-key fennel-proto-repl-mode-map (kbd "C-c C-q") #'fennel-proto-repl-quit)
(define-key fennel-proto-repl-mode-map (kbd "C-c C-c") #'fennel-proto-repl-interrupt)
(define-key fennel-proto-repl-mode-map (kbd "C-c C-S-o") #'fennel-proto-repl-clear-buffer)
(define-key fennel-proto-repl-mode-map (kbd "C-c C-o") #'fennel-proto-repl-clear-output)
(define-key fennel-proto-repl-mode-map (kbd "TAB") #'indent-for-tab-command)
(define-key fennel-proto-repl-mode-map (kbd "C-j") #'newline-and-indent)
(define-key fennel-proto-repl-mode-map (kbd ",") #'fennel-proto-repl-comma-command)

;;; Completion

(defvar fennel-proto-repl--symbol-types
  "(do (macro __fennel-proto-repl-symbol-type [v]
         (let [scope (get-scope)]
           (if (. scope.specials v) :special
               (. scope.macros v) :macro
               `(type ,(sym v)))))
       (let [types [%s]]
         (when (not= (length types) 0)
           types)))"
  "Template for getting a type of symbols for completion annotation.")

(defvar fennel-proto-repl--completion-kinds (make-hash-table :test 'equal)
  "A mapping between the completion items and their kinds.")

(defun fennel-proto-repl--completion-candidate-kind (item)
  "Annotate the completion kind of the ITEM for `company-mode' and `corfu'."
  (when fennel-mode-annotate-completion
    (pcase (gethash item fennel-proto-repl--completion-kinds)
      ("function" 'function)
      ("table" 'module)
      ("special" 'keyword)
      ("macro" 'macro)
      ("number" 'constant)
      ("boolean" 'boolean)
      ("string" 'string)
      (_ (if (string-match-p "\\." item) 'field 'variable)))))

(defun fennel-proto-repl--completion-annotate (item)
  "Annotate the completion kind of the ITEM."
  (let ((kind (fennel-proto-repl--completion-candidate-kind item)))
    (cond ((eq kind 'module) " table")
          ((eq kind 'variable) " definition")
          (kind (format " %s" kind))
          (t ""))))

(defun fennel-proto-repl--completions (sym)
  "Fetch completions for SYM.
Requests completions from the Fennel process, and then requests
their kinds in a separate request.  Will not preform completion
if the REPL is not available to process one."
  (unless (fennel-proto-repl--callbacks-pending)
    (when-let ((completions
                (condition-case nil
                    (fennel-proto-repl-send-message-sync
                     :complete sym #'ignore #'ignore)
                  (fennel-proto-repl--timeout nil))))
      (let ((kinds
             (condition-case nil
                 (car
                  ;; HACK: fennel returns vector of strings, we read
                  ;; it as an elisp array.
                  (read-from-string
                   (car
                    (fennel-proto-repl-send-message-sync
                     :eval
                     (fennel-proto-repl--minify-body
                      (format
                       fennel-proto-repl--symbol-types
                       (mapconcat
                        (lambda (x)
                          (format "(__fennel-proto-repl-symbol-type %S)" x))
                        completions " "))
                      t)
                     #'ignore
                     #'ignore))))
               (fennel-proto-repl--timeout nil)
               (error nil))))
        (clrhash fennel-proto-repl--completion-kinds)
        (when kinds
          (let* ((syms (vconcat completions))
                 (len (min (length syms) (length kinds))))
            (dotimes (i len)
              (puthash (aref syms i) (aref kinds i)
                       fennel-proto-repl--completion-kinds))))
        completions))))

(defun fennel-proto-repl--completion-table-with-cache (fun string)
  "Create cached dynamic completion table from function FUN.
This is a wrapper for `completion-table-dynamic' that saves the
last result from FUN for STRING, so that several lookups with the
same argument only need to call FUN once."
  (let (last-result)
    (completion-table-dynamic
     (lambda (_)
       (or last-result
           (setq last-result (funcall fun string)))))))

(defun fennel-proto-repl-complete ()
  "Return a list of completion data for `completion-at-point'."
  (interactive)
  (when-let ((bounds (bounds-of-thing-at-point 'symbol)))
    (let ((start (car bounds))
          (end (cdr bounds)))
      (list start end
            (fennel-proto-repl--completion-table-with-cache
             #'fennel-proto-repl--completions
             (buffer-substring-no-properties start end))
            :annotation-function #'fennel-proto-repl--completion-annotate
            :company-kind #'fennel-proto-repl--completion-candidate-kind
            :company-doc-buffer #'fennel-proto-repl--eldoc-get-doc-buffer))))

;;; Interaction

(defun fennel-proto-repl--check-for-repl (&optional inhibit-start)
  "Check if Fennel Proto REPL is running.
If the REPL is running, return the REPL buffer.  Otherwise, check
if the current buffer is linked to the dead REPL buffer and
restart it.  If the current buffer itself is a REPL, also restart
it.  If the current buffer is not linked to any REPL, ask to
start one.  If the optional argument INHIBIT-START is given don't
start the REPL only check for one."
  (cond
   ((comint-check-proc fennel-proto-repl--buffer)
    (get-buffer fennel-proto-repl--buffer))
   (inhibit-start nil)
   ((eq major-mode 'fennel-proto-repl-mode)
    (fennel-proto-repl fennel-program (current-buffer)))
   ((and fennel-proto-repl--buffer
         (buffer-live-p (get-buffer fennel-proto-repl--buffer)))
    (fennel-proto-repl fennel-program fennel-proto-repl--buffer))
   ((yes-or-no-p
     (format
      "Buffer %s is not linked to any Fennel Proto REPL.  Start a new one?"
      (buffer-name)))
    (fennel-proto-repl fennel-program))))

(defun fennel-proto-repl--display-result (result)
  "Display RESULT in the echo area."
  (let (message-log-max)
    (message "=> %s" (string-trim (string-join result "\t")))))

(defun fennel-proto-repl-eval-print-last-sexp (&optional pretty-print)
  "Evaluate the expression preceding point.
Print its value into the current buffer.
With an optional PRETTY-PRINT prefix pretty-print the result."
  (interactive "P")
  (when (fennel-proto-repl--check-for-repl)
    (let ((pos (point-marker)))
      (fennel-proto-repl-send-message
       :eval
       (buffer-substring-no-properties
        (save-excursion (backward-sexp) (point))
        (point))
       (lambda (result)
         (let ((result (string-join result "\t")))
           (with-current-buffer (marker-buffer pos)
             (goto-char (marker-position pos))
             (insert (format "\n%s"
                             (if pretty-print
                                 result
                               (fennel-proto-repl--minify-body
                                result 'no-newlines))))
             (set-marker pos nil))))))))

(defun fennel-proto-repl-eval-region (start end &optional and-go)
  "Send the current region to the Fennel REPL process.
If START is a string and END is nil, send the string to the
process.  Prefix argument AND-GO means switch to the REPL buffer
afterward."
  (interactive "r\nP")
  (when (fennel-proto-repl--check-for-repl)
    (fennel-proto-repl-send-message
     :eval
     (if (and (stringp start) (null end))
         (substring-no-properties start)
       (buffer-substring-no-properties start end))
     #'fennel-proto-repl--display-result))
  (when and-go (pop-to-buffer fennel-proto-repl--buffer)))

(defun fennel-proto-repl-eval-buffer (&optional and-go)
  "Send the whole buffer to the Fennel REPL process.
Prefix argument AND-GO means switch to the REPL buffer
afterward."
  (interactive "P")
  (fennel-proto-repl-eval-region (point-min) (point-max) and-go))

(defun fennel-proto-repl-eval-last-sexp (&optional and-go)
  "Send the previous sexp to the Fennel REPL process.
Prefix argument AND-GO means switch to the REPL buffer
afterward."
  (interactive "P")
  (fennel-proto-repl-eval-region
   (save-excursion (backward-sexp) (point))
   (point)
   and-go))

(defun fennel-proto-repl-eval-form-and-next ()
  "Send the previous sexp to the REPL process and move to the next one."
  (interactive "")
  (while (not (zerop (car (syntax-ppss))))
    (up-list))
  (fennel-proto-repl-eval-last-sexp)
  (forward-sexp))

(defun fennel-proto-repl-eval-paragraph (&optional and-go)
  "Send the current paragraph to the Fennel REPL process.
Prefix argument AND-GO means switch to the Lisp buffer afterward."
  (interactive "P")
  (save-excursion
    (mark-paragraph)
    (fennel-proto-repl-eval-region (point) (mark) and-go)))

(defun fennel-proto-repl-eval-defun (&optional and-go)
  "Send the previous sexp to the Fennel REPL process.
Prefix argument AND-GO means switch to the REPL buffer
afterward."
  (interactive "P")
  (fennel-proto-repl-eval-region
   (thing-at-point 'defun)
   nil
   and-go))

(defun fennel-proto-repl-interrupt ()
  "Send interrupt to the REPL process."
  (interactive)
  (with-current-buffer fennel-proto-repl--buffer
    (comint-skip-input))
  (with-current-buffer (fennel-proto-repl--process-buffer)
    (interrupt-process nil)))

(defun fennel-proto-repl--reload-handler (values)
  "Handler for the reload operation.
VALUES is a list with eiter an \"ok\" string or an error message.
This function tries to decompose the error message and provide an
interactable error screen."
  (when-let ((status (car-safe values)))
    (if (equal status "ok")
        (message "successfuly reloaded '%s'" fennel-module-name)
      (save-match-data
        (if (string-match "\\([^:]+:[0-9]+\\):[0-9]+\\([a-z[:space:]]+\\) \\(error:\\)"
                          status)
            (let ((message (substring status (1+ (match-end 3))))
                  (locus (string-trim (match-string 1 status)))
                  (kind (string-trim (match-string 2 status))))
              (fennel-proto-repl--display-error
               kind message
               (format "  %s: in %s" locus fennel-module-name)))
          (message "failed to reload '%s'" fennel-module-name))))))

(defun fennel-proto-repl-reload (ask?)
  "Reload the module for the current file.

ASK? forces module name prompt.

Tries to reload in a way that makes it retroactively visible; if
the module returns a table, then existing references to the same
module will have their contents updated with the new
value.  Requires installing the fennel.searcher.

Queries the user for a module name upon the first run for a given
buffer, or when given a prefix arg."
  (interactive "P")
  (comint-check-source buffer-file-name)
  (fennel-get-module ask? fennel-module-name)
  (when (and (file-exists-p (concat (file-name-base nil) ".lua"))
             (yes-or-no-p "Lua file for module exists; delete it first?"))
    (delete-file (concat (file-name-base nil) ".lua")))
  (fennel-proto-repl-send-message :reload fennel-module-name
                    #'fennel-proto-repl--reload-handler))

(defun fennel-proto-repl-show-documentation (symbol)
  "Show SYMBOL documentation in the REPL."
  (interactive (list (read-string
                      "Documentation: "
                      (symbol-name (lisp-fn-called-at-pt)))))
  (fennel-proto-repl-send-message
   :doc symbol
   (lambda (values)
     (when-let ((message (car-safe values)))
       (fennel-proto-repl--print (format "%s\n" message))))))

(defun fennel-proto-repl-show-var-documentation (symbol)
  "Show SYMBOL documentation in the REPL."
  (interactive (list (read-string
                      "Documentation: "
                      (symbol-name (lisp-var-at-pt)))))
  (fennel-proto-repl-show-documentation symbol))

(defun fennel-proto-repl-show-arglist (symbol)
  "Show SYMBOL arglist in the REPL."
  (interactive (list (read-string
                      "Arglist: "
                      (symbol-name (lisp-fn-called-at-pt)))))
  (fennel-proto-repl-send-message
   :eval (fennel-proto-repl--generate-query-command
          symbol
          fennel-proto-repl--arglist-query-template
          fennel-proto-repl--multisym-arglist-query-template)
   (lambda (message)
     (when-let ((arglist
                 (condition-case nil
                     (append (car (read-from-string (car message))) nil)
                   (error nil))))
       (fennel-proto-repl--print
        (format "Arglist for %s: [%s]\n"
                symbol
                (string-join arglist " ")))))))

(defun fennel-proto-repl-macroexpand ()
  "Show the macroexpansion of the expression at point in the REPL."
  (interactive)
  (fennel-proto-repl-send-message
   :eval (format "(macrodebug %s)" (thing-at-point 'sexp))
   #'ignore))

;;;###autoload
(define-minor-mode fennel-proto-repl-minor-mode
  "Fennel Proto REPL interaction mode.

\\{fennel-proto-repl-minor-mode-map}"
  :group 'fennel-mode
  :lighter " FPR interaction"
  :keymap (make-sparse-keymap)
  (if fennel-proto-repl-minor-mode
      (progn
        (when fennel-proto-repl--buffer
          (fennel-proto-repl-link-buffer))
        (add-hook 'completion-at-point-functions 'fennel-proto-repl-complete nil t)
        (add-hook 'xref-backend-functions 'fennel-proto-repl--xref-backend nil t)
        (fennel-proto-repl--setup-eldoc))
    (progn
      (remove-hook 'completion-at-point-functions 'fennel-proto-repl-complete t)
      (remove-hook 'xref-backend-functions 'fennel-proto-repl--xref-backend t)
      (fennel-proto-repl--setup-eldoc 'remove))))

(define-key fennel-proto-repl-minor-mode-map (kbd "C-x C-e") 'fennel-proto-repl-eval-last-sexp)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-e") 'fennel-proto-repl-eval-defun)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-q") 'fennel-proto-repl-quit)
(define-key fennel-proto-repl-minor-mode-map (kbd "M-'") 'ignore) ;; TODO add support?
(define-key fennel-proto-repl-minor-mode-map (kbd "C-M-x") 'fennel-proto-repl-eval-defun)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-n") 'fennel-proto-repl-eval-form-and-next)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-p") 'fennel-proto-repl-eval-paragraph)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-r") 'fennel-proto-repl-eval-region)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-b") 'fennel-proto-repl-eval-buffer)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-z") 'fennel-proto-repl-switch-to-repl)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-t") 'fennel-format)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-l") 'fennel-view-compilation)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-k") 'fennel-proto-repl-reload)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-f") 'fennel-proto-repl-show-documentation)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-d") 'fennel-proto-repl-show-documentation)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-v") 'fennel-proto-repl-show-var-documentation)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-a") 'fennel-proto-repl-show-arglist)
(define-key fennel-proto-repl-minor-mode-map (kbd "C-c C-p") 'fennel-proto-repl-macroexpand)

;;; REPL Error handling

(defvar fennel-proto-repl-compilation-error-regexp-alist
  '(lua-stacktrace fennel-test-error fennel-compile-error)
  "Alist that specifies how to match errors in Fennel compiler output.
See `compilation-error-regexp-alist' for more information.")

(defvar fennel-proto-repl-compilation-error-regexp-alist-alist
  '((lua-stacktrace
     "\\(?:^[ ]+\\([^[
:]+\\):\\([[:digit:]]+\\):[[:space:]]+in.+$\\)"
     1 2 nil 2)
    (fennel-compile-error
     "^Compile error: \\([^:]+\\):\\([[:digit:]]+\\):?\\([[:digit:]]+\\)?\\$"
     1 2 3 2))
  "Alist of values for `fennel-proto-repl-compilation-error-regexp-alist'.
See `compilation-error-regexp-alist-alist' for more information.")

(defvar fennel-proto-repl-compilation-mode-map
  (make-sparse-keymap))

(define-compilation-mode fennel-proto-repl-compilation-mode "Fennel Error"
  "Mode for displaying error messages from the Fennel Proto REPL."
  (define-key fennel-proto-repl-compilation-mode-map (kbd "g") 'ignore))

(defun fennel-proto-repl--display-error (type message &optional traceback)
  "Display MESSAGE in a special buffer.
TYPE is a kind of error, used to handle internal REPL errors."
  (pcase type
    ("proto-repl"
     (fennel-proto-repl--print
      (concat "Fennel Proto REPL was terminated due to an internal error:\n"
              (propertize (ansi-color-apply message)
                          'font-lock-face 'error)
              "\nLikely a bug in the client."))
     (delete-process fennel-proto-repl--process-buffer))
    (_
     (let* ((reason (ansi-color-apply (string-trim-right message)))
            (reason (if (string-suffix-p ":" reason)
                        (substring reason 0 (1- (length reason)))
                      reason)))
       (fennel-proto-repl--print
        (propertize (format "%s\n" reason) 'font-lock-face 'error))
       (when traceback
         (with-current-buffer (get-buffer-create "*Fennel Error*")
           (save-excursion
             (let ((inhibit-read-only t))
               (widen)
               (erase-buffer)
               (insert (format "%s error:\n\n" (capitalize type)))
               (insert reason)
               (let ((pos (point)))
                 (insert (format "\n\n%s\n" (string-trim traceback "\n")))
                 (insert "\n")
                 (add-text-properties
                  pos (point) '(font-lock-face font-lock-keyword-face)))
               (fennel-proto-repl-compilation-mode)
               (ansi-color-apply-on-region (point-min) (point-max))
               (pcase fennel-proto-repl-error-buffer-action
                 ('jump (pop-to-buffer (current-buffer)))
                 ('show (display-buffer (current-buffer))))))))))))

;;; Eldoc support

(defun fennel-proto-repl--eldoc-fn-in-current-sexp ()
  "Obtain the function name and position in an argument list."
  (save-excursion
    (let ((ppss (syntax-ppss)))
      (unless (or (nth 8 ppss)
                  (nth 3 ppss))
        (when (and (nth 1 ppss)
                   (equal ?\( (char-after (nth 1 ppss))))
          (let ((argument-index (1- (fennel-proto-repl--eldoc-num-skipped-sexps))))
            (when (< argument-index 0)
              (setq argument-index 0))
            (when-let ((sym (thing-at-point 'symbol)))
              (unless (string-prefix-p ":" sym)
                (cons sym argument-index)))))))))

(defun fennel-proto-repl--eldoc-num-skipped-sexps ()
  "Find the number of inner sexps from the sexp start to point."
  (let ((parse-sexp-ignore-comments t)
	(num-skipped-sexps 0))
    (condition-case _
	(progn
	  (condition-case _
	      (let ((p (point)))
		(forward-sexp -1)
		(forward-sexp 1)
		(when (< (point) p)
		  (setq num-skipped-sexps 1)))
	    (error))
	  (while
	      (let ((p (point)))
		(forward-sexp -1)
		(when (< (point) p)
		  (setq num-skipped-sexps (1+ num-skipped-sexps))))))
      (error))
    num-skipped-sexps))

(defun fennel-proto-repl--eldoc-format-function (message name pos)
  "Format eldoc MESSAGE for a Fennel function NAME.

POS is a position in an argument list."
  (when-let ((signature
              (condition-case nil
                  (append (car (read-from-string message)) nil)
                (error nil))))
    (let* ((method? (string-match-p ":" name))
           (args (if method?
                     (cdr signature)
                   signature))
           (pos (min (1- pos) (1- (length args)))))
      (when (>= pos 0)
        (setcar (nthcdr pos args)
                (propertize (nth pos args) 'face 'eldoc-highlight-function-argument)))
      (mapconcat 'identity args " "))))

(defun fennel-proto-repl--eldoc-fn-handler (values callback thing fn-info)
  "Handler for the eldoc CALLBACK.
Calls callback with the first elemnt of VALUES and THING based on
data from FN-INFO."
  (when-let ((message (car-safe values)))
    (when-let ((data (fennel-proto-repl--eldoc-format-function
                      message (car fn-info) (cdr fn-info))))
      (funcall callback data
               :thing thing
               :face 'font-lock-function-name-face))))

(defun fennel-proto-repl-eldoc-fn-docstring (callback &rest _)
  "Query for the arglist for the function call at point.
CALLBACK is executed by Eldoc once the arglist is returned by the Fennel
REPL process."
  (let ((fn-info (fennel-proto-repl--eldoc-fn-in-current-sexp)))
    (when-let ((fn (car fn-info)))
      (let* ((sym (substring-no-properties fn))
             (command (fennel-proto-repl--generate-query-command
                       sym fennel-proto-repl--arglist-query-template
                       fennel-proto-repl--multisym-arglist-query-template)))
        (if (not (fennel-proto-repl--callbacks-pending))
            (fennel-proto-repl--eldoc-fn-handler
             (fennel-proto-repl-send-message-sync
              :eval command #'ignore #'ignore)
             callback sym fn-info)
          (fennel-proto-repl-send-message
           :eval command
           (lambda (values)
             (fennel-proto-repl--eldoc-fn-handler
              values callback sym fn-info))
           #'ignore #'ignore)
          t)))))

(defun fennel-proto-repl--eldoc-var-handler (values callback thing)
  "Handler for the eldoc CALLBACK.
Calls callback with the first elemnt of VALUES and THING."
  (when-let ((val (car-safe values)))
    (when-let ((docstring (condition-case nil
                              (car (read-from-string val))
                            (error nil))))
      (funcall callback docstring
               :thing thing
               :face 'font-lock-variable-name-face))))

(defun fennel-proto-repl-eldoc-var-docstring (callback &rest _)
  "Query for the documentation of a symbol at point.
CALLBACK is executed by Eldoc once the documentation is returned
by the Fennel REPL process."
  (when-let ((sym (thing-at-point 'symbol 'no-properties)))
    (let ((ppss (syntax-ppss)))
      (unless (or (string-prefix-p ":" sym)
                  (string-prefix-p "," sym)
                  (nth 3 ppss)
                  (nth 4 ppss))
        (let ((command (fennel-proto-repl--generate-query-command
                        sym fennel-proto-repl--doc-query-template
                        fennel-proto-repl--multisym-doc-query-template)))
          (if (not (fennel-proto-repl--callbacks-pending))
              (fennel-proto-repl--eldoc-var-handler
               (fennel-proto-repl-send-message-sync
                :eval command
                #'ignore #'ignore)
               callback sym)
            (fennel-proto-repl-send-message
             :eval command
             (lambda (values)
               (fennel-proto-repl--eldoc-var-handler
                values callback sym))
             #'ignore #'ignore)
            t))))))

(defun fennel-proto-repl--setup-eldoc (&optional remove?)
  "Set up eldoc support for `fennel-proto-repl'.
If the optional argument REMOVE? is passed, remove the eldoc
support from the buffer.  Only configures eldoc if
`eldoc-documentation-functions' is present."
  (when (boundp 'eldoc-documentation-functions)
    (if remove?
        (progn (remove-hook 'eldoc-documentation-functions
                            #'fennel-proto-repl-eldoc-fn-docstring t)
               (remove-hook 'eldoc-documentation-functions
                            #'fennel-proto-repl-eldoc-var-docstring t))
      (progn (add-hook 'eldoc-documentation-functions
                       #'fennel-proto-repl-eldoc-fn-docstring nil t)
             (add-hook 'eldoc-documentation-functions
                       #'fennel-proto-repl-eldoc-var-docstring nil t)))))

(defun fennel-proto-repl--font-lock-doc-buffer ()
  "Apply Markdown font lock."
  (when (and fennel-eldoc-fontify-markdown
             (fboundp 'markdown-mode))
    (setq-local delay-mode-hooks t)
    (setq-local delayed-mode-hooks nil)
    (markdown-mode)
    (font-lock-fontify-region (point-min) (point-max))))

(defun fennel-proto-repl--eldoc-pre-format-doc-buffer ()
  "Preformat doc buffer.
If `fennel-eldoc-fontify-markdown' is t wraps the expression in a
code block."
  (save-match-data
    (save-excursion
      (goto-char (point-min))
      (set-syntax-table fennel-mode-syntax-table)
      (if (not fennel-eldoc-fontify-markdown)
          (forward-sexp)
        (insert "```fennel\n")
        (forward-sexp)
        (insert "\n```"))
      (newline)
      (fennel-proto-repl--font-lock-doc-buffer))))

(defun fennel-proto-repl--eldoc-get-doc-buffer (symbol)
  "Get documentation buffer for SYMBOL.
Intended for use with the `company-mode' or `corfu' packages."
  (let ((symbol (substring-no-properties symbol)))
    (let ((repl-buffer (get-buffer fennel-proto-repl--buffer)))
      (with-current-buffer (get-buffer-create fennel-proto-repl--doc-buffer)
        (erase-buffer)
        (condition-case nil
            (let ((fennel-proto-repl--buffer repl-buffer))
              (when-let ((doc (fennel-proto-repl-send-message-sync
                               :doc symbol #'ignore #'ignore)))
                (when-let ((doc (car-safe doc)))
                  (insert doc)
                  (fennel-proto-repl--eldoc-pre-format-doc-buffer)
                  fennel-proto-repl--doc-buffer)))
          (fennel-proto-repl--timeout nil))))))

;;; Xref

(cl-defmethod xref-backend-identifier-at-point ((_ (eql fennel-proto-repl)))
  "Return the relevant identifier at point."
  (when-let ((sym (thing-at-point 'symbol)))
    (unless (string-prefix-p ":" sym)
      (car (fennel-proto-repl--method-to-sym sym)))))

(cl-defmethod xref-backend-definitions ((_ (eql fennel-proto-repl)) sym)
  "Find definitions of SYM."
  (when-let ((definitions
               (condition-case nil
                   (fennel-proto-repl-send-message-sync :find sym #'ignore #'ignore)
                 (fennel-proto-repl--timeout nil))))
    (let ((items (mapcar (lambda (locus)
                           (let ((info (split-string locus ":")))
                             (xref-make
                              (car info)
                              (xref-make-file-location
                               (expand-file-name (car info))
                               (string-to-number (cadr info))
                               0))))
                         definitions)))
      items)))

(cl-defmethod xref-backend-identifier-completion-table
  ((_ (eql fennel-proto-repl)))
  "Return the completion table for identifiers."
  nil)

(defun fennel-proto-repl--xref-backend ()
  "Return the xref backend when Fennel Proto REPL is running."
  (when (and (fennel-proto-repl--process-buffer)
             (process-live-p
              (get-buffer-process
               (fennel-proto-repl--process-buffer))))
    'fennel-proto-repl))

(provide 'fennel-proto-repl)
;;; fennel-proto-repl.el ends here

;; Local Variables:
;; eval: (save-excursion
;;         (goto-char (point-min))
;;         (and hs-minor-mode
;;              (search-forward "(defvar fennel-proto-repl--upgrade" nil t)
;;              (hs-hide-block)))
;; End:
