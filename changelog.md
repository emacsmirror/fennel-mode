# Summary of user-visible changes

## 0.9.3 / ???

* Support `.fnlm` files.
* Remove old reloading mechanism in favor of the `,reload` REPL
  command
* Obsolete `fennel-mode-annotate-completion` in favor of
  `fennel-proto-repl`
* Highlight backquoted (``; `sym` ``) symbols in comments

## 0.9.2 / 2025-03-13

* Switch `M-.` find definition to using `xref`, allowing graceful
  fallback to eglot when the repl can't find the target.
* Avoid binding `q` in all Lua buffers when viewving compilation.
* Provide a customization variable to change what Lua major mode to
  use in a compilation view.
* Better handling of ANSI colors in Fennel Proto REPL.
* `ob-fennel` now can send Emacs Lisp hash tables to Fennel.
* major update to the `fennel-proto-repl` - new protocol version,
  changes in user-input handling, ability to extend with custom
  operations.
* Add Flymake support via `fennel-ls`
* Add dynamic font-locking support to the `fennel-proto-repl` mode
* Add project integration to the `fennel-proto-repl` mode
* Remove `M-'` in favor of `C-u M-.`

## 0.9.1 / 2024-04-24

* Add `fennel-proto-repl-read-handler` for custom `io.read` handling
* Fix bug with discarding undo history in proto-repl buffers
* Move Fennel Proto REPL settings to a subgroup
* Provide `fennel-proto-repl-fennel-module-name` for customizing
  Fennel module location for `fennel-proto-repl` to work with apps
  that embed Fennel.
* Update protocol to 0.5.0
* Add support to the `,return` command in `fennel-proto-repl`.
* Fix bug with `poly-org` overriding `compile-command` in
  `fennel-view-compilation`.

## 0.9.0 / 2023-08-14

* Added support for Org Babel via proto-repl

## 0.8.1 / 2023-04-23

* Log results and errors to the *Messages* buffer.
* Indicate REPL state in the mode-line.
* Add a binding to link the current buffer to the REPL.
* Allow using proto-repl from fennel-scratch via custom variable.

## 0.8.0 / 2023-04-17

* Remove `fennel-eldoc` module and Eldoc integration with
  `fennel-repl`.  Eldoc is now a feature of the `fennel-proto-repl`.
* Add `fennel-mode-repl-prompt` and `fennel-mode-repl-subprompt`
  variables that are now strings that are automatically escaped.
* Obsolete `fennel-mode-repl-prompt-regexp` and
  `fennel-mode-repl-subprompt-regexp` variables.
* Fix duplicate prompt bug in the `fennel-repl` REPL.

## 0.7.0 / 2023-04-03

* New protocol-based REPL integration via `fennel-proto-repl` command

## 0.6.0 / 2023-02-15

* Add fennel-mode to `interpreter-mode-alist`
* Add `fennel-macroexpand` command and a shortcut
* Add separate `antifennel.el` file
* Add `fennel-mode-repl-subprompt-regexp` for REPL subprompt recognition
* Add `fennel-mode-repl-prompt-readonly` custom
* Add experimental `fennel-repl-minify-code` custom that defines how
  to collapse code before sending it to the REPL
* Add new forms from Fennel 1.3.0
* Support the `with-*` and `def*` naming convention for body macros
* Add `fennel-format-region` command

## 0.5.0 / 2022-04-03

* Add optional Eldoc support when REPL is connected
* Add `fennel-mode-repl-prompt-regexp` to define a regular expression
  to recognize prompt

## 0.4.1 / 2021-11-22

* Add `fennel-scratch` command, that opens scratch buffer for Fennel evaluation
* Make REPL prompt readonly
* Add missing highlighting for names defined with `macro`

## 0.4.0 / 2021-10-26

* Add completion annotation and improve syntax highlighting consistency
* Highlight local definitions
* Highlight function definitions with `font-lock-function-name-face`
* Fix REPL prompt duplication when completing symbols
* Fix multiline input breaking the REPL interaction
* Fix `fennel-find-definition` to use `symbol-at-point` as default value
* Allow restarting and reusing fennel-repl buffer if present with C-c C-z
* Implement more content aware table indentation
* Inherit major mode from `prog-mode` instead of `lisp-mode`
* Bump required Emacs version to 26.1

## 0.3.1 / 2021-07-04

* Fix `M-.` to use the symbol at point by default
* Improve arglist and docstring lookup
* Fix several bugs around completion
* Fix a bug where the `fennel-program` setting was ignored
* Don't default to `--correlate` mode in repl

## 0.3.0 / 2021-06-18

* Add support for `completion-at-point`
* Add `fennel-format` command
* Extend jump-to-definition to working on functions in modules
* Add font-lock and indentation for new forms up to Fennel 0.9.3

## 0.2.0 / 2020-06-17

* Add font-lock for new fennel forms up to 0.4.1
* Add docstring lookup with `C-c C-d`
* Fail gracefully during compilation errors in fennel-reload

## 0.1.0 / 2018-11-01

* Fix imenu support
* Add fennel-view-compilation function
* Make fennel-reload safe for compiler errors
* Fixed a bug where indentation wouldn't work unless lisp-mode was loaded
* Add a setting to disable switching to repl on reload

## 0.0.2 / 2018-05-09

* Add fennel-reload
* Add imenu support
* Add find-definition
* Lots more keywords

## 0.0.1 / 2018-02-18

* Initial commit
