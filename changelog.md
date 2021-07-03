# Summary of user-visible changes

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
