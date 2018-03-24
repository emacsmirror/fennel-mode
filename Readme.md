# Fennel Mode

Font-lock, indentation, navigation, and repl support for the
[Fennel](https://github.com/bakpakin/Fennel) programming language.

Supports `M-x imenu` for local definitions.

## Installation

Place `fennel-mode.el` on your `load-path` and add this to your config:

```lisp
(autoload 'fennel-mode "/path/to/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
```

## Interactivity

Run `M-x run-lisp` from a `fennel-mode` buffer to open a repl
process. Once a repl is open, you can send code from a buffer to be
evaluated.

* `C-c C-e` - Evaluate current top-level form
* `C-c C-r` - Evaluate the region
* `C-c C-z` - Start or switch to repl buffer
* `M-.`     - Jump to the definition of the function at point
* `M-,`     - Jump back to where you were before jumping to definition

These functions assume a `fennel` executable is present on your
path. You can override the location by setting `inferior-lisp-program`.

## Copyright

Copyright © 2018 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later)
