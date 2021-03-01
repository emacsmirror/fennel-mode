# Fennel Mode

Font-lock, indentation, navigation, documentation, and repl support for the
[Fennel](https://fennel-lang.org) programming language.

Supports `M-x imenu` for quick navigation to local definitions.

## Installation

Place `fennel-mode.el` on your `load-path` and add this to your config:

```lisp
(autoload 'fennel-mode "/path/to/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
```

## Interactivity

Run `C-u M-x run-lisp RET fennel RET` to open a repl buffer. Once a
repl is open, you can send code from a fennel-mode buffer to be evaluated.

* `C-c C-z` - Start or switch to repl buffer
* `C-c C-e` - Evaluate current top-level form
* `C-c C-r` - Evaluate the region
* `C-c C-k` - Reload the module for the current file (requires `fennel.seacher`)
* `C-c C-d` - Ask for a value and show its docstring in the repl
* `C-c C-l` - Display compilation output for the current file
* `C-c C-t` - Reformat current buffer with [fnlfmt][1] (separate install)
* `M-.`     - Jump to the definition of the function at point
* `M-,`     - Jump back to where you were before jumping to definition

These functions assume a `fennel` executable is present on your
path. You can override the location by setting `inferior-lisp-program`.
For instance, if you have
[a stdio repl](https://gitlab.com/alexjgriffith/min-love2d-fennel/blob/master/lib/stdio.fnl)
in a [LÖVE](https://love2d.org) game, you can set this to `love .` to
get an interactive support with reloads.

## Copyright

Copyright © 2018-2021 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later)

[1]: https://git.sr.ht/~technomancy/fnlfmt
