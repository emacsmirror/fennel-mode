# Fennel Mode

Font-lock, indentation, navigation, and repl support for the
[Fennel](https://github.com/bakpakin/Fennel) programming language.

Supports `M-x imenu` for quick navigation to local definitions.

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
* `C-c C-k` - Reload the module for the current file (requires `fennel.seacher`)
* `C-c C-l` - Display compilation output for the current file
* `M-.`     - Jump to the definition of the function at point
* `M-,`     - Jump back to where you were before jumping to definition

Note that evaluating anything other than a whole module may not have
the intended effect unless you use globals since code is evaluated on
a per-chunk basis, and lexical scope is contained within a single
chunk. Sending the definition of a local to the repl will have no
effect, because your subsequent input in the repl won't have access to
the previous scope. Because of this it's best to require your module
and put it in a global, then reload your module to see your changes
and try them interactively.

These functions assume a `fennel` executable is present on your
path. You can override the location by setting `inferior-lisp-program`.
For instance, if you have
[a stdio repl](https://gitlab.com/alexjgriffith/min-love2d-fennel/blob/master/lib/stdio.fnl)
in a [LÖVE](https://love2d.org) game, you can set this to `love .` to
get an interactive support with reloads.

## Copyright

Copyright © 2018 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later)
