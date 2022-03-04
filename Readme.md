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

Note that `fennel-mode` doesn't inherit from `lisp-mode` since 0.4.0,
and instead switched to `prog-mode`.

## Interactivity

Run `M-x fennel-repl RET` to open a repl buffer. Once a repl is open,
you can send code from a fennel-mode buffer to be evaluated.

* `C-c C-z` - Start or switch to repl buffer
* `C-c C-e` - Evaluate current top-level form
* `C-c C-r` - Evaluate the region
* `C-c C-k` - Reload the module for the current file (requires `fennel.seacher`)
* `C-c C-d` - Ask for a value and show its docstring in the repl
* `C-c C-a` - Ask for a function and show its argument list in the repl
* `C-c C-l` - Display compilation output for the current file
* `C-c C-t` - Reformat current buffer with [fnlfmt][1] (separate install)
* `M-.`     - Jump to the definition of a globally-visible function
* `M-'`     - Jump to the definition of a function in a module
* `M-,`     - Jump back to where you were before jumping to definition
* `M-TAB`   - Completion at point (Fennel 0.9.3+)

These functions assume a `fennel` executable is present on your
path. You can override the location by setting `inferior-lisp-program`.
For instance, if you have
[a stdio repl](https://gitlab.com/alexjgriffith/min-love2d-fennel/blob/master/lib/stdio.fnl)
in a [LÖVE](https://love2d.org) game, you can set this to `love .` to
get an interactive support with reloads.

Note that finding the definition of a function with `M-.` only works when the
function is in scope for the repl, which means it's usually best to
load a module and set it as a global if you want to use it this way.

## Eldoc support

As of 0.5.0, the `eldoc-mode` is supported in regular file buffers,
meaning that when the connection to the REPL is established, echo area
will show known function arglists and variable documentation. In
addition to that documentation popup can be displayed if completion
frontend supports this feature.

## Copyright

Copyright © 2018-2021 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later)

[1]: https://git.sr.ht/~technomancy/fnlfmt
