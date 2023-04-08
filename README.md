# Fennel Mode

Font-lock, indentation, navigation, documentation, and repl support for the
[Fennel](https://fennel-lang.org) programming language.

Supports `M-x imenu` for quick navigation to local definitions.

## Installation

Add this to your config:

```lisp
(autoload 'fennel-mode "/path/to/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
```

Note that `fennel-mode` doesn't inherit from `lisp-mode` since 0.4.0, and
instead switched to `prog-mode`. This means that there is no longer any way to
declare shared functionality (such as paredit) that you want applied to every
lisp you use; you have to add hooks specifically to `fennel-mode-hook`.

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
* `C-c C-p` - Print macro expansion of expression at point in the repl
* `M-.`     - Jump to the definition of a globally-visible function
* `M-'`     - Jump to the definition of a function in a module
* `M-,`     - Jump back to where you were before jumping to definition
* `M-TAB`   - Completion at point (Fennel 0.9.3+)

These functions assume a `fennel` executable is present on your
path. You can override the location by setting `inferior-lisp-program`
or invoking `C-u M-x fennel-repl`. For instance, if you have [a stdio
repl][2] in a [LÖVE][3] game, you can set this to `love .`.

Note that finding the definition of a function with `M-.` only works when the
function is in scope for the repl, which means it's usually best to
load a module and set it as a global if you want to use it this way.

## Antifennel

This repo also contains `antifennel.el` which allows you to compile
Lua code to Fennel straight from a `lua-mode` buffer. It requires
installing [antifennel][6] first. Install it with:

```lisp
(autoload 'antifennel-mode "/path/to/fennel-mode/antifennel.el" nil t)
(add-hook 'lua-mode-hook 'antifennel-mode)
```

Now when `antifennel-mode` is active in a Lua buffer, you can press
`C-c C-f` to open a new buffer containing the Fennel equivalent of the
Lua code.

## Contributing

Send patches to the [Fennel mailing list][4]. If you prefer not to
use email you can send a pull request on the [Codeberg mirror][5].
Please byte-compile to check for warnings.

## Copyright

Copyright © 2018-2022 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later); see LICENSE

[1]: https://git.sr.ht/~technomancy/fnlfmt
[2]: https://gitlab.com/alexjgriffith/min-love2d-fennel/blob/master/lib/stdio.fnl
[3]: https://love2d.org
[4]: https://lists.sr.ht/%7Etechnomancy/fennel
[5]: https://codeberg.org/technomancy/fennel-mode
[6]: https://git.sr.ht/~technomancy/antifennel
