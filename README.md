# Fennel Mode

Font-lock, indentation, navigation, documentation, and REPL support for the
[Fennel](https://fennel-lang.org) programming language.

Supports `M-x imenu` for quick navigation to local definitions.

## Installation

Add this to your config:

```lisp
(autoload 'fennel-mode "/path/to/fennel-mode/fennel-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fnl\\'" . fennel-mode))
```

Note that `fennel-mode` doesn't inherit from `lisp-mode` since 0.4.0, and
instead switched to `prog-mode`.  This means that there is no longer any way to
declare shared functionality (such as paredit) that you want to be applied to every
lisp you use; you have to add hooks specifically to `fennel-mode-hook`.

## Interactivity

Run `M-x fennel-repl RET` to open a REPL buffer.  Once a REPL is open,
you can send code from a `fennel-mode` buffer to be evaluated.

### Bindings for `fennel-mode`

* `M-.`        - Jump to the definition of a globally-visible function
* `M-,`        - Jump back to where you were before jumping to definition
* `M-'`        - Jump to the definition of a function in a module
* `C-c C-k`    - Reload the module for the current file (requires `fennel.seacher`)
* `C-c C-l`    - Display compilation output for the current file
* `C-c C-z`    - Start or switch to REPL buffer
* `C-c C-t`    - Reformat current buffer with [fnlfmt][1] (separate install)
* `C-c C-d`    - Ask for a value and show its docstring in the REPL (also `C-c C-f`)
* `C-c C-v`    - Show docstring of variable at point
* `C-c C-p`    - Print macro expansion of expression at point in the REPL
* `C-x C-e`    - Evaluate last expression before the point
* `C-c C-e`    - Evaluate current top-level form (also `C-M-x`)
* `C-c C-n`    - Evaluate current top-level form, then move to the next
* `C-c C-r`    - Evaluate the region
* `C-c C-z`    - Start or switch to REPL buffer

### Bindings for `fennel-repl-mode`

* `TAB`        - Completion at point
* `C-c M-o`    - Clear the REPL output
* `C-c C-d`    - Ask for a value and show its docstring in the REPL (also `C-c C-f`)
* `C-c C-v`    - Show docstring of variable at point
* `M-.`        - Jump to the definition of a globally-visible function
* `C-c C-z`    - Toggle back to previous `fennel-mode` buffer
* `C-c C-q`    - Quit the REPL

These functions assume a `fennel` executable is present on your
path.  You can override the location by setting `inferior-lisp-program`
or invoking `C-u M-x fennel-repl`.  For instance, if you have [a stdio
REPL][2] in a [LÖVE][3] game, you can set this to `love .`.

Note that finding the definition of a function with `M-.` only works when the
function is in scope for the REPL, which means it's usually best to
load a module and set it as a global if you want to use it this way.

## Antifennel

This repo also contains `antifennel.el` which allows you to compile
Lua code to Fennel straight from a `lua-mode` buffer.  It requires
installing [antifennel][6] first.  Install it with:

```lisp
(autoload 'antifennel-mode "/path/to/fennel-mode/antifennel.el" nil t)
(add-hook 'lua-mode-hook 'antifennel-mode)
```

Now when `antifennel-mode` is active in a Lua buffer, you can press
`C-c C-f` to open a new buffer containing the Fennel equivalent of the
Lua code.

## Protocol-based REPL

A separate REPL integration is provided via the `fennel-proto-repl`
module.  This module provides a client that can connect to a regular
Fennel REPL, *upgrade* it with the [protocol][7] code, and provides a
more robust interactive experience.  Advantages over the default
`fennel-repl` are:

* IO and evaluation results are separated.
* Stack traces in Errors can be used to jump to the error location.
* Evaluation results are shown in the echo area - no need to keep the
  REPL window open.
* Running multiple REPLs is easier, and different buffers can be
  linked to different REPLs.
* Synchronous and Asynchronous API.
* Support for Eldoc and Xref.

Installation is similar to the other modules:

```lisp
(autoload 'fennel-proto-repl "/path/to/fennel-mode/fennel-proto-repl.el" nil t)
(add-hook 'fennel-mode-hook 'fennel-proto-repl-minor-mode)
```

The `fennel-proto-repl-minor-mode` re-binds all of the default REPL
interaction keys available in `fennel-mode` to use `fennel-proto-repl`
instead of a regular `fennel-repl`.  In addition to the usual
`fennel-mode` mappings listed above, additional commands are
available:

* `C-c C-S-l` - link the current buffer to a specific REPL session
* `C-c C-b`   - Evaluate the whole buffer
* `C-c C-a`   - Ask for a function and show its argument list in the REPL

Starting the REPL or switching to the existing one is done via the
same `C-c C-z` shortcut, and the new REPL session can be started at
any moment with the `fennel-proto-repl` command.  The buffer is
automatically linked to the newly created REPL.

Note that `fennel-proto-repl` requires a Fennel version recent enough
to include the `___repl___` variable.  If the Fennel doesn't have the
`___repl___` variable the Proto REPL won't be started.

## Org Babel support

Installation is similar to the other modules:

```lisp
(autoload 'ob-fennel "/path/to/fennel-mode/ob-fennel.el" nil t)
```

Then, the support for Fennel in Org buffers can be activated by adding
the following code to the Emacs init file:

```lisp
(with-eval-after-load 'org
  (require 'ob-fennel))
```

After that, the `#+begin_src fennel` code blocks can be executed in
Org buffers.  The integration requires the `fennel-proto-repl` module
to be available and operational.

The `ob-fennel` module supports evaluating code from the regular "src
blocks", and "inline src blocks" (`src_fennel[]{...}`), with the
ability to access variables and tables defined elsewhere in the file.
Multiple sessions can be used, and a different command to start the
REPL process can be passed via the `:fennel-cmd` header argument.

## Contributing

Send patches to the [Fennel mailing list][4]. If you prefer not to
use email you can send a pull request on the [Codeberg mirror][5].
Please byte-compile to check for warnings.

### Testing

This package has tests stored in the `tests` directory.  When
developing new functionality consider writing a test for it.  You can
use [cask][8] to run these tests like so:

```sh
$ cask install # needs to be done only once
$ cask exec ert-runner
```

It should automatically find all test files and execute all of the
tests.  Alternatively, tests can be ran via the
`ert-run-tests-batch-and-exit` combined with `find`:

```sh
$ find test -type f -exec emacs -batch -L . -l path/to/lua-mode.el -l {} -f ert-run-tests-batch-and-exit \;
```

## Copyright

Copyright © 2018-2023 Phil Hagelberg and contributors

Licensed under the same license as Emacs (GPL v3 or later); see LICENSE

[1]: https://git.sr.ht/~technomancy/fnlfmt
[2]: https://gitlab.com/alexjgriffith/min-love2d-fennel/blob/master/lib/stdio.fnl
[3]: https://love2d.org
[4]: https://lists.sr.ht/%7Etechnomancy/fennel
[5]: https://codeberg.org/technomancy/fennel-mode
[6]: https://git.sr.ht/~technomancy/antifennel
[7]: https://gitlab.com/andreyorst/fennel-proto-repl-protocol
[8]: https://github.com/cask/cask
