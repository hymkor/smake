v0.8.0
======
Jul 20, 2025

- Changed the output destination of messages from `(sh)`, `(make)`, `(chdir)`, and `(touch)` from os.Stderr to the stream returned by `(error-output)`.
- Implemented `(lookpath)`, which returns the full path of the first executable found in the directories listed in the `PATH` environment variable. (Written in Go, so it's faster than the Lisp version `(which)`, but only returns the first match.)
- Added the `-p` option to the executable to print the result of the last evaluated expression.
- Update gmnlisp to [v0.7.19]

[v0.7.19]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.19

v0.7.0
======
Jul 7, 2025

- Support specifying multiple rule files with -f option (comma-separated). The first existing file is used. Defaults to "Makefile.lsp,smake.lsp".
- Added `(executable-not-found-p)`, `(exit-error-p)`, and `(exit-code)` to inspect conditions raised by `(spawn ...)`.
- Implement `(ansi-to-utf8)`
- Make `(spawn)` and `(sh)` redirect their stdout and stderr to `(standard-output)` and (error-output) respectively
- Reimplement `(q)`, `(shell)` and `(sh-ignore-error)` in Lisp instead of Go
- Refactor `(file-for-each)`, `(which)`, `(updatep)` and `(probe-directory)`
- Add `*discard*` as a global output stream that silently ignores all writes
- Update gmnlisp to [v0.7.17]

[v0.7.17]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.17

v0.6.0
======
Jun 24, 2025

- Update gmnlisp to [v0.7.16]
- Added `import _ "github.com/hymkor/gmnlisp/eval"` to enable optional `(eval)` and `(load)` functions in gmnlisp v0.7.15.
- `*executable-name*` is set to the path of executable (Compatible with [lispect](https://github.com/hymkor/lispect) )
- Added `(if-some)`, `(when-some)` and `(cond-let)` macros for optional value binding and conditional execution.
- Implemented (which), a function similar to the Unix which command, that looks for executables in the system PATH.
- `(spawn)` and `(sh)` now flush standard-output and error-output before execution.

[v0.7.15]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.15
[v0.7.16]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.16

v0.5.1
======
Jun 10, 2025

- Update gmnlisp to [v0.7.14]

[v0.7.14]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.14

v0.5.0
======
May 15, 2025

- Added `*windows*`, which evaluates to `T` when `(getenv "OS")` equals `"Windows_NT"`.
- Added `*dev-null*`, which expands to `"NUL"` on Windows and `"/dev/null"` otherwise.
- Added `*exe-suffix*`, which expands to `".exe"` on Windows and an empty string otherwise.
- Added `*path-separator*` and `*path-list-separator*` for OS-specific path handling.
- Added `spawn`, implemented in Go, and reimplemented `x`, `spawnlp`, and `spawnvp` in Lisp.
- Added `probe-directory`.
- Added `join-path` as a clearer replacement for the previously used `joinpath` and `pathjoin`.
- Added `sh-ignore-error`, which behaves like the deprecated `(sh-)`.
- Deprecated `joinpath`, `pathjoin`, and `(sh-)`, but kept them for backward compatibility.
- Updated gmnlisp to [v0.7.11]

[v0.7.11]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.11

v0.4.3
======
Jan 16, 2025

- Add `(match REGEXP STRING)`
- Upgrade gmnlisp to [v0.7.8]

[v0.7.8]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.8

v0.4.2
======
Nov 25, 2024

- Update gmnlisp to [v0.7.5]

[v0.7.5]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.5

v0.4.1
=======
Jun 27, 2024

- The SOURCES of `(updatep TARGET SOURCES...)` can include the sub list of source files now
- Add `(spawnlp)` and `(spawnvp)`

v0.4.0
======
Jun 27, 2024

- Implement (string-fields "S")
- (wildcard) accepts N-arguments (N>=0)
- Update gmnlisp to [v0.7.0] \(latest master)
    - `(apply #'make)` can not be availale now
- Implement `(updatep OBJ SRC*)` which returns the newer files in `SRC` than `OBJ`
- Implement `-version` option

[v0.7.0]: https://github.com/hymkor/gmnlisp/releases/tag/v0.7.0

v0.3.0
======
Jan 16, 2023

- Re-implement (env)[defmacro version] supporting multi environment variables
- Remove (doenv)
- (sh) and (sh-) accept multi-statements
- (env) and (pushd) now uses (unwind-protect)
- macros now use (gensym)

v0.2.0
======
Oct 23, 2022

- Update gmnlisp to [v0.1.3] to use (defmacro)
- Add functions: (foreach) , (chdir) ,and (getwd)
- Add variable: windows (true if %OS% == `Windows_NT`)
- Replace: (env)[golang version] to (doenv)[defmacro version]
- Rename: (split-sequence) to (string-split)

[v0.1.3]: https://github.com/hymkor/gmnlisp/releases/tag/v0.1.3

v0.1.0
=======
Oct 16, 2022

- The first release
