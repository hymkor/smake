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
