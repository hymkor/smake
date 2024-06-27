- Implement (string-fields "S")
- (wildcard) accepts N-arguments (N>=0)
- Update gmnlisp to v0.6.0+ (latest master)
    - `(apply #'make)` can not be availale now
- Implement `(updatep OBJ SRC*)` which returns the newer files in `SRC` than `OBJ`

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

- Update gmnlisp to v0.1.3 to use (defmacro)
- Add functions: (foreach) , (chdir) ,and (getwd)
- Add variable: windows (true if %OS% == `Windows_NT`)
- Replace: (env)[golang version] to (doenv)[defmacro version]
- Rename: (split-sequence) to (string-split)

v0.1.0
=======
Oct 16, 2022

- The first release
