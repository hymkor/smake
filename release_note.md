- Re-implement (env)[defmacro version] supporting multi environment variables
- Remove (doenv)
- (sh) and (sh-) accept multi-statements

v0.2.0
======

- Update gmnlisp to v0.1.3 to use (defmacro)
- Add functions: (foreach) , (chdir) ,and (getwd)
- Add variable: windows (true if %OS% == `Windows_NT`)
- Replace: (env)[golang version] to (doenv)[defmacro version]
- Rename: (split-sequence) to (string-split)
