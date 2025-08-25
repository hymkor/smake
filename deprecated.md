SMake - Deprecated functions
============================

### (make MAINTARGET ('(TARGET [SOURCES...]) COMMANDS...)...) *[deprecated]*

If the file TARGET is newer than SOURCE or TARGET does not exist, execute COMMANDS.

The entry after MAINTARGET is evaluated when the TARGET equals the MAINTARGET
or the TARGET is written on other evaluated SOURCES.

Use `(case)` and `(updatep)` instead.

### (x "COMMAND" "ARG-1" "ARG-2" ...) *[deprecated]*

Use `(spawn)` instead

### (spawnlp "COMMAND" "ARG-1" "ARG-2" ...) *[deprecated]*

Use `(spawn)` instead

### (spawnvp "COMMAND" '("ARG-1" "ARG-2" ...)) *[deprecated]*

Use `(apply #'spawn "COMMAND" ARGV-LIST)` instead of `(spanvp "COMMAND" ARGV-LIST)`

### (foreach (KEY '(VALUE...)) COMMANDS...) *[deprecated]*

Use `(dolist (KEY VALUES) COMMANDS...)` instead

### \*args\* *[deprecated]*

Use `*argv*` insteead

### (-e FILENAME)

Use `(probe-file FILENAME)`

### (-d DIRNAME)

Use `(probe-directory DIRNAME)`

### (sh- "SHELL-COMMAND")

Use `(sh-ignore-error "SHELL-COMMAND")`

### $/

Use `*path-separator*`

### Local variables in available in (make) block

- $@ - the target filename
- $&lt; - the first source filename
- $? - the updated source filenames

### (pathjoin "DIR" .. "FNAME") , (joinpath "DIR"... "FNAME")

Use `(join-path)`

