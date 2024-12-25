SMake - Deprecated functions
============================

### (make MAINTARGET ('(TARGET [SOURCES...]) COMMANDS...)...) *[deprecated]*

If the file TARGET is newer than SOURCE or TARGET does not exist, execute COMMANDS.

The entry after MAINTARGET is evaluated when the TARGET equals the MAINTARGET
or the TARGET is written on other evaluated SOURCES.

Use `(case)` and `(updatep)` instead.

### ($ "$(VARNAME)") *[deprecated]*

Use `(format nil "...")` instead

Expand the value of the variable written in the string-literal.

- "$(x)" to the value of the symbol x or the environment variable.
- "$/" is same as "$($/)"

These are available in `(make)` block

- "$&lt;" is same as "$($&lt;)"
- "$?" is same as "$($?)"
- "$@" is same as "$($@)"

### (x "COMMAND" "ARG-1" "ARG-2" ...) *[deprecated]*

Use `(spawnlp)` instead

### (foreach (KEY '(VALUE...)) COMMANDS...) *[deprecated]*

Use `(dolist (KEY VALUES) COMMANDS...) instead
