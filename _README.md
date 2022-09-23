SMake (Make by S-expression)
============================

SMake is the build tool like make(UNIX) whose Makefile is written with S-expression.

Makefile.lsp:

```lisp
<%
(let ((line nil))
    (with-open-input-file (fd "Makefile.lsp")
        (while (setq line (read-line fd nil nil))
            (format (standard-output) "~a~%" line)
        )
    )
)
%>
```

## How to build SMake

```
go build
```

## The functions available in Makefile.lsp

### (make MAINTARGET ('(TARGET [SOURCES...]) COMMANDS...)...)

If the file TARGET is newer than SOURCE or TARGET does not exist, execute COMMANDS.

The entry after MAINTARGET is evaluated when the TARGET equals the MAINTARGET
or the TARGET is written on other evaluated SOURCES.

### (sh "SHELL-COMMAND")

Execute the shell command. If it fails, stop.

### (qs "SHELL-COMMAND")

Execute the shell command and return its standard-output.

### (echo STRING...)

Same as the UNIX command echo.

### (rm FILENAME...)

Same as the UNIX command rm.

### (touch FILENAME...)

Same as the UNIX command touch.

### (getenv "NAME")

Return the value of the environment variable NAME. If it does not exist, return nil.

### (glob "PATTERN")

Expand the PATTERN as a wildcard and return them as a list.

### (let),(format) and so on

They are compatible functions with ISLisp. See also [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

## The built-in variables

- $@ - the target filename
- $&lt; - the first source filename
- $? - the updated source filenames
- $/ - OS-specific path separator (Windows \ , UNIX / )
- \*args\* - the command-line arguments
- $1...$9 - the same as (elt \*args\* N)

## The macros in the STRING

(rule),(x),(touch),(1&gt;) and (1&gt;&gt;) expands these in their parameters.

- "$(x)" to the value of the symbol x or the environment variable.
- "$&lt;" is same as "$($&lt;)"
- "$?" is same as "$($?)"
- "$@" is same as "$($@)"
- "$/" is same as "$($/)"
