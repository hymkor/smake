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

Other examples:

- [examples/cc/Makefile.lsp](https://github.com/hymkor/smake/blob/master/examples/cc/Makefile.lsp) for C Project

Install
-------

Download the zipfile for your environment from [Releases](https://github.com/hymkor/smake/releases) and unzip.

If you have the scoop-installer,

```
scoop install https://raw.githubusercontent.com/hymkor/smake/master/smake.json
```

or

```
scoop bucket add hymkor https://github.com/hymkor/scoop-bucket
scoop install smake
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

### ($ "$(VARNAME)")

Expand the value of the variable written in the string-literal.

- "$(x)" to the value of the symbol x or the environment variable.
- "$&lt;" is same as "$($&lt;)"
- "$?" is same as "$($?)"
- "$@" is same as "$($@)"
- "$/" is same as "$($/)"

### (x "COMMAND" "ARG-1" "ARG-2" ...)

Execute the external executable directly. If it failes, top.

### (q "COMMAND" "ARG-1" "ARG-2" ...)

Execute the external executable directly and return its standard-output as string.

### (sh "SHELL-COMMAND")

Execute the shell command by CMD.exe or /bin/sh. If it fails, stop.

### (sh- "SHELL-COMMAND")

Same as (sh) but ignores errors.

### (shell "SHELL-COMMAND")

Execute the shell command and return its standard-output as string.
Same as $(shell "..") of GNU Make.

### (echo STRING...)

Same as the UNIX command echo.

### (rm FILENAME...)

Same as the UNIX command rm.

### (touch FILENAME...)

Same as the UNIX command touch.

### (foreach (KEY '(VALUE...)) COMMANDS...)

### (getenv "NAME")

Return the value of the environment variable NAME. If it does not exist, return nil.

### (setenv "NAME" "VALUE")

Set the environment variable "NAME" to "VALUE".

### (env (("NAME" "VALUE")...) COMMANDS...)

Set the environment variables and execute COMMANDS.
Then, restores them to thier original values.

### (wildcard "PATTERN"...)

Expand PATTERNs as a wildcard and return them as a list.

### (abspath "FILEPATH")

Same as $(abspath FILEPATH) of GNU Make

### (dir "FILEPATH")

Same as $(dir FILEPATH) of GNU Make

### (notdir "FILEPATH")

Same as $(notdir FILEPATH) of GNU Make

### (basename "FILEPATH")

Same as $(basename FILEPATH) of GNU Make

###  (pathjoin "DIR" .. "FNAME") , (joinpath "DIR"... "FNAME")

Make path with "DIR"... "FNAME".
(joinpath) is an alias of (pathjoin).
Same as filepath.Join of golang.

### (-e FILENAME)

If FILENAME exists, it returns t. Otherwise nil.
Same as -e of Perl.

### (-d DIRNAME)

If DIRNAME exists and it is a directory, it returns t. Otherwise nil.
Same as -d of Perl.

### (chdir "DIRNAME")

Change the current working directory to "DIRNAME"

### (getwd)

Returns the current working directory.

### (pushd "DIRNAME" COMMANDS)

Change the current directory to "DIRNAME" and execute COMMANDS like (progn).
After COMMANDS, return to the original current directory.

### (cp SRC... DST)

Copy file SRC... to DST (directory or new filename)

### (mv SRC... DST)

Move file SRC... to DST (directory or new filename)

### (string-split SEP SEQUENCE)

`(string-split #\: "a:b:c")` =&gt; `("a" "b" "c")`

### (shellexecute "ACTION" "PATH" \["PARAM"\] \["DIRECTORY"\])

Call Windows-API: shellexecute

### (string-fields "STRING")

Split "STRING" with whilte-spaces. This function is similar with [strings.Fields](https://pkg.go.dev/strings@go1.20.1#Fields) in golang

### (let),(format) and so on

They are compatible functions with ISLisp. See also [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

### windows

It is `t` (true) when %OS% is `Windows_NT`

## The built-in variables

- $@ - the target filename
- $&lt; - the first source filename
- $? - the updated source filenames
- $/ - OS-specific path separator (Windows \ , UNIX / )
- \*args\* - the command-line arguments
- $1...$9 - the same as (elt \*args\* N)
- $0 ... the current executing smake filename
