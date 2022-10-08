SMake (Make by S-expression)
============================

SMake is the build tool like make(UNIX) whose Makefile is written with S-expression.

Makefile.lsp:

```lisp
(let
  ((EXE (if (equal (getenv "OS") "Windows_NT") ".exe" "")))
  (make $1
    ((cons ($ "smake$(EXE)") (wildcard "*.go"))
     (sh "go fmt")
     (sh "go build")
     )
    ('("get")
     (sh "go get -u")
     (sh "go mod tidy")
     )
    ('("update")
     (apply #'touch (wildcard "*.go"))
     )
    ('("readme" "README.md" "Makefile.lsp")
     )
    ('("README.md" "_README.md" "Makefile.lsp")
       (sh ($ "gmnlpp$(EXE) $< > \"$@\""))
     )
    ('("clean")
     (rm ($ "smake$(EXE)~"))
     (rm ($ "smake$(EXE)"))
     (pushd
       "examples\cc"
       (x $0 "clean")
       )
     )
    ('("install")
     (mapc
       (lambda (path) (or (equal path $0) (cp $0 path)))
       (split-sequence #\newline (q "where" (notdir $0)))
       )
     )
    )
  )
```

Other examples:

- [examples/cc/Makefile.lsp](https://github.com/hymkor/smake/blob/master/examples/cc/Makefile.lsp) for C Project

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

### (getenv "NAME")

Return the value of the environment variable NAME. If it does not exist, return nil.

### (setenv "NAME" "VALUE")

Set the environment variable "NAME" to "VALUE".

### (wildcard "PATTERN")

Expand the PATTERN as a wildcard and return them as a list.

### (abspath "FILEPATH")

Same as $(abspath FILEPATH) of GNU Make

### (dir "FILEPATH")

Same as $(dir FILEPATH) of GNU Make

### (notdir "FILEPATH")

Same as $(notdir FILEPATH) of GNU Make

### (basename "FILEPATH")

Same as $(basename FILEPATH) of GNU Make

### (joinpath "DIR"... "FNAME")

Make path with "DIR"... "FNAME".
Same as filepath.Join of golang.

### (-e FILENAME)

If FILENAME exists, it returns t. Otherwise nil.
Same as -e of Perl.

### (-d DIRNAME)

If DIRNAME exists and it is a directory, it returns t. Otherwise nil.
Same as -d of Perl.

### (pushd "DIRNAME" COMMANDS)

Change the current directory to "DIRNAME" and execute COMMANDS like (progn).
After COMMANDS, return to the original current directory.

### (cp SRC... DST)

Copy file SRC... to DST (directory or new filename)

### (mv SRC... DST)

Move file SRC... to DST (directory or new filename)

### (split-sequence SEP SEQUENCE)

`(split-sequence #\: "a:b:c")` =&gt; `("a" "b" "c")`

### (shellexecute "ACTION" "PATH" \["PARAM"\] \["DIRECTORY"\])

Call Windows-API: shellexecute

### (let),(format) and so on

They are compatible functions with ISLisp. See also [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

## The built-in variables

- $@ - the target filename
- $&lt; - the first source filename
- $? - the updated source filenames
- $/ - OS-specific path separator (Windows \ , UNIX / )
- \*args\* - the command-line arguments
- $1...$9 - the same as (elt \*args\* N)
- $0 ... the current executing smake filename
