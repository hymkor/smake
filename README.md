SMake (Make by S-expression)
============================

SMake is the build tool like make(UNIX) whose Makefile is written with S-expression.

Makefile.lsp:

```lisp
(let
  ((EXE (if (equal (getenv "OS") "Windows_NT") ".exe" "")))
  (make $1
    ((cons "smake$(EXE)" (wildcard "*.go"))
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
       (sh "gmnlpp$(EXE) $< > \"$@\"")
     )
    ('("clean")
     (rm "smake$(EXE)~")
     (rm "smake$(EXE)")
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

### (echo STRING...)

Same as the UNIX command echo.

### (rm FILENAME...)

Same as the UNIX command rm.

### (touch FILENAME...)

Same as the UNIX command touch.

### (getenv "NAME")

Return the value of the environment variable NAME. If it does not exist, return nil.

### (wildcard "PATTERN")

Expand the PATTERN as a wildcard and return them as a list.

### (abspath "FILEPATH")

Same as $(abspath FILEPATH) of GNU Make

### (notdir "FILEPATH")

Same as $(notdir FILEPATH) of GNU Make

### (joinpath "DIR"... "FNAME")

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
