SMake (Make by S-expression)
============================

SMake is the build tool like make(UNIX) whose Makefile is written with S-expression.

Makefile.lsp:

```lisp
(let*
  ((windows (equal (getenv "OS") "Windows_NT"))
   (EXE (if windows ".exe" "")))
  (make
    ((cons "smake$(EXE)" (glob "*.go"))
     (sh "go fmt")
     (sh "go build")
     )
    ('("get")
     (sh "go get -u")
     (sh "go mod tidy")
     )
    ('("update")
     (touch "main.go")
     )
    ('("readme" "README.md")
     )
    ('("README.md" "_README.md" "Makefile.lsp")
       (sh "gmnlpp$(EXE) $< > \"$@\"")
     )
    ('("clean")
     (rm "smake$(EXE)~")
     )
  )
)
```

## How to build SMake

```
go build
```

## The functions available in Makefile.lsp

### (make ('(TARGET [SOURCE...]) COMMANDS...)...)

If the file TARGET is newer than SOURCE or TARGET does not exist, execute COMMANDS.

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

## The macros in the STRING

(rule),(x),(touch),(1&gt;) and (1&gt;&gt;) expands these in their parameters.

- "$(x)" to the value of the symbol x or the environment variable.
- "$&lt;" is same as "$($&lt;)"
- "$?" is same as "$($?)"
- "$@" is same as "$($@)"
- "$/" is same as "$($/)"
