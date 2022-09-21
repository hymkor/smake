SMake (Make by S-expression)
============================

```lisp
(let ((EXE (qs "go env GOEXE")))
  (make
    ((append '("smake$(EXE)") (glob "*.go"))
     (sh "go fmt")
     (sh "go build")
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

### (let),(format) and so on

They are compatible functions with ISLisp. See also [hymkor/gmnlisp](https://github.com/hymkor/gmnlisp)

## The built-in variables

- $@ - the target filename
- $&lt; - the first source filename
- $/ - OS-specific path separator (Windows \ , UNIX / )

## The macros in the STRING

(rule),(x),(touch),(1&gt;) and (1&gt;&gt;) expands these in their parameters.

- "$(x)" to the value of the symbol x
- "$&lt;" is same as "$($&lt;)"
- "$@" is same as "$($@)"
- "$/" is same as "$($/)"
