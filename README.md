SMake (Make by S-expression)
============================

```lisp
(let ((EXE (q "go" "env" "GOEXE")))
  (format (standard-output) "EXE=~a~%" EXE)
  (make
    ('("smake$(EXE)" "main.go")
     (x "go" "fmt")
     (x "go" "build")
     )
    ('("update")
     (touch "main.go")
     )
    ('("readme" "README.md")
     )
    ('("README.md" "_README.md" "Makefile.lsp")
     (1> $@
       (x "gmnlpp$(EXE)" $<)
       )
     )
    ('("clean")
     (rm "smake$(EXE)~")
     )
    ('("test")
     (format (standard-output) "~s~%" $)
     (echo (let ((hoge (assoc "hoge" $)))
             (if hoge (cdr hoge) "(not found)")))
     (echo "dollar=$(hoge)")
     (echo "os.PathSeparator='$/'" $/)
     )
    )
  )
```

## The functions available in Makefile.lsp

### (make ('(TARGET [SOURCE...]) COMMANDS...)...)

If the file TARGET is newer than SOURCE or TARGET does not exist, execute COMMANDS.

### (x COMMANDNAME ARGS...)

Execute the external COMMAND. If it fails, stop.

### (echo STRING...)

Same as the UNIX command echo.

### (rm FILENAME...)

Same as the UNIX command rm.

### (touch FILENAME...)

Same as the UNIX command touch.

### (1&gt; FILENAME COMMANDS...)

Open FILENAME for writing and redirect Standard-output to it while COMMANDS are executed.

### (1&gt;&gt; FILENAME COMMANDS...)

Open FILENAME for appending and redirect Standard-output to it while COMMANDS are executed.

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
