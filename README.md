SMake (Make by S-expression)
============================

```lisp
(let ((EXE (q "go" "env" "GOEXE")))
  (format (standard-output) "EXE=~a~%" EXE)
  (make
    ((update "smake$(EXE)" "main.go")
     (x "go" "fmt")
     (x "go" "build")
     )
    ((update "update")
     (touch "main.go")
     )
    ((update "readme" "README.md")
     )
    ((update "README.md" "_README.md" "Makefile.lsp")
     (1> $@
       (x "gmnlpp" $<)
       )
     )
    )
  )
```
