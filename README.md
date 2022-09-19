SMake (Make by S-expression)
============================

```
(let ((EXE (q "go" "env" "GOEXE")))
  (format (standard-output) "EXE=~a~%" EXE)
  (make
    ((update "smake$(EXE)" "main.go")
     (x "go" "fmt")
     (x "go" "build")
     )
    ((update "update")
     (x "touch" "main.go")
     )
    ((update "README.md" "_README.md" "Makefile.lsp")
     (1> $@
       (x "gmnlpp" $<)
       )
     )
    )
  )
```
