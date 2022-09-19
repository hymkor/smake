SMake (Make by S-expression)
============================

```lisp
(let ((EXE (q "go" "env" "GOEXE")))
  (format (standard-output) "EXE=~a~%" EXE)
  (make
    ((rule "smake$(EXE)" "main.go")
     (x "go" "fmt")
     (x "go" "build")
     )
    ((rule "update")
     (touch "main.go")
     )
    ((rule "readme" "README.md")
     )
    ((rule "README.md" "_README.md" "Makefile.lsp")
     (1> $@
       (x "gmnlpp$(EXE)" $<)
       )
     )
    )
  )
```
