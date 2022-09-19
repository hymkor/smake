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
    ((rule "clean")
     (rm "smake$(EXE)~")
     )
    ((rule "test")
     (format (standard-output) "~s~%" $)
     (echo (let ((hoge (assoc "hoge" $)))
             (if hoge (cdr hoge) "(not found)")))
     (echo "dollar=$(hoge)")
     )
    )
  )
```
