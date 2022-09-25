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
