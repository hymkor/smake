(let*
  ((EXE (if windows ".exe" ""))
   (NAME (notdir (getwd)))
   (AOUT (string-append NAME EXE))
   (SOURCE (wildcard "*.go"))
   )
  (make $1
    ((append (list AOUT "Makefile.lsp" "embed.lsp" "go.mod" "go.sum") SOURCE)
     (sh "go fmt"
         "go build")
     )
    ('("get")
     (sh "go get -u"
         "go get -u github.com/hymkor/gmnlisp@master"
         "go mod tidy")
     )
    ('("update")
     (apply #'touch SOURCE)
     )
    ('("readme" "README.md" "Makefile.lsp")
     )
    ('("README.md" "_README.md" "Makefile.lsp")
     (sh (format nil "gmnlpp ~s > ~s" $< $@))
     )
    ('("clean")
     (pushd "examples/cc"
       (x $0 "clean")
       )
     (apply #'rm (wildcard "*~"))
     (if (-e AOUT)
       (mv AOUT (format nil ".~a~~" AOUT))
       )
     )
    ('("install")
     (foreach (path (string-split #\newline (q "where" (notdir $0))))
       (if (not (equal path $0))
         (cp $0 path)))
     )
    ('("test")
     (x "go" "test")
     )
    ('("package")
     (let ((version (shell "git describe --tag")))
       (foreach (goos '("linux" "windows"))
         (foreach (goarch '("386" "amd64"))
           (env (("GOOS" goos) ("GOARCH" goarch))
             (let* ((exe (shell "go env GOEXE"))
                    (aout (string-append NAME exe)))
               (rm aout)
               (x "go" "build")
               (x "zip"
                  (string-append NAME "-" version "-" goos "-" goarch ".zip")
                  aout)
               )
             )
           )
         )
       )
     ) ; "package"
    ('("clean-zip")
     (apply #'rm (wildcard "*.zip"))
     )
    );make
  );let
; vim:set lispwords+=foreach,env,mapc,make,pushd,while,doenv:
