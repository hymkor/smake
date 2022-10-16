(let*
  ((EXE (if (equal (getenv "OS") "Windows_NT") ".exe" ""))
   (NAME (notdir (abspath ".")))
   (AOUT (string-append NAME EXE))
   (SOURCE (wildcard "*.go"))
   )
  (make $1
    ((cons AOUT SOURCE)
     (sh "go fmt")
     (sh "go build")
     )
    ('("get")
     (sh "go get -u")
     (sh "go mod tidy")
     )
    ('("update")
     (apply #'touch SOURCE)
     )
    ('("readme" "README.md" "Makefile.lsp")
     )
    ('("README.md" "_README.md" "Makefile.lsp")
       (sh ($ "gmnlpp$(EXE) $< > \"$@\""))
     )
    ('("clean")
     (pushd
       "examples/cc"
       (x $0 "clean")
       )
      (apply #'rm (wildcard "*~"))
      (if (-e AOUT)
        (mv AOUT (format nil ".~a~~" AOUT))
        )
     )
    ('("install")
     (mapc
       (lambda (path) (or (equal path $0) (cp $0 path)))
       (split-sequence #\newline (q "where" (notdir $0)))
       )
     )
    ('("test")
     (x "go" "test")
     )
    ('("package")
     (let ((version (shell "git describe --tag")))
       (mapc
         (lambda (goos)
           (env (("GOOS" goos))
             (mapc
               (lambda (goarch)
                 (env (("GOARCH" goarch))
                   (let* ((exe (shell "go env GOEXE"))
                          (aout (string-append NAME exe)))
                     (rm aout)
                     (x "go" "build")
                     (x "zip"
                        (string-append NAME "-" version "-" goos "-" goarch ".zip")
                        aout)
                     )
                   ) ; env GOARCH
                 ) ; lambda goarch
               '("386" "amd64")
               ) ; mapc
             ) ; env GOOS
           ) ; goos
         '("linux" "windows")
         ) ; mapc
       ) ; let
     ) ; "package"
    ('("clean-zip")
     (apply #'rm (wildcard "*.zip"))
     )
    );make
  );let
