(defglobal EXE    (if windows ".exe" ""))
(defglobal NAME   (notdir (getwd)))
(defglobal TARGET (string-append NAME EXE))
(defglobal SOURCE (wildcard "*.go"))
(defglobal ALL    (append (list TARGET "Makefile.lsp" "embed.lsp" "go.mod" "go.sum") SOURCE))

(make $1
  (ALL
   (sh "go fmt"
       "go build"))

  ('("get")
   (sh "go get -u"
       "go get -u github.com/hymkor/gmnlisp@master"
       "go mod tidy"))

  ('("touch")
   (foreach (fname SOURCE)
     (touch fname)))

  ('("readme" "README.md" "Makefile.lsp"))

  ('("README.md" "_README.md" "Makefile.lsp")
   (sh (format nil "gmnlpp ~s > ~s" $< $@)))

  ('("clean")
   (pushd "examples/cc"
     (x $0 "clean"))
   (foreach (fname (wildcard "*~"))
     (rm fname))
   (if (-e TARGET)
     (mv TARGET (string-append "." TARGET "~"))))

  ('("install")
   (foreach (path (string-split #\newline (q "where" (notdir $0))))
     (if (not (equal path $0))
       (cp $0 path))))

  ('("test")
   (x "go" "test"))

  ('("package")
   (let ((version (shell "git describe --tag")))
     (foreach (goos '("linux" "windows"))
       (foreach (goarch '("386" "amd64"))
         (env (("GOOS" goos) ("GOARCH" goarch))
           (let* ((exe (shell "go env GOEXE"))
                  (target (string-append NAME exe)))
             (rm target)
             (x "go" "build")
             (x "zip"
                (string-append NAME "-" version "-" goos "-" goarch ".zip")
                target)))))))

  ('("clean-zip")
   (foreach (fname (wildcard "*.zip"))
     (rm fname)))

  ('("manifest")
   (sh "make-scoop-manifest *.zip > smake.json")))

; vim:set lispwords+=foreach,env,mapc,make,pushd,while,doenv:
