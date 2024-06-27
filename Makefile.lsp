(defglobal EXE     (shell "go env GOEXE"))
(defglobal NAME    (notdir (getwd)))
(defglobal TARGET  (string-append NAME EXE))
(defglobal SOURCE  (wildcard "*.go"))
(defglobal NUL     (if windows "NUL" "/dev/null"))
(defglobal VERSION
  (catch
    'notag
    (with-handler
      (lambda (c) (throw 'notag "v0.0.0"))
      (shell (string-append "git describe --tags 2>" NUL)))))

(case $1
  (("get")
   (sh "go get -u"
       "go get -u github.com/hymkor/gmnlisp@master"
       "go mod tidy"))

  (("touch")
   (foreach (fname SOURCE)
     (touch fname)))

  (("clean")
   (pushd "examples/cc"
     (spawnlp $0 "clean"))
   (foreach (fname (wildcard "*~"))
     (rm fname))
   (if (-e TARGET)
     (mv TARGET (string-append "." TARGET "~"))))

  (("install")
   (foreach (path (string-split #\newline (q "where" (notdir $0))))
     (if (not (equal path $0))
       (cp $0 path))))

  (("test")
   (sh "go test"))

  (("dist")
   (foreach (goos '("linux" "windows"))
     (foreach (goarch '("386" "amd64"))
       (env (("GOOS" goos) ("GOARCH" goarch))
         (let* ((exe (shell "go env GOEXE"))
                (target (string-append NAME exe)))
           (rm target)
           (sh "go build")
           (spawnlp
             "zip"
             (string-append NAME "-" VERSION "-" goos "-" goarch ".zip")
             target))))))

  (("release")
   (let ((b (create-string-output-stream)))
     (format b "gh release create -d --notes \"\" -t")
     (format b " \"~A\"" VERSION)
     (format b " \"~A\"" VERSION)
     (foreach (zip (wildcard (string-append NAME "-" VERSION "-*.zip")))
       (format b " \"~A\"" zip))
     (sh (get-output-stream-string b))))

  (("clean-zip")
   (foreach (fname (wildcard "*.zip"))
     (rm fname)))

  (("manifest")
   (sh "make-scoop-manifest *.zip > smake.json"))

  (("readme")
   (sh "example-into-readme"))

  (t
    (if (updatep TARGET "Makefile.lsp" "embed.lsp" "go.mod" "go.sum" SOURCE)
      (progn
        (sh "go fmt")
        (spawnlp "go" "build" "-ldflags"
                 (string-append "-s -w -X main.version=" VERSION)))
      )
    ) ; end-t
  ) ; end-case

; vim:set lispwords+=foreach,env,mapc,make,pushd,while,doenv:
