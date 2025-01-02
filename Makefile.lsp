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
   (dolist (fname SOURCE)
     (touch fname)))

  (("clean")
   (pushd "examples/cc"
     (spawnlp $0 "clean"))
   (dolist (fname (wildcard "*~"))
     (rm fname))
   (if (-e TARGET)
     (mv TARGET (string-append "." TARGET "~"))))

  (("install")
   (dolist (path (string-split #\newline (q "where" (notdir $0))))
     (if (not (equal path $0))
       (cp $0 path))))

  (("test")
   (assert-eq (match "^a*$" "aaaa") '("aaaa"))
   (assert-eq (match "^v([0-9]+)\.([0-9]+)\.([0-9]+)$" "v10.20.30")
              '("v10.20.30" "10" "20" "30"))
   (assert-eq (match "^a*$" "hogehoge") nil)
   (assert-eq (catch
                'fail
                (with-handler
                  (lambda (c) (throw 'fail 'NG))
                  (match "(" "hogehoge")))
              'NG)
   (sh "go test")
   )

  (("dist")
   (dolist (goos '("linux" "windows"))
     (dolist (goarch '("386" "amd64"))
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
     (dolist (zip (wildcard (string-append NAME "-" VERSION "-*.zip")))
       (format b " \"~A\"" zip))
     (sh (get-output-stream-string b))))

  (("clean-zip")
   (dolist (fname (wildcard "*.zip"))
     (rm fname)))

  (("manifest")
   (sh "make-scoop-manifest *.zip > smake.json"))

  (("readme")
   (sh "example-into-readme"))

  (t
    (let ((ufiles (updatep TARGET "Makefile.lsp" "embed.lsp" "go.mod" "go.sum" SOURCE)))
      (if ufiles
        (progn
          (format (error-output) "Found update files: ~S~%" ufiles)
          (sh "go fmt")
          (spawnlp "go" "build" "-ldflags"
                   (string-append "-s -w -X main.version=" VERSION)))
        (progn
          (format (error-output) "No files updated~%")
          )
        ); if
      ); let
    ); t
  ); case

; vim:set lispwords+=env,mapc,pushd,while,doenv:
