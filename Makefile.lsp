(defglobal EXE     (if windows ".exe" ""))
(defglobal NAME    (notdir (getwd)))
(defglobal TARGET  (string-append NAME EXE))
(defglobal SOURCE  (wildcard "*.go"))
(defglobal ALL     (append (list TARGET "Makefile.lsp" "embed.lsp" "go.mod" "go.sum") SOURCE))
(defglobal NUL     (if windows "NUL" "/dev/null"))
(defglobal VERSION
  (catch
    'notag
    (with-handler
      (lambda (c) (throw 'notag "v0.0.0"))
      (shell (string-append "git describe --tags 2>" NUL)))))

(make $1
  (ALL
    (sh "go fmt"
        (format nil "go build -ldflags \"-s -w -X main.version=~A\"" VERSION)))

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

  ('("release")
   (let ((cmdline "gh release create -d --notes \"\" -t"))
     (setq cmdline (string-append cmdline " \"" VERSION "\""))
     (setq cmdline (string-append cmdline " \"" VERSION "\""))
     (foreach (zip (wildcard (string-append NAME "-" VERSION "-*.zip")))
       (setq cmdline (string-append cmdline " \"" zip "\"")))
     (sh cmdline)))

  ('("clean-zip")
   (foreach (fname (wildcard "*.zip"))
     (rm fname)))

  ('("manifest")
   (sh "make-scoop-manifest *.zip > smake.json")))

; vim:set lispwords+=foreach,env,mapc,make,pushd,while,doenv:
