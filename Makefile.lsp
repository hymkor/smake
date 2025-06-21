;; This Makefile.lsp is for SMake — a Lisp-based build tool used for
;; maintenance tasks. Building the project doesn’t require this file —
;; just run `go build`. If you're curious about SMake, see:
;; https://github.com/hymkor/smake

(defglobal EXE     (shell "go env GOEXE"))
(defglobal CURDIR  (getwd))
(defglobal NAME    (notdir CURDIR))
(defglobal TARGET  (string-append NAME EXE))
(defglobal SOURCE  (wildcard "*.go"))
(defglobal VERSION
  (catch
    'notag
    (with-handler
      (lambda (c) (throw 'notag "v0.0.0"))
      (shell (string-append "git describe --tags 2>" *dev-null*)))))

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
     (spawn $0 "clean"))
   (dolist (fname (wildcard "*~"))
     (rm fname))
   (if (probe-file TARGET)
     (mv TARGET (string-append "." TARGET "~"))))

  (("upgrade") ; upgrade the installed program with the newly built version
   (if (probe-file TARGET)
     (let ((delimiter (elt *path-list-separator* 0)))
       (dolist (dir (string-split delimiter (getenv "PATH")))
         (if (and (not (equalp CURDIR dir))
                  (probe-file (join-path dir TARGET)))
           (progn
             (format (standard-output) "copy \"~A\" to \"~A\" ? [Y or N] " TARGET dir)
             (if (equalp (read-line (standard-input) nil nil) "y")
               (cp TARGET dir))))))))

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
   (assert-eq (if-some
                (c (+ 1 2)) ; test-form
                (+ c 3) ; then-form
                0) ; else-form
              6) ; expect
   (assert-eq (if-some
                (c nil) ; test-form
                (+ c 3) ; then-form
                "NONE") ;else-form
              "NONE") ; expect
   (assert-eq (when-some
                (c (string-append "1" "2")) ; test-form
                (string-append c "3")); then-form
              "123"); expect
   (assert-eq (when-some
                (c nil) ; test-form
                (string-append c "3")); then-form
              nil); expect
   (sh "go test"))

  (("dist")
   (dolist (goos '("linux" "windows"))
     (dolist (goarch '("386" "amd64"))
       (env (("GOOS" goos) ("GOARCH" goarch))
         (let* ((exe (shell "go env GOEXE"))
                (target (string-append NAME exe)))
           (rm target)
           (sh "go build")
           (spawn
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
          (spawn "go" "build" "-ldflags"
                   (string-append "-s -w -X main.version=" VERSION)))
        (progn
          (format (error-output) "No files updated~%")
          )
        ); if
      ); let
    ); t
  ); case

; vim:set lispwords+=env,mapc,pushd,while,doenv:
