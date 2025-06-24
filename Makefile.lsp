;; This Makefile.lsp is for SMake — a Lisp-based build tool used for
;; maintenance tasks. Building the project doesn’t require this file —
;; just run `go build`. If you're curious about SMake, see:
;; https://github.com/hymkor/smake

(if-some (golist (which "go1.20.14"))
  (defglobal GOEXE (car golist))
  (defglobal GOEXE "go"))

(defglobal EXE     (shell (string-append GOEXE " env GOEXE")))
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
   (spawn GOEXE "get" "-u")
   (spawn GOEXE "mod" "tidy"))

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
             (finish-output (standard-output))
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
   (dolist (pattern '(((1 nil) first) ((nil 2) second) ((nil nil) otherwise)))
     (let ((target (car pattern))
           (expect (cadr pattern)))
       (assert-eq
         (cond-let
           ((foo (elt target 0)) 'first)
           ((bar (elt target 1)) 'second)
           (t 'otherwise))
         expect)))
   (spawn GOEXE "test"))

  (("dist")
   (dolist (goos '("linux" "windows"))
     (dolist (goarch '("386" "amd64"))
       (env (("GOOS" goos) ("GOARCH" goarch))
         (let* ((exe (q GOEXE "env" "GOEXE"))
                (target (string-append NAME exe)))
           (rm target)
           (spawn GOEXE "build")
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
    (if-some
      (ufiles (updatep TARGET "Makefile.lsp" "embed.lsp" "go.mod" "go.sum" SOURCE))
      (progn
        (format (error-output) "Found update files: ~S~%" ufiles)
        (spawn GOEXE "fmt" "./...")
        (spawn GOEXE "build" "-ldflags"
               (string-append "-s -w -X main.version=" VERSION)))
      (format (error-output) "No files updated~%")
      ); if-some
    ); t
  ); case

; vim:set lispwords+=env,mapc,pushd,while,doenv:
