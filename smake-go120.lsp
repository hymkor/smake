;;; The smake-template for building with Go 1.20.14
;;; Create an instance:
;;;     (defglobal make (load "smake-go120.lsp"))
;;; Call a method:
;;;     (funcall make 'build) or (funcall make "build")
(let* ((STARTTM (get-internal-run-time))
       (GO120   (lookpath "go1.20.14"))
       (GOEXE   (or GO120 "go"))
       (EXE     (shell (string-append GOEXE " env GOEXE")))
       (CURDIR  (getwd))
       (NAME    (notdir CURDIR))
       (TARGET  (string-append NAME EXE))
       (SOURCE  (wildcard "*.go"))
       (VERSION
         (catch
           'notag
           (with-handler
             (lambda (c) (throw 'notag "v0.0.0"))
             (with-error-output *discard*
               (q "git" "describe" "--tags"))))))
  (labels
    ((dbg!
       (x)
       (format (error-output) "[~S]~%" x)
       x)

     (find-str-file
       (word filename)
       (file-for-each filename (lambda (line) (string-index word line))))

     (is-main-package
       (dir)
       (block
         func
         (mapc
           (lambda (source)
             (file-for-each
               source
               (lambda (line)
                 (cond
                   ((string-index "go:build" line)
                    t)
                   ((string-index "package " line)
                    (if (string-index "_test" line)
                      t
                      (return-from func (string-index "main" line))))
                   (t
                     nil)))))
           (mapcan
             (lambda (fn) (if (string-index "_test.go" fn) nil (list fn)))
             (wildcard (join-path dir "*.go"))))))

     (go-build
       () 
       (spawn GOEXE "fmt")
       (if (is-main-package ".")
         (spawn GOEXE "build" "-ldflags"
                (string-append "-s -w -X main.version=" VERSION))
         (spawn GOEXE "build"))
       (string-append (notdir (getwd)) (q GOEXE "env" "GOEXE"))
       ) ; go-build

     (go-build-cmd
       (cmd-dir)
       (let*
         ((suffix (q GOEXE "env" "GOEXE"))
          (exe-name (join-path CURDIR (string-append (notdir cmd-dir) suffix))))
         (spawn GOEXE "fmt" "-C" cmd-dir)
         (spawn GOEXE "build" "-C" cmd-dir "-o"
                exe-name
                "-ldflags"
                (string-append "-s -w -X main.version=" VERSION))
         exe-name
         )
       ) ; go-build-cmd

     (make-dist
       (:rest exe-list)

       (let ((exe (q GOEXE "env" "GOEXE")))
         (apply
           #'spawn
           "zip" "-j"
           (string-append NAME "-" VERSION "-" 
                          (q GOEXE "env" "GOOS")
                          "-"
                          (q GOEXE "env" "GOARCH")
                          ".zip")
           exe-list)
         ) ; let
       ) ; make-dist
     ) ; labels-func

    (lambda (sub-command :rest args)
      (if (stringp sub-command)
        (setq sub-command (convert sub-command <symbol>)))
      (block sub-command
        (case sub-command
          ((touch)
           (mapc (lambda (fname) (touch fname))
                 SOURCE)
           )

          ((clean)
           (mapc (lambda (fname) (rm fname))
                 (wildcard "*~"))
           (finish-output (error-output))
           (if (probe-file TARGET)
             (mv TARGET (string-append "." TARGET "~")))
           (finish-output (error-output)))

          ((get)
           (spawn GOEXE "get" "-u")
           (if (consp GO120)
             (mapc
               (lambda (i)
                 (if (find-str-file (car i) "go.mod")
                   (spawn GOEXE "get" (string-append (car i) (cdr i)))))
               (list (cons "golang.org/x/sys" "@v0.30.0")
                     (cons "golang.org/x/text" "@v0.22.0")
                     (cons "golang.org/x/term" "@v0.29.0")))
             )
           (spawn GOEXE "mod" "tidy"))

          ((dist)
           (apply #'make-dist args))

          ((upgrade) ; upgrade the installed program with the newly built version
           (if (probe-file TARGET)
             (let ((delimiter (elt *path-list-separator* 0)))
               (mapc
                 (lambda (dir)
                   (if (and (not (equalp CURDIR dir))
                            (probe-file (join-path dir TARGET)))
                     (progn
                       (format (standard-output) "copy \"~A\" to \"~A\" ? [Y or N] " TARGET dir)
                       (finish-output (standard-output))
                       (if (equalp (read-line (standard-input) nil nil) "y")
                         (cp TARGET dir)))))
                 (string-split delimiter (getenv "PATH"))))))

          ((release)
           (apply #'spawn "gh" "release" "create" "-d" "--notes" "" "-t"
                  VERSION VERSION
                  (wildcard (string-append NAME "-" VERSION "-*.zip"))))

          ((manifest)
           (sh (string-append "make-scoop-manifest *.zip > " NAME ".json")))

          ((readme)
           (sh "example-into-readme"))

          ((test)
           (spawn GOEXE "test"))

          ((fmt)
           (spawn GOEXE "fmt" "./..."))

          ((build)
           (go-build))

          ((build-cmd)
           (apply #'go-build-cmd args))

          ((env)
           (mapc
             (lambda (c) (format (standard-output) "~S=~S~%" c (eval c)))
             '(GO120 GOEXE EXE CURDIR NAME TARGET SOURCE VERSION))
           )
          (t
            (error "~S: not command" sub-command))
          ); case
        ) ; block sub-command
      (format (standard-output)
              "~%Elapsed time: ~A seconds~%"
              (quotient (- (get-internal-run-time) STARTTM)
                        (internal-time-units-per-second)))
      ) ; lambda (method)
    ) ; label
  ) ; let*
