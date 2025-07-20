(defglobal make (load "smake-go120.lsp"))
(defglobal start-time (get-internal-run-time))

(case $1
  (("dist")
   (mapc
     (lambda (platform)
       (env
         (("GOOS"   (car platform))
          ("GOARCH" (car (cdr platform))))
         (let ((exe-list (funcall make 'build)))
           (funcall make 'dist exe-list))))
     '(("linux"   "386")
       ("linux"   "amd64")
       ("windows" "386")
       ("windows" "amd64"))))

  (("test")
   (load "smake-test.lsp"))

  (("build" "" nil)
   (funcall make 'build))

  (("bump")
   (let ((bump (load "smake-bump.lsp")))
     (funcall bump)))

  (t
    (funcall make $1))
  ) ; case

(format (standard-output)
        "~%Elapsed time: ~A seconds~%"
        (quotient (- (get-internal-run-time) start-time)
                  (internal-time-units-per-second)))
