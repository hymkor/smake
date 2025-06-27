(defglobal make (load "smake-go120.lsp"))

(case $1
  (("dist")
   (dolist (platform
             '(("linux"   "386")
               ("linux"   "amd64")
               ("windows" "386")
               ("windows" "amd64")))
     (env
       (("GOOS"   (car platform))
        ("GOARCH" (car (cdr platform))))
       (let ((exe-list (funcall make 'build)))
         (funcall make 'dist exe-list)))))

  (("test")
   (load "smake-test.lsp"))

  (("build" "" nil)
   (funcall make 'build))

  (t
    (funcall make $1))
  ) ; case
