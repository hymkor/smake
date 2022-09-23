; a.out: *.o
;     gcc -o $@
; .c.o:
;     gcc -c $<
(labels
  ((c-to-o (c)
           (string-append (subseq c 0 (- (length c) 2)) ".o"))
   (string-join
     (dem seq)
     (if seq
       (apply #'string-append (cdr (mapcan (lambda (c) (list dem c)) seq)))
       ""))
    (getfname (path)
      (let ((index nil))
        (while (setq index (string-index $/ path))
               (setq path (subseq path (1+ index)))
        )
        path)
    )
   ) ; flet param
  (let*
    ((c-files (glob "*.c"))
     (o-files (mapcar #'c-to-o c-files))
     (windows (equal (getenv "OS") "Windows_NT"))
     (exe (if windows ".exe" ""))
     (cwd (qs (if windows "cd" "pwd")))
     (a-out (string-append (getfname cwd) exe))
     )
    (apply
      #'make
      $1

      ((cons a-out o-files)
       (sh (string-join " " (cons "gcc -o $@" o-files)))
       )

      ('("clean")
       (apply #'rm a-out o-files)
       )

      (mapcar
        (lambda (c-fname)
          (list
            (list 'quote (list (c-to-o c-fname) c-fname))
            '(sh "gcc -c $<")
            )
          )
        c-files)
      )
  ) ; let* code
) ; flet code
