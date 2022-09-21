; a.out: *.o
;     gcc -o $@
; .c.o:
;     gcc -c $<
(flet
  ((c-to-o (c)
           (string-append (subseq c 0 (- (length c) 2)) ".o"))
   (string-join (dem seq)
                (apply #'string-append (mapcan (lambda (c) (list dem c)) seq)))
   ) ; flet param
  (let*
    ((c-files (glob "*.c"))
     (o-files (mapcar #'c-to-o c-files))
     )
    (apply
      #'make
      ((cons "a.out" o-files)
       (sh (string-append "gcc -o $@" (string-join " " o-files)))
       )
      ('("clean")
       (apply #'rm (cons "a.out" o-files))
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
