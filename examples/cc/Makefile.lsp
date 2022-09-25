; AOUT=$(notdir $(abspath .))$(EXE)
; OFILES=$(subst .c,.o,$(wildcard *.c))
; $(AOUT): $(OFILES)
;     gcc -o $@
; .c.o:
;     gcc -c $<
(labels
  ((c-to-o (c) (string-append (subseq c 0 (- (length c) 2)) ".o")))
  (let*
    ((c-files (wildcard "*.c"))
     (o-files (mapcar #'c-to-o c-files))
     (windows (equal (getenv "OS") "Windows_NT"))
     (exe (if windows ".exe" ""))
     (a-out (string-append (notdir (abspath ".")) exe))
     )
    (apply #'make $1

      ((cons a-out o-files)
       (apply #'x "gcc" "-o" $@ o-files)
       )

      ('("clean")
       (apply #'rm a-out o-files)
       )

      (mapcar
        (lambda (c-fname)
          (list
            (list 'quote (list (c-to-o c-fname) c-fname))
            '(x "gcc" "-c" $<)
            )
          )
        c-files)
      )
  ) ; let* code
) ; flet code
