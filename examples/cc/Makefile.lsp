; AOUT=$(notdir $(abspath .))$(EXE)
; OFILES=$(subst .c,.o,$(wildcard *.c))
; $(AOUT): $(OFILES)
;     gcc -o $@
; .c.o:
;     gcc -c $<
(let*
  ((c-to-o (lambda (c) (string-append (basename c) ".o")))
   (c-files (wildcard "*.c"))
   (o-files (mapcar #'c-to-o c-files))
   (exe (if windows ".exe" ""))
   (a-out (string-append (notdir (getwd)) exe)))
  (apply #'make $1
    ((cons a-out o-files)
     (apply #'x "gcc" "-o" $@ o-files))
    ('("clean")
     (apply #'rm a-out o-files))
    (mapcar
      (lambda (c)
        (let ((o (c-to-o c)))
          `((list ,o ,c)
            (x "gcc" "-c" ,c))))
      c-files))) ; let* code
; vim:set lispwords+=apply,make:
