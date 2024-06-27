; AOUT=$(notdir $(abspath .))$(EXE)
; OFILES=$(subst .c,.o,$(wildcard *.c))
; $(AOUT): $(OFILES)
;     gcc -o $@
; .c.o:
;     gcc -c $<
(defun c-to-o (c)
  (string-append (basename c) ".o"))
(let*
  (
   (c-files (wildcard "*.c"))
   (o-files (mapcar #'c-to-o c-files))
   (exe (if windows ".exe" ""))
   (a-out (string-append (notdir (getwd)) exe)))

  (make $1
    ((cons a-out o-files)
     ; .c.o:
     ;     gcc -c $<
     (dolist (c-src c-files)
       (let ((obj (c-to-o c-src)))
         (if (updatep obj c-src)
           (x "gcc" "-c" c-src))))
     ; $(AOUT): $(OFILES)
     ;     gcc -o $@
     (apply #'x "gcc" "-o" $@ o-files))

    ('("clean")
     (apply #'rm a-out o-files))
    )
  ) ; let* code
; vim:set lispwords+=apply,make:
