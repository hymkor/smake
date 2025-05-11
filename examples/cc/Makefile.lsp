; AOUT=$(notdir $(abspath .))$(EXE)
; OFILES=$(subst .c,.o,$(wildcard *.c))
; $(AOUT): $(OFILES)
;     gcc -o $@
; .c.o:
;     gcc -c $<

(defun c-to-o (c) (string-append (basename c) ".o"))

(defglobal c-files (wildcard "*.c"))
(defglobal o-files (mapcar #'c-to-o c-files))
(defglobal target  (string-append (notdir (getwd)) *exe-suffix*))

(case $1
  (("clean")
   (dolist (obj o-files)
     (if (probe-file obj)
       (rm obj)))
   (if (probe-file target)
     (rm target)))

  (t
    (dolist (c-src c-files)
      (if (updatep (c-to-o c-src) c-src)
        (spawn "gcc" "-c" c-src)))
    (apply #'spawn "gcc" "-o" target o-files))
  ) ; case

; vim:set lispwords+=apply,make:
