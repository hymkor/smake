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
     (rm obj))
   (rm target))

  (t
   (dolist (c-src c-files)
     (let ((obj (c-to-o c-src)))
       (if (updatep obj c-src)
         (x "gcc" "-c" c-src))))
   (apply #'x "gcc" "-o" target o-files))
  ) ; end-cond

; vim:set lispwords+=apply,make:
