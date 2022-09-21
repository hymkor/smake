; a.out: *.o
;     gcc -o $@
; .c.o:
;     gcc -c $<
(flet (
       (c-to-o (c) (string-append (subseq c 0 (- (length c) 2)) ".o"))
       (string-join
         (dem seq)
         (let ((d "") (buffer (create-string-output-stream)))
            (mapc (lambda (c)
                    (format-object buffer d nil)
                    (format-object buffer c nil)
                    (setq d dem))
                  seq)
            (get-output-stream-string buffer)
         )) ; string-join
       ) ; flet
  (let* (
      (c-files (glob "*.c"))
      (rules
        (mapcar (lambda (c)
                  (list (list 'quote (list (c-to-o c) c)) '(sh "gcc -c $<")))
                c-files))
      (o-files (mapcar #'c-to-o c-files))
      (aout-rule
        (list (list 'quote (cons "a.out" o-files))
              (list 'sh (string-append "gcc -o $@ " (string-join " " o-files)))))
      (clean-rule
        (list ''("clean") (cons 'rm (cons "a.out" o-files))))
      ) ; let* param
    (setq rules (cons aout-rule rules))
    (setq rules (append rules (list clean-rule)))
    (format (standard-output) "~s~%" rules)
    (apply #'make rules)
  ) ; let* code
) ; flet code
