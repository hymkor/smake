(defmacro foreach (pair &rest commands)
  (let ((key (car pair))
        (values (car (cdr pair))))
    `(mapc (lambda (,key) ,@commands) ,values))
  )
(defmacro env (_pairs &rest commands)
  (let ((pairs nil) (p nil))
    (while _pairs
      (setq p (car _pairs))
      (setq _pairs (cdr _pairs))
      (setq pairs (cons `(cons ,(car p) ,(elt p 1)) pairs))
      )
    (setq pairs (cons 'list pairs))
    `(let ((orig nil))
       (foreach (p ,pairs)
         (setq orig (cons (cons (car p) (getenv (car p))) orig))
         (setenv (car p) (cdr p)))
       ,@commands
       (foreach (p orig)
         (setenv (car p) (cdr p)))
       )))
(defun string-split (_sep str)
  (let ((result nil)
        (index nil)
        (sep (create-string 1 _sep)))
    (while (setq index (string-index sep str))
      (setq result (cons (subseq str 0 index) result))
      (setq str (subseq str (1+ index) (length str)))
      )
    (setq result (cons str result))
    (nreverse result)))
(defmacro pushd (wd &rest commands)
  `(let ((orig (getwd)))
     (chdir ,wd)
     ,@commands
     (chdir orig)
     ))
(defun echo (&rest strings)
  (let ((dem ""))
    (while strings
      (format t "~a~a" dem (car strings))
      (setq strings (cdr strings))
      (setq dem " "))
    (format t "~%")))
(defglobal windows (equal (getenv "OS") "Windows_NT"))
; vim:set lispwords+=foreach,env,mapc,make,pushd,while:
