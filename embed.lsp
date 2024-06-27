(defmacro foreach (vars &rest body)
  (let ((var (car vars))
        (values (elt vars 1))
        (rest (gensym)))
    `(block
       nil
       (let ((,var nil)(,rest ,values))
         (while ,rest
           (setq ,var (car ,rest))
           (setq ,rest (cdr ,rest))
           ,@body)))))
(defmacro env (_pairs &rest commands)
  (let ((pairs nil)
        (p nil)
        (POINTER (gensym))
        (ORIG (gensym)))
    (while _pairs
      (setq p (car _pairs))
      (setq _pairs (cdr _pairs))
      (setq pairs (cons `(cons ,(car p) ,(elt p 1)) pairs)))
    (setq pairs (cons 'list pairs))
    `(let ((,ORIG nil))
       (foreach (,POINTER ,pairs)
         (setq ,ORIG (cons (cons (car ,POINTER) (getenv (car ,POINTER))) ,ORIG))
         (setenv (car ,POINTER) (cdr ,POINTER)))
       (unwind-protect
         (progn ,@commands)
         (foreach (,POINTER ,ORIG)
           (setenv (car ,POINTER) (cdr ,POINTER)))))))
(defun string-split (_sep str)
  (let ((result nil)
        (index nil)
        (sep (create-string 1 _sep)))
    (while (setq index (string-index sep str))
      (setq result (cons (subseq str 0 index) result))
      (setq str (subseq str (1+ index) (length str))))
    (setq result (cons str result))
    (nreverse result)))
(defmacro pushd (wd &rest commands)
  (let ((ORIG (gensym)))
    `(let ((,ORIG (getwd)))
       (chdir ,wd)
       (unwind-protect
         (progn ,@commands)
         (chdir ,ORIG)))))
(defun echo (&rest strings)
  (let ((dem ""))
    (while strings
      (format t "~a~a" dem (car strings))
      (setq strings (cdr strings))
      (setq dem " "))
    (format t "~%")))
(defun -e (fname)
  (probe-file fname))
(defun try-cdr (c)
  (and c (consp c) (cdr c)))
(defun -d (fname)
  (let ((tmp (stat fname)))
    (and tmp (try-cdr (assoc 'is-dir tmp)))))
(defglobal windows (equal (getenv "OS") "Windows_NT"))
(defun updatep* (target :rest sources)
  (let ((newsrc nil))
    (dolist (s sources)
      (setq newsrc (append newsrc (if (consp s) s (list s)))))
    (apply #'updatep target newsrc)))
