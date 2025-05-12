(defmacro assert-eq (source expect)
  (let ((result (gensym)))
    `(let ((,result ,source))
       (if (equal ,result ,expect)
         (format (error-output) "OK: (assert-eq ~S ~S)~%"
                 (quote ,source)
                 ,expect)
         (progn
           (format (error-output) "NG: (assert-eq ~S ~S)~%  but ~S~%"
                   (quote ,source)
                   ,expect
                   ,result)
           (abort))))))

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
(defun probe-directory (fname)
  (let ((tmp (stat fname)))
    (and tmp (try-cdr (assoc 'is-dir tmp)))))
(defun -d (fname) (probe-directory fname)) ; deprecated
(defglobal windows (equal (getenv "OS") "Windows_NT")) ; deprecated
(defglobal *windows* (equal (getenv "OS") "Windows_NT"))
(defglobal *dev-null* (if *windows* "NUL" "/dev/null"))
(defglobal *exe-suffix* (if *windows* ".exe" ""))
(defun updatep (target :rest sources)
  (let ((newsrc nil))
    (dolist (s sources)
      (setq newsrc (append newsrc (if (consp s) s (list s)))))
    (apply #'updatep_ target newsrc)))
(defun x (cmd &rest params) ; deprecated
  (apply #'spawn cmd params))
(defun spawnlp (cmd &rest params) ; deprecated
  (apply #'spawn cmd params))
(defun spawnvp (cmd params) ; deprecated
  (apply #'spawn cmd params))
(defun pathjoin (&rest params) ; deprecated
  (apply #'join-path params))
(defun joinpath (&rest params) ; deprecated
  (apply #'join-path params))
