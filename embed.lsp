(defmacro foreach (pair &rest commands)
  (let ((key (car pair))
        (values (car (cdr pair))))
    `(mapc (lambda (,key) ,@commands) ,values))
  )
(defmacro doenv (pair &rest commands)
  (let* ((name (car pair))
         (value (car (cdr pair))))
    `(let ((orig (getenv ,name)))
        (setenv ,name ,value)
        ,@commands
        (setenv ,name orig))
    )
  )
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
(defglobal windows (equal (getenv "OS") "Windows_NT"))
; vim:set lispwords+=foreach,env,mapc,make,pushd,while:
