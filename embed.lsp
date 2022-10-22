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
        (if orig
          (setenv ,name orig))
        )
    )
  )
; vim:set lispwords+=foreach,env,mapc,make,pushd:
