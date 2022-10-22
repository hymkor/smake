(defmacro foreach (pair &rest commands)
  (let ((key (car pair))
        (values (car (cdr pair))))
    `(mapc (lambda (,key) ,@commands) ,values))
  )
; vim:set lispwords+=foreach,env,mapc,make,pushd:
