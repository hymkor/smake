;; experimantal scripts
;; https://github.com/takagi/lake

(defglobal task-list nil)

(defun lake (target)
  (format t "lake ~S~%" target)
  (assure <string> target)
  (mapc
    (lambda (c)
      (if (equal (car c) target)
        (funcall (cdr c))))
    task-list))

(defun add-child-task (key routine)
  (format t "add-child-task ~S~%" key)
  (assure <string> key)
  (assure <function> routine)
  (setq task-list (cons (cons key routine) task-list)))

(defun file-func (target depend f)
  (assure <list> depend)
  (assure <string> target)
  (assure <function> f)
  ; (setq depend (mapcan #'wildcard depend))
  (format t "file-func ~S ~S~%" target depend)
  (add-child-task
    target
    (lambda ()
      (mapc #'lake depend)
      (if (apply #'updatep target depend)
        (funcall (assure <function> f))
        (format t "not update: ~S ~%" depend)
        ))))

(defmacro file (target dependency-list &rest form)
  (if (and form (stringp (car form)))
    (setq form (cdr form)))
  (list 'file-func target (cons 'list dependency-list)
        (append '(lambda ()) form)))
;  `(file-func ,target ,dependency-list (lambda () ,@form)))

(defun task-func (task-name depend f)
  (assure <string> task-name)
  (assure <list> depend)
  (assure <function> f)
  (add-child-task
    task-name
    (lambda ()
      (mapc #'lake depend)
      (funcall f))))

(defmacro task (task-name dependency-list &rest form)
  (if (and form (stringp (car form)))
    (setq form (cdr form)))
  `(task-func ,task-name ,dependency-list (lambda () ,@form)))
