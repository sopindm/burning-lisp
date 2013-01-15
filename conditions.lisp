(in-package #:burning-lisp)

(defmacro with-restarts (form &rest clauses)
  (flet ((restart-invoker (cond sym)
	   `(,cond (lambda (cond) (invoke-restart ',sym cond))))
	 (restart-body (clause symbol)
	   (dbind ((&optional arg-name) &body body) (rest clause)
	     `(,symbol (,(or arg-name symbol)) ,@(unless arg-name `((declare (ignore ,symbol)))) ,@body))))
    (let ((restart-syms (mapcar (lambda (arg) (declare (ignore arg)) (gensym)) clauses)))
      `(restart-case
	   (handler-bind (,@(mapcar #'restart-invoker (mapcar #'first clauses) restart-syms))
	     ,form)
	 ,@(mapcar #'restart-body clauses restart-syms)))))

