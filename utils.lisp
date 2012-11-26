(in-package :cellular)

(defun bind-n (fn n arg &aux (z (1- n)))
  "Returns a closure with nth argument bound to arg."
  (lambda (&rest args)
    (apply fn (append (subseq args 0 z)
		      (list arg)
		      (nthcdr z args)))))

(defmacro with-grid-cells ((grid cell-sym h-sym w-sym) &rest body)
  (let ((cells (gensym)))
    `(let ((,cells (grid-cells ,grid)))
       (loop for ,h-sym from 1 to (grid-height ,grid) do
	    (loop for ,w-sym from 1 to (grid-width ,grid) 
	       for ,cell-sym = (aref ,cells ,h-sym ,w-sym) then (aref ,cells ,h-sym ,w-sym)
	       do ,@body)))))
		 