(in-package :cellular)

(defun make-automaton (&key states rules default-state name)
  (let ((default-state (first (find default-state states :key #'first)))
	(states (mapcar (bind-n #'apply 1 #'make-state) states)))
    (make-instance 'automaton :name name :states states :rules rules :default-state default-state)))

(defmethod grid-init ((grid grid) (automaton automaton))
  (let ((cells (grid-cells grid)))
    (loop for i from 0 to (1+ (grid-height grid)) do
	 (loop for j from 0 to (1+ (grid-width grid)) do
	      (change-state (aref cells i j) automaton (automaton-default-state automaton))))))

(defmethod grid-set-pattern ((grid grid) (automaton automaton) (pattern list))
  (flet ((pattern-setter (pattern) 
	   (destructuring-bind (new-state h w) pattern
	     (change-state (grid-cell grid h w) automaton new-state))))
    (mapc #'pattern-setter pattern)))

(defmethod next-generation ((grid grid) (automaton automaton))
  (with-grid-cells (grid cell h w)
    (mapcar (lambda (rule) 
	      (funcall rule cell grid automaton))
	    (automaton-rules automaton)))
  (with-grid-cells (grid cell h w)
    (maybe-change-state cell)))

(defparameter *conway-life* 
  (make-automaton :name "Conway's Game of Life"
		  :states '((:dead (255 255 255 255) #\ ) (:alive (0 0 0 255) #\x))
		  :default-state :dead
		  :rules (list
			  (count-neighbours-rule :alive :alive :dead 
						 (lambda (count) 
						   (or (< count 2) (> count 3))))
			  (count-neighbours-rule :dead :alive :alive 
						 (lambda (count)
						   (= count 3))))))

(defparameter *wireworld* 
  (make-automaton :name "Wireworld"
		  :states '((:empty (0 0 0 255) #\ ) (:wire (255 255 0 255) #\-)
			    (:head (255 0 0 255) #\|) (:tail (0 0 255 255) #\=))
		  :default-state :empty
		  :rules (list
			  (simple-change-rule :tail :wire)
			  (simple-change-rule :head :tail)
			  (count-neighbours-rule :wire :head :head 
						 (lambda (count) 
						   (or (= count 1) (= count 2)))))))
