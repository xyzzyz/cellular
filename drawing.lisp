(in-package :cellular)

(defun draw-grid (grid cell-size)
  (with-grid-cells (grid cell h w)
    (apply #'pal:draw-rectangle (pal:v (* cell-size (1- h)) (* cell-size (1- w))) cell-size cell-size 
	   (state-colour (cell-state cell)))))


(defun run-automaton (automaton pattern &key (cell-size 10) (grid-height 30) (grid-width 30) (sleep-time 1))
  (let ((grid (make-grid grid-height grid-width)))
    (grid-init grid automaton)
    (grid-set-pattern grid automaton pattern)
    (pal:with-pal (:width (* cell-size grid-width) :height (* cell-size grid-height) :title (concatenate 'string "Cellular - " (automaton-name automaton)))
      (pal:event-loop () 
	(pal:clear-screen 255 255 255)
	(draw-grid grid cell-size)
	(format t "Nextgen. ~%")
	(next-generation grid automaton)
	(sleep sleep-time)))))
	

(defun pattern-from-file (filename automaton) 
  (with-open-file (file (merge-pathnames filename) :direction :input)
    (loop with row = 1 with col = 1 with pattern = '()
       with char = #\X
       when (not (characterp char)) return pattern
       do (setq char (read-char file nil))
       (cond 
         ((eql char #\Newline) (incf row) (setq col 1))
         ((eql char #\ ) (incf col))
         ((characterp char) 
	  (push (list (state-from-representation char automaton) col row) pattern)
	  (incf col))))))
