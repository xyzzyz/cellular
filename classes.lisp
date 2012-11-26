(in-package :cellular)

(defclass state () 
  ((name :reader state-name :initarg :name)
   (colour :reader state-colour :initarg :colour :initform (list 0 0 0))
   (representation :reader state-representation :initarg :representation)))

(defun make-state (name colour representation)
  (make-instance 'state :name name :colour colour :representation representation))

(defmethod print-object ((state state) (stream stream))
  (format stream "#<STATE: ~A>" (state-name state)))

(defclass cell ()
  ((state :accessor cell-state :initarg :state :initform nil)
   (new-state :accessor cell-new-state :initform nil)
   (place :accessor cell-place :initarg :place :initform nil)))

(defmethod cell-x ((cell cell))
  (first (cell-place cell)))

(defmethod cell-y ((cell cell))
  (second (cell-place cell)))


(defmethod print-object ((cell cell) (stream stream))
  (format stream "#<CELL: ~A>" (state-name (cell-state cell))))

(defclass grid ()
  ((cells :accessor grid-cells :initarg :cells)))

(defun make-grid (h w)
  (let ((arr (make-array (list (+ h 2) (+ w 2)) :element-type 'cell)))
    (loop for i from 0 to (1+ h) do
	 (loop for j from 0 to (1+ w) do
	      (setf (aref arr i j) (make-instance 'cell :place (list i j)))))
    (make-instance 'grid :cells arr)))

(defmethod grid-cell ((grid grid) (x number) (y number))
  (aref (grid-cells grid) x y))

(defmethod grid-height ((grid grid))
  (- (array-dimension (grid-cells grid) 0) 2))

(defmethod grid-width ((grid grid))
  (- (array-dimension (grid-cells grid) 1) 2))

(defmethod cell-relative ((cell cell) (grid grid) (offx number) (offy number))
  (grid-cell grid (+ (cell-x cell) offx) (+ (cell-y cell) offy)))

(defmacro cell-accs (accessors)
  (let ((accs (loop for acc in accessors collect 
		   `(defmethod ,(first acc) ((cell cell) (grid grid))
		      (cell-relative cell grid ,(second acc) ,(third acc))))))
    `(progn 
       ,@accs)))

(cell-accs 
 ((cell-nw -1 -1)
  (cell-n 0 -1)
  (cell-ne 1 -1)
  (cell-w -1 0)
  (cell-e 1 0)
  (cell-sw -1 1)
  (cell-s 0 1)
  (cell-se 1 1)))

(defmethod cell-neighbours ((cell cell) (grid grid))
  (let ((offsets '#.(loop for i from -1 to 1 append
			(loop for j from -1 to 1 
			   unless (and (zerop i) (zerop j)) collect (list i j)))))
    (mapcar (lambda (off) (apply #'cell-relative cell grid off)) offsets)))

(defclass automaton ()
  ((name :reader automaton-name :initarg :name :initform "")
   (states :reader automaton-states :initarg :states :initform nil)
   (rules :reader automaton-rules :initarg :rules :initform nil)
   (default-state :reader automaton-default-state :initarg :default-state)))

(defmethod set-new-state ((cell cell) (automaton automaton) (state symbol))
  (setf (cell-new-state cell) (find state (automaton-states automaton) :key #'state-name)))

(defmethod change-state ((cell cell) (automaton automaton) (state symbol))
  (setf (cell-state cell) (find state (automaton-states automaton) :key #'state-name)))

(defmethod maybe-change-state ((cell cell))
  (with-slots (state new-state) cell
    (when new-state 
      (setf state new-state
	    new-state nil))))


(defmethod state-from-representation ((char character) (automaton automaton))
  (state-name (find char (automaton-states automaton) :key #'state-representation)))


   
