(defpackage :cellular 
  (:use :common-lisp)
  (:export 
   count-neighbours-rule
   simple-change-rule
   *conway-life*
   *wireworld*
   make-automaton
   run-automaton
   pattern-from-file))

(defpackage :cellular-user
  (:use :common-lisp :cellular))