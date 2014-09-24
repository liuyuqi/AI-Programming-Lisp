(defun full-inherit-get (object property)
  ;; do the inherit-get for multi-inherit
    (let ((value (get object property 'NA))
          (parentlist (get object 'isa)))
      (cond ((null object) nil)
            ((not (equal value 'NA)) value)
            (t (full-inherit-get-list parentlist property)))))

(defun full-inherit-get-list (lst property)
  ;; do the full-inherit-get for a list of objects. (breadth-first search all the time)
  ;; returns NIL if property value not available.
    (cond ((null lst) nil)
          ((not (equal (get (car lst) property 'NA) 'NA)) 
                (get (car lst) property))
          ((not (null (get (car lst) 'isa)))
                (full-inherit-get-list (append (cdr lst) (get (car lst) 'isa)) property))
          (t (full-inherit-get-list (cdr lst) property))))

;; test cases
(setf (get 'baking-soda 'isa) '(rising-agent deodorent antacid))
;(setf (get 'antacid 'edible) 'yes)
;(setf (get 'baking-soda 'edible) 'yes)
(print (full-inherit-get 'baking-soda 'edible))

