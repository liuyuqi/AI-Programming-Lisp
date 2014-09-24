(defun aver(x y z)
; compute the average of the three arguments
  (/ (+ x y z ) 3.0))


(defun neg-test(w x y)
; return T if any argument is negative
   (cond ((< w 0) t)
         ((< x 0) t)
         ((< y 0) t)))

(defun fact (y)
; compute n!
   (cond (((<= y 1)and(>= y 0)) 1)
         ((< y 0) nil)
         (t (* (fact (- y 1)) y))))

