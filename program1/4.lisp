(defun repeat (lst)
  ;; returns a list in which the jth element in 'lst' is repeated k times in a list
  (repeat-iter lst 1))

(defun repeat-iter (lst i)
  ;; the iter version of the repeat function
  (cond ((null lst) NIL)
        (t (append (list (rep (car lst) i)) (repeat-iter (cdr lst) (+ i 1))))))

(defun rep (j k)
  ;; repeat the element j for k times
  (cond ((= k 1) (list j))
        (t (cons j (rep j (- k 1))))))

;; test cases
(print (repeat '(4 6 3)))
(print (repeat '(2 4 1 2)))
