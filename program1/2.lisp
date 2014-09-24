(defun neg_first (lst)
  ;; return a list where all negative numbers in lst appears first, 
  ;; then other numbers, while keeping the order.
  (append (negs lst) (poss lst)))

(defun negs (lst)
  ;; returns a list containing all negative elements in 'lst'
  (cond ((null lst) ())
        ((< (car lst) 0) (cons (car lst) (negs (cdr lst))))
        (t (negs (cdr lst)))))

(defun poss (lst)
  ;; returns a list containing all non-negative elements in 'lst'
  (cond ((null lst) ())
        ((not (< (car lst) 0)) (cons (car lst) (poss (cdr lst))))
        (t (poss (cdr lst)))))

;; test cases
(print (neg_first '(5 -2 -6 3 -1)))
(print (neg_first '(-9 0 7 4 -6 -3 8 -9 4)))
