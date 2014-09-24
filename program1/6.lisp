(defun graft (struct lst)
  ;; grafts 'struct' into 'lst' at the end of each sublist of 'lst'
    (cond ((null lst) NIL)
          ((not (listp (car lst)))
                (cons (car lst) (graft struct (cdr lst))))
          ((not (atom (car lst))) 
                (cons (append (graft struct (car lst)) struct) (graft struct (cdr lst))))))

;; test cases
(print (graft '(2 3) '(4 (3 5) 7 (2))))
(print (graft '((a b) c) '(7 (5 (6)))))
(print (graft '(4 a (c)) '((a b) 2 (4 6 (3)) (7) 3 (3 8))))
(print (graft '((7)) '(((a)))))

