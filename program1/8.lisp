(defun multiply (exp1 exp2)
  ;; multiplies exp1 and exp2, and get the simple arithmetic expression of the product.
    (let ((expr1 (convert exp1))
          (expr2 (convert exp2)))
      (convert-back (multiply-simple expr1 expr2))))

(defun convert (lst)
  ;; converts the prefix expression "lst" into a powerlist
  ;; e.g. input = '(+ (* 16 (% y 2)) 9), output = '(9 0 16)
    (cond ((not (listp lst))
                (list lst))
          ((equal (car lst) '+)
                (convert-rest (cdr lst)))
          ((equal (car lst) '-)
                (tuplemin (convert (cadr lst)) (convert (caddr lst))))
          ((equal (car lst) '*)
                (cond ((not (listp (caddr lst))) 
                        (form-tuple (cadr lst) '(% y 1) 0))
                      (t (form-tuple (cadr lst) (caddr lst) 0))))
          ((equal (car lst) '%) 
                (form-tuple 1 lst 0))))

(defun convert-rest (lst)
  ;; converts the rest part of a long adding list
    (cond ((null lst) NIL)
          ((equal (car lst) 'y) (tupleadd '(0 1) (convert-rest (cdr lst))))
          ((numberp lst) (list lst))
          (t (tupleadd (convert (car lst)) (convert-rest (cdr lst))))))

(defun form-tuple (coefficient orig-list curr-expo)
  ;; forms a tuple from a exponential expression "orig-list".
  ;; e.g. input = '(% y 2), i.e.
  ;;    coefficient = 1, orig-list = '(% y 2), curr-expo = 0, then
  ;;      output = '(0 0 1)
    (let ((desired-expo (caddr orig-list)))
        (cond ((not (= desired-expo curr-expo))
                    (cons 0 (form-tuple coefficient orig-list (+ 1 curr-expo))))
              (t (list coefficient)))))

(defun tupleadd (tupleA tupleB)
  ;; add the two tuples. 
  ;; e.g. tupleA = '(0 0 3), tupleB = '(1 1 1), then returns '(1 1 4)
    (cond ((and (null tupleA) (null tupleB)) NIL)
          ((and (not (null tupleA)) (null tupleB))
                (cons (car tupleA) (tupleadd (cdr tupleA) tupleB)))
          ((and (null tupleA) (not (null tupleB)))
                (cons (car tupleB) (tupleadd tupleA (cdr tupleB))))
          ((and (not (null tupleA)) (not (null tupleB)))
                (cons (+ (car tupleA) (car tupleB)) (tupleadd (cdr tupleA) (cdr tupleB))))))

(defun tuplemin (tupleA tupleB)
  ;; take subtraction of tupleA-tupleB
    (cond ((and (null tupleA) (null tupleB)) NIL)
          ((and (not (null tupleA)) (null tupleB))
                (cons (car tupleA) (tuplemin (cdr tupleA) tupleB)))
          ((and (null tupleA) (not (null tupleB)))
                (cons (- 0 (car tupleB)) (tuplemin tupleA (cdr tupleB))))
          ((and (not (null tupleA)) (not (null tupleB)))
                (cons (- (car tupleA) (car tupleB)) (tuplemin (cdr tupleA) (cdr tupleB))))))

(defun multiply-simple (expr1 expr2)
  ;; do the multiplication of two tuples
    (let ((max-expo (- (+ (length expr1) (length expr2)) 2)))
      (multiply-simple-iter expr1 expr2 0 max-expo)))

(defun multiply-simple-iter (expr1 expr2 curr-expo max-expo)
  ;; the iteration function of multiply-simple
  ;; returns a product tuple of expr1*expr2
    (cond ((< curr-expo max-expo)
                (cons (get-product expr1 expr2 curr-expo) 
                      (multiply-simple-iter expr1 expr2 (+ 1 curr-expo) max-expo)))
          ((= curr-expo max-expo)
                (list (get-product expr1 expr2 curr-expo)))))

(defun get-product (expr1 expr2 curr-expo)
  ;; returns the product element of expr1*expr2 where 
  ;; (power of elems in expr1) plus (power of elems in expr2) is curr-expo.
    (get-product-iter expr1 expr2 0 curr-expo curr-expo))

(defun get-product-iter (expr1 expr2 idx1 idx2 curr-expo)
  ;; the iterator function of get-product, where idx1+idx2 = curr-expo
  ;; returns the single product of expr1[idx1]*expr2[idx2].
    (let ((mul1 (get-elem expr1 idx1))
          (mul2 (get-elem expr2 idx2))
          (max1 (- (length expr1) 1))
          (max2 (- (length expr2) 1)))
      (cond ((> idx2 max2)
                (get-product-iter expr1 expr2 (+ idx1 1) (- idx2 1) curr-expo))
            ((> idx1 max1) 0)
            ((< idx1 curr-expo)
                (+ (* mul1 mul2) (get-product-iter expr1 expr2 (+ idx1 1) (- idx2 1) curr-expo)))
            ((= idx1 curr-expo)
                (* mul1 mul2)))))

(defun get-elem (expr idx)
  ;; gets the element in 'expr' of index 'idx'
    (cond ((= idx 0) (car expr))
          (t (get-elem (cdr expr) (- idx 1)))))

(defun convert-back (lst)
  ;; convert the tuple back to prefix-notation of the expression.
    (cond ((= (length lst) 1) (list (car lst)))
          ((> (get-effective-length lst) 1)
                (cons '+ (convert-back-rest lst 0)))
          ((= (get-effective-length lst) 1)
                (convert-back-rest lst 0))))

(defun get-effective-length (lst)
  ;; get the number of non-zero elements in 'lst'
    (cond ((null lst) 0)
          ((= (car lst) 0) (get-effective-length (cdr lst)))
          (t (+ 1 (get-effective-length (cdr lst))))))

(defun convert-back-rest (lst expo)
  ;; converts the rest of the tuple back to prefix-notation expression.
  ;; argument lst: the remaining tuple
  ;; expo: current exponential value
    (cond ((null lst) NIL)
          ((not (null (cdr lst)))
            (cond ((not (null (form-expo (car lst) expo)))
                (append (convert-back-rest (cdr lst) (+ expo 1)) (list (form-expo (car lst) expo))))
                  (t (convert-back-rest (cdr lst) (+ expo 1)))))
          (t (list (form-expo (car lst) expo)))))

(defun form-expo (coefficient expo)
  ;; form the prefix expo expression.
  ;; e.g. coefficient = 3, expo = 2;
  ;;    then returns (* 3 (% y 2))
    (cond ((= coefficient 0) NIL)
          ((= expo 0) (list coefficient))
          ((= coefficient 1)
                (cond ((= expo 1) '(y))
                      ((> expo 1) (append '(% y) (list expo)))))
          (t (cond ((= expo 1) (append '(*) (list coefficient) '(y)))
                   ((> expo 1) (append '(*) (list coefficient) (list (append '(% y) (list expo)))))))))

(setq exp1 '(+ 5 (% y 3)))
(setq exp2 '(* 6 (% y 2)))
(print (multiply exp1 exp2))
(setq exp3 '(+ (* 3 (% y 2)) (* 7 (% y 5))))
(setq exp4 '(+ (* 2 (% y 6)) (* 5 (% y 3))))
(print (multiply exp3 exp4))
(setq exp5 '(* 3 (% y 2)))
(setq exp6 '(+ (* 7 (% y 3)) (* 2 y) 4))
(print (multiply exp5 exp6))
(setq exp7 '(+ (* 4 y) 3))
(setq exp8 '(- (* 4 y) 3))
(print (multiply exp7 exp8))

(print (multiply '(+ (* 6 (% y 4)) (* 2 (% y 3)) (% y 2) (* 3 y) 7) '(+ (* 2 (% y 5)) (% y 4) (* 2 (% y 3)) y 5)))
(print (multiply '(% y 5) '(+ (* 2 (% y 3)) 4)))
(print (multiply '(* 3 y) '(* 4 y)))
