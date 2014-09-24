(defun postfix (lst)
  ;; returns the postfix notation of the prefix-notated 'lst'
  ;; format: (operator operand1 operand2)
        ;; case 1: both operands are numbers.
  (cond ((and (not (listp (cadr lst))) (not (listp (caddr lst)))) 
            (append (cdr lst) (list (car lst))))
        ;; case 2: operand1 is a number, operand2 is an expression.
        ((and (not (listp (cadr lst))) (not (atom (caddr lst))))
            (list (cadr lst) (postfix (caddr lst)) (car lst)))
        ;; case 3: operand1 is an expression, operand2 is a number.
        ((and (not (atom (cadr lst))) (not (listp (caddr lst))))
            (list (postfix (cadr lst)) (caddr lst) (car lst)))
        ;; case 4: both operands are expressions
        (t (list (postfix (cadr lst)) (postfix (caddr lst)) (car lst)))))

; test cases
(print (postfix '(+ 2 3)))
(print (postfix '(* (+ 1 2) (- 10 8))))
(print (postfix '(* (+ (- 3 4) (/ 4 6)) 5)))
