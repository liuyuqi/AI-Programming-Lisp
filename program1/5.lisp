(defun transpose_t (lst)
  ;; outputs the transpose of lst
    (transpose_new (getlen (car lst)) lst))

(defun getlen (lst)
  ;; gets the j dimension value of lst
    (cond ((null lst) 0)
          (t (+ 1 (getlen (cdr lst))))))

(defun transpose_new (j lst)
  ;; outputs the transpose of lst with secondary dimension j
    (cond ((= j 0) NIL)
          (t (append (transpose_new (- j 1) lst) (list (transpose_action j lst))))))

(defun transpose_action (j lst)
  ;; returns a list containing all the element in the jth column of lst
    (cond ((null lst) NIL)
          (t (append (list (get_jth_elem j (car lst))) (transpose_action j (cdr lst))))))

(defun get_jth_elem (j lst)
  ;; return the jth element of lst
    (cond ((= j 1) (car lst))
          (t (get_jth_elem (- j 1) (cdr lst)))))

;; test cases
(print (transpose_t '((4 6 8) (7 5 2) (12 9 6))))
(print (transpose_t '((c a t) (d o g))))
  
