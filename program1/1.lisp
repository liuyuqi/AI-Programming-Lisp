(defun twice (first_lst sec_lst)
  ;; returns # of elements in 'first_lst' that occurs exactly twice in 'sec_lst'
  (cond ((null first_lst) 0)
        ((null sec_lst) 0)
        ((not (listp first_lst)) (is_twice first_lst sec_lst)) 
        (t (+ (twice (car first_lst) sec_lst) (twice (cdr first_lst) sec_lst)))))

(defun is_twice (target lst)
  ;; returns whether 'target' occurs twice in 'lst'
  (cond ((= (find_occur target lst) 2) 1)
        (t 0)))

(defun find_occur (target lst)
  ;; returns the # of occurrence of 'target' in the 'lst'
  (cond ((null lst) 0)
        ((= target (car lst)) (+ 1 (find_occur target (cdr lst))))
        (t (find_occur target (cdr lst)))))

;; test cases
(print (twice '(6 4 3) '(7 6 2 6 4 4 5 3 2 3 4)))
(print (twice '(8 3 2 3 5) '(7 3 8 4 5 8 3 5 4)))
