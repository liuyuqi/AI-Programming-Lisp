(defun traverse_in_order (tree)
  ;; performs an in-order traversal of tree
    (cond ((null tree) NIL)
          ;; the root has no child
          ((and (null (cadr tree)) (null (caddr tree)))
                (list (car tree)))
          ;; the root has only left child
          ((and (not (null (cadr tree))) (null (caddr tree)))
                (append (traverse_in_order (cadr tree)) (list (car tree))))
          ;; the root has only right child
          ((and (null (cadr tree)) (not (null (caddr tree))))
                (append (list (car tree)) (traverse_in_order (caddr tree))))
          ;; the root has both left and right child
          ((and (not (null (cadr tree))) (not (null (caddr tree))))
                (append (traverse_in_order (cadr tree)) (list (car tree)) (traverse_in_order (caddr tree))))))

;; test cases
(print (traverse_in_order '(35 (6 (3 () ()) (17 () (19 () ()))) (72 (41 () ()) ()))))
(print (traverse_in_order '(42 () (60 (55 () (58 () ())) (80 (73 () ()) (90 () ()))))))
