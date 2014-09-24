(defun outside (int1 int2 tree)
  ;; returns a list of all elements in the ordered binary tree
  ;;   that are not between [int1, int2];
  ;; The returned list must be in order.
    (prune int1 int2 NIL NIL tree))

(defun prune (int1 int2 range1 range2 tree)
  ;; do the actual work
    (cond ((null tree) NIL)
          ;; the root has no child
          ((and (null (cadr tree)) (null (caddr tree)))
                (cond ((or (< (car tree) int1) (> (car tree) int2))
                            (list (car tree)))
                      (t NIL)))
          ;; the root has only left child
          ((and (not (null (cadr tree))) (null (caddr tree)))
                (left_only int1 int2 range1 range2 tree))
          ;; the root has only right child
          ((and (null (cadr tree)) (not (null (caddr tree))))
                (right_only int1 int2 range1 range2 tree))
          ;; the root has both left and right child
          ((and (not (null (cadr tree))) (not (null (caddr tree))))
                (left_right int1 int2 range1 range2 tree))))

(defun in_range (int1 int2 elem)
  ;; tests whether elem is in range [int1, int2].
  ;; If is, return t; Otherwise, return NIL.
    (cond ((and (>= elem int1) (<= elem int2)) T)
          (t NIL)))

(defun left_only (int1 int2 range1 range2 tree)
  ;; deals with the case in which the tree only has the left subtree
    (let ((left (is_desired int1 int2 range1 (car tree) (cadr tree))))
      (cond ((null left) 
                (cond ((in_range int1 int2 (car tree)) NIL)
                      (t (list (car tree)))))
            (t (cond ((in_range int1 int2 (car tree))
                    (prune int1 int2 range1 (car tree) (cadr tree)))
                     (t (append (prune int1 int2 range1 (car tree) (cadr tree)) (list (car tree)))))))))

(defun right_only (int1 int2 range1 range2 tree)
  ;; deals with the case in which the tree only has the right subtree
    (let ((right (is_desired int1 int2 (car tree) range2 (caddr tree))))
      (cond ((null right) 
                (cond ((in_range int1 int2 (car tree)) NIL)
                      (t (list (car tree)))))
            (t (cond ((in_range int1 int2 (car tree))
                      (prune int1 int2 (car tree) range2 (caddr tree)))
                     (t (append (list (car tree)) (prune int1 int2 (car tree) range2 (caddr tree)))))))))

(defun left_right (int1 int2 range1 range2 tree)
  ;; deals with the case in which the tree only has the both the left and right subtrees
  (let ((left (is_desired int1 int2 range1 (car tree) (cadr tree)))
        (right (is_desired int1 int2 (car tree) range2 (caddr tree)))
        (no_root (in_range int1 int2 (car tree))))
    (cond ((and (not (null left)) (not (null right)))
             (cond ((null no_root) 
                        (append (prune int1 int2 range1 (car tree) left) (list (car tree)) (prune int1 int2 (car tree) range2 right)))
                   (t (append (prune int1 int2 range1 (car tree) left) (prune int1 int2 (car tree) range2 right)))))

          ((and (not (null left)) (null right))
             (cond ((null no_root) (append (prune int1 int2 range1 (car tree) left) (list (car tree))))
                   (t (prune int1 int2 range1 (car tree) left))))
          ((and (null left) (not (null right)))
             (cond ((null no_root) (append (list (car tree)) (prune int1 int2 (car tree) range2 right)))
                   (t (prune int1 int2 (car tree) range2 right)))))))

(defun is_desired (int1 int2 range1 range2 tree)
  ;; determines whether this tree is desired.
  ;; If yes, return the whole tree; Otherwise, return NIL
    (cond ((or (null range1) (null range2)) tree)
          ((and (>= range1 int1) (<= range2 int2)) NIL)
          (t tree)))

;; test cases
(print (outside 18 50 '(35 (6 (3 () ()) (17 () (19 () ()))) (72 (41 () ()) ()))))
(print (outside 57 75 '(42 () (60 (55 () (58 () ())) (80 (73 () ()) (90 () ()))))))

