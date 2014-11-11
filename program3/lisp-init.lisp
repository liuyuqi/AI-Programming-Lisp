(defmacro pp (fn)
   (list 'grindef fn))

(setf *print-level* nil)
(setf *print-length* nil)
(setf *print-miser-width* nil)
(setf *print-pretty* t)

(defun exit () (quit))


(defun vil (&optional lisp::filename)
  "calls vi editor -- loading new version of file upon return"
  (shell (format nil "vi ~a" lisp::filename))
  (load (format nil "~a" lisp::filename)))


(defun vi (&optional lisp::filename)
  "calls vi editor"
  (shell (format nil "vi ~a" lisp::filename)))



(defun emacsl (&optional lisp::filename)
  "calls emacs editor -- loading new version of file upon return"
  (ignore-errors (shell (format nil "emacs ~a" lisp::filename)))
  (load (format nil "~a" lisp::filename)))


(defun emacs (&optional lisp::filename)
  "calls emacs editor"
  (shell (format nil "emacs ~a" lisp::filename)))

; a function useful for printing out test runs
(defun print-eval (l)
  (mapcar (function (lambda (ele)
                      (print ele)
                      (terpri)
                      (print (eval ele))
                      (terpri)
                      (terpri)
                      ))
          l))

; a commented-out example of how to use the function -- this 
; shows how it would be used to turn in a run of the function match
; it prints out the function and then prints and evaluates the two
; tests
;
;(print-eval '(
;; put whatever expressions you want to test here
;; this might include (pp function-name) followed
;; by a number of tests of that function
;; e.g.,
; (pp match)
; (match '(a b c) '(a b c))
; (match '(?x b c) '(a b c))
;))



;--------------------------------------------------------------
; general lisp functions for manipulating symbols
;-------------------------------------------------------------
(defun implode (list)
    ;;; takes a list of single character atoms and combines them into a
    ;;; single atom. Common lisp has no built-in implode function.
    (cond ((null list) nil)
          ((read-from-string
            (concatenate 'string
                         (mapcar #'(lambda (x)
                                     (cond ((numberp x)(digit-char x))
                                           ((character x))))
                                 list))))))

(defun explode (atom)
   ;;; takes an atom and splits it into a list of single character
   ;;; atoms. Common lisp has no built-in explode function.
    (cond ((listp atom) atom)
          (t
           (let ((s (string atom)))
             (explodes s 0 (length s))))))

(defun explodes (str position len)
   ;;; recursive helping function for explode
   (cond ((eq position len) nil)
         ((cons (read-from-string (string (schar str position)))
                (explodes str (1+ position) len)))))

