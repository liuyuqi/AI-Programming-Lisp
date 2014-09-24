(defun inherit-get (object property)
  ;; tries to get the property of the object.
  ;; If succeed, returns the value of the property,
  ;;   otherwise, returns the property of isa(object).
    (let ((value (get object property 'NA))
          (parent (get object 'isa)))
        (cond ((null object) nil)
              ((not (equal value 'NA)) value)
              (t (inherit-get parent property)))))

;; test cases
(setf (get 'bird 'wings) 2)
(setf (get 'canary 'isa) 'bird)
(print (inherit-get 'canary 'wings))
