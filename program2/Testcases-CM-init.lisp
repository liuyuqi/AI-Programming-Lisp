
(defun test1 ()


(format t "The next functions set up the database")
(terpri)
(print-eval '(
(load "CityMapDatabase-14.lisp")))
(print-eval '(
(CityMap-setup)))
(format t "The next testcases call CityMapAgent")
(terpri)
 (print-eval '(
(CityMapAgent '(118 131) "thirtyfirst" '(119 131) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(print-eval '(
(CityMapAgent '(105 101) "first" '(110 101) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(terpri)
(print-eval '(
(CityMapAgent '(109 101) "first" '(111 115) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(terpri)
(print-eval '(
(CityMapAgent '(115 131) "thirtyfirst" '(131 111) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(terpri)
(print-eval '(
(CityMapAgent '(102 122) "twentysecond" '(102 101) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(terpri)
(print-eval '(
(CityMapAgent '(102 122) "twentysecond" '(105 101) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(terpri)
(print-eval '(
(CityMapAgent '(118 131) "thirtyfirst" '(120 123) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM)))

(terpri)
(format t "******************************************************")
(terpri)

)
        

