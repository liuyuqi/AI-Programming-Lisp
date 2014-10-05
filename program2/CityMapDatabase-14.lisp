; -------------------------------------------------------------------
; Database Management
; -------------------------------------------------------------------

(defun CityMap-setup ()
;set up the database for the City-Map Problem
(defun setup-intersections(lst)
; lst is a list of lists of intersection data where each sublist is 
;   in the form <x-coordinate> <y-coordinate> followed by the names 
;   of roads at this intersection
   (cond ((null lst) ())
         (t (let ((int (intern (prin1-to-string (+ (* (caar lst)1000) (cadar lst))))))
 ;              (break "here")
               (setf (get int 'type) 'intersection)
               (setf (get int 'options) (cddr (car lst)))
               (setf (get int 'x-coord) (caar lst))
               (setf (get int 'y-coord) (cadar lst)))
             (setup-intersections (cdr lst)))))
(defun setup-city-streets(lst)
; lst is a list of lists of city-street data where each sublist is 
;   in the form <name> <y-coord> (x-coord-start> <x-coord-end>
  (cond ((null lst) nil)
        (t (let ((street (intern (caar lst))))
              (setf (get street 'type) 'city-street)
              (setf (get street 'name) (caar lst))
              (setf (get street 'y-coord) (cadr (car lst)))
              (setf (get street 'x-coord-start) (caddr (car lst)))
              (setf (get street 'x-coord-end) (cadddr (car lst))))
           (setup-city-streets (cdr lst)))))
(defun setup-avenues(lst)
; lst is a list of lists of avenue data where each sublist is 
;   in the form <name> <x-coord> <y-coord-start> <y-coord-end>
  (cond ((null lst) nil)
        (t (let ((avenue (intern (caar lst))))
              (setf (get avenue 'type) 'avenue)
              (setf (get avenue 'name) (caar lst))
              (setf (get avenue 'x-coord) (cadr (car lst)))
              (setf (get avenue 'y-coord-start) (caddr (car lst)))
              (setf (get avenue 'y-coord-end) (cadddr (car lst))))
           (setup-avenues (cdr lst)))))
(defun setup-highways(lst)
; lst is a list of lists of highway data, where each sublist is 
;   in the form <name> followed by 2-tuples giving the access 
;   points, in order, for the highway
  (cond ((null lst) nil)
        (t (let ((highway (intern (caar lst))))
              (setf (get highway 'type) 'highway)
              (setf (get highway 'name) (caar lst))
              (setf (get highway 'access-points) (cdr (car lst))))
           (setup-highways (cdr lst)))))
              
(let ((city-streets 
       '(("first" 101 100 150)
         ("fourth" 104 122 127)
         ("eleventh" 111 119 150)
         ("fifteenth" 115 119 122)
         ("sixteenth" 116 105 122)
         ("eighteenth" 118 111 120)
         ("twentysecond" 122 100 122)
         ("twentyfourth" 124 113 120)
         ("twentysixth" 126 103 120)
         ("thirtyfirst" 131 110 140)))
       (avenues
        '(("ave-111" 111 101 150)
          ("ave-113" 113 122 150)
          ("ave-116" 116 126 150)
          ("ave-117" 117 118 124)
          ("ave-119" 119 111 115)
          ("ave-120" 120 118 132)
          ("ave-122" 122 101 122)
          ("ave-127" 127 101 115)))
       (highways
         '(("Ashway" (116 131) (120 127) (131 111))
           ("Kingshighway" (100 122) (108 116) (111 113) (127 102))
           ("Concordway" (122 117) (126 101))
           ("Marway" (102 101) (108 116) (111 120) (115 122) (120 127))
           ("Kirkway" (107 126) (111 132))))
       (intersections
        '((111 101 "first" "ave-111")
          (120 131 "thirtyfirst" "ave-120")
          (116 131 "ave-116" "Ashway")
          (120 127 "ave-120" "Ashway" "Marway")
          (131 111 "eleventh" "Ashway")
          (100 122 "twentysecond" "Kingshighway")
          (108 116 "sixteenth" "Kingshighway" )
          (111 113 "ave-111" "Kingshighway")
          (127 102 "ave-127" "Kingshighway")
          (122 117 "ave-122" "Concordway")
          (126 101 "first" "Concordway")
          (102 101 "first" "Marway" )
          (108 116 "sixteenth" "Marway")
          (111 120 "ave-111" "Marway")
          (115 122 "twentysecond" "Marway")
          (107 126 "twentysixth" "Kirkway")
          (111 132 "ave-111" "Kirkway")  
          (122 101 "first" "ave-122")
          (127 101 "first" "ave-127")
          (122 104 "fourth" "ave-122")
          (127 104 "fourth" "ave-127")
          (119 111 "eleventh" "ave-119")
          (122 111 "eleventh" "ave-122")
          (127 111 "eleventh" "ave-127")
          (119 115 "fifteenth" "ave-119")
          (122 115 "fifteenth" "ave-122")
          (111 116 "sixteenth" "ave-111")
          (122 116 "sixteenth" "ave-122")
          (111 118 "eighteenth" "ave-111")
          (117 118 "eighteenth" "ave-117")
          (120 118 "eighteenth" "ave-120")
          (113 122 "twentysecond" "ave-113")
          (117 122 "twentysecond" "ave-117")
          (120 122 "twentysecond" "ave-120")
          (113 124 "twentyfourth" "ave-113")
          (117 124 "twentyfourth" "ave-117")
          (120 124 "twentyfourth" "ave-120")
          (116 126 "twentysixth" "ave-116")
          (120 126 "twentysixth" "ave-120")
          
)))
    (setup-intersections intersections)
    (setup-city-streets city-streets)
    (setup-avenues avenues)
    (setup-highways highways)))



