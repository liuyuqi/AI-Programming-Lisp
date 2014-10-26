;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CityMapAgent Submission: Yuqi Liu.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sample-test ()
; This is an example call to CityMapAgent
   (CityMapAgent '(118 131) "thirtyfirst" '(120 123) 'goal-test-CM? 'successors-CM 'get-goal-estimate-CM))

; States are represented as a 2-tuple giving the x and y coordinates  
; of the current point on the map.
;
; Nodes in the search tree will be represented by atoms 
;     A node for a point on the map will be represented by turning the 
;     string "Node-" followed by the x and y coordinates of the point 
;     into an atom.  
;     This can be done via the intern function.  Thus a node for the 
;     point (120,145) will be gotten via 
;     (intern(concatenate 'string "Node-" "120" "145"))
;     and will appear as |Node-120145| if printed.
; Nodes have the following properties:
;  state - The point on the map that is the current state of the node.  
;          For example, the node |Node-120145| will have as its state  
;          property the 2-tuple (120 145). This is the node's state, 
;          since it is the current location of the agent on the map.
;  road - a string giving the name of the road that the agent is
;         currently on at this state.
;  parent - The predecessor of the node on the best path that has
;           been found so far from start-point to the point 
;           represented by the node.
;  action - The action, such as (point1 point2 road) that was used to 
;           get from point1 to point2 represented by the node, where  
;           road is a string giving the name of the city street,   
;           avenue, or highway that was used to get to the given map 
;           point.
;  arc-cost - the cost of the arc for action used to get to the
;           state represented by the node
;  best-path-cost - The cost of the best known path from the initial 
;                   state to the node.
;  cost-to-goal-estimate - The estimate of the cost to a goal from 
;          the state represented by this  node
;  least-cost-estimate - The overall estimate of the cost from the
;           initial state to goal going through node


;
; CityMapAgent takes six problem-dependant arguments:
;
;     start-point - a point on the map that is the initial state 
;                   for the search, represented as a 2-tuple of
;                   x-coordinate and y-coordinate.
;     road - the name of the road that the agent is on at start-point,
;            represented as a string
;     goal-point - a point on the map that one wishes to reach,
;                  represented as a 2-tuple
;     goal-test? -  a predicate that returns true for goal nodes 
;                   and false for non-goal nodes.
;     get-successors -  a function to compute successors of a state
;                   represented by a node.  The successors are each
;                   represented as (new-state road arc-cost) triples.  
;     get-goal-estimate - a function which takes a point on the map  
;                         and returns an estimate of the distance 
;                         to a goal.point on the map.

; CityMapAgent returns a 2-tuple whose first element is an optimal 
; path from start-point to goal-point represented as a list of 
; actions that are performed to get from start-point to goal-point
; and whose second element is the cost of this path.
;

 (defun CityMapAgent
  (start-point road goal-point goal-test? get-successors 
   get-goal-estimate) 
;create a node for start-point, and find the path to goal-point 
;   using Algorithm A*"
  (defun search-graph (open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; search-graph is the function that iterates through the open list.
  ;     It selects the front node on open list and tests whether 
  ;     its state is the goal.  If so, it gets the best path that has 
  ;     been found to this node, along with the path cost and returns 
  ;     it.  Otherwise it recursively calls search-graph with the new 
  ;     open and closed lists that result from expanding the graph 
  ;     with the successors of the selected node.
  ; returns a 2-element list, containing the sequence of actions
  ;     leading to the goal and the total cost of the path;
  ;     adjacent actions are combined into a single action if
  ;     the adjacent actions are on the same road.
     (cond((null (car open-closed)) nil)
          (t (let((selected-node (caar open-closed)))
                 (terpri)
         
                 (format t 
                    "The nodes, f-values, and actions on open list are ~A" 
                     (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (car open-closed)))
                 (terpri)
                 (format t 
                     "The nodes, f-values, and actions on closed list are ~A" 
                      (mapcar #'(lambda (x)
                              (list x (get x 'least-cost-estimate) 
                                      (get x 'action)))
                              (cadr open-closed)))
                 (terpri) (terpri)
                 (format t "Select a new node from open list")
                 (terpri) 
                (format t "The selected node is ~A" 
                          (caar open-closed))
                (terpri)
                (format t "Check if this node is the goal node")
                (terpri)
                (cond((funcall goal-test? selected-node goal-point)
                          (terpri)
                          (format t "This is the goal node")
                          (terpri)
                          (format t "Here is the list of actions and total path cost in the solution")
                          (terpri)
                          (get-path-and-total-cost selected-node))
                     (t (let ((successors (funcall get-successors
                                                   selected-node)))
                        (format t "This is NOT the goal node")
                        (terpri)
                        (format t "Its successors (and their arc costs) are ~A"
                                  successors)
                        (terpri)

                        (search-graph
                           (expand-graph 
                             successors
                             selected-node
                             (list (cdr (car open-closed))
                                   (cons selected-node 
                                         (cadr open-closed)))
                             get-successors
                             get-goal-estimate 
                             goal-point)))))))))
                         
; create a node for start-city and begin the search
  (search-graph 
   (list(list (create-node start-point road 0 nil 0 nil 
                           get-goal-estimate goal-point))
   nil)))
      
 (defun expand-graph
   (succs parent-node open-closed succ-fn est-goal goal-point)
;(break "entering expand-graph")
        ;; succs is the list of sucessors of parent-node
        ;; each element of succs is a tuple of the form 
        ;;    (new-state road arc-cost) triples such as 
        ;;    ((100 122) "Kingshighway" 15).
	;; expand-graph adds the list of successors of parent to 
        ;;    the graph and to open list.
	;; It must make sure that a successor has not already 
        ;;    been encountered (ie., is not already on open 
        ;;    or closed) and must check for updating the 
        ;;    shortest path if the state has been encountered 
        ;;    before
        ;; returns the resulting 2-tuple giving the open 
        ;;    and closed lists

   (cond ((null succs) open-closed)
	 (t 
;         process the next successor
           (let* ((state (caar succs))
                   (point (caar succs))
                   (node-name 
                      (intern (concatenate 'string 
                                 "Node-" 
                                 (prin1-to-string (car point)) 
                                 (prin1-to-string (cadr point))
                                                  )))
		   (arccost (caddar succs))
                   (action (list (get parent-node 'state)
                                 (caar succs) 
                                 (cadar succs)))
		   (cost (+ (get parent-node 'best-path-cost)
			    arccost)))
              (format t "     The next successor is ~A" (car succs))
              (terpri)
              ;(break "in expand-graph")
              (cond ((and (not (state-on state (cadr open-closed)))
			  (not (state-on state (car open-closed))))
; this successor is not on open or closed list
                       (format t "this successor is not on open or closed list") 
                       (terpri)    
                       (expand-graph (cdr succs)
                                      parent-node
                                     (list (add-to-open 
                                           (create-node (caar succs) 
                                                     (cadar succs)
                                                     (caddar succs)
                                                     parent-node 
                                                     cost 
                                                     action 
                                                     est-goal 
                                                     goal-point)
                                            (car open-closed))
                                         (cadr open-closed))
                                      succ-fn
                                      est-goal
                                      goal-point))
		    ((and (state-on state (car open-closed))
                          (< cost (get node-name 'best-path-cost)))
; this successor is already on open list and we have
;    found a better path to it
                     (format t "**** ON OPEN AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                   (update-node-open node-name
                                                      parent-node
                                                      succ-fn
                                                      cost
                                                      action
                                                      open-closed)
                                    succ-fn
                                    est-goal
                                    goal-point))
                     ((and (state-on state (cadr open-closed))
                           (< cost (get node-name 'best-path-cost)))
; this successor is already on closed list and we have
;    found a better path to it
                     (format t "*** ON CLOSED AND IT HAS A NEW BETTER PATH COST***")
                     (terpri)
                     (expand-graph (cdr succs)
                                    parent-node
                                    (update-node-closed node-name
                                                        parent-node
                                                        succ-fn
                                                        cost
                                                        action
                                                        open-closed)
                                    succ-fn
                                    est-goal
                                    goal-point))
		    (t 
; this successor is already on open or closed and the new path
;   to the node is not better than the existing path
                      (format t "this successor is on open or closed but path is not better")
                      (terpri)
                      (expand-graph (cdr succs)
				    parent-node
				    open-closed 
				    succ-fn
				    est-goal
                                    goal-point)))))))

(defun update-node-open 
  (n parent successor-fn cost-of-short-path action open-closed )
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the open list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  

  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent. 
  ; successor-fn is the parameter giving the function for
  ;   computing successors 
  ; update the properties of node n and, if necessary, its position
  ;  on open list
  ; return the adjusted open-closed list
  (let ((currOpenList (car open-closed))
        (closedList (cadr open-closed))
        (newArcCost (- cost-of-short-path (get parent 'best-path-cost)))
        (old-gValue (get n 'best-path-cost)))
    (setf (get n 'arc-cost) newArcCost)
    (setf (get n 'action) action)
    (setf (get n 'parent) parent)
    (setf (get n 'best-path-cost) cost-of-short-path)
    (setf (get n 'least-cost-estimate) (+ cost-of-short-path (get n 'cost-to-goal-estimate)))
    (list (get-new-open-list currOpenList n) closedList)))

(defun get-new-open-list (openList n)
  ;; re-sort the list with the modified element n
  (let ((listWithoutN (strip openList n)))
    (get-new-open-list-tail listWithoutN n)))

(defun get-new-open-list-tail (listWithoutN n)
  ;; perform an insersion sort of n into listWithoutN.
  (cond ((null listWithoutN) (list n))
        (t (let ((f-value-of-head (get (car listWithoutN) 'least-cost-estimate))
                 (f-value-of-new (get n 'least-cost-estimate)))
             (cond ((<= f-value-of-new f-value-of-head) (cons n listWithoutN))
                   (t (cons (car listWithoutN) (get-new-open-list (cdr listWithoutN) n))))))))

(defun strip (lst n)
  ;; removes the element 'n' from the openList 'lst',
  ;; returns lst without 'n'
  (cond ((null lst) nil)
        ((equal (car lst) n) (cons (car lst) (strip (cdr lst) n)))
        (t (strip (cdr lst) n))))

(defun update-node-closed (n parent successor-fn cost-of-short-path 
                           action open-closed)
  ; open-closed is a 2-element list whose first element is the
  ;   open list and whose second element is the closed list
  ; node n is on the closed list.
  ; a new shortest path from the initial state to node n has 
  ;   been found.
  ; parent is the parent node of node n on this new path.
  ; action is the action that moved from parent to node n.  
  ; cost-of-short-path is the cost of this new path from the
  ;   initial state to node n and goes through parent.  
  ; successor-fn is the parameter giving the function for
  ;   computing successors
  ; update the properties of node n and, if necessary, its
  ;   descendants on open and closed lists.
  ; return the adjusted open-closed list
  (let ((curr-open-list (car open-closed))
        (curr-closed-list (cadr open-closed))
        (new-arc-cost (- cost-of-short-path (get parent 'best-path-cost)))
        (old-gValue (get n 'best-path-cost)))
    (setf (get n 'arc-cost) new-arc-cost)
    (setf (get n 'action) action)
    (setf (get n 'parent) parent)
    (setf (get n 'best-path-cost) cost-of-short-path)
    (setf (get n 'least-cost-estimate) (+ cost-of-short-path (get n 'cost-to-goal-estimate)))
    (list (sort-open-list (get-node-list (change-f-g-chain (funcall successor-fn n)
                                                           curr-open-list 
                                                           successor-fn 
                                                           NIL)) 
                          curr-open-list)
          curr-closed-list)))

(defun sort-open-list (out-of-order-lst open-lst)
  ;; sort the open list, changing the position of elems in out-of-order-lst only.
  ;; BUGGY~
  (cond ((null out-of-order-lst) open-lst)
        (t (let* ((head (car out-of-order-lst))
                  (new-open-lst (get-new-open-list open-lst head)))
             (cond ((null (cdr out-of-order-lst)) new-open-lst)
                   (t (sort-open-list (cdr out-of-order-lst) new-open-lst)))))))

(defun get-node-list (suc-list)
  ;; converts successor list into the form of list of nodes.
  (cond ((null suc-list) NIL)
        (t (let* ((head-of-lst (car suc-list))
                  (head-xy (car head-of-lst))
                  (node-obj (intern (concatenate 'string
                                                 "Node-"
                                                 (prin1-to-string (car head-xy))
                                                 (prin1-to-string (cadr head-xy))))))
             (cond ((null (cdr suc-list)) (list node-obj))
                   (t (cons node-obj (get-node-list (cdr suc-list)))))))))

(defun change-f-g-chain (to-change-lst open-list successor-fn changed-lst)
  ;; change the f and g value of all descendents of nodes in to-change-list.
  ;; Returns a list including the nodes that are on the open list.
  (cond ((null to-change-lst) NIL)
        (t (let* ((head-to-change (car to-change-lst))
                  (head-xy (car head-to-change))
                  (head-node (intern (concatenate 'string "Node-" 
                                                  (prin1-to-string (car head-xy))
                                                  (prin1-to-string (cadr head-xy)))))
                  (suc-of-head (funcall successor-fn head-node))
                  (parent-gValue (get (get head-node 'parent) 'best-path-cost))
                  (self-new-gValue (+ parent-gValue (get head-node 'arc-cost)))
                  (self-new-fValue (+ self-new-gValue (get head-node 'cost-to-goal-estimate))))
             (setf (get head-node 'best-path-cost) self-new-gValue)
             (setf (get head-node 'least-cost-estimate) self-new-fValue)
             (change-chain-tail head-to-change head-xy suc-of-head open-list to-change-lst successor-fn changed-lst)))))

(defun change-chain-tail (head-to-change head-xy suc-of-head open-list to-change-lst successor-fn changed-lst)
  ;; does the remaining job for change-f-g-chain function.
  (cond ((state-on head-xy open-list)
            (let ((next-iter (change-f-g-chain (cdr to-change-lst) open-list successor-fn (cons head-to-change changed-lst))))
              (cond ((null next-iter) (list head-to-change))
                    (t (cons head-to-change next-iter)))))
        (t (change-f-g-chain (get-new-to-change-list suc-of-head (cdr to-change-lst) changed-lst)
                             open-list 
                             successor-fn
                             (cons head-to-change changed-lst)))))

(defun get-new-to-change-list (suc-of-head old-to-change-lst changed-lst)
  ;; Returns a list with elements in suc-of-head all eliminated by the following rule:
  ;;   if they are in the old-to-change-lst or changed-lst, then eliminate those elements.
  (let ((new-successors (eliminate-lst-lst-orig changed-lst old-to-change-lst suc-of-head)))
    (cond ((null new-successors) old-to-change-lst)
          (t (append new-successors old-to-change-lst)))))

(defun eliminate-lst-lst-orig (lst1 lst2 orig-lst)
  ;; Removes lst1 from lst2
  ;; Returns the modified lst2
  (cond ((null orig-lst) NIL)
        ((or (in-list (car orig-lst) lst1) (in-list (car orig-lst) lst2))
             (eliminate-lst-lst-orig lst1 lst2 (cdr orig-lst)))
        (t (cons (car orig-lst) (eliminate-lst-lst-orig lst1 lst2 (cdr orig-lst))))))

(defun in-list (elem lst)
  ;; Returns T if elem is in lst, otherwise return NIL
  (cond ((null lst) NIL)
        ((equal elem (car lst)) T)
        (t (in-list elem (cdr lst)))))
 
(defun state-on (state lst)
;(break "entering state-on")
; state is a state represented as a 2-tuple giving the
;   coordinates of the point represented by the state
; lst is an open or closed list
; return true if a node on lst has this point as its state
    (let* ((head-node (car lst))
           (state-of-head (get head-node 'state)))
        (cond ((equal state state-of-head) t)
              ((null (cdr lst)) nil)
              (t (state-on state (cdr lst))))))
       
(defun add-to-open (n open)
; n is a node and open is the open list
; add n to the open list in the correct position 
; return the new open list
      (cond ((null open) (list n))
            (t (let ((fValue (get n 'least-cost-estimate))
                     (fValueHead (get (car open) 'least-cost-estimate)))
                 (cond ((> fValue fValueHead) (cons (car open) (add-to-open n (cdr open))))
                       (t (cons n open)))))))

(defun create-node 
  (point road arc-cost parent cost-of-short-path action est-goal goal-point)
  ; point is a 2-tuple representing a point on the map
  ; create a new node with this point as its state and
  ;   with the appropriate properties
  ; road is a string giving the name of the road that the
  ;   agent is on at point
  ; parent is the parent node.
  ; action is the action that moved from parent to point.  
  ; cost-of-short-path is the cost of the path from the
  ;   initial state to the state represented by this new
  ;   node and goes through parent.
  ; goal-point is a 2-tuple representing the goal
  ; est-goal is a parameter giving the function for estimating
  ;   the cost of getting to the goal from this new node 
  ; create a new node with the appropriate properties
  ; return the created node.
(let ((node (intern (concatenate 'string 
                                 "Node-" 
                                 (prin1-to-string (car point))
                                 (prin1-to-string (cadr point))
                                 ))))
  (setf (get node 'state) point)
  (setf (get node 'road) road)
  (setf (get node 'arc-cost) arc-cost)
  (setf (get node  'parent) parent)
  (setf (get node 'action) action)
  (setf (get  node 'best-path-cost) cost-of-short-path)
  (setf (get node 'cost-to-goal-estimate) (funcall est-goal point goal-point)) 
  (setf (get  node 'least-cost-estimate)
        (+ cost-of-short-path (get node 'cost-to-goal-estimate)))
  node))

(defun get-path-and-total-cost (node)
; node is a node in the graph
; return a list consisting of two elements: the path (in terms of 
;    successive actions) that was taken to get to node and and 
;   cost of that path
    (let* ((result (get-path-and-total-cost-new node))
           (path (car result))
           (path-cost (cadr result)))
      (list (path-condense path) path-cost)))

(defun get-path-and-total-cost-new (node)
  ;; returns a list consisting of two elements
  ;; 1. path
  ;; 2. cost of path
    (cond ((null (get node 'parent)) (list nil 0))
          (t (let* ((result (get-path-and-total-cost-new (get node 'parent)))
                    (path-list (car result))
                    (path-cost (cadr result)))
               (list (append path-list (list (get node 'action))) 
                     (+ path-cost (get node 'arc-cost)))))))

(defun path-condense (path-list)
  ;; argument path-list: a list of actions.
    (cond ((null (cdr path-list)) path-list)
          ((equal (get-kth-elem (car path-list) 3) 
                  (get-kth-elem (cadr path-list) 3))
              (path-condense (cons (combine (car path-list) (cadr path-list)) (cddr path-list))))
          (t (cons (car path-list) (path-condense (cdr path-list))))))

(defun combine (elem1 elem2)
  ;; returns a combined tuple of elem1 and elem2
    (list (car elem1) (cadr elem2) (caddr elem1)))

(defun get-kth-elem (lst k)
  ;; returns the kth element of the list 'lst'
    (cond ((= k 1) (car lst))
          (t (get-kth-elem (cdr lst) (- k 1)))))

(defun successors-CM (node)
  ;; a wrapper and debugger function of successor-CM
  (successors-CM-new node))

(defun successors-CM-new (node)
; node is a node in the search graph
; return a list of the successors of the state represented by
;   this node, with each successor given as
;   (new-point road arc-cost ) triples, such as 
;   ((100 122) "Kingshighway" 15)
    (let ((intersec-obj (on-intersection node))
          (street-obj (on-street node))
          (avenue-obj (on-avenue node)))
        (cond ((not (null intersec-obj)) (get-suc-intersec node intersec-obj))
              ((not (null street-obj)) (get-suc-street node street-obj))
              ((not (null avenue-obj)) (get-suc-ave node avenue-obj))
              (t (successors-CM-new node)))))

(defun on-intersection (node)
  ;; determines whether node is on intersection.
  ;; If yes, return the atom of intersection; Otherwise, return NIL.
  (let* ((xy-co (get node 'state))
         (x-co (car xy-co))
         (y-co (cadr xy-co))
         (intersec-atom (intern (concatenate 'string (prin1-to-string x-co) (prin1-to-string y-co)))))
    (cond ((equal (get intersec-atom 'type) 'intersection)
                intersec-atom)
          (t NIL))))

(defun get-suc-intersec (node intersec-obj)
  ;; wrapper function of get-suc-intersec-new
    (get-suc-intersec-new node (get intersec-obj 'options)))

(defun get-suc-intersec-new (node option-list)
  ;; returns a list of triples who are successors of node (which is on intersec)
  ;; triple format: (new-point road arc-cost)
  (let* ((name-of-head (car option-list))
         (road-atom (intern name-of-head))
         (type-of-head (get road-atom 'type)))
    (cond ((equal type-of-head 'city-street)
            (cond ((null (cdr option-list)) (get-suc-street node road-atom))
                  (t (append (get-suc-street node road-atom) (get-suc-intersec-new node (cdr option-list))))))
          ((equal type-of-head 'avenue)
            (cond ((null (cdr option-list)) (get-suc-ave node road-atom))
                  (t (append (get-suc-ave node road-atom) (get-suc-intersec-new node (cdr option-list))))))
          ((equal type-of-head 'highway)
             (let ((a-points (get road-atom 'access-points)))
               (cond ((null (cdr option-list)) (get-suc-highway node road-atom a-points))
                     (t (append (get-suc-highway node road-atom a-points) 
                                (get-suc-intersec-new node (cdr option-list))))))))))

(defun get-suc-highway (node hway a-points)
  ;; returns a list of triples (new-point road arc-cost) who are successors of node.
  ;; The node is on highway atom 'hway' with access points 'a-points'.
  (let ((hway-name (symbol-name hway))
        (node-state (get node 'state)))
    (cond ((equal node-state (car a-points))
              (list (list (cadr a-points) hway-name (get-hway-cost node-state (cadr a-points)))))
          ((and (equal node-state (cadr a-points)) (not (null (cddr a-points))))
              (list (list (car a-points) hway-name (get-hway-cost node-state (car a-points)))
                    (list (caddr a-points) hway-name (get-hway-cost node-state (caddr a-points)))))
          ((and (equal node-state (cadr a-points)) (null (cddr a-points)))
              (list (list (car a-points) hway-name (get-hway-cost node-state (cadr a-points)))))
          (t (get-suc-highway node hway (cdr a-points))))))

(defun get-hway-cost (state1 state2)
  ;; Returns the arc cost between state1 and state2 on highway.
  (let ((diff-x (- (car state1) (car state2)))
        (diff-y (- (cadr state1) (cadr state2))))
    (* 1.5 (sqrt (+ (* diff-x diff-x) (* diff-y diff-y))))))

(defun on-avenue (node)
  ;; determines whether node is on avenues
  ;; If yes, return the atom of avenue; Otherwise, return NIL.
  (let* ((road-name (get node 'road))
         (road-atom (intern road-name))
         (road-type (get road-atom 'type)))
    (cond ((equal road-type 'avenue) road-atom)
          (t NIL))))

(defun get-suc-ave (node ave-obj)
  ;; returns a list of triples (new-point road arc-cost) who are successors of node.
  ;; The node is on avenue 'ave-obj' but not on an intersection.
    (let* ((x-y (get node 'state))
           (x-coord (car x-y))
           (y-coord (cadr x-y))
           (up-neighbor (get-ave-neighbor (+ y-coord 1) ave-obj))
           (down-neighbor (get-ave-neighbor (- y-coord 1) ave-obj))
           (ave-name (get ave-obj 'name)))
      (cond ((and (null up-neighbor) (not (null down-neighbor)))
                (list (list down-neighbor ave-name 2)))
            ((and (not (null up-neighbor)) (null down-neighbor))
                (list (list up-neighbor ave-name 2)))
            ((and (not (null up-neighbor)) (not (null down-neighbor)))
                (list (list down-neighbor ave-name 2) (list up-neighbor ave-name 2)))
            (t NIL))))

(defun get-ave-neighbor (y-coord ave-obj)
  ;; returns (x, y-coord) if it is on ave-obj.
  ;; Otherwise, return NIL.
    (let ((x-coord (get ave-obj 'x-coord))
          (y-start (get ave-obj 'y-coord-start))
          (y-end (get ave-obj 'y-coord-end)))
      (cond ((or (< y-coord y-start) (> y-coord y-end)) NIL)
            (t (list x-coord y-coord)))))

(defun on-street (node)
  ;; determines whether node is on city-street.
  ;; If yes, return the atom of street; Otherwise, return NIL.
  (let* ((road-name (get node 'road))
         (road-atom (intern road-name)))
    (cond ((equal (get road-atom 'type) 'city-street) road-atom)
          (t NIL))))

(defun get-suc-street (node street-obj)
  ;; returns a list of triples (new-point road arc-cost) who are successors of node.
  ;; The node is on street 'street-obj' but not on an intersection.
    (let* ((x-y (get node 'state))
           (x-coord (car x-y))
           (y-coord (cadr x-y))
           (left-neighbor (get-street-neighbor (- x-coord 1) street-obj))
           (right-neighbor (get-street-neighbor (+ x-coord 1) street-obj))
           (street-name (get street-obj 'name)))
      (cond ((and (null left-neighbor) (not (null right-neighbor)))
                (list (list right-neighbor street-name 2)))
            ((and (not (null left-neighbor)) (null right-neighbor))
                (list (list left-neighbor street-name 2)))
            ((and (not (null left-neighbor)) (not (null right-neighbor)))
                (list (list left-neighbor street-name 2) (list right-neighbor street-name 2))))))

(defun get-street-neighbor (x-coord street-obj)
  ;; Test whether street neighbor is on the street (in the bound of street-obj).
  ;; If found, returns the (x, y) of neighbor; Otherwise return NIL.
    (let ((x-start (get street-obj 'x-coord-start))
          (x-end (get street-obj 'x-coord-end))
          (y-coord (get street-obj 'y-coord)))
      (cond ((or (< x-coord x-start) (> x-coord x-end)) NIL)
            (t (list x-coord y-coord)))))

(defun goal-test-CM? (node goal-point)
; node is a node and goal-point is a 2-tuple giving the coordinates
;    of the goal22 point on the map
; return true if the state for this node is goal-point
    (cond ((equal (get node 'state) goal-point) t)
          (t NIL)))

(defun get-goal-estimate-CM (point goal-point)
; point and goal-point are both 2-tuples representing points 
;   on the map 
; return an estimate of the cost of getting from point to goal-point
    (get-distance point goal-point))

(defun get-distance (point1 point2)
  ;; gets the distance between point1 and piont2
    (let ((diff-x (- (car point2) (car point1)))
          (diff-y (- (cadr point2) (cadr point1))))
      (sqrt (+ (* diff-x diff-x) (* diff-y diff-y)))))

