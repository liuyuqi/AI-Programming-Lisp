
; ActionRecipes is a global variable, containing a list of action_recipe 
;   structures giving the preconditions and effects of actions

; goal_list is a global variable that is a list of structures of
;    type literal

; initial_state_list is a global variable that is a list of structures 
;    of type literal

; Assumptions: actions, preconditions, and effects have no parameters

(defun Task-planner ()
; goal_list and initial_state_list are global variables 
; goal_list is a list of structures of type literal that constitute the goal
; initial_state_list is a list of structures of type literal that are true 
;   in the initial world
; ActionRecipes is a global variable containing a list of action_recipe
;   structures giving the action definitions for this problem
; Return a partially ordered plan for achieving goal_list
(let* ((start_node
         (make-plan_node 
              :name (gensym)
              :act (make-action 
                       :act_name 'START
                       :parameters nil)
              :preconditions nil
              :effects 
                 initial_state_list))
       (goal_node
          (make-plan_node
               :name (gensym)
               :act (make-action
                       :act_name 'FINISH
                       :parameters nil)
               :preconditions
                  goal_list
               :effects nil))
        (init_partial_plan
           (make-partial_plan 
                :plan_nodes (list start_node goal_node)
                :causal_links nil
                :order_constraints
                   (list (make-order_constraint
                            :before_node start_node
                            :after_node goal_node))
                :open_precs
                   (mapcar #'(lambda(x)(make-open_precondition
                                         :precondition x
                                         :node goal_node))
                           (plan_node-preconditions goal_node)))))
   (search-graph (list init_partial_plan))))
          
(defun search-graph (open_list)
; open_list is a list of partial plans that remain to be expanded
; outpit the results of finding a plan that achieves the propositions
;    in the global variable goal_list; the propositions in
;    the global variable initial_state_list describe what is
;    true in the initial state.  The initial state only contains
;    propositions that are needed for the problem.  Do not assume 
;    anything about other propositions.
   (cond ((null open_list) (princ "**** FAILURE****") 'FAILURE)
         (t (let ((goal (complete-plan? open_list)))
              (if goal (output-results goal)
                       (let ((part_plan (select open_list)))
                         (search-graph (process-partial-plan 
                                         part_plan
                                         (remove-from-open part_plan open_list)))))))))

(defun complete-plan? (open_list)
; open_list is a list of structures of type partial_plan
; return true if any of these is a complete plan --- ie., a plan
;   with no open preconditions
    (cond ((null open_list) NIL)
          ((null (partial_plan-open_precs (car open_list)))
                (car open_list))
          (t (complete-plan? (cdr open_list)))))

(defun select (open_list)
; selects the partial plan on open_list that has the fewest actions
; and returns it
    (cond ((null (cdr open_list)) (car open_list))
          (t (let* ((first_plan_action_num (get-number-of-nodes (partial_plan-plan_nodes (car open_list))))
                    (sec_plan_action_num (get-number-of-nodes (partial_plan-plan_nodes (cadr open_list)))))
               (cond ((>= first_plan_action_num sec_plan_action_num)
                        (select (cdr open_list)))
                     (t (select (cons (car open_list) (cddr open_list)))))))))

(defun get-number-of-nodes (node_list)
  ;; returns the number of nodes in node_list
    (cond ((null node_list) 0)
          (t (+ 1 (get-number-of-nodes (cdr node_list))))))


(defun remove-from-open (part_plan open_list)
; open_list is a list of structures of type partial_plan
; part_plan is a structure of type partial_plan
; returns the result of removing part_plan from open_list
   (cond((null open_list) nil)
        ((equal part_plan (car open_list))
          (cdr open_list))
        (t (cons (car open_list) (remove-from-open part_plan (cdr open_list))))))

(defun select-prec (precs)
; precs is a list of structures of type open_precondition
; select one of them and return it
(car precs))



(defun process-partial-plan (part_plan open_list)
(setf *print-level* nil)
(setf *print-length* nil)
(terpri)
(terpri)
;
;(princ "*** The partial plan that I have selected to work on is the following:")
;(terpri)(terpri)
;(pprint part_plan)
;(terpri)
;(terpri)
;(princ "Here is what this partial plan represents")
;(terpri)
;(terpri)
;(output-results part_plan)
;(break "have entered process-partial-plan")
; part_plan is a partial plan that is to be worked on
; open_list is a list of the other partial plans that have been constructed
; return a new open_list that consists of the partial plans currently
;   on open_list along with those generated by addressing one of the
;   open preconditions of part_plan
    (let*((open_prec (select-prec (partial_plan-open_precs part_plan)))
          (action_nodes_existing (find-existing-nodes-with-action 
                                (open_precondition-precondition open_prec)
                                (partial_plan-plan_nodes part_plan)))
          (action_nodes_new (find-new-nodes-with-action
                                (open_precondition-precondition open_prec)
                                ACTION_RECIPES)))
       (expand-partial-plan-with-new-actions 
          part_plan
          open_prec              
          (expand-partial-plan-with-existing-actions 
                part_plan
                open_prec
                open_list 
                action_nodes_existing) 
          action_nodes_new)))

(defun find-existing-nodes-with-action (literal plan_nodes)
; literal is a structure of type literal
; plan_nodes is a list of plan_nodes
; return a list of those members of plan_nodes with an effect matching literal
   (cond ((null plan_nodes) nil)
         ((literal-is-member-of literal (plan_node-effects (car plan_nodes)))
             (cons (car plan_nodes)
                   (find-existing-nodes-with-action 
                           literal
                          (cdr plan_nodes))))
         (t (find-existing-nodes-with-action literal (cdr plan_nodes)))))

(defun find-new-nodes-with-action (literal action_recipes)
; literal is a structure of type literal
; action_recipes is a list of structures of type action_recipe
; return a list of newly constructed plan nodes for each new action that has 
;    an effect matching literal
; a list of available action definitions is given in the global
;    variable action_recipes 
   (cond ((null action_recipes) nil)
         ((is-effect-of-action-recipe literal (car action_recipes))
              (cons (make-plan_node
                      :name (gensym)
                      :act (action_recipe-act (car action_recipes))
                      :preconditions (action_recipe-preconditions
                                          (car action_recipes))
                      :effects (action_recipe-effects
                                          (car action_recipes)))
                    (find-new-nodes-with-action 
                          literal (cdr action_recipes))))
         (t (find-new-nodes-with-action literal (cdr action_recipes)))))

(defun expand-partial-plan-with-existing-actions 
   (part_plan open_prec open_list action_nodes_existing)
; part_plan is a structure of type partial_plan
; open_prec is a structure of type open_precondition
; open_list is a list of partial plans
; action_nodes_existing is a list of structures of type plan_node
;    that are already in part_plan and which represent an action whose
;    effect matches open_prec
; return open_list with the addition of new partial plans (structures of 
;    type partial_plan) that represent expansions of part_plan that satisfy 
;    open_prec
; any new partial plans added to open_list must be consistent
;(print "in function expand-partial-plan-with-existing-actions, open list is now length: ")
;(print (length open_list))
    (cond ((null action_nodes_existing) open_list)
          (t (let* ((node_Aj (open_precondition-node open_prec))
                    (node_Ai (car action_nodes_existing))
                    (new_part_plan (set-new-partial-plan part_plan open_prec node_Ai node_Aj))
                    (conflict_list (consistent? new_part_plan)))
             (cond ((equal T conflict_list) (cons new_part_plan open_list))
                   (t (let ((new_open_list (try-to-revise-inconsistent-plan (list (list new_part_plan conflict_list))
                                                                                  open_list)))
                        (cond ((not (equal open_list new_open_list)) new_open_list)
                              (t ;(print "(expand-partial-plan-with-existing-actions) trying other existing options... ") 
                                 (expand-partial-plan-with-existing-actions part_plan
                                                                            open_prec
                                                                            open_list
                                                                            (cdr action_nodes_existing)))))))))))

(defun set-new-partial-plan (old_part_plan p node_Ai node_Aj)
  ;; Returns a structure of type partial_plan with the following updates:
  ;; 1. remove p from the open_precondition of part plan;
  ;; 2. Adds causal-link node_Ai -> node_Aj (p);
  ;; 3. Adds order constraint node_Ai < node_Aj;
  (let* ((new_open_precs (remove-precondition p (partial_plan-open_precs old_part_plan)))
         (new_causal_links (cons (create-causal-link node_Ai node_Aj p)
                                 (partial_plan-causal_links old_part_plan)))
         (new_order_constraints (remove-duplicate-order-constraint 
                                    (cons (create-order-constraint node_Ai node_Aj)
                                          (partial_plan-order_constraints old_part_plan))))
         (plan_nodes (partial_plan-plan_nodes old_part_plan)))
    (create-new-part-plan plan_nodes new_causal_links new_order_constraints new_open_precs)))

(defun create-causal-link (node_Ai node_Aj p)
  ;; Returns a new structure of type causal_link where:
  ;;   plan_node_1 = node_Ai, plan_node_2 = node_Aj, protected_prop = p.
    (make-causal_link :plan_node_1 node_Ai
                      :plan_node_2 node_Aj
                      :protected_prop (open_precondition-precondition p)))

(defun create-order-constraint (node_Ai node_Aj)
  ;; Returns a new structure of type order_constraint where:
  ;;   before_node = node_Ai, after_node = node_Aj.
    (make-order_constraint :before_node node_Ai
                           :after_node node_Aj))

(defun create-new-part-plan (pnodes clinks oconstraints oprecs)
  ;; Returns a new structure of type partial_plan where:
  ;;   plan_nodes = pnodes, causal_links = clinks, order_constraints = oconstraints, open_precs = oprecs.
    (make-partial_plan :plan_nodes pnodes
                       :causal_links clinks
                       :order_constraints oconstraints
                       :open_precs oprecs))

(defun try-to-revise-inconsistent-plan (inconsistent_list open_list)
  ;; tries to revise a conflict plan.
  ;;   format of items in inconsistent_list: (inconststant_plan conflict_list)
  ;; Returns an open_list with revised plan.
    (cond ((null inconsistent_list) open_list)
          (t (let* ((item (car inconsistent_list))
                    (inconsistent_plan (car item))
                    (conflict_list (cadr item))
                    (fixed_plans (fix-inconsistent-plan inconsistent_plan conflict_list))
                    (fixed_plan_1 (car fixed_plans))
                    (fixed_plan_2 (cadr fixed_plans))
                    (conflict_list_1 (consistent? fixed_plan_1))
                    (conflict_list_2 (consistent? fixed_plan_2))
                    (new_inconsistent_list (insert-new-inconsistent-list inconsistent_list
                                                                         fixed_plan_1 fixed_plan_2 
                                                                         conflict_list_1 conflict_list_2))
                    (new_open_list (insert-new-open-list open_list 
                                                         fixed_plan_1 fixed_plan_2 
                                                         conflict_list_1 conflict_list_2)))
   ;            (terpri) (terpri)
   ;            (print "in function try-to-revise-inconsistent-plan, length of new_inconsistent_list is ")
   ;            (print (length new_inconsistent_list))
   ;            (print "in function try-to-revise-inconsistent-plan, length of new_open_list is ")
   ;            (print (length new_open_list))
               (try-to-revise-inconsistent-plan new_inconsistent_list new_open_list)))))

(defun insert-new-open-list (old_open_list plan1 plan2 conflict_list_1 conflict_list_2)
  ;; arguments: conflict_list: T if corresponding plan is consistent,
  ;;                           <inconsistent_list> if inconsistent,
  ;;                           NIL if unavailable.
  ;; Return an adjusted open_list.
    (cond ((and (equal T conflict_list_1) (equal T conflict_list_2))
            (append (list plan1) (list plan2) old_open_list))
          ((and (not (equal T conflict_list_1)) (equal T conflict_list_2))
            (append (list plan2) old_open_list))
          ((and (equal T conflict_list_1) (not (equal T conflict_list_2)))
            (append (list plan1) old_open_list))
          (t old_open_list)))

(defun insert-new-inconsistent-list (old_inconsistent_list plan1 plan2 conflict_list_1 conflict_list_2)
  ;; arguments: conflict_list: same as insert-new-open-list function.
  ;; Returns an adjusted inconsistent list: adding new inconsistent items, removing the head of the old list.
    (cond ((and (not (atom conflict_list_1)) (not (atom conflict_list_2)))
            (cons (list plan1 conflict_list_1) 
                  (cons (list plan2 conflict_list_2) 
                        (cdr old_inconsistent_list))))
          ((and (atom conflict_list_1) (not (atom conflict_list_2)))
            (cons (list plan2 conflict_list_2) 
                  (cdr old_inconsistent_list)))
          ((and (not (atom conflict_list_1)) (atom conflict_list_2))
            (cons (list plan1 conflict_list_1) 
                  (cdr old_inconsistent_list)))
          (t (cdr old_inconsistent_list))))

(defun fix-inconsistent-plan (inconsistent_plan conflict_list)
  ;; tries to fix the inconsistent plan.
  ;; Returns a list of two new fixed plans (plan1 plan2).
  ;;    of which one with Aj < Ak, the other with Ak < Ai.
  ;; If any plan is invalid, place NIL.
    (let* ((conf (car conflict_list))
           (clink (conflict-causal_link conf))
           (node_Ai (causal_link-plan_node_1 clink))
           (node_Aj (causal_link-Plan_node_2 clink))
           (node_Ak (conflict-node conf))
           (new_order_const_1 (create-order-constraint node_Aj node_Ak))
           (new_order_const_2 (create-order-constraint node_Ak node_Ai))
           (plan_nodes (partial_plan-plan_nodes inconsistent_plan))
           (causal_links (partial_plan-causal_links inconsistent_plan))
           (order_constraints_1 (cons new_order_const_1 (partial_plan-order_constraints inconsistent_plan)))
           (order_constraints_2 (cons new_order_const_2 (partial_plan-order_constraints inconsistent_plan)))
           (open_preconditions (partial_plan-open_precs inconsistent_plan))
           (new_plan_1 (create-new-part-plan plan_nodes causal_links order_constraints_1 open_preconditions))
           (new_plan_2 (create-new-part-plan plan_nodes causal_links order_constraints_2 open_preconditions)))
      (check-availability-new-plans new_plan_1 new_plan_2 node_Ai node_Aj)))

(defun check-availability-new-plans (plan1 plan2 node_Ai node_Aj)
  ;; check whether plan1 and plan2 is available.
  ;; Returns a list of plans (plan1 plan2) if available. Place NIL if any plan is not available.
    (let* ((nodes1 (partial_plan-plan_nodes plan1))
           (nodes2 (partial_plan-plan_nodes plan2))
           (start_node_2 (get-start-node nodes2))
           (finish_node_1 (get-finish-node nodes1))
           (available_plan_1 (and (not (equal finish_node_1 node_Aj))
                                  (no-cycles plan1)))
           (available_plan_2 (and (not (equal start_node_2 node_Ai))
                                  (no-cycles plan2))))
   ;   (print "In function check-availability-new-plans, available plan 1 and available plan 2 is :")
   ;   (print available_plan_1)
   ;   (print available_plan_2)
      (cond ((null available_plan_1)
                (cond ((null available_plan_2) (list NIL NIL))
                      (t (list NIL plan2))))
            ((null available_plan_2) (list plan1 NIL))
            (t (list plan1 plan2)))))

(defun expand-partial-plan-with-new-actions 
    (part_plan open_prec open_list action_nodes_new)
; part_plan is a structure of type partial_plan
; open_prec is a structure of type open_precondition
; open_list is a list of partial plans
; action_nodes_new is a list of structures of type plan_node that each
;    represent an action whose effect matches open_prec and which are 
;    not already in part_plan
; return open_list with the addition of new partial plans (structures 
;    of type partial_plan) that represent expansions of part_plan that 
;    satisfy open_prec
; any new partial plans added to open list must be consistent
;(print "in function expand-partial-plan-with-new-actions, open list is now length: ")
;(print (length open_list))
    (cond ((null action_nodes_new) open_list)
          (t (let* ((node_Aj (open_precondition-node open_prec))
                    (node_Ai (car action_nodes_new))
                    (new_part_plan (set-new-partial-plan-with-new-action part_plan 
                                                                         open_prec 
                                                                         node_Ai 
                                                                         node_Aj))
                    (conflict_list (consistent? new_part_plan))
                    (open_precond_spoiled (check-open-precond node_Ai node_Aj part_plan)))
             (cond ((equal T open_precond_spoiled)
                        (expand-partial-plan-with-new-actions part_plan
                                                              open_prec
                                                              open_list
                                                              (cdr action_nodes_new)))
                   ((equal T conflict_list) (cons new_part_plan open_list))
                   (t (let ((new_open_list (try-to-revise-inconsistent-plan (list (list new_part_plan conflict_list))
                                                                                  open_list)))
                        (cond ((not (equal open_list new_open_list)) new_open_list)
                              (t ;(print "(expand-partial-plan-with-new-actions) trying other new options...  ")
                                 (expand-partial-plan-with-new-actions part_plan 
                                                                       open_prec 
                                                                       open_list 
                                                                       (cdr action_nodes_new)))))))))))

(defun check-open-precond (node_Ai node_Aj part_plan)
  ;; checks whether the effects of node_Ai may spoil some open precondition of node_Aj.
  ;; Returns T if spoils, otherwise return NIL.
    (let* ((effects_Ai (plan_node-effects node_Ai))
           (open_precs (partial_plan-open_precs part_plan))
           (open_precs_in_Aj (select-open-precs-in-node open_precs node_Aj)))
      (cond ((null open_precs_in_Aj) NIL)
            (t (check-literal-violation effects_Ai open_precs_in_Aj)))))

(defun select-open-precs-in-node (open_precs node_Aj)
  ;; select all the literals in list 'open_precs' that are the preconditions of node Aj.
  ;; If not found, return NIL.
    (cond ((null open_precs) NIL)
          ((equal node_Aj (open_precondition-node (car open_precs)))
                (cons (car open_precs) (select-open-precs-in-node (cdr open_precs) node_Aj)))
          (t (select-open-precs-in-node (cdr open_precs) node_Aj))))

(defun check-literal-violation (effects_Ai open_precs_in_Aj)
  ;; checks whether some literal of 'effects_Ai' violates any open_precs_in_Aj.
  ;; Returns T if violation is found. Otherwise return NIL.
    (cond ((null effects_Ai) NIL)
          (t (let* ((effect (car effects_Ai))
                    (effect_evil (check-literal-violation-single effect open_precs_in_Aj)))
               (cond ((null effect_evil) (check-literal-violation (cdr effects_Ai) open_precs_in_Aj))
                     (t T))))))

(defun check-literal-violation-single (effect open_precs)
  ;; checks whether the effect violates any open_precs.
  ;; Return T if violation found. Otherwise return NIL.
    (cond ((null open_precs) NIL)
          (t (let* ((precond_literal (open_precondition-precondition (car open_precs)))
                    (effect_sign (literal-sign effect))
                    (effect_pname (literal-pname effect))
                    (precond_sign (literal-sign precond_literal))
                    (precond_pname (literal-pname precond_literal)))
               (cond ((and (equal effect_pname precond_pname) (not (equal effect_sign precond_sign)))
                        T)
                     (t (check-literal-violation-single effect (cdr open_precs))))))))

(defun set-new-partial-plan-with-new-action (old_part_plan p node_Ai node_Aj)
  ;; Returns a structure of type partial_plan with the following updates:
  ;; 1. adds node_Ai to PLAN_NODES in PARTIAL_PLAN;
  ;; 2. adds START < node_Ai and node_Ai < FINISH and node_Ai < node_Aj to ORDER_CONSTRAINTS in PARTIAL_PLAN;
  ;; 3. removes p from the OPEN_PRECONDITIONS in PARTIAL_PLAN and
  ;;      adds preconditions of node_Ai to OPEN_PRECONDITIONS in PARTIAL_PLAN.
  ;; 4. adds node_Ai -> node_Aj (p) to CAUSAL_LINKS in PARTIAL_PLAN.
    (let* ((new_plan_nodes (cons node_Ai (partial_plan-plan_nodes old_part_plan)))
           (start_node (get-start-node new_plan_nodes))
           (finish_node (get-finish-node new_plan_nodes))
           (new_order_constraints (remove-duplicate-order-constraint 
                                    (append (list (create-order-constraint start_node node_Ai))
                                            (list (create-order-constraint node_Ai finish_node))
                                            (list (create-order-constraint node_Ai node_Aj))
                                            (partial_plan-order_constraints old_part_plan))))
           (new_open_precs (append (form-prec-list node_Ai (plan_node-preconditions node_Ai))
                                   (remove-precondition p (partial_plan-open_precs old_part_plan))))
           (new_causal_links (cons (create-causal-link node_Ai node_Aj p)
                                   (partial_plan-causal_links old_part_plan))))
      (create-new-part-plan new_plan_nodes new_causal_links new_order_constraints new_open_precs)))

(defun remove-duplicate-order-constraint (lst)
  ;; removes all duplicate entrys in the order-constraint list.
    (cond ((null lst) NIL)
          ((null (cdr lst)) lst)
          ((in-list (car lst) (cdr lst))
                (remove-duplicate-order-constraint (cdr lst)))
          (t (cons (car lst) (remove-duplicate-order-constraint (cdr lst))))))

(defun in-list (item lst)
  ;; returns T if item is in lst. Otherwise return NIL.
    (cond ((null lst) NIL)
          ((equal item (car lst)) T)
          (t (in-list item (cdr lst)))))


(defun form-prec-list (n literals)
  ;; Returns a list of structures of type open_precondition.
    (cond ((null literals) NIL)
          ((null (cdr literals))
            (list (make-open_precondition :precondition (car literals)
                                          :node n)))
          (t (cons (make-open_precondition :precondition (car literals)
                                           :node n)
                   (form-prec-list n (cdr literals))))))

(defun consistent? (new_part_plan)
; new_part_plan is a structure of type partial_plan
; return true if it is consistent --- that is, the protected
;   preconditions on its causal links are not threatened by any
;   of its actions
    (cond ((null new_part_plan) NIL)
          (t (let* ((nodes (partial_plan-plan_nodes new_part_plan))
                    (clinks (partial_plan-causal_links new_part_plan))
                    (order_consts (partial_plan-order_constraints new_part_plan))
                    (conflict_list (check-consistency new_part_plan nodes clinks order_consts)))
;               (print "In function consistent?, conflict list is now: ")
;               (print conflict_list)
               (cond ((null conflict_list) T)
                     (t conflict_list))))))

(defun check-consistency (plan nodes clinks order_consts)
  ;; performs the check-concistency task.
  ;; Returns a list of structures of type conflict.
    (cond ((null nodes) NIL)
          (t (let* ((effects (plan_node-effects (car nodes)))
                    (all_nodes (partial_plan-plan_nodes plan))
                    (suspicious_list (get-suspicious-list (car nodes) 
                                                          effects 
                                                          clinks)))
               (cond ((null suspicious_list) (check-consistency plan (cdr nodes) clinks order_consts))
                     (t 
                       (let ((conflict_list (check-and-create-conflict-list suspicious_list order_consts))
                             (tail (check-consistency plan (cdr nodes) clinks order_consts)))
                         (cond ((null conflict_list) tail)
                               ((null tail) conflict_list)
                               (t (append conflict_list tail))))))))))

(defun check-and-create-conflict-list (suspicious_list order_consts)
  ;; performs the order constraint check on the suspicious list.
  ;;   checks whether there is order constraint to prevent Ak from occuring between Ai and Aj.
  ;; Return a list of structure of type conflicts.
    (cond ((null suspicious_list) NIL)
          (t (let* ((clink (caar suspicious_list))
                    (nodeK (cadr (car suspicious_list)))
                    (nodeI (causal_link-plan_node_1 clink))
                    (nodeJ (causal_link-plan_node_2 clink))
                    (result_k_i (check-order-const nodeK nodeI order_consts))
                    (result_j_k (check-order-const nodeJ nodeK order_consts)))
               (cond ((and (null result_k_i) (null result_j_k))
                        (cons (create-conflict-structure clink nodeK)
                              (check-and-create-conflict-list (cdr suspicious_list) order_consts)))
                     (t (check-and-create-conflict-list (cdr suspicious_list) order_consts)))))))

(defun create-conflict-structure (clink nodeK)
  ;; returns a structure of type conflict.
    (make-conflict :causal_link clink
                   :node nodeK))

(defun check-order-const (before_node after_node order_consts)
  ;; Checks whether there is an order constraint that before_node < after_node.
  ;; Returns NIL if there is no constraint. Otherwise, return something else.
    (cond ((null order_consts) NIL)
          (t (let ((node1 (order_constraint-before_node (car order_consts)))
                   (node2 (order_constraint-after_node (car order_consts))))
               (cond ((and (equal node1 before_node) (equal node2 after_node))
                        (car order_consts))
                     (t (check-order-const before_node after_node (cdr order_consts))))))))
                              
(defun get-suspicious-list (node effects clinks)
  ;; find all suspicious conflicts for all clinks that
  ;;    protected_prop is the protected proposition of a clink, AND
  ;;    neg protected_prop is member of 'effects', which is a list of literals.
  ;; ALL members of 'effects' are effects of 'node'.
  ;; Returns a list of suspicious items. Suspicious item have the following format:
  ;;    (clink nodeK), where nodeK is the node whose action threatens the clink.
    (cond ((null clinks) NIL)
          (t (let* ((clink (car clinks))
                    (prop_clink (causal_link-protected_prop clink))
                    (neg_prop_clink (get-neg-literal prop_clink)))
               (cond ((literal-is-member-of neg_prop_clink effects)
                        (cons (list clink node) (get-suspicious-list node effects (cdr clinks))))
                     (t (get-suspicious-list node effects (cdr clinks))))))))

(defun get-neg-literal (l)
  ;; Returns the negative form of literal 'l'.
    (cond ((null (literal-sign l))
             (make-literal :sign 'NEG
                           :pname (literal-pname l)))
          (t (make-literal :pname (literal-pname l)))))


(defun literal-is-member-of (literal literal_list)
; literal_list is a list of structures of type literal 
; return T if literal is a member of literal_list
  (cond ((null literal_list) nil)
        ((equal-literals literal (car literal_list))
          t)
        (t (literal-is-member-of literal (cdr literal_list)))))

(defun equal-literals (literal1 literal2)
; literal1 and literal2 are structures of type literal
; return T if they represent the same literal
(and (equal (literal-sign literal1) (literal-sign literal2))
     (equal (literal-pname literal1) (literal-pname literal2))))
     

(defun is-effect-of-action-recipe (literal recipe)
; literal is a structure of type literal 
; recipe is a structure of type action_recipe
; return T if literal is an effect of the action described by recipe
(literal-is-member-of literal (action_recipe-effects recipe)))
         


(defun remove-precondition (prec open_prec_list)
; prec is a structure of type open_precondition
; open_prec_list is a list of structures of type open_precondition
; return open_prec_list after removing prec
(cond ((null open_prec_list) nil)
      ((and (equal-literals 
               (open_precondition-precondition prec)
               (open_precondition-precondition (car open_prec_list)))
            (equal (plan_node-name (open_precondition-node prec))
                   (plan_node-name (open_precondition-node (car open_prec_list)))))
       (cdr open_prec_list))
      (t (cons (car open_prec_list)
               (remove-precondition prec (cdr open_prec_list))))))

(defun get-start-node (nodes)
; nodes is a list of structures of type plan_node
; return the node that is the START node
(cond((null nodes) nil)
     ((equal (action-act_name (plan_node-act (car nodes))) 'START)
        (car nodes))
     (t (get-start-node (cdr nodes)))))

(defun get-finish-node (nodes)
; nodes is a list of structures of type plan_node
; return the node that is the FINISH node
(cond((null nodes) nil)
     ((equal (action-act_name (plan_node-act (car nodes))) 'FINISH)
        (car nodes))
     (t (get-finish-node (cdr nodes)))))


(defun no-cycles (part_plan)
; part_plan is a structure of type partial_plan
; assumes that no order constraint in part_plan places an action
;   before the Start node or after the Finish node
; return true if the order_constraints of part_plan do not cycles
; otherwise return nil
   (check-for-cycles (partial_plan-order_constraints part_plan)
                     (list(list(get-start-node (partial_plan-plan_nodes part_plan))))))

(defun check-for-cycles (constraints check_list)
; this function is used in testing for cycles but nowhere else
; constraints is a list of order_constraints
; check_list is a list of sequences of plan_nodes
; return true if none of the sequences in check_list can be expanded
;   into a cycle using the ordering given in order_constraints and
;   return nil otherwise
   (cond((null check_list) t)
        (t (let((node_list (add-next-node constraints (car check_list))))
              (cond((equal node_list 'CYCLE) nil)
                   (t (check-for-cycles 
                          constraints
                          (append node_list (cdr check_list)))))))))

(defun add-next-node (constraints node_list)
; this function is used in testing for cycles by check-for-cycles
; constraints is a list of structures of type order_constraint
; node_list is an ordered sequence of structures of type plan_node
; return an expanded node_list that contains an additional element
;   as first element of the list, representing a longer ordered sequence
;   of nodes going toward the Finish node as specified by constraints
;   without leading to a cycle in the plan
; return 'CYCLE if a cycle results
   (cond((null constraints) nil)
        ((equal (order_constraint-before_node (car constraints))
                (car node_list))
          (cond ((equal (action-act_name
                           (plan_node-act
                             (order_constraint-after_node
                                (car constraints))))
                        'FINISH)
                  (add-next-node (cdr constraints) node_list))
                ((is-in? (plan_node-name
                           (order_constraint-after_node
                              (car constraints)))
                         node_list)
                 'CYCLE)
                (t (cons
                      (cons (order_constraint-after_node (car constraints))
                            node_list)
                      (add-next-node (cdr constraints) node_list)))))
        (t (add-next-node (cdr constraints) node_list))))

(defun is-in? (node_name node_list)
; this function is used in functions for testing for cycles
; node_name is the name part of a structure of type plan_node
; node_list of a list of structures of type plan_node
; return true if node_name is the name part of an element of node_list
(cond((null node_list) nil)
     ((equal node_name (plan_node-name (car node_list)))
        t)
     (t (is-in? node_name (cdr node_list)))))
                                      
(defun output-results (plan)
; plan is a structure of type partial_plan
; print the nodes, their causal links, and their order constraints
  (princ "Nodes")
  (terpri)
  (output-nodes (partial_plan-plan_nodes plan))
  (terpri)
  (terpri)
  (princ "Order constraints")
  (terpri)
  (output-order-constraints (partial_plan-order_constraints plan))
  (terpri)
  (terpri)
  (princ "Causal links")
  (terpri)
  (output-causal-links (partial_plan-causal_links plan))
  (terpri)
  (terpri)
  (princ "Drawing goes here")
  (terpri)(terpri)(terpri)(terpri)(terpri)(terpri)(terpri)(terpri)
  (terpri)(terpri)(terpri)(terpri)(terpri)(terpri)(terpri)(terpri))


(defun output-nodes (nodes)
; nodes is a list of structures of type plan_node
; print their names
   (cond ((null nodes) t)   
         (t (princ (plan_node-name (car nodes)))
            (princ "   ")
            (princ (action-act_name (plan_node-act (car nodes))))
            (terpri)
            (output-nodes (cdr nodes)))))

(defun output-order-constraints (constraints)
; constraints is a list of structures of type order_constraint
; print the order constraints
   (cond((null constraints) t)
        (t (princ (plan_node-name (order_constraint-before_node (car constraints))))
           (princ "  Before  ")
           (princ (plan_node-name (order_constraint-after_node (car constraints))))
           (terpri)
           (output-order-constraints (cdr constraints)))))

(defun output-causal-links (causal_links)
; causal_links is a list of structures of type causal_link
; print the nodes and the protected propositions
   (cond ((null causal_links) t)
         (t (princ (plan_node-name(causal_link-plan_node_1(car causal_links))))
            (princ " satisfies ")
            (cond((equal 'NEG 
                        (literal-sign
                           (causal_link-protected_prop
                             (car causal_links))))
                    (princ (literal-sign
                             (causal_link-protected_prop(car causal_links))))
                    (princ " ")))
            (princ (literal-pname(causal_link-protected_prop(car causal_links))))
            (cond ((not(null(literal-parameters
                            (causal_link-protected_prop(car causal_links)))))
                     (princ "(")
                     (print-parameters
                         (literal-parameters
                            (causal_link-protected_prop(car causal_links))))
                     (princ ")")))
            (princ " for node ")
            (princ (plan_node-name(causal_link-plan_node_2(car causal_links))))
            (terpri)
            (output-causal-links (cdr causal_links)))))

(defun print-literals(llist)
; llist is a list of literals
; print them
(cond((null llist) (terpri))
     (t (terpri) 
        (print-literal (car llist))
        (print-literals (cdr llist)))))

(defun print-literal (lit)
; lit is a structure of type literal
; print lit
(cond ((equal 'NEG (literal-sign lit))
        (princ (literal-sign lit))
        (princ " ")))
(princ (literal-pname lit))
(cond ((not(null (literal-parameters lit)))
         (princ "(")
         (print-parameters (literal-parameters lit)) 
         (princ ")")))
(terpri))
 

(defun print-parameters (params)
; params is a list of structures of type Constant
; print them
(cond((null params) nil)
     (t (princ (constant-cname (car params)))
        (princ " ")
        (print-parameters (cdr params)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PROGRAM ENTRY

(load "lisp-init.lisp")
(load "Task-planner-testcases-14.lisp")
(testing)
