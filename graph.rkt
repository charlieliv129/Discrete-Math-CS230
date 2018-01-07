;;;
;;;cs230.ps3.scm
;;;
;;; For this problem set, when defining functions do not use types in 
;;; the lambda expressions. Instead, you should add a comment as to what 
;;; the type should be.

;;; Do not use (require racket/base) for this problem set.
;; ----- Useful functions -----

;; Define a predicate member? that returns #t if obj is a member of
;; lst and #f otherwise.

;; Contrast with the builtin member function, which returns the
;; sublist of lst starting with obj if obj is in the list.
(require racket/class)

(define member?
  (lambda (obj lst)
    (not (not (member obj lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;foldr and foldl are provided in scheme                                                                                                                                           
;;                                                                                                                                                                                 
;;(define accumulate                                                                                                                                                               
;; (lambda (initial op l)                                                                                                                                                          
;;    (cond ((null? l) initial)                                                                                                                                                    
;;      (else                                                                                                                                                                      
;;        (op (car l) (accumulate initial op (cdr l)))))))                                                                                                                         
;;                                                                                                                                                                                 
;;Note: (accumulate  '() cons '(1 2 3 4)) => '(1 2 3 4)
;;(define foldr (lambda (op init lst) (accumulate init op lst)))                                         
;;Note: (foldr cons '() '(1 2 3 4)) => '(1 2 3 4)                                                                              
;;Whereas: (foldl cons '() '(1 2 3 4) => '(4 3 2 1)  
;; ------ Data type definitions -----

;; Directed graph class definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(vertices <graph>) => list of vertices
;;(edges <graph>) => list of edges 
;;
(defclass <graph> ()
  (vertices :initarg :vertices :accessor vertices) 
  (edges :initarg :edges :accessor edges))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(name <vertex>) => the name of the vertex
;;ex: (name (make-vertex 'a)) => a
;;
(defclass <vertex> ()
  (name :initarg :name :accessor name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start <directed-edge>) => first vertex of directed-edge
;;ex: (start (v1 v2)) => v1
;;(finish <directed-edge>) => second vertex of directed-edge
;;ex: (finish (v1 v2)) => v2
;;
(defclass <directed-edge> ()
  (start :initarg :start :accessor start)
  (finish :initarg :finish :accessor finish))

(define make-vertex
  (lambda (name)
    (make <vertex> :name name)))

(define make-edge
  (lambda (a b) ;a <vertex> b <vertex>
    (make <directed-edge> :start a :finish b)))

;; Two vertices are considered equal if their names are equal

(define equal-vertex?
  (lambda (v1 v2)
    (eq? (name v1) (name v2))))

;; lookup-vertex takes a name and a list of vertices, and finds a vertex
;; with that name.  Useful when you have the name of a vertex and need
;; the vertex itself.

(define lookup-vertex
  (lambda (vname vlist)
    (cond ((null? vlist) #f)
          ((equal? vname (name (first vlist))) (first vlist)) ;replaced car with first
          (else (lookup-vertex vname (rest vlist)))))) ;replaced cdr with rest

;; make-graph takes two lists whose atoms are symbols, one of the form
;;   (v1 v2 v3 ...) 
;; which becomes the list of vertices and the other of the form
;;   ((u1 u2) (u3 u4) ...) 
;; which becomes the list of edges.

(define make-graph
  (lambda (v-names e-list)
    (let* ((v (map make-vertex v-names))
           (create-edge 
              (lambda (name1 name2)
                (make-edge (lookup-vertex name1 v)
                           (lookup-vertex name2 v)))))
        (make <graph>
              :vertices v        
              :edges (map create-edge
                          (map first e-list)
                          (map second e-list))))))

;; Convert a list of vertices to a list of names of vertices

(define name-vertices
  (lambda (vlist)
    (map name vlist)))

;;;Same as standard member function but works with vertices
(define member-vertices 
  (lambda (a lat) 
    (cond ((null? lat) #f) 
          ((equal-vertex? a (car lat)) lat) 
          (else (member-vertices a (cdr lat))))))

;; Find the set difference of two sets represented as lists.  That is,
;; return a list consisting of everything in list1 that is not in
;; list2

(define set-diff-vertices
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member-vertices (car list1) list2) (set-diff-vertices (cdr list1) list2))
          (#t (cons (car list1) (set-diff-vertices (cdr list1) list2))))))

;; Take the union of two sets represented as lists -- no duplicates

(define union
  (lambda (list1 list2)
    (cond ((null? list1) list2) 
          ((member (car list1) list2) (union (cdr list1) list2))
          (else (cons (car list1) (union (cdr list1) list2))))))
          

;; Take the intersection of two sets represented as lists 

(define intersection
  (lambda (list1 list2)
    (cond ((null? list1) '()) 
          ((member (car list1) list2) 
             (cons (car list1) (intersection (cdr list1) list2)))
          (else (intersection (cdr list1) list2)))))

;;; ----- TESTING EXAMPLES -----
(define g1 (make-graph '(a b c d e) 
		      '((a b) (a c) (b c) (b e) (c d) (d b))))
(define g2 (make-graph '(a b c) '((a b) (b a) (a c) (c a) (b c))))
(define g3 (make-graph '(a b c d) '((a b) (b c) (a c) (c b) (d b))))
(define g4 (make-graph '(a b c d) '((a b) (a c) (b a) (c a) (a d) (b c) (c b))))

;; ----- Problem 1 -----
(define get-edge
  (lambda (de) ;de is a directed edge
    (finish de)))

(define exits
  (lambda (v1 g) ;v1 is a vertex, g is a graph
    (map get-edge (filter (lambda (x)
                            (equal-vertex? (start x) v1)) (edges g)))))

(define verify-path
  (lambda (g lst) ;g = graph, lst = list
    (cond ((null? (cdr lst)) #t)   ;(cddr lst) = null
          ((not (member? (cadr lst) (exits (car lst) g))) #f)
          (else (verify-path g (cdr lst))))))

;(name-vertices (exits (lookup-vertex 'b (vertices g1)) g1))
;(c e)

;(name-vertices (exits (lookup-vertex 'e (vertices g1)) g1))
;()

;(verify-path g1 
 ;   (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d b e)))
;#t

;(verify-path g1 
;    (map (lambda (x) (lookup-vertex x (vertices g1))) '(a b c d e)))
;#f

  
;; ----- Problem 2 -----
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(label <labeled-edge>) => label of labeled-edge
;;ex: (label (make-labeled-edge a b l)) => l
;;
(defclass <labeled-edge> (<directed-edge>)
  (label :initarg :label :accessor label))

(define make-labeled-edge
  (lambda (a b l) ;a <vertex> b <vertex> l <obj>
    (make <labeled-edge> :start a :finish b :label l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(start-state <automaton>) => start-state of automaton
;;(final-states <automaton>) => list of final-states of automaton
;;    
(defclass <automaton> (<graph>)
  (start-state :initarg :start-state :accessor start-state)
  (final-states :initarg :final-states :accessor final-states)) ;start-state <symbol> final-states <list>
;;note: all accessors that apply to graph apply to automaton
  
;; make-automaton takes four parameters.  
;; The first is a list of symbols of the form (v1 v2 v3 ...) which 
;;   becomes the list of vertices.
;; The second is a list of triples of the form 
;;   ((u1 u2 l1) (u3 u4 l2) ...) which becomes the list of labeled
;;   edges (with the u's symbols which represent vertices and the l's 
;;   objects which become the labels).
;; The third is a single symbol for the start state.
;; The fourth is a list of symbols that represent final states.

(define make-automaton
  (lambda (v-names e-list s-state f-states) ;v-names <list>, e-list <list>, s-state <symbol>, f-states <list>
    (let* ((v (map make-vertex v-names))
           (create-labeled-edge 
              (lambda (name1 name2 label)
                (make-labeled-edge (lookup-vertex name1 v) 
                                   (lookup-vertex name2 v) 
                                   label))))
      (make <automaton>
            :vertices v
            :edges (map create-labeled-edge
                        (map first e-list)
                        (map second e-list)
                        (map third e-list))
            :start-state s-state
            :final-states f-states))))

(define dfa1
  (make-automaton '(a b c) 
	    '((a a 0) (a b 1) (b a 1) (b c 0) (c b 0) (c c 1))
            'a '(a)))

;;; ----- Problem 3 -----

(define step-dfa
  (lambda (auto sName sym) 
    (let ((result (filter (lambda (x)
                            (and (= (label x) sym)
                                 (eq? (name (start x)) sName)))
                  (edges auto))))
      (cond ((null? result) #f)
            ((not (null? (cdr result))) #f)
            (else (name (finish (car result))))))))
           

; (step-dfa dfa1 'c 1) ;==> c ;
; (step-dfa dfa1 'd 0) ;==> #f
; (step-dfa dfa1 'a 0) ;==> a
; (step-dfa dfa1 'a 1) ;==> b
; (step-dfa dfa1 'a 2) ;==> #f

;(define bad-dfa
 ;(make-automaton '(a b c) 
;	    '((a a 0) (a b 0) (b a 1) (b c 0) (c b 0) (c c 1))
 ;           'a '(a)))

; (step-dfa bad-dfa 'a 0) ;==> #f

;; ----- Problem 4 -----

(define simulate-dfa
  (lambda (auto lst) ;auto is automaton, lst is input sequence
    (if (null? lst) (member? (start-state auto) (final-states auto))
        (letrec ((loop (lambda (v1 remStates) ;sName is vertex, remState is list
                         (if (null? remStates) (member? v1 (final-states auto))
                               (let ((next (step-dfa auto v1 (car remStates))))
                                 (if (boolean? next) #f
                                     (loop next (cdr remStates))))))))
          (loop (start-state auto) lst)))))
                                 
 ;(simulate-dfa dfa1 '(1 0 0 1)) ;==> #t
 ;(simulate-dfa dfa1 '(1 0 1 1)) ;==> #f

(define integer->binary
  (lambda (n)
    (cond ((eq? n 0) '())
	  (else (append (integer->binary (quotient n 2)) 
                        (list (if (even? n) 0 1)))))))
          
 ;(simulate-dfa dfa1 (integer->binary 12))
 ;(simulate-dfa dfa1 (integer->binary 10))


;; ----- Problem 5 -----
(define nfa1
  (make-automaton '(a b c d e)
	    '((a a 0) (a a 1) (a b 1) (a c 0) (b d 1) (c e 0)
	      (d d 0) (d d 1) (e e 0) (e e 1))
	    'a
	    '(d e)))

(define step-nfa
  (lambda (auto sName sym) 
    (let ((result (filter (lambda (x)
                            (and (= (label x) sym)
                                 (eq? (name (start x)) sName)))
                  (edges auto))))
      (if (null? result) '()
          (map (lambda (v)
                 (name (finish v))) result)))))

(define simulate-nfa
  (lambda (auto lst) ;auto is automaton, lst is input sequence
        (letrec ((loop (lambda (v1 remStates)  ;v1 is current vertex, remState is list
                         (if (null? remStates) (cons v1 '())
                            (foldr union '()
                             (map (lambda (x) (loop x (cdr remStates)))
                                  (step-nfa auto v1 (car remStates))))))))
          (not (null? (filter (lambda (x)(member? x (final-states auto)))
           (loop (start-state auto) lst)))))))

;(step-nfa nfa1 'a 1)
;(step-nfa nfa1 'b 1)
;(step-nfa nfa1 'c 1)
;(step-nfa nfa1 'e 2)
;(simulate-nfa nfa1 '(1 0 1))  ;==> f
;(simulate-nfa nfa1 '(0 0 0))  ;==>t
;(simulate-nfa nfa1 '(0 1 0 1 1))  ;==>t
;(simulate-nfa nfa1 '())  ;==>f
;(simulate-nfa nfa1 '(0))  ;==>f
;(simulate-nfa nfa1 '(0 0 1 1))  ;==> t
            
;; ----- Problem 6 -----



(define contains-true
    (lambda (lst)
     (not (null? (filter (lambda (x) x)lst)))))

(define path?
  (lambda (start target graph)
    (letrec ((loop (lambda (v seen)
                   (let ((possible (set-diff-vertices (exits v graph) seen)))
                     (cond ((eq? (name v) target) #t)
                           ((null? possible) #f)
                           (else
                            (contains-true (map (lambda (x) (loop x (cons v seen))) possible))))))))
      (loop (lookup-vertex start (vertices graph)) '() ))))
                           

 ;(path? 'a 'e g1) ;==> #t
 ;(path? 'd 'a g1) ;==> #f
 ;(path? 'a 'c g2); ==> #t
 ;(path? 'c 'b g2); ==> #t
 ;(path? 'd 'd g3); ==> #t
 ;(path? 'a 'd g3); ==> #f
 ;(path? 'b 'd g4); ==> #t

;; ----- Problem 7 -----
(defclass <vertex+parent> (<vertex>)
  (parent :initarg :parent :accessor parent))

(define make-vertex+parent
  (lambda (v p) ;v <vertex>, p <obj>                                                                                                                                               
    (make <vertex+parent> :name (name v) :parent p)))

(define find-path
  (lambda (start target graph)
   (letrec ((loop (lambda (v seen path)
                   (let ((possible (set-diff-vertices (exits v graph) seen)))
                     (cond ((eq? (name v) target) (append path (list v)))
                           ((null? possible) #f)
                           (else
                            (let ((result (filter (lambda (x) (not (boolean? x)))
                                                  (map (lambda (x) (loop x (cons v seen)
                                                                         (append path (list v))))
                                                       possible))))
                              (if (null? result) #f (car result)))))))))
      (loop (lookup-vertex start (vertices graph)) '() '() ))))

;(name-vertices (find-path 'a 'e g1)) ;(a b e)
;(find-path 'd 'a  g1) ;#f
;(name-vertices (find-path 'a 'c g2)) ;(a c) or (a b c)
;(name-vertices (find-path 'c 'b g2)) ;(c a b)
;(name-vertices (find-path 'd 'd g3)) ;(d)
;(find-path 'a 'd g3) ;#f
;(name-vertices (find-path 'b 'd g4)) ;(b a d)


;; ----- Extra Credit --------
(define make-dfa
  (lambda (v-names e-list s-state f-states)
    (letrec ((loop (lambda (e checked)
                   (if (null? e) (make-automaton v-names e-list s-state f-states)
                       (let ((p (cons (car (car e))(cddr (car e)))))
                         (if (member? p checked)
                             #f
                             (loop (cdr e) (cons p checked))))))))
      (loop e-list '()))))

 ; (make-dfa '(a b c) 
;	    '((a a 0) (a b 1) (b a 1) (b c 0) (c b 0) (c c 1))
 ;           'a '(a)) ; ==> this is dfa1, should make an automaton
  ;(make-dfa '(a b c d e)
;	    '((a a 0) (a a 1) (a b 1) (a c 0) (b d 1) (c e 0)
;	      (d d 0) (d d 1) (e e 0) (e e 1))
;	    'a
;	    '(d e)); ==> this is nfa1, should return #f
  