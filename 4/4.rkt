#lang racket
(require redex)
(require redex/reduction-semantics)
(require rackunit)

;; Redex DADL
(define-language dadl
  (c (config n at (r ...)))
  (r (room n desc (e ...)))
  (e (exit dir to))
  (dir NORTH SOUTH EAST WEST)
  (to string)
  (n string)
  (at string)
  (desc string))

;; -----------------------------------------------------------------------------

;; Problem 1 Extend your Redex model of DADL with a general reduction
;; relation, dubbed ->dd. The relation generates all possible successor
;; configurations for any given configuration. That is, it really is a
;; traversal relation not a function.

(define ->dd 
  (reduction-relation 
   dadl
   #:domain c
   (--> (config n at (r_1 ... (room at desc (e_1 ... (exit dir to) e_2 ...)) r_2 ...))
	(config n to (r_1 ... (room at desc (e_1 ... (exit dir to) e_2 ...)) r_2 ...)))))

(module+ test
  (test-equal (apply-reduction-relation ->dd config-with-ordered-rooms)
	      (term ((config
		      "Ryan"
		      "ell"
		      ((room "curry" "piano" ((exit EAST "ell")))
		       (room
			"ell"
			"husky"
			((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
		       (room "tunnels" "creepy" ())
		       (room "krentzman" "outside" ((exit NORTH "curry"))))))))

  (test-equal
   (apply-reduction-relation
    ->dd 
    (term (config "Ryan" "ell"
		  ((room "curry" "piano" ((exit EAST "ell")))
		   (room "ell" "husky" ((exit WEST "curry")
					(exit NORTH "krentzman")
					(exit EAST "tunnels")))
		   (room "tunnels" "creepy" ())
		   (room "krentzman" "outside" ((exit NORTH "curry")))))))
   (term ((config
	   "Ryan"
	   "tunnels"
	   ((room "curry" "piano" ((exit EAST "ell")))
	    (room
	     "ell"
	     "husky"
	     ((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
	    (room "tunnels" "creepy" ())
	    (room "krentzman" "outside" ((exit NORTH "curry")))))
	  (config
	   "Ryan"
	   "krentzman"
	   ((room "curry" "piano" ((exit EAST "ell")))
	    (room
	     "ell"
	     "husky"
	     ((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
	    (room "tunnels" "creepy" ())
	    (room "krentzman" "outside" ((exit NORTH "curry")))))
	  (config
	   "Ryan"
	   "curry"
	   ((room "curry" "piano" ((exit EAST "ell")))
	    (room
	     "ell"
	     "husky"
	     ((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
	    (room "tunnels" "creepy" ())
	    (room "krentzman" "outside" ((exit NORTH "curry")))))))))

;; -----------------------------------------------------------------------------

;; is the traversal path vervifed by ->dd
(define-metafunction dadl
  ->dd-traverse-equivalence-conjecture : c -> boolean
  [(->dd-traverse-equivalence-conjecture c)
   (ddtec/a c (traverse c))])

(define-metafunction dadl
  ddtec/a : c (at ...) -> boolean
  [(ddtec/a c ()) true]
  [(ddtec/a c (at_1 at_2 ...)) 
   (ddtec/a c_2 (at_2 ...))
   (where c_2 (next-config c at_1))
   (side-condition (term c_2))]
  [(ddtec/a c (at_1 at_2 ...)) false])

(define-metafunction dadl
  next-config : c at -> c or false
  [(next-config c at)
   c_2
   (where (c ...) ,(apply-reduction-relation ->dd (term c)))
   (where c_2 (get-matching  at))])


(module+ test (test-results))

;; =============================================================================

;; Test Configurations
;; Basically Same from P1
(define config1 (term (config "Matthias" "TheLab" ())))
(define config2 (term (config "Matthias" "TheLab" ((room "TheLab" "piano" ())))))
(define config3
  (term (config "Matthias" "TheLab"
		((room "TheLab" "piano" ((exit NORTH "Punters")))
		 (room "Punters" "piano" ())))))
(define config-with-ordered-rooms
  (term (config "Ryan" "curry"
		((room "curry" "piano" ((exit EAST "ell")))
		 (room "ell" "husky" ((exit WEST "curry")
				      (exit NORTH "krentzman")
				      (exit EAST "tunnels")))
		 (room "tunnels" "creepy" ())
		 (room "krentzman" "outside" ((exit NORTH "curry")))))))

;; traverse : config -> void
;; Crawl a DADL dungeon according to the traversal
;; rules from p1 and log rooms to a list
(define-metafunction dadl
  traverse : c -> (n ...)
  [(traverse (config n at (r ...)))
   (traverse* at (r ...) ())])

;; traverse* : name (room ...) (name ...) -> (name ...)
;; Accumulator fn for Traverse : Accumulates a list of
;; rooms traversed
(define-metafunction dadl
  traverse* : at (r ...) (n ...) -> (n ...) 
  [(traverse* at (r ...) (n ...))
   (traverse* to (r ...) (n ... at))
   (where ((exit dir to) e_rest ...) (lookup-exits at (r ...)))
   (side-condition (not (member (term to) (term (n ...)))))]
  [(traverse* at (r ...) (n ...)) (n ... at)])

;; lookup-exits : name (room ...) -> (exit ...)
;; Given a list of room defs, and a room name,
;; get back the configured exits for that room
(define-metafunction dadl
  lookup-exits : n (r ...) -> (e ...)
  [(lookup-exits n ()) ,(error "No such room: " (term n))]
  [(lookup-exits n ((room n desc (e ...)) r_rest ...)) (e ...)]
  [(lookup-exits n ((room n_diff desc (e ...)) r_rest ...))
   (lookup-exits n (r_rest ...))])

(check-exn exn:fail? (lambda () (term (traverse config1))))
(test-equal (term (traverse ,config2)) (list "TheLab"))
(test-equal (term (traverse ,config3)) (list "TheLab" "Punters"))
(test-equal (term (traverse ,config-with-ordered-rooms)) (list "curry" "ell"))
