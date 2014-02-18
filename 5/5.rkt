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

(define-extended-language dadl-C dadl
  (C (config n at (r_1 ... hole r_2 ...))))

;; Standard relation on ->dd. Always takes the first exit.
(define ->dd-standard
  (reduction-relation
   dadl-C #:domain c
   (--> (in-hole (config n at (r_1 ... hole r_2 ...))
		 (room at desc ((exit dir to) e ...)))
	(in-hole (config n to (r_1 ... hole r_2 ...))
		 (room at desc ((exit dir to) e ...))))))

(module+ test
  (test--> 
   ->dd-standard
   config-with-ordered-rooms
   (term 
    (config
     "Ryan"
     "ell"
     ((room "curry" "piano" ((exit EAST "ell")))
      (room
       "ell"
       "husky"
       ((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
      (room "tunnels" "creepy" ())
      (room "krentzman" "outside" ((exit NORTH "curry")))))))
  (test--> 
   ->dd-standard
   config-with-ordered-rooms2
   (term 
    (config
     "Ryan"
     "curry"
     ((room "curry" "piano" ((exit EAST "ell")))
      (room
       "ell"
       "husky"
       ((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
      (room "tunnels" "creepy" ())
      (room "krentzman" "outside" ((exit NORTH "curry")))))))
  (test--> 
   ->dd-standard
   config-with-ordered-rooms3
   (term 
    (config
     "Ryan"
     "krentzman"
     ((room "curry" "piano" ((exit EAST "ell")))
      (room
       "ell"
       "husky"
       ((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
      (room "tunnels" "creepy" ())
      (room "krentzman" "outside" ((exit NORTH "curry"))))))))




;; =============================================================================
;; -----------------------------------------------------------------------------

;; Problem 1 Extend your Redex model of DADL with a general reduction
;; relation, dubbed ->dd. The relation generates all possible successor
;; configurations for any given configuration. That is, it really is a
;; traversal relation not a function.

;; The relation generates all possible successor configurations for
;; any given configuration. Each step is equivalent to a step in a
;; traversal
(define ->dd
  (reduction-relation
   dadl-C #:domain c
   (--> (in-hole (config n at (r_1 ... hole r_2 ...))
		 (room at desc (e_1 ... (exit dir to) e_2 ...)))
	(in-hole (config n to (r_1 ... hole r_2 ...))
		 (room at desc (e_1 ... (exit dir to) e_2 ...))))))

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

(module+ trace
  (require redex/gui)

  (define drunk-dadl
    (term (config "spencer" "library" 
		  ((room "library" "has many grad students :("
			 ((exit EAST "connor larkin's")))
		   (room "connor larkin's" "starting off the night well"
			 ((exit SOUTH "park")
			  (exit WEST "library")
			  (exit EAST "bukowski's")))
		   (room "bukowski's" "it's getting kind of depressing"
			 ((exit WEST "connor larkin's")
			  (exit NORTH "pour house")
			  (exit EAST "pour house")))
		   (room "pour house" "$7 nachos are #winning"
			 ((exit WEST "bukowski's")
			  (exit SOUTH "bukowski's")
			  (exit NORTH "area four")))
		   (room "area four" "mmmm pizza and manhattans"
			 ((exit SOUTH "pour house")
			  (exit NORTH "park")))
		   (room "park" "first time i ever tried laphroig"
			 ((exit SOUTH "area four")
			  (exit NORTH "connor larkin's")))))))

  (traces ->dd drunk-dadl))

;; -----------------------------------------------------------------------------

;; CONJECTURE ==> the rooms generated by traverse represent a repeated
;; reduction on ->dd
(define-metafunction dadl
  ->dd-traverse-equivalence-conjecture : c -> boolean
  [(->dd-traverse-equivalence-conjecture c)
   (ddtec/a c (at_2 ...))
   ;; We ignore the first value in the list given by traverse because
   ;; it is the room we start in.
   (where (at at_2 ...) (traverse c))])


;; ddtec/a : c (at ...) -> boolean
;; ACCUMULATOR for ->dd-traverse-equivalence-conjecture given the traverse path.
(define-metafunction dadl
  ddtec/a : c (at ...) -> boolean
  [(ddtec/a c ()) #t]
  [(ddtec/a c (at_1 at_2 ...)) 
   (ddtec/a c_2 (at_2 ...))
   (where c_2 (next-config c at_1))
   (side-condition (term c_2))]
  [(ddtec/a c (at_1 at_2 ...)) #f])

;; next-config : c at -> c or #f
;; returns c2 such that c ->dd c2 and c2's at is the given at
(define-metafunction dadl
  next-config : c at -> c or #f
  [(next-config c at)
   c_2
   (where (c_1 ...) ,(apply-reduction-relation ->dd (term c)))
   (where c_2 (get-matching at (c_1 ...)))]
  [(next-config c at) #f])

;; get-matching : at (c ...) -> c or #f
;; picks a config c where c is at "at"
(define-metafunction dadl
  get-matching : at (c ...) -> c or #f
  [(get-matching at (c_0 ... (config n at (r ...)) c_2 ...))
   (config n at (r ...))]
  [(get-matching at (c ...)) #f])

(module+ test
  (test-equal (term (->dd-traverse-equivalence-conjecture ,config-with-ordered-rooms)) #t)

  ;;get-matching
  (let ([cfgs (apply-reduction-relation ->dd config-with-ordered-rooms)])
    (test-equal (term (get-matching "ell" ,cfgs)) (first cfgs))))

(module+ check
  (redex-check dadl
	       c
	       (term (->dd-traverse-equivalence-conjecture c))
	       #:attempts 10000)
  (redex-check dadl
	       c
	       (term (->dd-traverse-equivalence-conjecture c))
	       #:attempts 10000 #:source traverse)
  (redex-check dadl
	       c
	       (term (->dd-traverse-equivalence-conjecture c))
	       #:attempts 10000 #:source ->dd)
  
  ;; Attempts to check this claim with redex-check fail because
  ;; redex-check generates syntax trees that are not semantically
  ;; valid.
  )



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
(define config-with-ordered-rooms2
  (term (config "Ryan" "ell"
		((room "curry" "piano" ((exit EAST "ell")))
		 (room "ell" "husky" ((exit WEST "curry")
				      (exit NORTH "krentzman")
				      (exit EAST "tunnels")))
		 (room "tunnels" "creepy" ())
		 (room "krentzman" "outside" ((exit NORTH "curry")))))))
(define config-with-ordered-rooms3
  (term (config "Ryan" "ell"
		((room "curry" "piano" ((exit EAST "ell")))
		 (room "ell" "husky" ((exit NORTH "krentzman")
				      (exit EAST "tunnels")
				      (exit WEST "curry")))
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
					; [(lookup-exits n ()) ,(error "No such room: " (term n))]
  [(lookup-exits n ((room n desc (e ...)) r_rest ...)) (e ...)]
  [(lookup-exits n ((room n_diff desc (e ...)) r_rest ...))
   (lookup-exits n (r_rest ...))])

(module+ test
  (check-exn exn:fail? (lambda () (term (traverse config1))))
  (test-equal (term (traverse ,config2)) (list "TheLab"))
  (test-equal (term (traverse ,config3)) (list "TheLab" "Punters"))
  (test-equal (term (traverse ,config-with-ordered-rooms)) (list "curry" "ell")))

(module+ test (test-results))

