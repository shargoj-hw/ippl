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

(define-judgment-form dadl
  #:mode (wf-dd I)
  #:contract (wf-dd c)

  [(unique-names (r ...))
   (no-self-cycle* (r ...))
   (no-exit-same-dir* (r ...))
   (not-in-the-void (config n at (r ...)))
   (properly-connected (r ...))
   -------------------------
   (wf-dd (config n at (r ...)))])

;; all rooms have unique names
(define-judgment-form dadl
  #:mode (unique-names I)
  #:contract (unique-names (r ...))

  [-------------------------
   (unique-names ())]

  [(unique-names (r ...))
   (where ((room n_rest desc_rest (e_rest ...)) ...) (r ...))
   (not-eq n n_rest) ...
   -------------------------
   (unique-names ((room n desc (e ...)) r ...))])

(module+ test
  (test-equal (judgment-holds
	       (unique-names
		((room "lib" "smelly" ((exit NORTH "foo")))
		 (room "foo" "barry" ((exit SOUTH "lib")))
		 (room "lib" "not smelly??!!?" ()))))
	      #f)
  (test-equal (judgment-holds
	       (unique-names
		((room "lib" "smelly" ((exit NORTH "foo")))
		 (room "foo" "barry" ((exit SOUTH "lib"))))))
	      #t))

;; no room specifies an exit to itself
(define-judgment-form dadl
  #:mode (no-self-cycle* I)
  #:contract (no-self-cycle* (r ...))

  [(no-self-cycle r) ...
   ------------------------- "no-self-cycle*"
   (no-self-cycle* (r ...))])

(define-judgment-form dadl
  #:mode (no-self-cycle I)
  #:contract (no-self-cycle r)

  [(not-eq n to) ...
   ------------------------- "no-cycle"
   (no-self-cycle (room n desc ((exit dir to) ...)))])


(module+ test
  (test-equal (judgment-holds (not-eq "lib" "foo")) #t)
  (test-equal (judgment-holds (not-eq "lib" "lib")) #f)

  (test-equal (judgment-holds
	       (no-self-cycle (room "lib" "filthy" ((exit NORTH "foo")))))
	      #t)
  (test-equal (judgment-holds
	       (no-self-cycle (room "lib" "filthy" ((exit NORTH "lib")))))
	      #f)

  (test-equal (judgment-holds
	       (no-self-cycle* ((room "lib" "filthy" ((exit NORTH "foo"))))))
	      #t)
  (test-equal (judgment-holds
	       (no-self-cycle* ((room "lib" "filthy" ((exit NORTH "lib")))))) 
	      #f))

;; no room specifies two exits in the same direction going to the same room
(define-judgment-form dadl
  #:mode (no-exit-same-dir* I)
  #:contract (no-exit-same-dir* (r ...))

  [-------------------------
   (no-exit-same-dir* ())]
  
  [(no-exit-same-dir (e ...)) ...
   -------------------------
   (no-exit-same-dir* ((room n desc (e ...)) ...))])

(define-judgment-form dadl
  #:mode (no-exit-same-dir I)
  #:contract (no-exit-same-dir (e ...))

  [-------------------------
   (no-exit-same-dir ())]
  
  [(not-eq e e_rest) ...
   (no-exit-same-dir (e_rest ...))
   -------------------------
   (no-exit-same-dir (e e_rest ...))])

(module+ test
  (test-equal (judgment-holds
	       (no-exit-same-dir*
		((room "lib" "filthy" ((exit NORTH "foo")))))) #t)

  (test-equal (judgment-holds
	       (no-exit-same-dir*
		((room "lib" "filthy"
		       ((exit NORTH "lib") (exit NORTH "lib")))))) #f))

;; the player’s location specifies an actual room
(define-judgment-form dadl
  #:mode (not-in-the-void I)
  #:contract (not-in-the-void c)

  [(matches-one at (at_2 ...))
   -------------------------
   (not-in-the-void (config n at ((room at_2 n_2 (e ...)) ...)))])

(define-judgment-form dadl
  #:mode (matches-one I I)
  #:contract (matches-one at (at ...))

  [-------------------------
   (matches-one at (at at_2 ...))]

  [(matches-one at_1 (at_2 ...))
   -------------------------
   (matches-one at_1 (at_!_1 at_2 ...))])

(module+ test
  (test-equal (judgment-holds
	       (not-in-the-void
		(config "spencer" "void" ((room "lib" "stanks" ()))))) #f)

  (test-equal (judgment-holds
	       (not-in-the-void
		(config "spencer" "void" ((room "void" "purplish" ()))))) #t))

;; the rooms are properly connected to each other, that is, if room x
;; specifies a south-bound door to room y, the latter room must come
;; with a north-bound door going to x.

(define-judgment-form dadl
  #:mode (properly-connected I)
  #:contract (properly-connected (r ...))
  
  [(has-connection (r ...) r) ...
   -------------------------
   (properly-connected (r ...))])

(define-judgment-form dadl
  #:mode (has-connection I I)
  #:contract (has-connection (r ...) r)

  [(has-opposite-exit (r ...) n_cur e_cur) ...
   -------------------------
   (has-connection (r ...) (room n_cur desc_cur (e_cur ...)))])

(define-judgment-form dadl
  #:mode (has-opposite-exit I I I)
  #:contract (has-opposite-exit (r ...) to e)

  [(opposite-exits dir_e dir_r)
   -------------------------
   (has-opposite-exit
    (r_1 ... (room to desc (e_1 ... (exit dir_r to_return) e_2 ...)) r_2 ...)
    to_return
    (exit dir_e to))])

(define-judgment-form dadl
  #:mode (opposite-exits I I)
  #:contract (opposite-exits dir dir)
  [-------------------------
   (opposite-exits NORTH SOUTH)]
  [-------------------------
   (opposite-exits EAST WEST)]
  [-------------------------
   (opposite-exits WEST EAST)]
  [-------------------------
   (opposite-exits SOUTH NORTH)])

(module+ test
  (test-equal (judgment-holds
	       (properly-connected
		((room "baz" "bub" ((exit NORTH "foo")))
		 (room "foo" "fark" ((exit SOUTH "baz")))))) 
	      #t)

  (test-equal (judgment-holds
	       (properly-connected 
		((room "baz" "bub" ((exit NORTH "foo")))
		 (room "foo" "fark" ((exit EAST "baz"))))))
	      #f))

;; This is a hack because we couldn't get side-conditions to work. B-|
(define-judgment-form dadl
  #:mode (not-eq I I)
  
  [------------------------- "not-eq"
   (not-eq any_!_1 any_!_1)])

;; =============================================================================

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
       ((exit NORTH "krentzman") (exit EAST "tunnels") (exit WEST "curry")))
      (room "tunnels" "creepy" ())
      (room "krentzman" "outside" ((exit NORTH "curry"))))))))

;; -----------------------------------------------------------------------------

;; Conjecture: the ->dd-standard relation follows the same path as traverse
(define-metafunction dadl
  traverse/->dd-standard-equivalence-conjecture : c -> boolean
  [(traverse/->dd-standard-equivalence-conjecture c)
   (tddsec/gr c (traverse c))])

;; Iterate over (at ...) checking that ->dd-standard follows the same path
(define-metafunction dadl
  tddsec/gr : c (at ...) -> boolean
  [(tddsec/gr c ()) #t]
  [(tddsec/gr (config n at_here (r ...)) (at_here at_next ...))
   (tddsec/gr c_next (at_next ...))
   (where (c_next) ,(apply-reduction-relation 
		     ->dd-standard
		     (term (config n at_here (r ...)))))]
  [(tddsec/gr c (at_here at_next ...)) #f])

(module+ test
  (check-equal?
   (term (traverse/->dd-standard-equivalence-conjecture
	  ,config-with-ordered-rooms))
   #t)
  (check-equal?
   (term (traverse/->dd-standard-equivalence-conjecture
	  ,config-with-ordered-rooms2))
   #t)
  (check-equal?
   (term (traverse/->dd-standard-equivalence-conjecture
	  ,config-with-ordered-rooms3))
   #t))

(module+ check
  ;; fails due to redex check generating badly formed configs
  (redex-check dadl c (term (traverse/->dd-standard-equivalence-conjecture c))))

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
   (list->set (apply-reduction-relation
	       ->dd 
	       (term (config "Ryan" "ell"
			     ((room "curry" "piano" ((exit EAST "ell")))
			      (room "ell" "husky" ((exit WEST "curry")
						   (exit NORTH "krentzman")
						   (exit EAST "tunnels")))
			      (room "tunnels" "creepy" ())
			      (room "krentzman" "outside" ((exit NORTH "curry"))))))))
   (list->set (term ((config
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
		       (room "krentzman" "outside" ((exit NORTH "curry")))))
		     (config
		      "Ryan"
		      "tunnels"
		      ((room "curry" "piano" ((exit EAST "ell")))
		       (room
			"ell"
			"husky"
			((exit WEST "curry") (exit NORTH "krentzman") (exit EAST "tunnels")))
		       (room "tunnels" "creepy" ())
		       (room "krentzman" "outside" ((exit NORTH "curry"))))))))))

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
