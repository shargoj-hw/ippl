#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket

;; This module implements the DADL language. Its main function reads configurations from STDIN
;; and computes a left-hand traversal path for the player through the room map after type checking 
;; the configuration for basic consistency conditions. 

(provide
  ;; -> Void 
  ;; traverse a configuration 
  ;; effect: read a configuration from stdin and write a sequence of room names to stdout 
  main)

;; ---------------------------------------------------------------------------------------------------
;; IMPLEMENTATION

(require "3-parser.rkt")

(module+ test (require rackunit))

;; ---------------------------------------------------------------------------------------------------
;; MAIN 

(define (main) 
  (let* [(config (xexpr->config (dadl->xexpr (current-input-port))))
	 (type-checked?
	  (andmap (lambda (pred) (pred config)) 
		  (list rooms-unique? no-recursive-rooms? 
			no-exit-conflicts?  valid-start? 
			reflexive-exits?)))]
    (if type-checked?
	(for [(room (crawl-dungeon config))]
	  (displayln room))
	(displayln "type error!"))))

;; =============================================================================

(require xml)
(collapse-whitespace #t)
(require rackunit)

;;; Runner: 
#;
(module+ main
  (let* [(config (xexpr->config (dadl->xexpr (current-input-port))))
	 (type-checked?
	  (andmap (lambda (pred) (pred config)) 
		  (list rooms-unique? no-recursive-rooms? 
			no-exit-conflicts?  valid-start? 
			reflexive-exits?)))]
    (if type-checked?
	(for [(room (crawl-dungeon config))]
	  (displayln room))
	(displayln "type error!"))))

;;; Data Definitions:

;; A Config is a:
;;   * (make-config String String [listOf Room])
(struct config (player-name start rooms) #:transparent)

;; A Room is a:
;;   * (make-room String String [listOf Direction])
(struct room (name description exits) #:transparent)

;; An Exit is a:
;;   * (make-exit Direction String)
(struct exit (dir to) #:transparent)

;; A Direction is one of:
;;   * NORTH
;;   * SOUTH
;;   * EAST
;;   * WEST
(define NORTH 'north)
(define SOUTH 'south)
(define EAST 'east)
(define WEST 'west)

;;; Test Data:
;; Test input XML input streams
(define exml1
  (open-input-string
   "<configuration player=\"Matthias\" at=\"TheLab\"></configuration>"))
(define exml2
  (open-input-string
   (string-append "<configuration player=\"Matthias\" at=\"TheLab\">"
		  "  <room name=\"TheLab\" description=\"piano\">"
		  "  </room>"
		  "</configuration>")))
(define exml3
  (open-input-string
   (string-append "<configuration player=\"Matthias\" at=\"TheLab\">"
		  "  <room name=\"TheLab\" description=\"piano\">"
		  "    <exit direction=\"north\" to=\"Punters\" />"
		  "  </room>"
		  "  <room name=\"Punters\" description=\"piano\">"
		  "  </room>"
		  "</configuration>")))

;; Test parsed XML data
(define exexpr1 '(configuration ((at "TheLab") (player "Matthias"))))
(define exexpr2
  '(configuration ((at "TheLab") (player "Matthias"))
                  (room ((description "piano") (name "TheLab")))))
(define exexpr3
  '(configuration ((at "TheLab") (player "Matthias"))
                  (room ((description "piano") (name "TheLab"))
                        (exit ((direction "north") (to "Punters"))))
                  (room ((description "piano") (name "Punters")))))

;; Test Configurations
(define config1 (config "Matthias" "TheLab" '()))
(define config2 (config "Matthias" "TheLab" (list (room "TheLab" "piano" '()))))
(define config3
  (config "Matthias" "TheLab"
          (list (room "TheLab" "piano" (list (exit NORTH "Punters")))
                (room "Punters" "piano" '()))))
(define config-with-recursive-rooms 
  (config "Matthias" "punters" 
          (list (room "punters" "homey" (list (exit NORTH "punters")
                                              (exit SOUTH "CCIS")))
                (room "CCIS" "homey" (list (exit NORTH "punters"))))))
(define config-with-ordered-rooms
  (config "Ryan" "curry"
          (list (room "curry" "piano" (list (exit EAST "ell")
                                            (exit SOUTH "krentzman")))
                (room "ell" "husky" (list (exit WEST "curry")
                                          (exit NORTH "krentzman")
                                          (exit EAST "tunnels")))
                (room "tunnels" "creepy" (list (exit WEST "ell")))
                (room "krentzman" "outside" (list (exit NORTH "curry")
                                                  (exit SOUTH "ell"))))))
(define config-with-nonunique-rooms
  (config "Matthias" "punters" 
          (list (room "punters" "homey" '())
                (room "punters" "homey" '()))))
(define config-with-invalid-start1
  (config "Ryan" "punters" '()))
(define config-with-invalid-start2
  (config "Ryan" "punters" (list (room "CCIS" "smelly" '()))))
(define config-with-exit-conflicts
  (config "Jim" "library" 
          (list (room "library" "stuffed" (list (exit NORTH "t")
                                                (exit NORTH "t")))
                (room "t" "smelly" (list (exit SOUTH "library"))))))
(define config-with-nonreflective-exits
  (config "Matthias" "punters" 
          (list (room "punters" "homey" (list (exit NORTH "punters")
                                              (exit SOUTH "CCIS")))
                (room "CCIS" "homey" (list (exit EAST "punters"))))))
;; dadl->xexpr : Input-Port -> Xexpr
;; Read the DADL XML from the given input port and generate an x-expression
;; representation.
(define (dadl->xexpr inport)
  (xml->xexpr ((eliminate-whitespace '(configuration room exit))
               (document-element (read-xml inport)))))

(check-equal? (dadl->xexpr exml1) exexpr1)
(check-equal? (dadl->xexpr exml2) exexpr2)
(check-equal? (dadl->xexpr exml3) exexpr3)

;; xexpr->config : Xexpr -> Configuration
;; Parse an x-expression that represents a Configuration from DADL
;; and create the appropriate Config struct.
;; Will raise an exception if format does not meet Config spec.
(define (xexpr->config xexpr)
  (letrec [(get
	    (lambda (n l)
	      (let [(v (assoc n l))]
		(if v (second v) v))))
           (parse-config
	    (match-lambda
	     [(list 'configuration attribs rooms ...)
	      (config (get 'player attribs)
		      (get 'at attribs)
		      (map parse-room rooms))]))
           (parse-room
	    (match-lambda
	     [(list 'room attribs exits ...)
	      (room (get 'name attribs)
		    (get 'description attribs)
		    (map parse-exit exits))]))
           (parse-exit
	    (match-lambda
	     [(list 'exit attribs)
	      (exit (parse-direction
		     (get 'direction attribs))
		    (get 'to attribs))]))
           (parse-direction
	    (lambda (d)
	      (cond [(string-ci=? d "north") NORTH]
		    [(string-ci=? d "south") SOUTH]
		    [(string-ci=? d "east") EAST]
		    [(string-ci=? d "west") WEST]
		    [else (error "invalid direction" d)])))]
    (parse-config xexpr)))

(check-equal? (xexpr->config exexpr1) config1)
(check-equal? (xexpr->config exexpr2) config2)
(check-equal? (xexpr->config exexpr3) config3)


;; lookup-room : String (listOf Room) -> (or room #f)
;; Looks up the room with the given name, returns false if not found
(define (lookup-room name lor)
  (findf (lambda (r) (string=? (room-name r) name)) lor))

;; lookup-exit : String (listOf Exit) -> (or exit #f)
;; Looks up the exit with the given to, returns false if not found
(define (lookup-exit to loe)
  (findf (lambda (e) (string=? (exit-to e) to)) loe))

;; crawl-dungeon : Config -> [Listof String]
;; Crawls the given config layout according to the
;; first-possible-exit rule, and returns the list of visited rooms when done.
;; With a helper for an accumulator
(define (crawl-dungeon aconfig)
  (let [(lor (config-rooms aconfig))]
    (local [(define (crawl-dungeon* seen)
              (let* [(current-room  (or (lookup-room (first seen) lor)
                                        (error "room not found" (first seen))))
                     (current-exits (if current-room 
					(room-exits current-room) 
					'()))
                     (next-exit?    (and (not (empty? current-exits))
                                         (not (member 
					       (exit-to (first current-exits)) 
					       seen))))
                     (next-exit     (if next-exit?
					(first current-exits)
					#f))]
                (if next-exit
		    (crawl-dungeon* (cons (exit-to next-exit) seen))
		    (reverse seen))))]
      (crawl-dungeon* (list (config-start aconfig))))))

(check-exn exn:fail? (lambda () (crawl-dungeon config1)))
(check-equal? (crawl-dungeon config2) (list "TheLab"))
(check-equal? (crawl-dungeon config3) (list "TheLab" "Punters"))
(check-equal? (crawl-dungeon config-with-ordered-rooms) (list "curry" "ell"))

;;; Type Checks

;; rooms-unique? : Config -> boolean
;; Checks that all rooms have unique names
(define (rooms-unique? c)
  (let [(room-names (map room-name (config-rooms c)))]
    (equal? room-names (remove-duplicates room-names))))

(check-false (rooms-unique? config-with-nonunique-rooms))
(check-true (rooms-unique? config-with-ordered-rooms))

;; no-recursive-rooms? : Config -> boolean
;; Checks that no room specifies an exit to itself
(define (no-recursive-rooms? c) 
  (local [(define (recursive-room? r)
            (let [(name (room-name r))
                  (exit-names (map exit-to (room-exits r)))]
              (not (member name exit-names))))]
    (andmap recursive-room? (config-rooms c))))

(check-false (no-recursive-rooms? config-with-recursive-rooms))
(check-true (no-recursive-rooms? config-with-ordered-rooms))

;; no-exit-conflicts? : Config -> boolean
;; Checks that no room specifies two exits in the same direction going to the 
;; same room
(define (no-exit-conflicts? c)
  (local [(define (exits-no-duplicates? exits) 
            (equal? exits (remove-duplicates exits)))]
    (andmap (lambda (r) (exits-no-duplicates? (room-exits r))) 
	    (config-rooms c))))

(check-false (no-exit-conflicts? config-with-exit-conflicts))
(check-true (no-exit-conflicts? config-with-ordered-rooms))

;; valid-start? : Config -> boolean
;; Checks that the player’s location specifies an actual room
(define (valid-start? c)
  (list? (member (config-start c) (map room-name (config-rooms c)))))

(check-false (valid-start? config-with-invalid-start1))
(check-false (valid-start? config-with-invalid-start2))
(check-true (valid-start? config-with-ordered-rooms))

;; reflexive-exits? : Config -> boolean
;; Checks that the rooms are properly connected to each other, that is, if room x
;; specifies a south-bound door to room y, the latter room must come
;; with a north-bound door going to x.
(define (reflexive-exits? c)
  (let [(lor (config-rooms c))]
    (local [(define (opposite-direction d)
              (cond [(equal? d NORTH) SOUTH]
                    [(equal? d SOUTH) NORTH]
                    [(equal? d EAST) WEST]
                    [(equal? d WEST) EAST]))
            ;; name is name of origin room, e is the exit to be checked
            (define (reflexive-exit? name e)
              (let* [(r (lookup-room (exit-to e) lor))
                     (es (room-exits r))
                     (e-check (lookup-exit name es))]
                (and (equal? (opposite-direction (exit-dir e-check))
                             (exit-dir e))
                     (equal? (opposite-direction (exit-dir e))
                             (exit-dir e-check)))))
            (define (room-reflexive-exits? r)
              (andmap (lambda (e) (reflexive-exit? (room-name r) e)) 
                      (room-exits r)))]
      (andmap room-reflexive-exits? (config-rooms c)))))

(check-false (reflexive-exits? config-with-nonreflective-exits))
(check-true (reflexive-exits? config-with-ordered-rooms))
