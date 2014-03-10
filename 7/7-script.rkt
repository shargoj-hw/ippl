#! /bin/sh
#|
exec racket -tm "$0" ${1+"$@"}
|#
#lang racket

;; This module runs the DADL Redex model as if it were a DADL processor. 
;; Its main function reads configurations from STDIN and uses the evaluate
;; function from the model to type-check the configuration and, unless there
;; are type error, to determine a left-hand path. 

(provide main)

(require redex "6.rkt" "3-parser.rkt")

;; ---------------------------------------------------------------------------------------------------
;; TESTER: run the model on existing external test cases 

;; -> Void 
;; effect: read a configuration from stdin and write a sequence of room names to stdout 

(module+ test
  (test-equal 
   ;; this external test failed because the first type system
   ;; accepted exits to itself
   (with-output-to-string (λ () (with-input-from-file "Tests/1-2in.xml" main))) 
   "type error!\n"))

(define (main) 
  (define configuration (xexpr->configuration (read-configuration)))
  (define path-through (term (evaluate ,configuration)))
  (if (string? path-through)
      (displayln path-through)
      (for ((room path-through)) (displayln room))))

;; ---------------------------------------------------------------------------------------------------
;; PARSING: translate external representation into Redex configuration 

;; Xexpr -> DD.c
;; parse an Xexpr into a Configuration if possible 
(module+ test
  (define x1
    (term 
     (configuration 
      (player "matthias" living) 
      ((room living "piano" ((east sitting)))
       (room sitting "sofa" ((west living)))))))
  
  (define x2
    (term 
     (configuration (player "matthias" living) ())))
  
  ;; I exported DD and could therefore run these tests:
  ;; (test-equal (redex-match? DD c x1) #t)
  ;; (test-equal (redex-match? DD c x2) #t)
  
  (test-equal 
   (xexpr->configuration
    '(configuration ((name "matthias") (at "living"))
                    (room ((name "living") (description "piano"))
                          (exit ((direction "east") (to "sitting"))))
                    (room ((name "sitting") (description "sofa"))
                          (exit ((direction "west") (to "living"))))))
   x1)
  
  (test-equal
   (xexpr->configuration 
    '(configuration ((at "living") (name "matthias"))))
   x2))

(define (xexpr->configuration x)
  (match x 
    [(list 'configuration (list-no-order `(at ,location) `(name ,name)) rooms ...)
     `(configuration (player ,name ,(string->symbol location)) ,(map xexpr->room rooms))]
    [else (error 'xexpr->configuration "not a configuration: ~e" x)]))

;; Xexpr -> DD.r
;; parse an Xexpr into a Room, if possible 
(define (xexpr->room x)
  (match x 
    [(list 'room (list-no-order `(name ,name) `(description ,description)) exits ...)
     `(room ,(string->symbol name) ,description ,(map xexpr->exit exits))]
    [(list 'room (list-no-order `(name ,name)) exits ...)
     `(room ,(string->symbol name) "no description supplied" ,(map xexpr->exit exits))]
    [else (error 'xexpr->room "not a room: ~e" x)]))

;; Xexpr -> DD.(d x) 
;; parse an Xexpr into an Exit, if possible 
(define (xexpr->exit x)
  (match x 
    [(list 'exit (list-no-order `(direction ,direction) `(to ,goal)))
     `(,(string->symbol direction) ,(string->symbol goal))]
    [else (error 'xexpr->exit "not an exit: ~e" x)]))
