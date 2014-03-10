#lang racket

;; This module models the DADL language with Redex: 
;; -- the syntax 
;; -- the static (type) checking 
;; -- a (standard) reduction relation 
;; -- and an evaluation function that maps configurations to left-hand paths.

(provide
 ;; DD is the DADL syntax, modeled in Redex
 DD 

 ;; DD.c -> [List-of Symbol] or "type error!"
 ;; (term (evaluate c)) type-checks c and, if type-correct,
 ;; determines a left-hand path of the player through c using
 ;; the reduction relation
 evaluate)

;; -----------------------------------------------------------------------------
;; IMPLEMENTATION

(require redex)

(define-language DD
  (c (configuration p (r ...)))
  (p (player named x))
  (r (room x items e))
  (e ((d x) ...))
  (d north
     east
     south 
     west)
  (items string)
  (named string) ; name 
  (x variable-not-otherwise-mentioned))

(define-metafunction DD-static 
  evaluate : c -> (x ...) or string
  ...)
