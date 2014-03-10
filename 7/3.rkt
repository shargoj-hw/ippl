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
  ...)
