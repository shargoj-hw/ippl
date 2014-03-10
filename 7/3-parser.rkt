#lang at-exp racket

;; This module provides a configuration reader that reads an XML configuration 
;; from STDIN and produces a direct X-expression representation of the XML. 

(provide
 ;; An XConfiguration is:
 ;;  (cons 'configuration (list (list 'at String) (list 'name String)) [List-of XRoom])
 ;; An XRoom is: 
 ;;  (cons 'room (list (list 'description String) (list 'name String)) [List-of XExit])
 ;; An XExit is: 
 ;;  (cons 'exit (list (list 'direction XDirection) (to String))))
 ;; An XDirection is one of: 
 ;;  -- "east"
 ;;  -- "west"
 ;;  -- "north" 
 ;;  -- "south"
 
 ;; read an XML configuration and deliver it as a configuration 
 read-configuration ;; -> XConfiguration
 )

;; ---------------------------------------------------------------------------------------------------
;; READING 

(require xml)

(module+ test
  (require rackunit))

;; -> Configuration 
;; create a configuration from an XML specification at current input port
;; effect: read from port 

(module+ test
  (define sample-configuration
    '(configuration 
      ((at "living") (name "matthias"))
      (room ((description "piano") (name "living"))
            (exit ((direction "east") (to "sitting"))))
      (room ((description "piano") (name "sitting"))
            (exit ((direction "east") (to "dining")))
            (exit ((direction "west") (to "living"))))
      (room ((description "piano") (name "dining"))
            (exit ((direction "west") (to "sitting"))))))
  
  (define sample-configuration:string
    @string-append{
                   <configuration at="living" name="matthias">
                   <room name="living" description="piano">
                   <exit direction="east" to="sitting" />
                   </room>
                   <room name="sitting" description="piano">
                   <exit direction="east" to="dining" />
                   <exit direction="west" to="living" />
                   </room>
                   <room name="dining" description="piano">
                   <exit direction="west" to="sitting" />
                   </room>
                   </configuration>
                   })
  
  (check-equal? 
   (with-input-from-string sample-configuration:string read-configuration)
   sample-configuration))

(define (read-configuration)
  (define as-xml (read-xml/element))
  (define wo-white ((eliminate-whitespace '() not) as-xml))
  (define as-xexpr (xml->xexpr wo-white))
  as-xexpr)
