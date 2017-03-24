#lang racket
;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #3
;;
;; Haley Beavers
;; W01168992
;;
;; identifies and returns the shorter of two lists 

(define shorter
  (lambda (ls1 ls2)
    (define trim
      (lambda (ls11 ls22)
        (cond
          ((and (not (null? ls11)) (not (null? ls22))) (trim (cdr ls11) (cdr ls22)) )
          (else (cond
                  ((null? ls11) ls1)
                  (else ls2))))))
    (trim ls1 ls2)))
    
    