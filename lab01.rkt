#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #1
;;
;; Haley Beavers
;; W01168992
;;
;; Calculates pi to user specified accuracy
;; using a slowly converging series
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;calculates pi to user specified accuracy 
(define make-pi
  (lambda (acc)
    (series 4.0 1.0 0 acc)))
    
;recursive series function
(define series
  (lambda (n d s acc)
    ;next factor
    (define nf
      (lambda (n d)
        (/ (* n -1) (+ d 2))))
    ;next sum
    (define ns
      (lambda (s n d)
        (+ s (/ n d))))
    ;absolute value
    (define abs
      (lambda (nf)
        (if (< nf 0)
            (* nf -1)
            nf)))
    ;accuracy met?
    (if (< (abs(/ n d)) acc)
      s
      (series (* n -1) (+ d 2) (ns s n d) acc))))
          



    

    