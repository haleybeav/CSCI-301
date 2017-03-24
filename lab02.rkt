#lang r5rs
;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #2
;;
;; Haley Beavers
;; W01168992
;;
;; This program uses conintued fractions to
;; approximate the [reciprocal of the] Golden Ratio, Euler's Number
;; [minus 2], and Lambert's Tangent.


; Continued Fraction Function Calls
; [for your copy-and-pasting pleasure]
; I.   Golden Ratio
;      (cont-frac (lambda (x) 1.0)  
;                 (lambda (x) 1.0)
;                  100)             
; II.  Euler's Number
;      (cont-frac (lambda (x) 1.0)  
;                  euler-d
;                  100)  
; III. Tangent Function
;      (cont-frac (make-lambert-n x) 
;                  lambert-d               
;                  100)     

  (define cont-frac
    (lambda (n d t)
      (define counter 
        (lambda (i)
          (cond
            ((< i t) (/ (n i) (+ (d i) (counter (+ i 1)))))
            (else (/ (n i) (d i ))))))
         (counter 1.0)
    ))
           
  ; Euler's Number
    ; after first two elements thinks of even numbered elements
    ; as functions of nth number of triplets '1,1,e' after frist two elements 
    (define euler-d
      (lambda (nth)
        (cond
          ((= nth 1) 1)
          ((= nth 2) 2)
          ((= (% (- nth 2) 3) 0) (+ (* (/ nth 3) 2) 2))
          (else 1))))
    ; Modulo
      (define %
        (lambda (s n)
          (if (< (- s n) 0)
               s
              (% (- s n) n))))

  ; Lambert's Tangent, or, mytan
    ; calculates numerator, nth odd number 
    (define lambert-d
      (lambda (o)
        (+ -1 (* 2 o))))
    ; calculates denominator of desired tan(x) and
    ; negates to create desired subtraction operation 
    (define make-lambert-n
      (lambda (x) 
        (lambda (i)
          (cond
            ((= 1 i) x)
            (else (* -1 (* x x)))))))