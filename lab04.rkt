#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #4
;;
;; Haley Beavers
;; W01168992
;;
;; Computes compositions of functions as well as
;; repeated compositions.
;;
;; Repeated Applications Calls
;; Recursive -> repeated-rec 
;; Iterative -> repeated-iter
;; 
;; Happy Composing!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Compose
(define compose
  (lambda (f1 f2)
    (lambda (x)
      (f1 (f2 x)))))

; Square
(define square
  (lambda (x)
    (* x x)))

; Add One 
(define add1
  (lambda (x)
    (+ x 1)))

; Repeated Iterative Tail-Recursive  
(define repeated-iter
  (lambda (f1 nth)
    (lambda (arg)
      ; Iterative Tail-Recursive Procedure 
      (define loop
        (lambda (iter num)
          (if (= 1 iter)
              (f1 num)
              (loop (- iter 2) ((compose f1 f1) num)))))
      ; Calling Iterative Procedure 
      (cond
        ((= nth 0) arg)
        ((> nth 0) (if (odd? nth)
                       (loop nth arg)
                       (loop (- nth 1) (f1 arg))))
        (else "Rut-Ro")))))

; Repeated Recusion 
(define repeated-rec
  (lambda (f1 nth)
    (lambda (arg)
      ; Recursive Procedure 
      (define rec
        (lambda (arg nth)
          (if (zero? nth) 
            arg
            (f1 (rec arg (- nth 1))))))
      ; Calling Recursive Procedure 
      (cond
        ((> nth -1) (rec arg nth))
        (else "Rut-Ro")))))
