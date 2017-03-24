#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #6
;;
;; Haley Beavers
;; W01168992
;;
;; An, intermediate solution, this
;; program converts a user-defined
;; number to it's word form through
;; a sextillion for the most part.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns number with
; English place-name
; input number : output list or string?

; in first example geof says he wants the
; empty list if the user inputs '0' and in
; the second set of instructions he wants
; the program to output 'zero' if the user
; inputs '0'. Contradiction. 

(define number-name
   (lambda (n)
     (cond
       ((= n 0) '())
       ((<= n 100) (list n))
       (else ((trip (leng-num (car (trim (list n))))) ((digits (car (trim (list n)))) '()))))))

; splits number into list of
; lists with each element in list
; a list of an individual number
; input number, empty list : output list or list of lists
(define digits
  (lambda (n)
    (lambda (ls)
      (if (> n 0)
        ((digits (quotient n 10)) (append (list (list (remainder n 10))) ls))
         ls))))

; uses list of lists created
; by digits procedure to find
; length of user-passed number
; input number : output number 
(define leng-num
  (lambda (n)
    (length ((digits n) '()))))

; creates list of triplets
; within user-passed number
; input number, list of lists : list of lists 
; (define ls 123456) ->
;   ((trip (leng-num ls)) ((digits ls) '())) ->
;     '((123) (456)) 
(define trip
  (lambda (size)
    (lambda (ls)
      (cond
        ((= size 0) ((name-append (length ls)) ls))
        ((= (% size 3) 0) ((trip (- size 3)) (cdddr (append ls (list (list (+ (* 100 (caar ls)) (+ (* 10 (caadr ls)) (caaddr ls)))))))))                
        ((= (% size 3) 2) ((trip (- size 2)) (cddr (append ls (list (list (+ (* 10 (caar ls)) (caadr ls))))))))
        ((= (% size 3) 1) ((trip (- size 1)) (cdr (append ls (list (car ls))))))))))
                          
; appends names to triplets
; according to length of list
; input number, list of lists : output list
(define name-append
  (lambda (size)
    (lambda (ls)
      (cond
        ((= size 0) ls)
        ((eqv? 0 (caar ls)) ((name-append (- size 1)) (cdr ls)))
        ((= size 1) ((name-append (- size 1)) (append (cdr ls) (car ls))))
        (else ((name-append (- size 1)) (append (cdr ls) (append (car ls) (name size)))))))))

; naming function
; based on length of number
; input number : output list 
(define name
  (lambda (size)
    (cond
      ((= size 2) '(thousand))
      ((= size 3) '(million))
      ((= size 4) '(billion))
      ((= size 5) '(trillion))
      ((= size 6) '(quadrillion))
      ((= size 7) '(quintillion))
      ((= size 8) '(sextillion))
      (else "Number passed is in the septillions and above, please pass a number in the sextillions and below."))))

; trims insignificant
; leading zeroes
; input list : output list 
(define trim
  (lambda (ls)
    (if (eqv? (car ls) 0)
        (trim (cdr ls))
        ls)))

; Symbolic Modulus Operator
; superfluous, yet oh so necessary
; input number, number : output number 
  (define %
    (lambda (s n)
      (if (< (- s n) 0)
         s
        (% (- s n) n))))