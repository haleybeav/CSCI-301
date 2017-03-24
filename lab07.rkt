#lang racket
(require racket/trace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #7
;;
;; Haley Beavers
;; W01168992
;;
;; This program converts a user-defined
;; number to it's word form from 0 through
;; to the sextillions.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; returns number with
; English place-name
; input number : output list 
(define number-name
   (lambda (n)
     (cond
       ((< n 0) "Please enter a number greater than or equal to zero.")
       ((= n 0) '(zero))
       (else (de-list (map (lambda (y) (re-list y)) (map (lambda (z) (if-number z)) ((trip (leng-num (car (trim (list n))))) ((digits (car (trim (list n)))) '())))))))))

; converts a list of lists
; into a single list
; input list of lists : output list 
(define de-list
  (lambda (ls)
    (if (list? (car ls))
        (de-list (append (cdr ls) (car ls)))
        ls)))
    
; converts non-list elements
; to lists to prep. for appending
; input list : output list
(define re-list
  (lambda (y)
    (if (list? y)
         y
        (list y))))

; returns name of numbers
; under one thousand
; input number : output list
(define if-number
  (lambda (z)
    (if (number? z)
        (name<1000 z)
         z)))

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
      (else "Number passed is in the septillions and above, please enter a positive number in the sextillions and below."))))

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

; returns name of user-input
; integer in words.
; input number : output list 
(define name<1000
  (lambda (n)
    (cond
      ((= n 0) '(zero))
      ((< n 0) "Please enter an integer greater than or equal to zero and less than one thousand.")
      ((<= 1000 n) "Please enter an integer greater than or equal to zero and less than one thousand.")
      (else (append (hundreds n) (tens-ones n))))))

; logic to decide whether or
; not there is a number in the
; hundreds place and if so
; append the place-name 'hundred'
; to the number's one's place name
; which is fetched by the ones-hundreds
; procedure
; input number : output list
(define hundreds
  (lambda (n)
    (if (= (quotient n 100) 0)                                
      '()
      (append (ones-hundreds (quotient n 100)) '(hundred)))))

; logic to decide whether or
; not there is a number in the
; tens place and if so check to
; see if number is in the teens
; and therefore has an irregular
; number name and must be have the
; one's and ten's place passed to
; the teens procedure for naming 
; input number : output list
(define tens-ones
  (lambda (n)
     (if (and (>= (remainder n 100) 10) (<= (remainder n 100) 19))
       (teens (remainder n 10))
       (append (tens (quotient (remainder n 100) 10)) (ones-hundreds (remainder n 10))))))
    
; returns list containing
; word for number in the
; one's place.
; input number : output list 
(define ones-hundreds
  (lambda (n)
    (cond
      ((= n 1) '(one))
      ((= n 2) '(two))
      ((= n 3) '(three))
      ((= n 4) '(four))
      ((= n 5) '(five))
      ((= n 6) '(six))
      ((= n 7) '(seven))
      ((= n 8) '(eight))
      ((= n 9) '(nine))
      (else '()))))

; returns list containing
; word for number in the
; 'teen's' place as those
; names are irregular an
; independent naming-procedure
; was required.
; input number : output list 

(define teens
  (lambda (n)
    (cond
      ((= n 0) '(ten))
      ((= n 1) '(eleven))
      ((= n 2) '(twelve))
      ((= n 3) '(thirteen))
      ((= n 4) '(fourteen))
      ((= n 5) '(fifteen))
      ((= n 6) '(sixteen))
      ((= n 7) '(seventeen))
      ((= n 8) '(eighteen))
      (else '(nineteen)))))

; returns list containing
; word for number in the
; ten's place.
; input number : output list 
(define tens
  (lambda (n)
    (cond
      ((= n 2) '(twenty))
      ((= n 3) '(thirty))
      ((= n 4) '(fourty))
      ((= n 5) '(fifty))
      ((= n 6) '(sixty))
      ((= n 7) '(seventy))
      ((= n 8) '(eighty))
      ((= n 9) '(ninety))
      (else '()))))