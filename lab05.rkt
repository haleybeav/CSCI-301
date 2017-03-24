#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301
;; Winter 2017
;;
;; Lab #5
;;
;; Haley Beavers
;; W01168992
;;
;; Translates user-input string to Pig Latin.
;; Helper Function Guide 
;; operation      input       output 
;; piglatin       string      string
;; first-vowel    list char   list char
;; which-vowel    list char   char
;; is-vowel?      char        #t/#f
;; before-vowel   list char   list char 
;;                empty list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define piglatin
  (lambda (str)
    ; mapping of list of characters which will be converted to list of strings and then to a singular string using list->string operation.
    (string-join (map (lambda (list-char) (if (is-vowel? (car list-char))
                                             ; if the first character in a string is a vowel append "way" to end.
                                             (list->string (append  list-char '(#\w #\a #\y)))
                                             ; otherwise, first vowel + rest of word + characters before first vowel + "ay".
                                             (list->string (append (member (first-vowel list-char) list-char) (append  ((before-vowel list-char) '()) '(#\a #\y))))))
    ; converting user-input string, str, to list of words and then to list of list of characters in each individual word to be mapped again above. 
    (map (lambda (list-str) (string->list list-str)) (string-split str))))))

; finds the first vowel in list of characters
; passes list of vowels to which-vowel
; which will return first singular vowel character
; in list of vowel characters.
(define first-vowel
  (lambda (split-char)
    (if (member (car split-char)  '(#\a #\e #\i #\o #\u))
        (which-vowel (member (car split-char)  '(#\a #\e #\i #\o #\u)))
        (first-vowel (cdr split-char)))))
                                       
; input a list containing a, e, i,
; o or u returns first element
; of input list.
(define which-vowel
  (lambda (found-vowel)
    (cond
      ((eqv? (car found-vowel) #\a) #\a)
      ((eqv? (car found-vowel) #\e) #\e)
      ((eqv? (car found-vowel) #\i) #\i)
      ((eqv? (car found-vowel) #\o) #\o)
      (else #\u))))

; returns true or false values 
; determines whether or not 
; a char is a vowel defined
; as a, e, i, o, or u. 
(define is-vowel?
  (lambda (let)
    (cond
      ((eqv? let #\a) #t)
      ((eqv? let #\e) #t)
      ((eqv? let #\i) #t)
      ((eqv? let #\o) #t)
      ((eqv? let #\u) #t)
      (else #f))))

; returns list of char
; before the first vowel
; when input a list of char and
; an empty list so a function call
; might look like ((before-vowel list-char) '()).
(define before-vowel
  (lambda (list-char)
    (lambda (letters-before)
      (if (eqv? (first-vowel list-char) (car list-char))
          letters-before
          ((before-vowel (cdr list-char)) (append letters-before (list (car list-char))))))))


