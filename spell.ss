; *********************************************
; *  314 Principles of Programming Languages  *
; *  Fall 2015                                *
; *  Roberto Merino                           *
; *********************************************
#lang racket

(include "include.ss")
(include "dictionary.ss")

;;-------------------------------------------------
;(require racket/include)
;; contains "ctv", "A", and "reduce" definitions
;(load "include.ss")
;; contains simple dictionary definition
;(load "test-dictionary.ss")
;; -----------------------------------------------------

(define number
  (lambda (x)
    (cond[(null? (cdr x)) (reduce + (list(* 5387 31)) (car x)) ]
         [else  (+(car x) (* 31 (number(cdr x))))])))
   
;compare-helper
(define compare-helper
  (lambda (one lstdict)
    (cond[(null? lstdict) 0]
         [(= one (car lstdict)) 1]
         [(not( = one (car lstdict))) (compare-helper one (cdr lstdict))])))

;compares the list and the word
(define compare
  (lambda (lstword lstdict)
    (cond[(null? lstword) #t]
         [(= (compare-helper (car lstword) lstdict) 1)  (compare (cdr lstword) lstdict)   ]
         [(= (compare-helper (car lstword) lstdict) 0) #f])))

;;takes in a list of hash functions and the specific word 
(define convert
  (lambda (func word)
    (cond[(null? func) '()]
         [else (cons ((car func) word) (convert (cdr func) word))])))


;Repeats through the list 
(define helper
  (lambda (f1 l)
    (map f1 l)))

;;takes in hash functions and the dictionary
;;creates a list of hash values from the dictionary  
(define convertAll
  (lambda (f l)
    (cond[(null? f) '()]
         [else (append (helper (car f) l) (convertAll (cdr f) l))])))   
   
;; -----------------------------------------------------
;; KEY FUNCTION

(define key
  (lambda (w) 
    (number(map( lambda(x) (ctv x)) w))))

; -----------------------------------------------------
;; EXAMPLE KEY VALUES
;;   (key '(h e l l o))     = 154238504134
;;   (key '(w a y))         = 160507203 
;;   (key '(r a i n b o w)) = 148230379423562
;; -----------------------------------------------------

;; HASH FUNCTION GENERATORS

;; value of parameter "size" should be a prime number
(define gen-hash-division-method
  (lambda (size)
    (lambda (word)
      (modulo (key word) size))))

;; value of parameter "size" is not critical
;; Note: hash functions may return integer values in "real"
;; takes care of numbers such as 17.0 => 17

(define gen-hash-multiplication-method
  (lambda (size)
    (lambda (k)
      (inexact->exact (floor (* size (-(*(key k) 0.6780329887) (floor (*(key k) 0.6780329887))))))))) 
    ;; range of values: 0..size-1

;; -----------------------------------------------------
;; EXAMPLE HASH FUNCTIONS AND HASH FUNCTION LISTS

(define hash-1 (gen-hash-division-method 70111))
(define hash-2 (gen-hash-division-method 89997))
(define hash-3 (gen-hash-multiplication-method 700224))
(define hash-4 (gen-hash-multiplication-method 900))

(define hashfl-1 (list hash-1 hash-2 hash-3 hash-4))
(define hashfl-2 (list hash-1 hash-3))
(define hashfl-3 (list hash-2 hash-3))

;; -----------------------------------------------------
;; EXAMPLE HASH VALUES
;;   to test hash function implementation
;;
;;  (hash-1 '(h e l l o))     ==> 53236
;;  (hash-1 '(w a y))         ==> 23124 
;;  (hash-1 '(r a i n b o w)) ==> 17039 
;;
;;  (hash-2 '(h e l l o))     ==> 25588 
;;  (hash-2 '(w a y))         ==> 42552 
;;  (hash-2 '(r a i n b o w)) ==> 70913 
;;
;;  (hash-3 '(h e l l o))     ==> 415458.0 
;;  (hash-3 '(w a y))         ==> 390702.0 
;;  (hash-4 '(r a i n b o w)) ==> 503286.0 
;;
;;  (hash-4 '(h e l l o))     ==> 533.0
;;  (hash-4 '(w a y))         ==> 502.0
;;  (hash-4 '(r a i n b o w)) ==> 646.0
;; -----------------------------------------------------

;;takes in a list a hash functions and the dictionary of words
;;compares hash values from the specific word with a list of hash values from the dictionary
(define gen-checker
  (lambda (hashfunctionlist dict)
    (lambda(word)
      (compare (convert hashfunctionlist word) (convertAll hashfunctionlist dict)))))

;; -----------------------------------------------------
;; SPELL CHECKERS

(define checker-1 (gen-checker hashfl-1 dictionary))
(define checker-2 (gen-checker hashfl-2 dictionary))
(define checker-3 (gen-checker hashfl-3 dictionary))

;;-----------------------------------------------------
;; EXAMPLE APPLICATIONS OF A SPELL CHECKER
;;These are some sample test cases

(checker-1 '(a r g g g g))      ;==> #f
(checker-1 '(h e l l o))        ;==> #t
(checker-1 '(a r g g g g))      ;==> #f
(checker-2 '(h e l l o))        ;==> #t
(checker-1 '(h e l l o))        ;==> #t
(checker-1 '(w a y))            ;==> #t 
(checker-1 '(r a i n b o w))    ;==> #t
(checker-1 '(r a i n b o))      ;==> #f
(checker-1 '(h e a v y w e i g h t))   ;==> #t
(checker-1 '(h e a v y w e i l h t))   ;==> #t
