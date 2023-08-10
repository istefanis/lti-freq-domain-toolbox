#|
lti-freq-domain-toolbox | Functions for studying LTI (linear time-invariant) dynamical systems 
Copyright (C) 2014-2023  Ioannis Stefanis

This file is part of lti-freq-domain-toolbox.

lti-freq-domain-toolbox is free software: you can redistribute it and/or modify it under the terms of 
the GNU General Public License Version 3 as published by the Free Software Foundation.

lti-freq-domain-toolbox is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
See the GNU General Public License Version 3 for more details.

You should have received a copy of the GNU General Public License Version 3 along with lti-freq-domain-toolbox. 
If not, see <https://www.gnu.org/licenses/>.
|#




#|
Parts of this library have been implemented as demonstrated in:

Structure and Interpretation of Computer Programs second edition. 
The MIT Press with the McGraw-Hill Book Company, 1996
Harold Abelson and Gerald Jay Sussman with Julie Sussman, foreword by Alan J. Perlis. 

(noted below as "SICP"), or are modifications of code presented there.

The book is available under a Creative Commons Attribution-ShareAlike 4.0 International License 
(https://creativecommons.org/licenses/by-sa/4.0/). See: <http://mitpress.mit.edu/sicp>.
|#






#lang racket

(require "general.rkt")
(provide (all-defined-out))







; //////////  Symbolic algebra library  //////////



;///// Table storing the available algebraic procedures for all data types, and the operations accompanying it (see: SICP 2.4.3) 

(define global-array '()) 

(define (make-entry k v) (list k v)) 
(define (key entry) (car entry)) 
(define (value entry) (cadr entry)) 

(define (put op type item) 
  (define (put-helper k array) 
    (cond ((null? array) (list (make-entry k item))) 
          ((equal? (key (car array)) k) array) 
          (else (cons (car array) (put-helper k (cdr array)))))) 
  (set! global-array (put-helper (list op type) global-array))) 

(define (get op type) 
  (define (get-helper k array) 
    (cond ((null? array) #f) 
          ((equal? (key (car array)) k) (value (car array))) 
          (else (get-helper k (cdr array))))) 
  (get-helper (list op type) global-array)) 

(define (put-coercion type1 type2 item) (put type1 type2 item))
(define (get-coercion type1 type2) (get type1 type2))






;////// Tag attached to a data object in order to indicate the data type (see: SICP 2.4.2)

(define (attach-tag tag z) ;attach tag to contents to create a tagged data object
  (if (eq? tag 'complex)
      z
      (cons tag z)))




(define (contains-algebraic-symbols? tree)

  (define (search-tree-for-symbols result t)
    (if (null? t)
        result
        (if (list? (car t))
            (or (search-tree-for-symbols result (car t))
                (search-tree-for-symbols result (cdr t)))    
            (or (and (symbol? (car t)) (not (or (eq? (car t) '+)
                                                (eq? (car t) '-)
                                                (eq? (car t) '*)
                                                (eq? (car t) '/))))
                (search-tree-for-symbols result (cdr t))))))

  (search-tree-for-symbols #f tree))




(define (type-tag datum) ;extract tag from the tagged data object
  (if (pair? datum)
      (if (and (list? datum) (contains-algebraic-symbols? datum) (or (eq? (car datum) '+)
                                                                     (eq? (car datum) '-)
                                                                     (eq? (car datum) '*)
                                                                     (eq? (car datum) '/)))
          'symbol
          (car datum))
      (cond ((number? datum) 'complex)
            ((symbol? datum) 'symbol)
            ((and (symbol? datum) (not (or (eq? datum '+)
                                           (eq? datum '-)
                                           (eq? datum '*)
                                           (eq? datum '/))))
             'symbol)
            (error "Bad tagged datum - TYPE-TAG" datum))))




(define (contents datum) ;extract contents from the tagged data object
  (if (pair? datum)
      (if (and (list? datum) (contains-algebraic-symbols? datum) (or (eq? (car datum) '+)
                                                                     (eq? (car datum) '-)
                                                                     (eq? (car datum) '*)
                                                                     (eq? (car datum) '/)))
          datum
          (cdr datum))
      (cond ((number? datum) datum)
            ;((symbol? datum) datum)
            ;#|
            ((and (symbol? datum) (not (or (eq? datum '+)
                                           (eq? datum '-)
                                           (eq? datum '*)
                                           (eq? datum '/))))
             datum)
            ;|#
            (error "Bad tagged datum - CONTENTS" datum))))






;//////  Generic (for all data types) operations (see: SICP 2.5.1)

(define (get-numer x) (apply-generic 'get-numer x))
(define (get-denom x) (apply-generic 'get-denom x))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


(define (negation x) (apply-generic 'negation x))

;(define (greatest-common-divisor a b) (apply-generic 'gcd a b))
(define (reduce n d) (apply-generic 'reduce n d)) 

;(define (sqrt-g z) (apply-generic 'sqrt-g z))
;(define (square-g z) (apply-generic 'square-g z))
;(define (sine z) (apply-generic 'sine z))
;(define (cosine z) (apply-generic 'cosine z))
;(define (atangent z1 z2) (apply-generic 'atangent z1 z2))

;(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))





;///// Application of the appropriate procedure to arguments of each data type (see: SICP 2.4.3)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types"
                 (list op type-tags))))))







;////// Symbolic algebra package

(define (install-symbolic-package)
  
  (put 'add '(symbol symbol) (λ (x y) (list '+ x y)))
  (put 'sub '(symbol symbol) (λ (x y) (list '- x y)))
  (put 'mul '(symbol symbol) (λ (x y) (list '* x y)))
  (put 'div '(symbol symbol) (λ (x y) (list '/ x y)))
  
  (put 'add '(symbol complex) (λ (x y) (if (= y 0) 
                                           x
                                           (list '+ x y))))
  
  (put 'add '(complex symbol) (λ (x y) (if (= x 0)
                                           y
                                           (list '+ x y))))
  
  (put 'sub '(symbol complex) (λ (x y) (if (= y 0)
                                           x
                                           (list '- x y))))
  
  (put 'sub '(complex symbol) (λ (x y) (if (= x 0)
                                           (list '- y)
                                           (list '- x y))))
  
  (put 'mul '(symbol complex) (λ (x y) (cond ((= y 0) 0)
                                             ((= y 1) x)
                                             (else (list '* x y)))))
  
  (put 'mul '(complex symbol) (λ (x y) (cond ((= x 0) 0)
                                             ((= x 1) y)
                                             (else (list '* x y)))))
  
  (put 'div '(symbol complex) (λ (x y) (if (= y 1)
                                           x
                                           (list '/ x y))))
  
  (put 'div '(complex symbol) (λ (x y) (if (= x 0)
                                           0
                                           (list '/ x y))))
  
  ;(put 'gcd '(complex complex) (λ (a b) (gcd a b)))
  
  ;(put 'sqrt-g '(complex) (λ (x) (sqrt x)))
  ;(put 'square-g '(complex) (λ (x) (* x x)))
  ;(put 'sine '(complex) (λ (x) (sin x)))
  ;(put 'cosine '(complex) (λ (x) (cos x)))
  ;(put 'atangent '(complex complex) (λ (x y) (atan x y)))
  
  ;(put 'equ? '(complex complex) (λ (x y) (= x y)))
  (put '=zero? '(symbol) (λ (x) #f))
  (put 'negation '(symbol) (λ (x) '(- x)))
  
  ;(put 'exp '(complex complex) (λ (x y) (expt x y)))
  
  ;'done-symbolic
  )







;////// Complex numbers algebra package

(define (install-complex-package)
  
  (put 'add '(complex complex) (λ (x y) (+ x y)))
  (put 'sub '(complex complex) (λ (x y) (- x y)))
  (put 'mul '(complex complex) (λ (x y) (* x y)))
  (put 'div '(complex complex) (λ (x y) (/ x y)))
  
  ;(put 'gcd '(complex complex) (λ (a b) (gcd a b)))
  
  ;(put 'sqrt-g '(complex) (λ (x) (sqrt x)))
  ;(put 'square-g '(complex) (λ (x) (* x x)))
  ;(put 'sine '(complex) (λ (x) (sin x)))
  ;(put 'cosine '(complex) (λ (x) (cos x)))
  ;(put 'atangent '(complex complex) (λ (x y) (atan x y)))
  
  ;(put 'equ? '(complex complex) (λ (x y) (= x y)))
  (put '=zero? '(complex) (λ (x) (= x 0)))
  (put 'negation '(complex) (λ (x) (- x)))
  
  ;(put 'exp '(complex complex) (λ (x y) (expt x y)))
  
  ;'done-complex
  )







;////// Ratios algebra package (see: SICP 2.5.1)

(define (install-ratio-package) 
  
  (define (numer r) (car r))
  (define (denom r) (cadr r))
  
  (define (make-rat n d) (list n d))
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  
  ;(define (equ?-rat x y) (and (equ? (numer x) (numer y)) (equ? (denom x) (denom y))))
  ;(define (=zero?-rat x) (=zero? (numer x)))
    
  
  
  (put 'get-numer '(ratio) numer)
  (put 'get-denom '(ratio) denom)
  
  
  (define (tag r) (attach-tag 'ratio r))
  
  
  (put 'add '(ratio ratio) (λ (x y) (tag (add-rat x y))))
  (put 'sub '(ratio ratio) (λ (x y) (tag (sub-rat x y))))
  (put 'mul '(ratio ratio) (λ (x y) (tag (mul-rat x y))))
  (put 'div '(ratio ratio) (λ (x y) (tag (div-rat x y))))
  
  ;(put 'sqrt-g '(rational) (lambda (x) (sqrt (/ (numer x) (denom x)))))
  ;(put 'square-g '(rational) (lambda (x) (tag (make-rat (square (numer x)) (square (denom x))))))
  ;(put 'sine '(rational) (lambda (x) (sin (/ (numer x) (denom x)))))
  ;(put 'cosine '(rational) (lambda (x) (cos (/ (numer x) (denom x)))))
  ;(put 'atangent '(rational rational) (lambda (x y) (atan (/ (numer x) (denom x)) (/ (numer y) (denom y)))))
  
  ;(put 'equ? '(rational rational) (lambda (x y) (equ?-rat x y)))
  ;(put '=zero? '(rational) =zero?-rat)
  
  ;(put 'negation '(rational) (lambda (x) (/ (- (numer x)) (denom x))))
  
  (put 'make 'ratio (λ (n d) (tag (make-rat n d))))
    
  
  ;'done-ratio
  )


(define (make-ratio n d)
  ((get 'make 'ratio) n d))









;////// Polynomials algebra package (see: SICP 2.5.3)


(define (install-poly-dense-package)
  
  
  ; orig
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? v) (symbol? v))
  (define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  
  
  ;////// add-poly
  
  ; orig
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var - ADD-POLY" (list p1 p2))))
  
  ; orig
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1
                                 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2
                                 (add-terms (rest-terms L2) L1)))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  

  
  (define (adjoin-term t1 L) ;it returns ordered lists - see: SICP ex. 2.89
    #|
    (newline)
    (displayln t1)
    (displayln L)
    |#
    (if (and (number? (coeff t1)) (=zero? (coeff t1)))
        L
        (if (not (empty-termlist? L))
            (let ((t2 (first-term L)))
              (cond ((> (order t1) (order L)) (adjoin-term t1 (cons 0 L)))
                    ((= (order t1) (order L)) 
                     
                     #|
                     (displayln "test")
                     (displayln (coeff t1))
                     (displayln (coeff t2))
                     |#
                     
                     (cons 
                      (add (coeff t1) (coeff t2))
                      (cdr L)))
                    (else (cons t2 (adjoin-term t1 (cdr L))))))
            t1)))  ;// ex. '(5 0)
  
  

    
  ;////// sub-poly
  
  (define (sub-poly p1 p2) 
    (add-poly p1 (neg-poly p2)))
  
  (define (neg-poly p) 
    (make-poly (variable p)
               (negate-terms (term-list p))))
  
  (define (negate-terms L)
    (if (empty-termlist? L)
        '()
        (cons (negation (car L)) (negate-terms (cdr L)))))
  

  
   
  ;////// mul-poly  
  
  ; orig
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var - MUL-POLY" (list p1 p2))))
  
  ; orig
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  ; orig
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term (make-term (+ (order t1) (order t2))
                                  (mul (coeff t1) (coeff t2))) 
                       (mul-term-by-all-terms t1 (rest-terms L))))))
  
  

   
  ;////// div-poly (see: SICP ex. 2.90)
  
  ;// it returns only the quotient if the remainder is zero
  
  #|
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (let ((results-list (div-terms (term-list p1) (term-list p2))))
          (if (> (length results-list) 1)
              (if (null? (cadr results-list))
                  (make-poly-dense (variable p1) (car results-list))
                  (map (λ (x) (make-poly-dense (variable p1) x)) results-list))
              (map (λ (x) (make-poly-dense (variable p1) x)) results-list)))
        (error "Polys not in same var - DIV-POLY" (list p1 p2))))
  |#
  ;#|
  
  
  ; orig-3
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (if (null? (cadr result))
              (if (null? (car result))
                  (list (make-poly-dense (variable p1) '(0)))
                  (list (make-poly-dense (variable p1) (car result))))
              
              (map (λ (x) (if (null? x)
                              (make-poly-dense (variable p1) '(0))
                              (make-poly-dense (variable p1) x)))
                   result)))
        (error "Polys not in same var - DIV-POLY" (list p1 p2))))
  ;|# 
  
  
  #| 
  ; orig-2
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (map (λ (x) (if (null? x)
                        (make-poly-dense (variable p1) '(0))
                        (make-poly-dense (variable p1) x)))
             (div-terms (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var - DIV-POLY" (list p1 p2))))
  ;|#  
  
  
  #|
  ; orig
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1)
                        (variable p2))
        (map (λ (x) (make-poly-dense (variable p1) x)) 
             (div-terms (term-list p1)
                        (term-list p2)))
        (error "Polys not in same var - DIV-POLY" (list p1 p2))))
  ;|#  
  
  
  ; orig
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       ; compute rest of result recursively
                       (div-terms (add-terms L1 (mul-term-by-all-terms (make-term new-o (negation new-c)) L2)) L2)                      
                       ))
                  ; form complete result
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result))
                  ))))))
  
  
  
  ;//////  gcd (see: SICP ex. 2.94)
  
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        (make-poly-dense (variable p1) (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var - GCD-POLY" (list p1 p2))))
  
  
  (define (gcd-terms L1 L2)
    (if (empty-termlist? L2)
        
        ;// simplification of the integer coefficients of the result
        (let ((coeff-list L1))
          (let ((gcdivisor (apply gcd coeff-list)))
            (mul-scheme-number-by-all-terms (/ 1 gcdivisor) L1)))
        
        (gcd-terms L2 (pseudoremainder-terms L1 L2))))
  
  
  (define (remainder-terms L1 L2) 
    (cadr (div-terms L1 L2)))
  
  
  
  ;//////  see: SICP ex. 2.96
  
  (define (pseudoremainder-terms L1 L2) 
    (cadr (div-terms 
           (mul-scheme-number-by-all-terms
            (expt (coeff (first-term L2))
                  (+ 1 (order (first-term L1)) (- 0 (order (first-term L2))))) L1) L2)))
  
  (define (mul-scheme-number-by-all-terms x L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t1 (first-term L)))
          (adjoin-term (make-term (order t1) 
                                  
                                  (mul x (coeff t1))
                                  
                                  ; unstable:
                                  ; (round-to-3-digits (* x (coeff t1)))
                                  
                                  ) 
                       (mul-scheme-number-by-all-terms x (rest-terms L))))))
  
  
  
  
  ;//////  reduce (see: SICP ex. 2.97)
  
  ; orig
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) 
                        (variable p2))
        
        ;// added:
        (if (equal? (term-list p1) (term-list p2))
            ;'(1 1) ;change
            (list (make-poly-dense (variable p1) '(1)) (make-poly-dense (variable p1) '(1)))
            (map (lambda (x) (make-poly-dense (variable p1) x)) 
                 (reduce-terms (term-list p1) (term-list p2))))
        (error "Polys not in same var - REDUCE-POLY" (list p1 p2))))
  
  
  
  ;#|
  (define (reduce-terms L1 L2)
    (let ((GCD (gcd-terms L1 L2))
          (max-order 
           (max (order (first-term L1)) 
                (order (first-term L2)))))
      
      #|
      (newline)
      (display "gcd :")
      (displayln GCD)
      |#      
      
      (let ((results-list-n        
             (div-terms 
              (mul-scheme-number-by-all-terms 
               (expt (coeff (first-term GCD))
                     (+ 1 max-order (- 0 (order (first-term GCD))))) L1) GCD))
            (results-list-d
             (div-terms 
              (mul-scheme-number-by-all-terms 
               (expt (coeff (first-term GCD))
                     (+ 1 max-order (- 0 (order (first-term GCD))))) L2) GCD)))
        
        #|
        (newline)
        (display "results-list-n :")(displayln results-list-n)
        (display "results-list-d :")(displayln results-list-d)
        |#        
        
        ;// division with GCD returns no remainder, so:
        (let ((reduced-n (car results-list-n))
              (reduced-d (car results-list-d)))
          
          ;// simplification of the integer coefficients
          (let ((coeff-list-n reduced-n)
                (coeff-list-d reduced-d))
            
            #|
            (newline)
            (display "coeff-list-n :")(displayln coeff-list-n)
            (newline)
            (display "coeff-list-d :")(displayln coeff-list-d)
            |#
            
            
            #| integer coefficients:
            (let ((gcdivisor (apply gcd (append coeff-list-n coeff-list-d))))
              #|
              (newline)
              (display "gcdivisor :")(displayln gcdivisor)
              |#              
              ;// result
              (list (mul-scheme-number-by-all-terms (/ 1 gcdivisor) reduced-n)
                    (mul-scheme-number-by-all-terms (/ 1 gcdivisor) reduced-d)))             
            |#
            
            
            ;// result-orig
            (list (mul-scheme-number-by-all-terms (/ 1.0 (car coeff-list-n)) reduced-n)
                  (mul-scheme-number-by-all-terms (/ 1.0 (car coeff-list-n)) reduced-d))
            
            
            ;// result
            (list (map (λ (x) (round-decimal x 3)) (mul-scheme-number-by-all-terms (/ 1.0 (car coeff-list-n)) reduced-n))
                  (map (λ (x) (round-decimal x 3)) (mul-scheme-number-by-all-terms (/ 1.0 (car coeff-list-n)) reduced-d)))            
            )))))
  ;|#
  
  
  
  
  ;// the following constructors and selectors are defined for terms 
  ;// represented as lists of (+ order 1) elements: (coeff 0 0...0)
  (define (the-empty-termlist) '())
  (define (first-term L) (make-term (- (length L) 1) (car L)))
  (define (rest-terms L) (cdr L))
  (define (empty-termlist? L) (null? L))
  
  (define (make-term order coeff)
    (if (< order 0)
        '()
        (cons coeff (make-term (- order 1) 0))))
  (define (order t) (- (length t) 1))
  (define (coeff t) (car t))
  
  
  
  
  ;// interface to the rest of the system
  (define (tag p) (attach-tag 'poly-dense p))
  (put 'make 'poly-dense (λ (var terms) (tag (make-poly var terms))))
  
  
  (put 'add '(poly-dense poly-dense) (λ (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(poly-dense poly-dense) (λ (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(poly-dense poly-dense) (λ (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(poly-dense poly-dense) div-poly)
  

  ;// it will allow adjoin-term to work for polynomials with coefficients that are themselves polynomials
  ;(put '=zero? '(poly-dense) (lambda (p) (=zero?-termlist (term-list p))))
  (put 'negation '(poly-dense) (λ (p) (tag (neg-poly p))))
  
  ;(put 'gcd '(poly-dense poly-dense) gcd-poly)
  (put 'reduce '(poly-dense poly-dense) reduce-poly)
  
  ;'done-poly-dense
  ;(newline)
  ;'lti-freq-domain-toolbox_loaded
  )



(define (make-poly-dense var terms)
  ((get 'make 'poly-dense) var terms))







;///// Installing the algebraic procedures of each package to the table (see: SICP 2.4.3) 

(install-symbolic-package)
(install-complex-package)
(install-ratio-package)
(install-poly-dense-package)







;///// tests

#|
(add 1 2)
(add 1 1.5)
(add 1 (make-rectangular 1 4))
(add (make-ratio 3 5) (make-ratio 2 5))
(sub (make-ratio 3 5) (make-ratio 3 5))
(define p1 (make-poly-dense 'x '(1 0 1)))
(define p2 (make-poly-dense 'x '(1 0 0 1)))
(define r (make-ratio p2 p1))
r
(newline)
(add p1 p2)
(sub p1 p2)
(mul p1 p2)
(sub p2 (make-poly-dense 'x '(1)))
(newline)
(div p1 p2)
|#

