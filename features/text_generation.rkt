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






#lang racket

(require "../math_library/general.rkt")
(require "../math_library/symbolic_algebra.rkt")
(require "../elements/general.rkt")
(require "../util/display_modes.rkt")
(provide (all-defined-out))



(define-namespace-anchor n_anchor)
(define anchor (namespace-anchor->namespace n_anchor))







; //////////   E. Transforming the tf value expression to the string format, for display  //////////



; list-to-list-of-strings tranforms an expanded polynomial expression in (prefix notation) list format
; to a simple list of strings of each element of the expression in infix notation, ex.:
;
;     '(+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))    =>    '("s^" "1" " + " "1.6")


(define (list-to-list-of-strings lst)
  
  
  (define (list-to-string coeff-init rest)
    
    (define (list-to-string2 coeff)
      (if (null? coeff)
          (cons ")"
                rest)
          (if (= (length coeff) 1)
              (cond 
                ;((list? (car coeff)) (cons "f1" (list-to-string2 (cdr coeff))))
                ((list? (car coeff)) (list-to-string (car coeff) (list-to-string2 (cdr coeff))))
                
                ((symbol? (car coeff)) (cons (symbol->string (car coeff)) (list-to-string2 (cdr coeff))))
                ((number? (car coeff)) (cons (number->string (car coeff)) (list-to-string2 (cdr coeff)))))
              (cond 
                ;((list? (car coeff)) (cons "f1" (cons " " (list-to-string2 (cdr coeff)))))
                ((list? (car coeff)) (list-to-string (car coeff) (cons " " (list-to-string2 (cdr coeff)))))
                
                
                ((symbol? (car coeff)) (cons (symbol->string (car coeff)) 
                                             (cons " " (list-to-string2 (cdr coeff)))))
                ((number? (car coeff)) (cons (number->string (car coeff)) 
                                             (cons " " (list-to-string2 (cdr coeff)))))))))
    
    (if (null? coeff-init)
        rest
        (cons "("
              (cond 
                ;((list? (car coeff-init)) (cons "f1" 
                ;                                  (cons " " (list-to-string2 (cdr coeff-init)))))
                ((list? (car coeff-init)) (cons "f1" 
                                                (cons " " (list-to-string2 (cdr coeff-init)))))
                
                ((symbol? (car coeff-init)) (cons (symbol->string (car coeff-init)) 
                                                  (cons " " (list-to-string2 (cdr coeff-init)))))
                ((number? (car coeff-init)) (cons (number->string (car coeff-init)) 
                                                  (cons " " (list-to-string2 (cdr coeff-init)))))))))
  
  
  
  
  ; for all the list elements except the first one:
  (define (list-to-printable-list-2 l)
    
    ;(newline)
    ;(display "coeff1: ")
    ;(display coeff1)
    ;(newline)
    
    ;cheb
    (let* ((coeff1 (car (cdr (car (cdr l)))))
           (coeff (if (number? coeff1) 
                      (real-part coeff1)
                      coeff1)))
      (let ((sign (if (number? coeff)
                      (if (> coeff 0)
                          " + "
                          " - ")
                      " + ")))
        
        
        ; simpifications for coeff 0 and 1:
        (cond ((eq? coeff 0)
               (if (not (pair? (car (cddr l))))
                   '()
                   (list-to-printable-list-2 (car (cddr l)))))
              
              ((or (eq? coeff 1) (eq? coeff -1))
               (if (not (pair? (car (cddr l))))
                   (list sign (number->string (abs coeff)))
                   (cons sign
                         (cons "s^"
                               (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                     (list-to-printable-list-2 (car (cddr l))))))))
              
              (else
               (if (not (pair? (car (cddr l))))
                   (cond ((list? coeff) (cons sign (list-to-string coeff '())))
                         ((symbol? coeff) (list sign (symbol->string coeff)))
                         (else (list sign (number->string (abs coeff)))))
                   (cond ((list? coeff) 
                          (cons sign
                                (list-to-string coeff
                                                (cons "*s^"
                                                      (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                                            (list-to-printable-list-2 (car (cddr l))))))))
                         ((symbol? coeff) 
                          (cons sign
                                (cons (symbol->string coeff)
                                      (cons "*s^"
                                            (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                                  (list-to-printable-list-2 (car (cddr l))))))))
                         (else 
                          (cons sign
                                (cons (number->string (abs coeff))
                                      (cons "*s^"
                                            (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                                  (list-to-printable-list-2 (car (cddr l)))))))))
                   ))
              ))))
  
  
  
  ; for the first list element (special handling so as not to add its sign if that positive):
  (let ((coeff (car (cdr (car (cdr lst))))))
    
    ;(newline)
    ;(display "coeff: ")
    ;(display coeff)
    ;(newline)
    
    (cond ((eq? coeff 0) 
           (if (not (pair? (car (cddr lst))))
               '()
               (list-to-printable-list-2 (car (cddr lst)))))
          
          ((or (eq? coeff 1) (eq? coeff -1))
           (if (not (pair? (car (cddr lst))))
               (cond ((list? coeff) (list-to-string coeff '()))
                     ((symbol? coeff) (list (symbol->string coeff)))
                     (else (list (number->string (abs coeff)))))
               (if (> coeff 0)
                   (cons "s^" 
                         (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                               (list-to-printable-list-2 (car (cddr lst)))))
                   (cons "-" (cons "s^"
                         (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                               (list-to-printable-list-2 (car (cddr lst)))))))))
          
          (else
           (if (not (pair? (car (cddr lst))))
               (cond ((list? coeff) (list-to-string coeff '()))
                     ((symbol? coeff) (list (symbol->string coeff)))
                     (else (list (number->string (abs coeff)))))
               (cond ((list? coeff) 
                      (list-to-string coeff
                                      (cons "*s^"
                                            (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                                                  (list-to-printable-list-2 (car (cddr lst)))))))
                     ((symbol? coeff) 
                      (cons (symbol->string coeff)
                            (cons "*s^"
                                  (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                                        (list-to-printable-list-2 (car (cddr lst)))))))
                     (else 
                      (cons (number->string coeff)
                            (cons "*s^"
                                  (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                                        (list-to-printable-list-2 (car (cddr lst))))))))                                  
               ))
          )))





; list-of-strings-to-string appends a list of strings to a string, ex.:
;
;     '("s^" "1" " + " "1.6")   =>   "s^1 + 1.6"

(define (list-of-strings-to-string l)
  (if (> (length l) 1)
      (apply string-append l) ;string-append takes as input a list of strings
      (car l)))





; display-tf handles the display process, after all the format transformations are done:

(define (display-tf nom-string den-string)
  (let ((l1 (string-length nom-string))
        (l2 (string-length den-string)))
    (cond ((or (> l1 40) (> l2 40))
           ;(newline)
           (displayln "tf:")
           (newline)
           (displayln nom-string)
           (displayln (make-ratio-line 46))
           (displayln den-string)
           (newline))
          (else  
           (let ((max-length (max l1 l2)))
             (let ((max-l1 (- max-length l1))
                   (max-l2 (- max-length l2)))
               (display "      ")
               (display (make-space-line (round (/ max-l1 2))))
               (displayln nom-string)
               (display "tf:   ")
               (displayln (make-ratio-line max-length))
               (display "      ")                      
               (display (make-space-line (round (/ max-l2 2))))
               (displayln den-string)
               (newline)))))))


(define (make-space-line length) (make-string length #\ ))
(define (make-ratio-line length) (make-string length #\-))





; display tests:

#|
(define test '(/ (+ (* 6 (expt s 10)) 
                    (+ (* 8 (expt s 9)) 
                       (+ (* 6 (expt s 8)) 
                          (+ (* -4 (expt s 7)) 
                             (+ (* 4 (expt s 6)) 
                                (+ (* 2 (expt s 5)) 
                                   (+ (* 5 (expt s 4)) 
                                      (+ (* 7 (expt s 3)) 
                                         (+ (* 7 (expt s 2)) 
                                            (+ (* 5 (expt s 1)) 
                                               (+ (* 2 (expt s 0)) 0)))))))))))
                 (+ (* -6 (expt s 10)) 
                    (+ (* 4 (expt s 9)) 
                       (+ (* 4 (expt s 8)) 
                          (+ (* 14 (expt s 7)) 
                             (+ (* 8 (expt s 6)) 
                                (+ (* -2 (expt s 5)) 
                                   (+ (* -3 (expt s 4)) 
                                      (+ (* -5 (expt s 3)) 
                                         (+ (* -7 (expt s 2)) 
                                            (+ (* -5 (expt s 1)) 
                                               (+ (* -2 (expt s 0)) 0)))))))))))
                 ))


(define test2 '(/ (+ (* 1 (expt s 0)) 0) 
                  
                  (+ (* 1 (expt s 1)) 
                     (+ (* 0 (expt s 0)) 0)))
  )





(define n-l (list-to-printable-list (cadr test2)))
(define d-l (list-to-printable-list (caddr test2)))

(newline)
(displayln n-l)
(newline)
(displayln d-l)



(define n-s (printable-list-to-string n-l))
(define d-s (printable-list-to-string d-l))

(newline)
;(displayln n-s)
(newline)
;(displayln d-s)
;|#












; //////////   F1. Modifying the format of the circuit's tf value for display and evaluation  //////////



; fast-display procedure displays a circuit's simplified tf in three formats - for testing:
;
;     (ratio (poly-dense s 5 8) (poly-dense s 1 0))
;
;           s^1 + 1.6
;     tf:   ---------
;            0.2*s^1
;
;     (/ (+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))
;        (+ (* 0.2 (expt s 1)) (+ (* 0 (expt s 0)) 0)))

(define (fast-display block)
  (let ((res (get-simplified-block-value block)))
    (newline)
    (display res)
    (newline)
    (newline)
    (display (ratio-to-list res))
    (newline)
    (newline)))




; map-with-s expands lists of the poly format:
;
;     '(poly-dense s 1 1.6)    =>    '(+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))

(define (expand-polynomial list-orig)
  
  (define (loop lst)
    (let ((l (if (null? lst)
                 0
                 (length lst))))
      (cond ((= l 0) 0)
            #|
            ((= l 1)
             (list '+ (car lst)
                   0))
            ((= l 2)
             (list '+ 
                   (list '* (car lst) 's)
                   (loop (cdr lst))))
|#
            ((= l (length list-orig))
             (list '+ 
                   (list '* (car lst) (list 'expt 's (- l 1))) 
                   (loop (cdr lst))))
            (else
             (list '+ (list '* (car lst) (list 'expt 's (- l 1)))
                   (loop (cdr lst)))))))
  
  (loop list-orig))







; before a ratio of polynomials is reduced by ratio-to-list,
; the coefficient lists must be evaluated so that the coefficients are simplified 
; - no evaluations must be done on symbols:

(define (contains-symbols?-tree l)
  (if (null? l)
      #f
      (if (list? (car l))
          (or (contains-symbols?-tree (car l))
              (contains-symbols?-tree (cdr l)))    
          (or (and (symbol? (car l)) (not (or (eq? (car l) '+)
                                              (eq? (car l) '-)
                                              (eq? (car l) '*)
                                              (eq? (car l) '/))))
              (contains-symbols?-tree (cdr l))))))



(define (round-decimal-tree l) 
  (if (null? l)
      '()
      (if (list? (car l))
          (cons (round-decimal-tree (car l))
                (round-decimal-tree (cdr l)))
          (if (symbol? (car l))
              (cons (car l) (round-decimal-tree (cdr l)))
              (cons (round-decimal (car l) 3) (round-decimal-tree (cdr l)))))))






; ratio-to-list:
; (i)   gets as input the simplified value in the poly format, ex.:
;
;       > (bode (pi-controller 5 8 a))
;       > (get-simplified-block-value a)
;       '(ratio (poly-dense s 5 8) (poly-dense s 1 0))
;
; (ii)  performs the reduction of the polynomial using reduce from the symbolic algebra package, ex.:
;
;       '((poly-dense s 1 1.6) (poly-dense s 0.2 0))
;
; (iii) expands the result using expand-polynomial, ex:
;
;       '(+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))
;       '(+ (* 0.2 (expt s 1)) (+ (* 0 (expt s 0)) 0))
;
; (iv)  transforms the latter to the string format ready for display using
;       list-to-list-of-strings and then list-of-strings-to-string, and finally displays it, ex.:
;
;       '("s^" "1" " + " "1.6")
;       '("0.2" "*s^" "1")
;
;       "s^1 + 1.6"
;       "0.2*s^1"
;
;             s^1 + 1.6
;       tf:   ---------
;              0.2*s^1
;
; (v)   returns the expanded value (iii) to the calling function, ready for evaluation, ex.:
;
;       '(/
;         (+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))
;         (+ (* 0.2 (expt s 1)) (+ (* 0 (expt s 0)) 0)))


(define (ratio-to-list value . disp) ;(i)
  (let ((a (get-numer value)) ;'(poly-dense s 5 8)
        (b (get-denom value)))
    (let ((a-term-list (cdr (cdr a))) ;'(5 8)
          (b-term-list (cdr (cdr b))))

      #|
      (newline)
      (newline)
      (display a-term-list)
      (newline)
      (display b-term-list)
      (newline)
      |#
      (let* ((res (or (contains-symbols?-tree a-term-list) ;(ii)
                      (contains-symbols?-tree b-term-list)))
             
             (division (if res
                           
                           ;no reduction in this case - so numbers must be rounded
                           (list (make-poly-dense 's (map (λ (x) (if (list? x)
                                                                     (if (contains-symbols?-tree x)
                                                                         (round-decimal-tree x)
                                                                         (round-decimal (eval x anchor) 3))
                                                                     (if (not (symbol? x))
                                                                         (round-decimal (eval x anchor) 3)
                                                                         x))) 
                                                          a-term-list))

                                 (make-poly-dense 's (map (λ (x) (if (list? x)
                                                                     (if (contains-symbols?-tree x)
                                                                         (round-decimal-tree x)
                                                                         (round-decimal (eval x anchor) 3))
                                                                     (if (not (symbol? x))
                                                                         (round-decimal (eval x anchor) 3)
                                                                         x)))
                                                          b-term-list)))

                           ;reduction
                           (reduce (make-poly-dense 's (map (λ(x) (eval x anchor)) a-term-list))
                                   (make-poly-dense 's (map (λ(x) (eval x anchor)) b-term-list)))))
             
             (nom (expand-polynomial (cdr (cdr (car division))))) ;(iii)
             (den (expand-polynomial (cdr (cdr (cadr division))))))

        #|
        (let ((nom (map-with-s a-term-list))
              (den (map-with-s b-term-list)))
        
        (newline) 
        (display a-term-list)
        (display division)
        (display nom)
        (newline)
        (display den)
        (newline)
        |#
        
        ;chebyshev:
        #|
        (newline)
        (display den)
        (newline)
        (display (list-to-printable-list nom))
        (newline)
        (display (list-to-printable-list den))
        (newline)
        |#
        
        (when (null? disp)
          (newline)
          (newline)
          (display-tf (list-of-strings-to-string (list-to-list-of-strings nom)) ;(iv)
                      (list-of-strings-to-string (list-to-list-of-strings den))))

        (list '/ nom den))))) ;(v)





; a lite version of the same procedure:

(define (ratio-to-list-lite value)
  (let ((a (get-numer value))
        (b (get-denom value)))
    (list '/ 
          (expand-polynomial (cdr (cdr a)))
          (expand-polynomial (cdr (cdr b))))
    ))








; //////////   F2. Adding f(w) variables and computing the total block value   //////////


(define (get-total-tfs-value block)
  
  ;(set-logger-mode! 'nil)
  ;(newline)
  
  ;(let ((cached-simplification-result (simplify block)))
  (cons 'λ (cons '(s fw1 fw2 fw3 fw4) 
                 (list (ratio-to-list (get-simplified-block-value block))))))




; f(w) functions: for adding expressions of w or s to the tf polynomial
; - use them only in the s-domain:

(define (simple-delay w T) (exp (* (make-rectangular 0 (- w)) T)))
(define (simple-delay-s s T) (/ 1 (exp (* s T))))

(define (comb-filter w a) (+ 1 (* a (simple-delay w 0.05))))

(define (fw1-func w) (comb-filter w 0.5))
(define (fw2-func w) (comb-filter w 0.7))
(define (fw3-func w) (simple-delay w 1))
(define (fw4-func s) (simple-delay-s s 1))

;(compare (tf '(0.02 fw1) '(1)) (tf '(0.02 fw2) '(1)))



