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

(require "../math_library/symbolic_algebra.rkt")
(require "../elements/general.rkt")
(provide (all-defined-out))







; //////////   E. Generate the ready-for-evaluation total block tf expression, display it, and add f(w) functions to it   //////////


(define (get-total-tf-expression-with-display reduced-total-tf)
  
  ;(set-logger-mode! 'nil)
  ;(newline)
  ;(let ((cached-simplification-result (simplify block)))
  
  (cons 'λ (cons '(s fw1 fw2 fw3 fw4) 
                 (list (tf-to-expanded-expression-with-display reduced-total-tf)))))





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





(define (substitute-s-with-w total-tf-evaluation)
  (λ(w) (total-tf-evaluation (make-rectangular 0 w) 
                             (fw1-func w)  ;4 slots for f(w) functions provided here 
                             (fw2-func w)
                             (fw3-func w) 
                             (fw4-func w))))







; //////////   F. Transform a tf value to an expanded expression for evaluation, and display its infix expression equivalent  //////////


; 'tf-to-expanded-expression-with-display':
;
; (i)   gets as input the reduced value in the poly format, ex.:
;
;       '(ratio (poly-dense s 1 1.6) (poly-dense s 0.2 0))
;
; (ii)  expands the result using 'expand-polynomial', ex:
;
;       '(+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))
;       '(+ (* 0.2 (expt s 1)) (+ (* 0 (expt s 0)) 0))
;
; (iii) transforms the latter to the string format ready for display using
;       'prefix-list-to-infix-string-list', and finally displays it using 'display-tf', ex.:
;
;       '("s^" "1" " + " "1.6")
;       '("0.2" "*s^" "1")
;
;       append a list of strings to a string:
;       "s^1 + 1.6"
;       "0.2*s^1"
;
;             s^1 + 1.6
;       tf:   ---------
;              0.2*s^1
;
; (iv)  returns the expanded value (ii) to the calling function, ready for evaluation, ex.:
;
;       '(/
;         (+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))
;         (+ (* 0.2 (expt s 1)) (+ (* 0 (expt s 0)) 0)))

(define (tf-to-expanded-expression-with-display reduced-total-tf . disp) ;(i)
  (let* ((nom (expand-polynomial (cdr (cdr (get-numer reduced-total-tf))))) ;(ii)
         (den (expand-polynomial (cdr (cdr (get-denom reduced-total-tf))))))

    #|
    (newline) 
    (displayln nom)
    (displayln den)

    ;chebyshev:
    (displayln (list-to-printable-list nom))
    (displayln (list-to-printable-list den))
    |#
        
    (when (null? disp)
      (newline)
      (newline)

      (display-tf (apply string-append (prefix-list-to-infix-string-list nom)) ;(iii)
                  (apply string-append (prefix-list-to-infix-string-list den))))

    (list '/ nom den))) ;(iv)







;///// Util functions for parsing & displaying


; 'expand-polynomial' expands lists of the poly format:
;
;     '(1 1.6)    =>    '(+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))

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





; 'prefix-list-to-infix-string-list' tranforms an expanded polynomial expression in prefix notation list format
; to a list of strings of each element of the expression in infix notation, ex.:
;
;     '(+ (* 1 (expt s 1)) (+ (* 1.6 (expt s 0)) 0))    =>    '("s^" "1" " + " "1.6")

(define (prefix-list-to-infix-string-list lst)
  
  ;helper function
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
                ((list? (car coeff-init)) (cons "f1" (cons " " (list-to-string2 (cdr coeff-init)))))
                
                ((symbol? (car coeff-init)) (cons (symbol->string (car coeff-init)) 
                                                  (cons " " (list-to-string2 (cdr coeff-init)))))
                ((number? (car coeff-init)) (cons (number->string (car coeff-init)) 
                                                  (cons " " (list-to-string2 (cdr coeff-init)))))))))
  
  
  ;helper function
  ; for all the list elements except the first one:
  (define (list-to-printable-list-2 l)
    
    ;(newline)
    ;(display "coeff1: ")
    ;(displayln coeff1)
    
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
    ;(displayln coeff)
    
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





; 'display-tf' handles the display process, after all the format transformations are done:

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
                     (+ (* 0 (expt s 0)) 0))))



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





; 'fast-display' displays a circuit's simplified tf in three formats - for testing:
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
    (displayln res)
    (newline)
    (displayln (tf-to-expanded-expression-with-display res))
    (newline)))
