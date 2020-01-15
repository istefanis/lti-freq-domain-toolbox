#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#






#lang racket

(require "../math_library/symbolic_algebra.rkt")
(require "../elements/general.rkt")
(provide (all-from-out "../math_library/symbolic_algebra.rkt"))
(provide (all-from-out "../elements/general.rkt"))
(provide (all-defined-out))



(define-namespace-anchor n_anchor)
(define anchor (namespace-anchor->namespace n_anchor))







; //////////   E. Transforming the tf value expression to the string format, for display  //////////



; a function that tranforms a list structure to a simple list of strings of each element:

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
               (cons "s^"
                     (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                           (list-to-printable-list-2 (car (cddr lst)))))))
          
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





; a function that appends a list of strings to a string:

(define (list-of-strings-to-string l)
  (if (> (length l) 1)
      (apply string-append l) ;string-append takes as input a list of strings
      (car l)))


(define (make-space-line length) (make-string length #\ ))

(define (make-ratio-line length) (make-string length #\-))





; display tf handles the display process - after all the format transformations are done:

(define (display-tf nom-string den-string)
  (let ((l1 (string-length nom-string))
        (l2 (string-length den-string)))
    (cond ((or (> l1 40) (> l2 40))
           (newline)
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






; //// display tests:

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












; //////////   F. Modifying the format of the circuit's tf value for the interpreter  //////////



; fast-display procedure displays a circuit's simplified tf in three formats - for testing:

(define (fast-display block)
  (let ((res (get-simplified-block-value block)))
    (display res)
    (newline)
    (newline)
    (display (ratio-to-list res))
    (newline)
    (newline)))




; map-with-s expands lists of the poly format, to polynomials:

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
; i)   gets as input the simplified value in the poly format
; ii)  performs the reduction of the polynomial using reduce from the symbolic algebra package
; iii) expands the result using expand-polynomial
; iv)  transforms the latter to the string format ready for display using
;      list-to-list-of-strings and then list-of-strings-to-string
; v)   displays the tf string
; vi)  returns the expanded value to the calling function, ready for evaluation


(define (ratio-to-list value . disp)
  (let ((a (get-numer value))
        (b (get-denom value)))
    (let ((a-term-list (cdr (cdr a)))
          (b-term-list (cdr (cdr b))))
      ;(newline)
      ;(newline)
      ;(display a-term-list)
      ;(newline)
      ;(display b-term-list)
      ;(newline)
      (let* ((res (or (contains-symbols?-tree a-term-list)
                      (contains-symbols?-tree b-term-list)
                      ))
             
             (division (if res
                           ; no reduction in this case - so numbers bust be rounded
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
                           (reduce (make-poly-dense 's (map (λ(x) (eval x anchor)) a-term-list))
                                   (make-poly-dense 's (map (λ(x) (eval x anchor)) b-term-list)))))
             (nom (expand-polynomial (cdr (cdr (car division)))))
             (den (expand-polynomial (cdr (cdr (cadr division))))))
        
        ;(let ((nom (map-with-s a-term-list))
        ;      (den (map-with-s b-term-list)))
        
        ;(newline) 
        ;(display a-term-list)
        ;(display division)
        ;(display nom)
        ;(newline)
        ;(display den)
        ;(newline)
        
        ; cheb:
        ;#|
        ;(newline)
        ;(display den)
        ;(newline)
        ;(display (list-to-printable-list nom))
        ;(newline)
        ;(display (list-to-printable-list den))
        ;(newline)
        (when (null? disp)
          (display-tf (list-of-strings-to-string (list-to-list-of-strings nom))
                      (list-of-strings-to-string (list-to-list-of-strings den))))
        ;|#
        
        ;(newline)
        (list '/ nom den)))))






; a lite version of the same procedure:

(define (ratio-to-list-lite value)
  (let ((a (get-numer value))
        (b (get-denom value)))
    (list '/ 
          (expand-polynomial (cdr (cdr a)))
          (expand-polynomial (cdr (cdr b))))
    ))



