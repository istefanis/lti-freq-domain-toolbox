#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#






#lang racket

(require math/base)
(require "math_library/general.rkt")
(require "math_library/symbolic_algebra.rkt")
(require "elements/general.rkt")
(require "elements/block.rkt")
(require "elements/tf.rkt")
(require "elements/adder.rkt")

(provide (all-defined-out))







; //////////   D. circuit examples and elementary blocks  //////////


(define (feedback-loop-test1 block) ;parent block
  
  (define tf1 (make-tf (make-ratio (make-poly-dense 's '(1 0 1 1 0))
                                   (make-poly-dense 's '(1)))
                       block))
  
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (make-adder block))
  (define add2 (adder block))
  
  (connect-serially tf1 tf2)
  (connect tf2 add1)
  (connect add1 tf1)
  (connect tf1 add2)
  (connect add2 tf3)
  (connect tf3 add2)
  
  block)  ; return the block where the elements were stored so as to be passed to simplify




(define (multiple-outputs-test1 block)
  
  (define tf1 (tf '(1 0 1 1 0) '(1) block))  
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  
  (connect add1 tf2)
  (connect tf2 tf3)
  (connect tf2 tf4)
  (connect tf2 tf5)
  
  block)




(define (serial-adders-test1 block)
  
  (define tf1 (tf '(1 0 1 1 0) '(1) block))  
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  
  (connect add1 add2)
  (connect add2 tf5)
  (connect tf5 add1)
  
  block)




(define (parallel-tfs-test1 block)
  
  (define tf1 (tf '(1 0 1 1 0) '(1) block))
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  
  (connect add1 tf1)
  (connect add1 tf2)
  (connect add1 tf3)
  (connect tf1 add2)
  (connect tf2 add2)
  (connect tf3 add2)
  
  block)




(define (circuit1 block)  
  (define tf1 (tf '(-10 0 1) '(1 2 1) block))
  block)



(define (circuit2 block)
  (define tf1 (make-tf (make-ratio (make-poly-dense 's '(2)) 
                                   (make-poly-dense 's '(1 1)))
                       block)) 
  (define tf2 (tf '(1 2 5) '(8) block))
  (connect tf1 tf2)
  block)



(define (circuit3 block)
  
  (define tf1 (tf '(1 0 0 0 0) '(1 1 0) block))
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf  '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  (define tf7 (tf '(1) '(4 0 0) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  (define add3 (adder block))
  (define add4 (adder block))
  
  (connect add1 tf2)
  (connect add1 tf3)
  (connect add1 tf4)
  
  (connect tf2 add2)
  (connect tf3 add2)
  
  (connect tf3 add3)
  (connect tf4 add3)
  
  (connect add2 tf1)
  (connect tf1 add1)
  
  (connect add2 tf5)
  (connect tf5 add4)
  (connect add3 add4)
  
  (connect add4 tf6)
  (connect tf6 add4)
  (connect tf6 tf7)
  (connect tf7 add1)
  
  block)






; //// elementary blocks:


(define (phase-delay-circuit block)  
  (define tf1 (tf '(5 1) '(8 1) block))
  block)




(define (integrator block)
  (define tf1 (tf '(1) '(1 0) block))
  block)




(define (sine block)
  (define tf1 (tf '(1) '(1 0 1) block))
  block)




; //// pid controllers:


(define (pi-controller kc ti block)
  (define tf1 (tf (list (list '* kc ti) kc)  ; writen this way so as to accept symbols (and so work with tune)
                  (list ti 0)
                  block))
  block)




(define (pd-controller kc td block) 
  (define tf1 (tf (list (list '* td kc) kc)
                  '(1)
                  block))
  block)




(define (pid-controller kc ti td block)
  (define tf1 (tf (list (list '* kc ti td) (list '* kc ti) kc)
                  (list ti 0)
                  block))
  block)




; //// Cheb filters:

(define cheb-threshold 1000)

(define (cheb-threshold! x)
   (set! cheb-threshold x))

(define (cheb-t1 n e w0 block)
  
  (define (theta m)
    (/ (* pi (- (* 2 m) 1)) (* 2 n)))
  
  (define (spm m)
    (+ (- (* (sinh (/ (asinh (/ 1 e)) n))
             (sin (theta m))))
       (* (make-rectangular 0 1.0) (cosh (/ (asinh (/ 1 e)) n)) (cos (theta m)))))
  
  
  (set! cheb-threshold (/ 1 (sqrt (+ 1 (* e e)))))
  
  ;(define b (make-block))
  
  
  ;(define tf0 (tf '(1) (list (* (expt 2 (- n 1)) e)) b))
  (define tf0 (tf '(1) (list (* (expt 2 (- n 1)) e)) block))
  ;(define tf1 (tf '(1) (list 1 (- (spm 1))) b))
  ;(define tf2 (tf '(1) (list 1 (- (spm 2))) b))
  ;(define tf3 (tf '(1) (list 1 (- (spm 3))) b))
  
  
  (define (connection-function tfm m)
    (when (< m n)
      (let ;((f1 (tf '(1) (list 1 (- (spm (+ m 1)))) b)))
          ((f1 (tf '(1) (list 1 (- (spm (+ m 1)))) block)))
        (connect tfm f1)
        (connection-function f1 (+ m 1)))))
  
  (connection-function tf0 0)
  
  block)






; //// time delay blocks:


; approximating e-sT using pade functions 
; (the arithmetic methods fail when trying to model delay by just adding e-sT as a tf):

#|
(define (pade5 T)
  (tf (list (* T T T T)
            (* -120 T T T)
            (* 1260 T T)
            (* -6720 T)
            15120)
      (list (* T T T T T)
            (* 25 T T T T)
            (* 300 T T T)
            (* 2100 T T)
            (* 8400 T)
            15120)))
|#



(define (pade m n T block)
  
  ; the values returned for the greater values of i are rather small,
  ; so must be multiplied by an appropriate coefficient:
  (define coeff (/ 1000000 (expt T 5)))
  
  (define (p i)
    ; 100
    (/ (* coeff
          (expt -1 i)
          (factorial (+ m n (- 0 i)))
          (factorial m))
       (* (factorial (+ m n))
          (factorial i)
          (factorial (- m i)))))
  
  (define (q i)
    (/ (* coeff
          (factorial (+ m n (- 0 i)))
          (factorial n))
       (* (factorial (+ m n))
          (factorial i)
          (factorial (- n i)))))
  
  #|
  (displayln (p 0))
  (displayln (p 1))
  (displayln (p 2))
  (displayln (p 3))
  (displayln (p 4))
  (displayln (p 5))
  |#
  
  ;(displayln (map (λ (x) (* (p x) (expt T x))) (integers m)))
  
  
  (define tf1 (tf (map (λ (x) (* (p x) (expt T x))) (integers m))  ; integers from 0 to m
                  (map (λ (x) (* (q x) (expt T x))) (integers n))
                  block))
  
  tf1)




; T<7
(define (delay block T)
  
  (newline)
  (displayln "delay approximated using pade(4,5)")
  
  (define tf2 (pade 4 5 T block))
  
  (connect tf2 (cadr (get-tfs block)))
  
  block)



