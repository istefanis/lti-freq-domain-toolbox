

#|
lti-freq-domain-toolbox
Copyright (C) 2014 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#







#lang racket

(require "functions.rkt")
(provide (all-from-out "functions.rkt"))
(provide (all-defined-out))








; //////////   Examples  //////////



(define examples 
  '(
    
    
    
    ;I. simplification examples:
    
    
    (bode (feedback-loop-test1 a))
    (bode (multiple-outputs-test1 a))
    (bode (serial-adders-test1 a))
    (bode (parallel-tfs-test1 a))
    (bode (circuit1 a))
    (bode (circuit2 a))
    (bode (circuit3 a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a))
    (bode a) ;the value of the block's total tf is the that of the latest installed
    (bode a)
    
    
    
    
    
    ;II. elementary blocks:
    
    
    (bode (phase-delay-circuit a))
    (bode (integrator a))
    (bode (sine a))
    (bode (pi-controller 5 8 a))
    (bode (pd-controller 5 8 a))
    (bode (pid-controller 5 8 4 a))
    
    
    (evolve (pid-controller 6 7 3 a))
    (evolve (pid-controller 6 17 3 a))
    
    
    
    
    
    
    ;III. by function:
    
    
    ;F:
    (define sinx (tf '(1) '(1 0 1 0) a))
    (F a) ;not: (F sinx)
    
    (F (integrator a))
    
    
    
    
    ;bode:
    
    ;doesn't work:
    ;(bode (tf '(1) '(1 0) a))
    ;works:
    (define tf1 (tf '(1) '(1 0) a))
    (bode a)
    
    
    (bode (cheb-t1 4 8 1 a))
    (bode (cheb-t1 6 1 1 a))
    (bode (pi-controller 5 8 a))
    
    (define tf1 (tf '(2 3) '(1 0) b))
    (define tf2 (tf '(4 3 6) '(2 1) b))
    (connect tf1 tf2)
    (bode b)
    
    (define tf1 (tf '(2 3) '(1 0) b))
    (define tf2 (tf '(4 3 6) '(2 1) a))
    (connect tf1 tf2)
    (bode a)
    
    
    
    
    ;compare (blocks):
    (define c1 (make-block))
    (define c2 (make-block))
    (define tf1 (tf '(1) '(1 0 1) c1)) ;could use a and b which are already defined
    (define tf2 (tf '(5) '(1 0 1) c2))
    (compare c1 c2)
    
    ;works because the pid functions return the block
    (compare (pid-controller 6 7 3 a) (pi-controller 7 8 b))
    
    ;adding functions of w:
    (define tf1 (tf '(0.02 fw1) '(1) a))
    (define tf2 (tf '(fw2) '(1) b)) ;comb filter
    (compare a b)
    
    ;doesn't work - compare function compares blocks not functions
    ;- tf function returns the transfer function not the block
    ;(compare (tf '(0.02 fw1) '(1) a)  (tf '(fw2) '(1) b))
    
    ;doesn't work since the two functions defined on same block:
    ;(define tf1 (tf '(1) '(1 0 1) a))
    ;(define tf2 (tf '(5) '(1 0 1) a))
    ;(compare a a)
    
    
    
    
    ;tune:
    (tune (pi-controller 5 'y a) '(= AR 140) 0.01)
    
    
    
    
    ;nyquist:
    (nyquist (circuit1 a))
    
    
    
    
    ;step:
    (step (sine a) 5)
    
    (define tf1 (tf '(-8 5) '(0.4 2.2 1) a))
    (define tf2 (tf '(1) '(0.3 0.1 1) b))
    (step a 5) ;inverse response
    (step b 5) ;with damping
    
    ;doesn't work unless raising the lower limit of the first function being evaluated inside step to 1
    ;(define tf3 (tf '(1) '(1 0 1) a))
    ;(define delay1 (tf '((/ 1 (exp (* s 1)))) '(1) a)) ;or try: (define delay1 (tf '(fw4) '(1) a))
    ;(connect delay1 tf3)
    ;(step a 1)
    
    
    
    ;impulse:
    (impulse (sine a))
    
    
    
    
    ;trajectory:
    (trajectory (sine a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a))
    (trajectory a) ;with damping
    
    
    
    
    ;delay using pade:
    (step (delay (sine a) 2) 2)
    (step (delay (delay (sine a) 1) 1) 2)
    
    
    
    
    ;delay using fw functions - the fw functions only work at the s-domain:
    (define tf1 (tf '((* 5 fw3)) '(1 1) a))
    (bode a)
    
    
    
    
    ;delay methods comparison:
    (define tf1 (tf '(1) '(1 0 1) a))
    (define delay1 (tf '(fw3) '(1) a))
    (connect delay1 tf1)
    (compare (delay (sine b) 1) a)
    
    
    
    
    
    
    
    ;VI. abstraction using blocks:
    
    
    ;serial connections:
    
    (define a1 (make-block a))
    (define a2 (make-block a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a1))
    (define tf2 (tf '(1) '(5 0.1 1) a1))
    (connect tf1 tf2)
    (bode a1)
    (define tf3 (tf '(1) '(5 0.1 1) a2))
    (connect a1 a2)
    
    ;(set! display-mode 'test)
    ;(simplify a)
    
    (bode a)
    (compare a a1)
    
    
    
    
    ;feedback loop:
    
    (define a1 (make-block a))
    (define a2 (make-block a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a1)) ;also try this: (define tf1 (tf '(1) '(0.3 fw1 1) a1))
    (define tf2 (tf '(1) '(6 0.1 1) a1))
    (connect tf1 tf2)
    (bode a1)
    (define tf3 (tf '(1) '(5 0.1 1) a2))
    (connect a1 a2)
    ;(bode a) ;10^6
    
    (define a3 (make-block a))
    (define tf4 (tf '(1) '(4 0.1 1) a3))
    (connect a2 a3)
    ;(bode a) ;10^8
    (define a4 (make-block a))
    (define tf5 (tf '(1) '(1) a4))
    (connect a2 a4)
    
    
    (define add1 (make-adder a))
    (connect add1 a1) ;must put an adder in front of it
    (connect a3 add1)
    
    ;(set! display-mode 'test)
    ;(simplify a)
    
    (bode a)
    (compare a a1)
    
    
    ))





; run the examples

(define (run-examples . speed)
  (if (and (not (null? speed)) (eq? (car speed) 'slow))
      (begin (map (λ (x) (newline)
                    (newline)
                    (displayln x)
                    (eval x anchor)
                    (sleep 10))
                  examples) 'all_examples_run)
      (begin (map (λ(x) (eval x anchor)) examples)
             'all_examples_run)))



;(run-examples)