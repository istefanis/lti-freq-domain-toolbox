#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#






#lang racket

(require math/base)
(require "elements/general.rkt")
(require "elements/block.rkt")
(require "elements/tf.rkt")
(require "elements/adder.rkt")
(require "functionality/metrics.rkt")
(require "functionality/display_modes.rkt")
(require "functionality/text_generation.rkt")
(require "functionality/plot_generation.rkt")
(require "functionality/time_domain.rkt")
(require "examples.rkt")

(provide (all-from-out "elements/general.rkt"))
(provide (all-from-out "elements/block.rkt"))
(provide (all-from-out "elements/tf.rkt"))
(provide (all-from-out "elements/adder.rkt"))
(provide (all-from-out "functionality/metrics.rkt"))
(provide (all-from-out "functionality/display_modes.rkt"))
(provide (all-from-out "functionality/text_generation.rkt"))
(provide (all-from-out "functionality/plot_generation.rkt"))
(provide (all-from-out "functionality/time_domain.rkt"))
(provide (all-from-out "examples.rkt"))
(provide (all-defined-out))



(define-namespace-anchor n_anchor)
(define anchor (namespace-anchor->namespace n_anchor))








; //////////   Examples  //////////


;0. initialize a, b, c, d blocks

(define a (block))
(define b (block))
(define c (block))
(define d (block))



(define examples_list
  '(

    
    ;I. simplification examples:
    
    
    (bode (circuit1 a))
    (bode (circuit2 a))
    (bode (circuit3 a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a))
    (bode a) ;the value of the block's total tf is that of the latest installed
    (bode a)
    ;(bode (feedback-loop-test1 a))
    ;(bode (multiple-outputs-test1 a))
    ;(bode (serial-adders-test1 a))
    ;(bode (parallel-tfs-test1 a))


    
    
    
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
    
      
    ;Bode
    ;(bode (tf '(1) '(1 0) a))       ;it doesn't work this way
    (define tf1 (tf '(1) '(1 0) a))  ;it works
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

    
    ;Compare (blocks)
    (define c1 (block))
    (define c2 (block))
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
      
    
    ;Tune
    (tune (pi-controller 5 'y a) '(= AR 140) 0.01)
    
    
    ;Nyquist
    (nyquist (circuit3 a))
    
    
    ;Step
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
   
    
    ;Impulse
    (impulse (sine a))
    
        
    ;Trajectory
    (trajectory (sine a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a))
    (trajectory a) ;with damping
       
    
    ;Delay using Pade:
    (step (delay (sine a) 2) 2)
    (step (delay (delay (sine a) 1) 1) 2)
    
    
    ;Delay using fw functions - the fw functions only work at the s-domain:
    (define tf1 (tf '((* 5 fw3)) '(1 1) a))
    (bode a)
    
    
    ;Delay methods comparison:
    (define tf1 (tf '(1) '(1 0 1) a))
    (define delay1 (tf '(fw3) '(1) a))
    (connect delay1 tf1)
    (compare (delay (sine b) 1) a)
    
    
    
    
    
    
    
    ;VI. abstraction using blocks:
    
    
    ;serial connections:
    
    (define a1 (block a))
    (define a2 (block a))
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
    
    (define a1 (block a))
    (define a2 (block a))
    (define tf1 (tf '(1) '(0.3 0.1 1) a1)) ;also try this: (define tf1 (tf '(1) '(0.3 fw1 1) a1))
    (define tf2 (tf '(1) '(6 0.1 1) a1))
    (connect tf1 tf2)
    (bode a1)
    (define tf3 (tf '(1) '(5 0.1 1) a2))
    (connect a1 a2)
    ;(bode a) ;10^6
    
    (define a3 (block a))
    (define tf4 (tf '(1) '(4 0.1 1) a3))
    (connect a2 a3)
    ;(bode a) ;10^8
    (define a4 (block a))
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
                  examples_list) 'all_examples_run)
      (begin (map (λ(x) (eval x anchor)) examples_list)
             'all_examples_run)))



;(run-examples)