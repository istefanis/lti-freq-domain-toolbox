#|
lti-freq-domain-toolbox | Functions for studying LTI (linear time-invariant) dynamical systems 
Copyright (C) 2014-2022  Ioannis Stefanis

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

(require math/base)
(require math/number-theory)

;for testing:
;(require "math_library/general.rkt")
;(require "math_library/numerical_analysis.rkt")
;(require "math_library/symbolic_algebra.rkt")

(require "elements/general.rkt")
(require "elements/block.rkt")
(require "elements/tf.rkt")
(require "elements/adder.rkt")
(require "auxiliary/metrics.rkt")
(require "auxiliary/display_modes.rkt")
(require "functionality/text_generation.rkt")
(require "functionality/plot_freq_domain.rkt")
(require "functionality/plot_time_domain.rkt")
(require "circuits.rkt")

(provide (all-from-out "elements/general.rkt"))
(provide (all-from-out "elements/block.rkt"))
(provide (all-from-out "elements/tf.rkt"))
(provide (all-from-out "elements/adder.rkt"))
(provide (all-from-out "auxiliary/metrics.rkt"))
(provide (all-from-out "auxiliary/display_modes.rkt"))
(provide (all-from-out "functionality/text_generation.rkt"))
(provide (all-from-out "functionality/plot_freq_domain.rkt"))
(provide (all-from-out "functionality/plot_time_domain.rkt"))
(provide (all-from-out "circuits.rkt"))
(provide (all-defined-out))








(define-namespace-anchor n_anchor)
(define anchor (namespace-anchor->namespace n_anchor))


;initialize a, b, c, d blocks
(define a (block))
(define b (block))
(define c (block))
(define d (block))


(newline)








; //////////   Examples  //////////


(define examples_list
  '(


    (define (displayc s)
      (newline)
      (newline)
      (display (make-string 52 #\_))
      (newline)
      (newline)
      (display s)
      (newline))



    
    ;I. simplification examples:
    
    
    (displayc "> (bode (circuit1 a))")
    (bode (circuit1 a))

    (displayc "> (bode (circuit2 a))")
    (bode (circuit2 a))

    (displayc "> (bode (circuit3 a))")
    (bode (circuit3 a))

    (displayc "> (bode a)")
    (define tf1 (tf '(1) '(0.3 0.1 1) a))
    (bode a) ;the value of the block's total tf is that of the latest installed
    
    ;(bode (feedback-loop-test1 a))
    ;(bode (multiple-outputs-test1 a))
    ;(bode (serial-adders-test1 a))
    ;(bode (parallel-tfs-test1 a))


    
    
    
    ;II. elementary blocks:
    

    (displayc "> (bode (phase-delay-circuit a))")
    (bode (phase-delay-circuit a))

    (displayc "> (bode (integrator a))")
    (bode (integrator a))

    (displayc "> (bode (sine a))")
    (bode (sine a))

    (displayc "> (bode (pi-controller 5 8 a))")
    (bode (pi-controller 5 8 a))

    (displayc "> (bode (pd-controller 5 8 a))")
    (bode (pd-controller 5 8 a))

    (displayc "> (bode (pid-controller 5 8 4 a))")
    (bode (pid-controller 5 8 4 a))
    
    (displayc "> (evolve (pid-controller 6 7 3 a))")
    (evolve (pid-controller 6 7 3 a))

    (displayc "> (evolve (pid-controller 6 17 3 a))")
    (evolve (pid-controller 6 17 3 a))
    
    
    
    
    
    ;III. by function:
    
    ;F:
    (displayc "> (F a)")
    (define sinx (tf '(1) '(1 0 1 0) a))
    (F a) ;not: (F sinx)

    (displayc "> (F (integrator a))")
    (F (integrator a))
    
      
    ;Bode
    (displayc "> (bode a)")
    ;(bode (tf '(1) '(1 0) a))       ;it doesn't work this way
    (define tf1 (tf '(1) '(1 0) a))  ;it works
    (bode a)

    (displayc "> (chebyshev-type1 4 8 1 a)")
    (bode (chebyshev-type1 4 8 1 a))
    
    (displayc "> (chebyshev-type1 6 1 1 a)")
    (bode (chebyshev-type1 6 1 1 a))

    ;(displayc "> (bode b)")
    ;(define tf1 (tf '(2 3) '(1 0) b))
    ;(define tf2 (tf '(4 3 6) '(2 1) b))
    ;(connect tf1 tf2)
    ;(bode b)

    (displayc "> (bode a)")
    (define tf1 (tf '(2 3) '(1 0) b))
    (define tf2 (tf '(4 3 6) '(2 1) a))
    (connect tf1 tf2)
    (bode a)

    (displayc "> (bode a)")
    (define tf1 (tf '(5 0) '(1 1 1) a)) ;band-pass filter
    (bode a)

    
    ;Compare
    (displayc "> (compare c1 c2)")
    (define c1 (block))
    (define c2 (block))
    (define tf1 (tf '(1) '(1 0 1) c1)) ;could use a and b which are already defined
    (define tf2 (tf '(5) '(1 0 1) c2))
    (compare c1 c2)
    
    ;works because the pid functions return the block
    (displayc "> (compare (pid-controller 6 7 3 a) (pi-controller 7 8 b))")
    (compare (pid-controller 6 7 3 a) (pi-controller 7 8 b))
    
    ;adding f(w) functions of the frequency w:
    (displayc "> (compare a b)")
    (define tf1 (tf '(0.02 fw1) '(1) a))
    (define tf2 (tf '(fw2) '(1) b)) ;comb filter
    (compare a b)
    
    ;doesn't work - compare takes as inputs blocks, not functions
    ;- tf function returns the transfer function not the block
    ;(compare (tf '(0.02 fw1) '(1) a)  (tf '(fw2) '(1) b))
    
    ;doesn't work since the two functions are defined in the same block:
    ;(define tf1 (tf '(1) '(1 0 1) a))
    ;(define tf2 (tf '(5) '(1 0 1) a))
    ;(compare a a)
      
    
    ;Tune
    (displayc "> (tune (pi-controller 5 'y a) '(= AR 140) 0.01)")
    (tune (pi-controller 5 'y a) '(= AR 140) 0.01)
    
    
    ;Nyquist
    (displayc "> (nyquist (circuit3 a))")
    (nyquist (circuit3 a))



    ;block re-initialization:
    ;if the elements stored inside a block will no longer be used, it can be manually re-initialized
    ;
    ;warning: errors may arise if previously stored elements are attempted to be used again
    (re-initialize-block! a)

    
    
    ;Step
    (displayc "> (step (sine a) 5)")
    (step (sine a) 5)
    
    (define tf1 (tf '(-8 5) '(0.4 2.2 1) a))
    (define tf2 (tf '(1) '(0.3 0.1 1) b))
    (displayc "> (step a 5)")
    (step a 5) ;inverse response
    (displayc "> (step b 5)")
    (step b 5) ;with damping
    
    ;doesn't work, unless raising the lower limit of the first function being evaluated inside step to 1
    ;(define tf3 (tf '(1) '(1 0 1) a))
    ;(define delay1 (tf '((/ 1 (exp (* s 1)))) '(1) a)) ;or try: (define delay1 (tf '(fw4) '(1) a))
    ;(connect delay1 tf3)
    ;(step a 1)
   
    
    ;Impulse
    (displayc "> (impulse (sine a))")
    (impulse (sine a))
    
        
    ;Trajectory
    (displayc "> (trajectory (sine a))")
    (trajectory (sine a))

    (displayc "> (trajectory a)")
    (define tf1 (tf '(1) '(0.3 0.1 1) a))
    (trajectory a) ;with damping
       

    ;Delay addition 
    
    ;Method I: using Pade functions:
    (displayc "> (step (pade-delay 2 (sine a)) 2)")
    (step (pade-delay 2 (sine a)) 2)
    ;(step (pade-delay 1 (pade-delay 1 (sine a))) 2) ;that also works, but is very slow with pade[6/6]
    
    ;Method II: using f(w) functions - they work only in the s-domain:
    ;(define tf1 (tf '((* 5 fw3)) '(1 1) a))
    ;(bode a)
    
    ;Methods comparison:
    (displayc "> (compare (pade-delay 1 (sine b)) a)")
    (define tf1 (tf '(1) '(1 0 1) a))
    (define fw-delay1 (tf '(fw3) '(1) a)) ;fw3 is defined in: ../functionality/text_generation.rkt
    (connect fw-delay1 tf1)
    (compare (pade-delay 1 (sine b)) a)
    
    
    
    #|
    
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

|#

    ))





; run the examples

(define (run-examples . speed)
  (if (and (not (null? speed)) (eq? (car speed) 'slow))
      (begin (map (λ (x)
                    (eval x anchor)
                    (sleep 5))
                  examples_list) 'all_examples_run)
      (begin (map (λ(x) (eval x anchor)) examples_list)
             'all_examples_run)))



;(run-examples)