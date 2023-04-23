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

(require plot)
(require "../math_library/numerical_analysis.rkt")
(require "../math_library/symbolic_algebra.rkt")
(require "../elements/general.rkt")
(require "total_tf_parsing.rkt")
(provide (all-defined-out))







; //////////   H. Time domain response computation and plot-generating functions  //////////


;The Plot library, by Neil Toronto, is used in all the following plot-creating fuctions

(plot-font-size 10)
(plot-width 420)


;min & max time [s]
(define t-min (/ 1 1000)) ;when writen in this form - instead of 0.001 - the time domain plots are generated faster by the Plot library 
(define t-max 30)





;///// Impulse response plot

; L{d(t)}=1

(define (impulse block)
  
  (let* ((reduced-total-tf (reduce-block-value (get-simplified-block-value block)))
         (total-tf-expression (get-total-tf-expression-with-display reduced-total-tf))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is possible:
         (total-tf-evaluation (eval total-tf-expression anchor)) ;total-tf-evaluation is a function of s and w
         (tfs (λ(s) (total-tf-evaluation s 0 0 0 0))))   
    
    
    (for-each
     displayln
     
     (list 
      
      (parameterize ([plot-title "Time response f(t) plot"]
                     [plot-height   200]
                     [plot-x-label  "t [s]"]
                     [plot-y-label  "f(t)"]
                     [plot-y-far-tick-labels? #t])
        
        (plot (list
               ;(axes)
               (tick-grid)
               (function (λ (t) (talbot-method tfs t 150)) t-min t-max)
               (function (λ (t) -10.0) t-min t-max #:color 0 #:style 'dot)
               (function (λ (t) 10.0) t-min t-max #:color 0 #:style 'dot)
               (function (λ (t) 0) t-min t-max #:color 0 #:style 'dot))))
      
      
      (make-space-line 10)))))









(define (impulse-deriv block)
  
  (let* ((reduced-total-tf (reduce-block-value (get-simplified-block-value block)))
         (total-tf-expression (get-total-tf-expression-with-display reduced-total-tf))
         (total-tf-evaluation (eval total-tf-expression anchor)) ;total-tf-evaluation is a function of s and w
         (tfs (λ(s) (total-tf-evaluation s 0 0 0 0))))
    
    
    (for-each
     displayln
     
     (list
      
      (parameterize ([plot-title "t - df/dt plot"]
                     [plot-height   200]
                     [plot-x-label  "t [s]"]
                     [plot-y-label  "df/dt"]
                     [plot-y-far-tick-labels? #t])
        
        (plot (list
               ;(axes)
               (tick-grid)
               (function (deriv (λ (t) (talbot-method tfs t 150))) t-min t-max)
               (function (λ (t) -10.0) t-min t-max #:color 0 #:style 'dot)
               (function (λ (t) 10.0) t-min t-max #:color 0 #:style 'dot))))
      
      
      (make-space-line 10)))))







;///// Trajectory plot

(define (trajectory block)
  
  (let* ((reduced-total-tf (reduce-block-value (get-simplified-block-value block)))
         (total-tf-expression (get-total-tf-expression-with-display reduced-total-tf))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is possible:
         (total-tf-evaluation (eval total-tf-expression anchor)) ;total-tf-evaluation is a function of s and w
         (tfs (λ(s) (total-tf-evaluation s 0 0 0 0))))
    
    
    
    (let ((t-list (cdr (build-list 1000 (λ (x) (* x 0.01)))))
          (f (λ (t) (talbot-method tfs t 150))))
      
      (for-each
       displayln
       
       (list 
        (parameterize ([plot-title "Time domain df(t)/dt - f(t) trajectory plot"]
                       [plot-height   200]
                       [plot-x-label  "df(t)/dt"]
                       [plot-y-label  "f(t)"]
                       [plot-y-far-tick-labels? #t])
          
          (plot (list
                 (axes)
                 (tick-grid)
                 (point-label (vector (f 0.01) ((deriv f) 0.01)) "initial value" #:anchor 'top-left)
                 (lines (map vector (map f t-list) (map (deriv f) t-list))))))
        
        (make-space-line 10))))))







;///// Step response plot

; stp<40

(define (step block gain)
  
  (define total-tf-expression '())
  
  ;(newline)
  ;(set-logger-mode! 'nil) 
  
  (let* ((reduced-total-tf (reduce-block-value (get-simplified-block-value block)))
         (num (get-numer reduced-total-tf))
         (den (get-denom reduced-total-tf)))    
    
    ; adding the step tf ([gain]/[1 0]):
    
    (let ((new-num (mul num (make-poly-dense 's (list gain))))
          (new-den (mul den (make-poly-dense 's (list 1 0)))))
      
      #|
      (newline)
      (displayln new-num)
      (displayln new-den)
      |#    
      
      ; just for displaying:
      (tf-to-expanded-expression-with-display reduced-total-tf)
      (newline)
      (displayln "with gain:")
      (tf-to-expanded-expression-with-display (make-ratio new-num new-den))

      
      (set! total-tf-expression
            (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (tf-to-expanded-expression-with-display (make-ratio new-num new-den) 'do-not-display)))))
      ))
  
  
  (define total-tf-evaluation (eval total-tf-expression anchor))
  (define (tfs s) (total-tf-evaluation s
                                       0
                                       0
                                       0
                                       (fw4-func s)))
  
  
  (for-each
   displayln
   
   (list 
    
    (parameterize ([plot-title "Time response f(t) plot"]
                   [plot-height   200]
                   [plot-x-label  "t [s]"]
                   [plot-y-label  "f(t)"]
                   [plot-y-far-tick-labels? #t])
      
      (plot (list
             (axes)
             (tick-grid)
             (function (λ (t) (talbot-method tfs t 150)) t-min t-max)
             (function (λ (t) -10.0) t-min t-max #:color 0 #:style 'dot)
             (function (λ (t) 10.0) t-min t-max #:color 0 #:style 'dot)
             (function (λ (t) gain) t-min t-max #:color 0 #:style 'dot)
             )))
    
    
    (make-space-line 10)))
  
  )



