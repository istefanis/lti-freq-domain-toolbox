#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#






#lang racket

(require plot)
(require "../math_library/general.rkt")
(require "../math_library/numerical_analysis.rkt")
(require "../math_library/symbolic_algebra.rkt")
(require "../elements/general.rkt")
(require "../auxiliary/display_modes.rkt")
(require "text_generation.rkt")
(provide (all-defined-out))







; //////////   H. Time domain response computation and plot-generating functions  //////////


;The Plot library, by Neil Toronto, is used in all the following plot-creating fuctions

(plot-font-size 10)
(plot-width 420)


;min & max time [s]
(define t-min (/ 1 1000)) ;when writen in this form - instead of 0.001 - the time domain plots are generated faster by the Plot library 
(define t-max 30)






;///// Talbot algorithm for Laplace inversion

(define (Talbot F t N)
  
  ; stepsize initialization:
  (let ((h (/ (* 2 pi) N))
        (shift 0.0))
    
    
    (when (= t 0) (display "error - Inverse transform can not be calculated for t=0 - TALBOT"))
    
    
    ; loop is evaluating the Laplace inversion at each point theta which is based on the trapezoidal rule:
    (define (loop k ans)
      (if (> k N)
          
          (* (/ h (* 2 (make-rectangular 0 1) pi)) ans)
          
          (let* ((theta (+ (* -1 pi) (* (+ k 0.5) h)))
                 
                 (z (+ shift (* (/ N t) (+ (* 0.5017 theta (cot (* 0.6407 theta))) -0.6122 
                                           (* 0.2645 (make-rectangular 0 1) theta)))))
                 
                 (dz (* (/ N t) (+ (* -0.5017 0.6407 theta (expt (csc (* 0.6407 theta)) 2)) 
                                   (* 0.5017 (cot (* 0.6407 theta))) (* 0.2645 (make-rectangular 0 1))))))
            
            
            (loop (+ k 1) (+ ans (* (exp (* z t)) (F z) dz))))))
    
    (loop 0 0)))







;///// Impulse response plot

; L{d(t)}=1

(define (impulse block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs s) (tfs-value-evaluation s 0 0 0 0)) 
    
    
    
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
               (function (λ (t) (real-part (Talbot tfs t 150))) t-min t-max)
               (function (λ (t) -10.0) t-min t-max #:color 0 #:style 'dot)
               (function (λ (t) 10.0) t-min t-max #:color 0 #:style 'dot)
               (function (λ (t) 0) t-min t-max #:color 0 #:style 'dot))))
      
      
      (make-space-line 10)))))









(define (impulse-deriv block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs s) (tfs-value-evaluation s 0 0 0 0))
    
    
    
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
               (function (deriv (λ (t) (real-part (Talbot tfs t 150)))) t-min t-max)
               (function (λ (t) -10.0) t-min t-max #:color 0 #:style 'dot)
               (function (λ (t) 10.0) t-min t-max #:color 0 #:style 'dot))))
      
      
      (make-space-line 10)))))







;///// Trajectory plot

(define (trajectory block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs s) (tfs-value-evaluation s 0 0 0 0))
    
    
    
    (let ((t-list (cdr (build-list 1000 (λ (x) (* x 0.01)))))
          (f (λ (t) (real-part (Talbot tfs t 150)))))
      
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
  
  (define total-tfs-value '())
  
  (newline)
  (display-mode-nil!)
  
  
  (let ((num (get-numer (get-simplified-block-value block)))
        (den (get-denom (get-simplified-block-value block))))
    
    
    #|
          (displayln (get-value (car tfs)))
          (newline)
          (displayln num)
          (displayln den)
          |#          
    
    
    ; adding the step tf ([gain]/[1 0]):
    
    (let ((new-num (mul num (make-poly-dense 's (list gain))))
          (new-den (mul den (make-poly-dense 's (list 1 0)))))
      
      #|
            (newline)
            (displayln new-num)
            (displayln new-den)
            |#
      
      
      ; just for displaying:
      (ratio-to-list (get-simplified-block-value block))
      (newline)
      (displayln "with gain:")
      (newline)
      (ratio-to-list (make-ratio new-num new-den))
      
      #|
            (newline)
            (displayln (make-ratio new-num new-den))
            (newline)
            (newline)
            (ratio-to-list (make-ratio new-num new-den) 'do-not-display)
            (newline)
            (displayln 'test-1)
            |#
      
      (set! total-tfs-value (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (make-ratio new-num new-den) 'do-not-display)))))
      ))
  
  
  (define tfs-value-evaluation (eval total-tfs-value anchor))
  (define (tfs s) (tfs-value-evaluation s
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
             (function (λ (t) (real-part (Talbot tfs t 150))) t-min t-max)
             (function (λ (t) -10.0) t-min t-max #:color 0 #:style 'dot)
             (function (λ (t) 10.0) t-min t-max #:color 0 #:style 'dot)
             (function (λ (t) gain) t-min t-max #:color 0 #:style 'dot)
             )))
    
    
    (make-space-line 10)))
  
  )



