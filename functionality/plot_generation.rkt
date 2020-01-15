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
(require "display_modes.rkt")
(require "text_generation.rkt")
(require "../examples.rkt")
(provide (all-defined-out))







; //////////   G. Plot-generating functions (Bode, Nyquist, etc.)  //////////


; the PLoT library, by Neil Toronto <neil.toronto@gmail.com>, 
; is used for generating the following plots

; in all the following plot-creating fuctions, block is a circuit building expression 
; that returns the block to be simplified, such as: (circuit1 a) or: (pi-controller 7 5 a)




(newline)
(plot-font-size 10)






(define (get-total-tfs-value block)
  
  (display-mode-nil!)
  (newline)
  
  ;(let ((cached-simplification-result (simplify block)))
  (cons 'λ (cons '(s fw1 fw2 fw3 fw4) 
                 (list (ratio-to-list (get-simplified-block-value block))))))
;)








(define (F block)  ; tf is evaluated here
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         (tfs-value-evaluation (eval total-tfs-value anchor)))
    
    (define (tfs s) (tfs-value-evaluation s 0 0 0 0))
    ;(fw1-func 0) 
    ;(fw2-func w)
    ;(fw3-func w)
    ;(fw4-func w)))
    
    
    (plot-height 300)
    (plot-title "F(s) plot")
    
    
    (for-each 
     displayln
     
     (list (parameterize ([plot-x-label      "s"]
                          [plot-y-label      "F(s)"]
                          ; the log transform is not defined on negative arguments, so:
                          ;[plot-x-transform cbrt-transform])
                          [plot-x-transform  (stretch-transform -5 5 10)])
             
             (plot (list (axes)
                         (tick-grid)
                         (function (λ (s) (tfs s)) -100 100))
                   #:y-min -20
                   #:y-max 20))
           
           (make-space-line 10)))))










(define (bode-plot-parameters)
  (plot-height 200)
  ;(plot-title "Bode plot")
  ;(plot-title (string-append (make-space-line 10) "Bode plot"))
  (plot-title (string-append (make-space-line 7) "Bode plot"))
  (plot-x-label "Frequency [rad/s]")
  )








; angle unwarping - from (-180 < angle < 180) to (-540 < angle < 180):

(define had-neg-values-1 #f)
(define passed-180-1 #f)
(define passed-360-1 #f)

(define had-neg-values-2 #f)
(define passed-180-2 #f)
(define passed-360-2 #f)



(define (get-had-neg-values id)
  (if (= id 1)
      had-neg-values-1
      had-neg-values-2))
(define (set-had-neg-values-t id)
  (if (= id 1)
      (set! had-neg-values-1 #t)
      (set! had-neg-values-2 #t)))


(define (get-passed-180 id)
  (if (= id 1)
      passed-180-1
      passed-180-2))
(define (set-passed-180-t id)
  (if (= id 1)
      (set! passed-180-1 #t)
      (set! passed-180-2 #t)))


(define (get-passed-360 id)
  (if (= id 1)
      passed-360-1
      passed-360-2))
(define (set-passed-360-t id)
  (if (= id 1)
      (set! passed-360-1 #t)
      (set! passed-360-2 #t)))




(define (unwarp-angle-simple ang w id)
  
  (if (and (eq? (get-had-neg-values id) #t) (> ang 0)) ;cheb: (> ang (/ pi 2)))
      (- ang (* 2 pi))
      (if (or (< ang 0) (and (= ang 0) (> w 0.001)))
          (begin
            (set-had-neg-values-t id)
            ang)
          ang))
  )


(define (unwarp-angle-simple-2 ang w id)
  
  (if (and (eq? (get-had-neg-values id) #t) (> ang (/ pi 2)))
      (- ang (* 2 pi))
      (if (or (< ang 0) (and (= ang 0) (> w 0.001)))
          (begin
            (set-had-neg-values-t id)
            ang)
          ang))
  )


(define (unwarp-angle ang w id)
  
  (cond ((and (eq? (get-had-neg-values id) #t) (eq? (get-passed-360 id) #t) 
              (> (+ ang (* 2 pi)) 0)
              )
         (- ang (* 2 pi)))
        
        ;#|
        ((and (eq? (get-had-neg-values id) #t) (not (eq? (get-passed-180 id) #t))
              (> ang 0) 
              (< ang (/ pi 2)))
         ;works for pid:
         ang
         ;works for delay:
         ;(- ang (* 2 pi))
         )
        ;|#
        
        ((and (eq? (get-had-neg-values id) #t) ;(eq? (get-passed-180 id) #t)
              (> ang 0))
         (when ;(< (- ang (* 2 pi)) (* (- 1.8) pi))
             (and (eq? (get-passed-180 id) #t) (< (- ang (* 2 pi)) 0))
           (set-passed-360-t id))
         
         ;change:
         (when (> (abs (- ang (* 2 pi))) (* 0.8 pi))
           (set-passed-180-t id))
         
         (- ang (* 2 pi)))
        
        (else
         (when (or (< ang 0) (and (= ang 0) (> w 0.001)))
           (set-had-neg-values-t id))
         (when (> (abs ang) (* 0.8 pi))
           (set-passed-180-t id))
         ang))
  )









; fw-functions - for adding functions of w or s to the tf poly
; - better to use them only in the s-domain:

(define (simple-delay w T) (exp (* (make-rectangular 0 (- w)) T)))
(define (simple-delay-s s T) (/ 1 (exp (* s T))))

(define (comb-filter w a) (+ 1 (* a (simple-delay w 0.05))))

(define (fw1-func w) (comb-filter w 0.5))
(define (fw2-func w) (comb-filter w 0.7))
(define (fw3-func w) (simple-delay w 1))
(define (fw4-func s) (simple-delay-s s 1))

;(compare (tf '(0.02 fw1) '(1)) (tf '(0.02 fw2) '(1)))








;///// Bode plot

(define (bode block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))
    
    
    (let* ((AR-low (magnitude (tfs (/ 1 1000))))
           (AR-high (magnitude (tfs 500)))
           (AR-min-value (min AR-low AR-high 0.707 0.099))
           (AR-100 (magnitude (tfs 100)))
           (AR-001 (magnitude (tfs 0.01)))
           
           
           ; bandwidth
           (b-threshold (min (max AR-low AR-high) 0.707 cheb-threshold))
           
           (fb (λ (w) (- (magnitude (tfs w)) b-threshold)))
           
           (wb-init (half-interval-method                            
                     fb
                     0.001 500))
           
           (wb1 (if (not (eq? wb-init #f))
                    (half-interval-method                            
                     fb
                     0.001 (+ wb-init 0.001))
                    #f))
           
           (wb2 (if (not (eq? wb1 #f))
                    (half-interval-method                            
                     fb
                     (+ wb-init 0.001) 300)
                    #f))
           
           
           ;gain margin
           (fc-g (λ (w) (- (let ((f1 (angle (tfs w))))
                             
                             (unwarp-angle-simple-2 f1 w 1)
                             ;(unwarp-angle f1 w 1)
                             
                             ) (- pi))))
           
           (wc-g (half-interval-method                            
                  fc-g
                  0.001 500))
           
           (gain-margin (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                            (begin ;(display wc-g)
                              (/ 1 (magnitude (tfs wc-g))))
                            (begin ;(display wc-g)
                              #f)))
           
           
           ; phase margin
           (fc-p (λ (w) (- (magnitude (tfs w)) 1)))
           
           (wc-p (half-interval-method                            
                  fc-p
                  0.001 500))
           
           
           (f1 (if (not (eq? wc-p #f)) (angle (tfs wc-p)) #f))
           (phase-margin (if (not (eq? wc-p #f))
                             (+ 180 
                                (* 180 (/ 1 pi)
                                   
                                   ; no searching is being done here, just one substitution
                                   ;#|
                                   (if (and (eq? had-neg-values-1 #t) (> f1 0)) ; old:(> f1 (/ pi 2))
                                       (- f1 (* 2 pi))
                                       (if (< f1 0)
                                           (begin
                                             (set! had-neg-values-1 #t)
                                             f1)
                                           f1))
                                   ;|#                                
                                   
                                   ))
                             #f))
           
           )
      
      
      
      
      (cheb-threshold! 1000)
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      (bode-plot-parameters) 
      
      (for-each 
       displayln
       
       (list (parameterize ([plot-y-label "Magnitude (abs)"]
                            [plot-y-transform log-transform]
                            [plot-y-ticks (log-ticks)]
                            [plot-x-transform log-transform]
                            [plot-x-ticks (log-ticks)]
                            )
               
               (plot (list 
                      ;(axes)
                      (tick-grid)
                      
                      ;#|
                      (function (λ (w) (magnitude (tfs w)))
                                (/ 1 1000) 500 ;#:color 3
                                )
                      ;|#                    
                      
                      #|
                    (function-interval
                     (λ (w) 1)
                     (λ (w) (magnitude (tfs-value-evaluation (make-rectangular 0 w))))
                     (/ 1 1000) 500 #:color 3 #:line2-color 3 #:line1-style 'transparent)
                    |#                    
                      
                      (function (λ (x) 1) #:color 3 #:style 'dot)
                      (function (λ (x) b-threshold) #:color 3 #:style 'dot #:y-min 0.099 #:y-max 10.001)
                      
                      ;#|
                      (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                          (error-bars (list 
                                       (vector wc-g
                                               (/ (+ 1 (magnitude (tfs wc-g))) 2)
                                               (/ (- 1 (magnitude (tfs wc-g))) 2))))
                          '())
                      ;|#
                      
                      )))
             
             
             (parameterize ([plot-y-label "Phase [deg]"]
                            [plot-x-transform log-transform]
                            [plot-x-ticks (log-ticks)]
                            )
               
               (plot (list 
                      ;(axes)
                      (tick-grid)
                      (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot)
                      ;#|
                      (function (λ (w) (* 180 (/ 1 pi)
                                          (let ((f1 (angle (tfs w))))
                                            
                                            f1
                                            
                                            )))
                                (/ 1 1000) 500  #:color 3 #:style 'dot
                                )
                      ;|#
                      (function (λ (w) (* 180 (/ 1 pi)
                                          (let ((f1 (angle (tfs w))))
                                            
                                            ;f1
                                            ;(unwarp-angle-simple f1 w 1)
                                            (unwarp-angle f1 w 1)
                                            
                                            )))
                                (/ 1 1000) 500  ;#:color 3
                                )
                      #|
                    (function-interval (λ (w) 0)
                                       (λ (w) (* 180 (/ 1 pi)
                                                 ;#|
                                                 (let ((f1 (angle (tfs-value-evaluation (make-rectangular 0 w)))))
                                                   
                                                   ;f1
                                                   ;(unwarp-angle-simple f1 w 1)
                                                   (unwarp-angle f1 w 1)
                                                   
                                                   )))
                                       ;|#
                                       (/ 1 1000) 500  #:color 1 #:line2-color 1 #:line1-style 'transparent)
                    |#
                      
                      (if (not (eq? wc-p #f)) 
                          (error-bars (list 
                                       (vector wc-p (+ (- 180) (/ phase-margin 2)) (/ phase-margin 2))))
                          '())
                      
                      
                      (function (λ (x) 0) #:color 0 #:style 'dot))))
             
             (make-space-line 10)))
      
      
      
      
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      
      
      (cond ((> (/ AR-low AR-high) 2)
             (display "Low-pass filter:")
             (newline)
             (newline))    
            ((> (/ AR-high AR-low) 2)
             (display "High-pass filter:")
             (newline)
             (newline))
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             (display "Band-pass filter:")
             (newline)
             (newline)))    
      
      
      
      (cond ((eq? wb1 #f) 
             (display "bandwidth    = (0,inf)"))
            ((eq? wb2 #f)
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]"))))
            ((> (round-decimal wb2 2) (round-decimal wb1 2))
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))
                 (begin
                   (display "bandwidth    = [")
                   (display (round-decimal wb1 2))
                   (display ",")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))))
            (else
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]")))))
      (display " - thresh.: ")
      (display (round-decimal b-threshold 3))
      (newline)
      
      
      
      ; roll-off:
      
      (cond ((> (/ AR-low AR-high) 2)
             
             ;Low-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))    
            
            ((> (/ AR-high AR-low) 2)
             
             ;High-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))
            
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             
             ;Band-pass filter:         
             (define roll-off-low (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (define roll-off-high (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off (low)  = ")
             (display roll-off-low)
             (display " (log(ΔAR)/log(Δw))")
             (newline)
             (display "roll-off (high) = ")
             (display roll-off-high)
             (display " (log(ΔAR)/log(Δw))")
             (newline)))  
      
      
      
      (if (not (eq? gain-margin #f))
          (begin (display "gain margin  = ")
                 (display (round-decimal gain-margin 2))
                 (newline))
          (begin (display "gain margin  = inf")
                 (newline)))
      
      
      
      (if (not (eq? phase-margin #f))
          ;(if (> phase-margin 0)
          (begin (display "phase margin = ")
                 (display (round-decimal phase-margin 2))
                 (display " [deg]")
                 (newline)
                 (newline))
          ;   (begin (newline)))
          (begin (display "phase margin = inf")
                 (newline)
                 (newline)))
      
      ))
  
  )







;///// Compare two systems Bode plot

(define (compare block1 block2)  ; tf1 and tf2 appropriate functions so that after the simplification two functions will remain 
  
  (display-mode-nil!)
  
  (let* ((total-tfs-value1 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block1))))))
         (tfs-value-evaluation1 (eval total-tfs-value1 anchor))                               ;change: cadr
         (total-tfs-value2 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block2))))))
         (tfs-value-evaluation2 (eval total-tfs-value2 anchor)))                                                      ;change: tf1 
    
    
    (define (tfs1 w) (tfs-value-evaluation1 (make-rectangular 0 w) 
                                            (fw1-func w) 
                                            (fw2-func w)
                                            (fw3-func w) 
                                            (fw4-func w)))
    (define (tfs2 w) (tfs-value-evaluation2 (make-rectangular 0 w) 
                                            (fw1-func w) 
                                            (fw2-func w)
                                            (fw3-func w) 
                                            (fw4-func w)))
    
    
    
    (bode-plot-parameters)
    
    (for-each
     displayln
     
     (list 
      
      (parameterize ([plot-y-label      "Magnitude (abs)"]
                     [plot-y-transform  log-transform]
                     [plot-y-ticks      (log-ticks)]
                     [plot-x-transform  log-transform]
                     [plot-x-ticks      (log-ticks)]
                     )
        
        (plot (list ;(axes)
               (tick-grid)
               (function (λ (w) (magnitude (tfs1 w)))
                         (/ 1 1000) 500
                         #:label "tf1")
               (function (λ (w) (magnitude (tfs2 w)))
                         (/ 1 1000) 500
                         #:color 3 
                         #:label "tf2")
               (function (λ (x) 1) #:color 0 #:style 'dot)
               (function (λ (x) 0.707) #:color 0 #:style 'dot))
              ))
      
      
      (parameterize ([plot-y-label      "Phase [deg]"]
                     [plot-x-transform  log-transform]
                     [plot-x-ticks      (log-ticks)])
        
        (plot (list ;(axes)
               (tick-grid)
               (function
                (λ (w) (* 180 (/ 1 pi)
                          (let ((f1 (angle (tfs1 w))))
                            
                            ;f1
                            (unwarp-angle f1 w 1)
                            
                            )))                        
                
                (/ 1 1000) 500
                #:label "tf1")
               (function
                (λ (w) (* 180 (/ 1 pi)
                          (let ((f1 (angle (tfs2 w))))
                            
                            ;f1
                            (unwarp-angle f1 w 2)
                            
                            )))              
                
                (/ 1 1000) 500
                #:color 3 
                #:label "tf2")
               (function (λ (x) 0) #:color 0 #:style 'dot)
               (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
      
      (make-space-line 10))
     
     )
    
    (set! had-neg-values-1 #f)
    (set! passed-180-1 #f)
    (set! passed-360-1 #f)
    
    (set! had-neg-values-2 #f)
    (set! passed-180-2 #f)
    (set! passed-360-2 #f)
    
    ))






;///// Evolve system Bode plot

(define evolve
  (let ((last-value '()))
    
    (lambda (block1)
      
      (newline)
      (display-mode-nil!)
      
      (let* ((total-tfs-value1 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block1))))))
             (tfs-value-evaluation1 (eval total-tfs-value1 anchor)))
        
        (define (tfs1 w) (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                (fw1-func w) 
                                                (fw2-func w)
                                                (fw3-func w) 
                                                (fw4-func w)))
        
        
        
        (bode-plot-parameters)
        
        (if (null? last-value)
            
            (begin (set! last-value total-tfs-value1)
                   
                   (for-each
                    displayln
                    
                    (list (parameterize ([plot-y-label      "Magnitude (abs)"]
                                         [plot-y-transform  log-transform]
                                         [plot-y-ticks      (log-ticks)]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)])
                            
                            (plot (list ;(axes)
                                   (tick-grid)
                                   (function (λ (w) (magnitude (tfs1 w)))
                                             (/ 1 1000) 500
                                             #:label "tf")
                                   (function (λ (x) 1) #:color 0 #:style 'dot)
                                   (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                          
                          
                          (parameterize ([plot-y-label      "Phase [deg]"]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)])
                            
                            (plot (list ;(axes)
                                   (tick-grid)
                                   (function (λ (w) (* 180 (/ 1 pi)
                                                       (angle (tfs1 w))))
                                             (/ 1 1000) 500
                                             #:label "tf1")
                                   (function (λ (x) 0) #:color 0 #:style 'dot)
                                   (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                          
                          (make-space-line 10))
                    ))
            
            
            (let* ((temp last-value)
                   (temp-value-evaluation (eval temp anchor)))
              
              (define (tfs-temp w) (temp-value-evaluation (make-rectangular 0 w) 
                                                          (fw1-func w) 
                                                          (fw2-func w)
                                                          (fw3-func w) 
                                                          (fw4-func w)))
              
              (set! last-value total-tfs-value1)
              
              (for-each
               displayln
               
               (list (parameterize ([plot-y-label      "Magnitude (abs)"]
                                    [plot-y-transform  log-transform]
                                    [plot-y-ticks      (log-ticks)]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (function (λ (w) (magnitude (tfs1 w)))
                                        (/ 1 1000) 500
                                        #:label "tf")
                              (function (λ (w) (magnitude (tfs-temp w)))
                                        (/ 1 1000) 500
                                        #:color 0 #:style 'dot
                                        #:label "last-tf")
                              (function (λ (x) 1) #:color 0 #:style 'dot)
                              (function (λ (x) 0.707) #:color 0 #:style 'dot))
                             
                             ))
                     
                     (parameterize ([plot-y-label      "Phase [deg]"]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs1 w))))
                                        (/ 1 1000) 500
                                        #:label "tf1")
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs-temp w))))
                                        (/ 1 1000) 500
                                        #:color 0 #:style 'dot
                                        #:label "last-tf")
                              (function (λ (x) 0) #:color 0 #:style 'dot)
                              (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                     
                     (make-space-line 10))
               ))
            ))    
      )))







;///// Tune system Bode plot

(define (tune block condition w)
  
  (define total-tfs-value '())
  (define m1 0)
  (define m2 0)
  
  (display-mode-nil!)
  
  ;change:
  ;(let ((simplification-result (simplify block)))
  ;(display simplification-result)
  
  ;change:
  ;(if (void? simplification-result)
  
  ;change:
  (newline)
  
  (set! total-tfs-value (cons 'λ (cons '(y) (list (ratio-to-list (get-simplified-block-value block))))))
  ;ratio-to-list-lite
  ;change:
  #|
        (begin (newline)
               (display "- cached simplification")
               (newline)
               (newline)
               (set! total-tfs-value (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list-lite simplification-result)))))))
        |#
  
  
  (if (= (length condition) 3)
      
      (if (eq? (car condition) '=)
          
          (let ((target (caddr condition)))    
            
            (cond ((eq? (cadr condition) 'AR)
                   
                   (set! m1 (newton-meth-for-solv-eq                                         
                             (λ (y) (- (magnitude ((eval (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list total-tfs-value)))
                                                               (make-rectangular 0 w) 
                                                               (fw1-func w) 
                                                               (fw2-func w)
                                                               (fw3-func w)
                                                               (fw4-func w)) anchor) y)) target))
                             1))
                   
                   (define tfs-value-evaluation1 (eval (list (cons 'λ (cons '(y) (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (caddr total-tfs-value))))))) m1) anchor))
                   
                   ;(newline)
                   (display "y = ")
                   (display (round-decimal m1 3)) 
                   (newline)
                   (newline)
                   (newline)
                   
                   
                   (bode-plot-parameters)
                   
                   (for-each
                    displayln
                    
                    (list 
                     
                     (parameterize ([plot-y-label      "Magnitude (abs)"]
                                    [plot-y-transform  log-transform]
                                    [plot-y-ticks      (log-ticks)]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (point-label (vector (* 1.0 w) (* 1.0 target)))
                              (function (λ (w) (magnitude (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                                                 (fw1-func w) 
                                                                                 (fw2-func w)
                                                                                 (fw3-func w)
                                                                                 (fw4-func w))))
                                        (/ 1 1000) 500)
                              (function (λ (x) 1) #:color 0 #:style 'dot)
                              (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                     
                     
                     (parameterize ([plot-y-label      "Phase [deg]"]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                                                (fw1-func w) 
                                                                                (fw2-func w)
                                                                                (fw3-func w)
                                                                                (fw4-func w)))))
                                        (/ 1 1000) 500)
                              
                              (function (λ (x) 0) #:color 0 #:style 'dot)
                              (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                     
                     
                     (make-space-line 10))
                    
                    ))
                  
                  
                  
                  ((eq? (cadr condition) 'ph)
                   
                   (set! m2 (newton-meth-for-solv-eq
                             (λ (y) (- (* 180 (/ 1 pi) (angle ((eval (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list total-tfs-value))) (make-rectangular 0 w) 
                                                                           (fw1-func w) 
                                                                           (fw2-func w)
                                                                           (fw3-func w)
                                                                           (fw4-func w)) anchor) y))) target))
                             1))
                   
                   (define tfs-value-evaluation2 (eval (list (cons 'λ (cons '(y) (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (caddr total-tfs-value))))))) m2) anchor))                     
                   
                   ;(newline)
                   
                   (if (eq? m2 #f)
                       (begin (display "no such y")
                              (newline)
                              (newline))
                       
                       (begin (display "y = ")
                              (display (round-decimal m2 3))                                
                              (newline)
                              (newline)
                              (newline)
                              
                              
                              
                              (bode-plot-parameters)
                              
                              (for-each
                               displayln
                               
                               (list 
                                
                                (parameterize ([plot-y-label      "Magnitude (abs)"]
                                               [plot-y-transform  log-transform]
                                               [plot-y-ticks      (log-ticks)]
                                               [plot-x-transform  log-transform]
                                               [plot-x-ticks      (log-ticks)])
                                  
                                  (plot (list ;(axes)
                                         (tick-grid)
                                         (function (λ (w) (magnitude (tfs-value-evaluation2 (make-rectangular 0 w) 
                                                                                            (fw1-func w) 
                                                                                            (fw2-func w)
                                                                                            (fw3-func w)
                                                                                            (fw4-func w))))
                                                   (/ 1 1000) 500)
                                         (function (λ (x) 1) #:color 0 #:style 'dot)
                                         (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                                
                                
                                (parameterize ([plot-y-label      "Phase [deg]"]
                                               [plot-x-transform  log-transform]
                                               [plot-x-ticks      (log-ticks)])
                                  
                                  (plot (list ;(axes)
                                         (tick-grid)
                                         (point-label (vector (* 1.0 w) (* 1.0 target)))
                                         (function (λ (w) (* 180 (/ 1 pi)
                                                             (angle (tfs-value-evaluation2 (make-rectangular 0 w) 
                                                                                           (fw1-func w) 
                                                                                           (fw2-func w)
                                                                                           (fw3-func w)
                                                                                           (fw4-func w)))))
                                                   (/ 1 1000) 500)
                                         (function (λ (x) 0) #:color 0 #:style 'dot)
                                         (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                                
                                (make-space-line 10))
                               
                               ))
                       ))
                  
                  
                  (else (error "Unrecognized condition"))))
          
          (error "Unrecognized condition"))
      
      (error "Unrecognized condition"))
  
  ;)
  
  )






;///// Nyquist plot

(define (nyquist block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))
    
    
    (let* ((AR-low (magnitude (tfs (/ 1 1000))))
           (AR-high (magnitude (tfs 500)))
           (AR-min-value (min AR-low AR-high 0.707 0.099))
           (AR-100 (magnitude (tfs 100)))
           (AR-001 (magnitude (tfs 0.01)))
           
           
           ; bandwidth
           (b-threshold (min (max AR-low AR-high) 0.707 cheb-threshold))
           
           (fb (λ (w) (- (magnitude (tfs w)) b-threshold)))
           
           (wb-init (half-interval-method                            
                     fb
                     0.001 500))
           
           (wb1 (if (not (eq? wb-init #f))
                    (half-interval-method                            
                     fb
                     0.001 (+ wb-init 0.001))
                    #f))
           
           (wb2 (if (not (eq? wb1 #f))
                    (half-interval-method                            
                     fb
                     (+ wb-init 0.001) 300)
                    #f))
           
           
           ;gain margin
           (fc-g (λ (w) (- (let ((f1 (angle (tfs w))))
                             
                             (unwarp-angle-simple-2 f1 w 1)
                             ;(unwarp-angle f1 w 1)
                             
                             ) (- pi))))
           
           (wc-g (half-interval-method                            
                  fc-g
                  0.001 500))
           
           (gain-margin (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                            (begin ;(display wc-g)
                              (/ 1 (magnitude (tfs wc-g))))
                            (begin ;(display wc-g)
                              #f)))
           
           
           ; phase margin
           (fc-p (λ (w) (- (magnitude (tfs w)) 1)))
           
           (wc-p (half-interval-method                            
                  fc-p
                  0.001 500))
           
           
           (f1 (if (not (eq? wc-p #f)) (angle (tfs wc-p)) #f))
           (phase-margin (if (not (eq? wc-p #f))
                             (+ 180 
                                (* 180 (/ 1 pi)
                                   
                                   ; no searching is being done here, just one substitution
                                   ;#|
                                   (if (and (eq? had-neg-values-1 #t) (> f1 0)) ; old:(> f1 (/ pi 2))
                                       (- f1 (* 2 pi))
                                       (if (< f1 0)
                                           (begin
                                             (set! had-neg-values-1 #t)
                                             f1)
                                           f1))
                                   ;|#                                
                                   
                                   ))
                             #f))
           
           )
      
      
      
      
      (cheb-threshold! 1000)
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      (plot-height 400)
      (plot-title "Nyquist plot")
      (plot-x-label "Real part")
      (plot-y-label "Imaginary part")
      
      
      (for-each 
       displayln
       
       (list 
        (parameterize (;[plot-x-transform cbrt-transform]
                       ;[plot-y-transform cbrt-transform])
                       [plot-x-transform  (stretch-transform -1.5 1.5 20)]
                       [plot-y-transform  (stretch-transform -1.5 1.5 20)])
          (plot (list 
                 (axes)
                 (tick-grid)
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             0.0000001 10
                             #:x-min -10 #:x-max 10 #:y-min -10 #:y-max 10
                             #:color 3
                             #:label "0<=w<oo"
                             )
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             10 (expt 10 3)
                             ;#:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                             #:color 3
                             )
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             -10 0.0000001
                             ;#:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                             #:color 2
                             #:label "-oo<w<=0"
                             )
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             (- (expt 10 3)) -10
                             ;#:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                             #:color 2
                             )
                 (points (list (vector -1 0))
                         ;#:sym 'fullcircle2
                         #:sym 'times
                         #:color 1
                         )
                 
                 ; error bars are vertical - can't be used here
                 #|
               (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                          (error-bars (list 
                                       (vector wc-g
                                               (/ (+ 1 (magnitude (tfs wc-g))) 2)
                                               (/ (- 1 (magnitude (tfs wc-g))) 2))))
                          '())
               |#
                 
                 )))
        
        
        
        
        (make-space-line 10)))
      
      
      
      
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      
      
      (cond ((> (/ AR-low AR-high) 2)
             (display "Low-pass filter:")
             (newline)
             (newline))    
            ((> (/ AR-high AR-low) 2)
             (display "High-pass filter:")
             (newline)
             (newline))
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             (display "Band-pass filter:")
             (newline)
             (newline)))    
      
      
      
      (cond ((eq? wb1 #f) 
             (display "bandwidth    = (0,inf)"))
            ((eq? wb2 #f)
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]"))))
            ((> (round-decimal wb2 2) (round-decimal wb1 2))
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))
                 (begin
                   (display "bandwidth    = [")
                   (display (round-decimal wb1 2))
                   (display ",")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))))
            (else
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]")))))
      (display " - thresh.: ")
      (display (round-decimal b-threshold 3))
      (newline)
      
      
      
      ; roll-off:
      
      (cond ((> (/ AR-low AR-high) 2)
             
             ;Low-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))    
            
            ((> (/ AR-high AR-low) 2)
             
             ;High-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))
            
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             
             ;Band-pass filter:         
             (define roll-off-low (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (define roll-off-high (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off (low)  = ")
             (display roll-off-low)
             (display " (log(ΔAR)/log(Δw))")
             (newline)
             (display "roll-off (high) = ")
             (display roll-off-high)
             (display " (log(ΔAR)/log(Δw))")
             (newline)))  
      
      
      
      (if (not (eq? gain-margin #f))
          (begin (display "gain margin  = ")
                 (display (round-decimal gain-margin 2))
                 (newline))
          (begin (display "gain margin  = inf")
                 (newline)))
      
      
      
      (if (not (eq? phase-margin #f))
          ;(if (> phase-margin 0)
          (begin (display "phase margin = ")
                 (display (round-decimal phase-margin 2))
                 (display " [deg]")
                 (newline)
                 (newline))
          ;   (begin (newline)))
          (begin (display "phase margin = inf")
                 (newline)
                 (newline)))
      
      ))
  
  )




