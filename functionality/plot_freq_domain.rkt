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
(require "../circuits.rkt")
(provide (all-defined-out))








; //////////   G. Frequency domain plot-generating functions  //////////


;The Plot library, by Neil Toronto, is used in all the following plot-creating fuctions

(plot-font-size 10)


;min & max frequency [rad/s]
(define w-min (/ 1 1000)) ;only when writen in this form - instead of 0.001 - the freq domain plots are generated correctly by the Plot library 
(define w-max 500)






;///// F(s) plot

(define (F block)  ;tf is evaluated here
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         (tfs-value-evaluation (eval total-tfs-value anchor)))
    
    (define (tfs s) (tfs-value-evaluation s 0 0 0 0)) ;no slots for f(w) functions provided here 
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
                          ;the log transform is not defined on negative arguments, so:
                          ;[plot-x-transform cbrt-transform])
                          [plot-x-transform  (stretch-transform -5 5 10)])
             
             (plot (list (axes)
                         (tick-grid)
                         (function (λ (s) (tfs s)) -100 100))
                   #:y-min -20
                   #:y-max 20))
           
           (make-space-line 10)))))








;///// Bode plots auxiliary functions


;Common plot parameters for all Bode plot-creating functions

(define (bode-plot-parameters)
  (plot-height 200)
  (plot-title (string-append (make-space-line 7) "Bode plot"))
  (plot-x-label "Frequency [rad/s]"))



;Angle unwarping: from (-180 < angle < 180) to (-540 < angle < 180), in Figures 1 & 2

(define had-neg-values-1 #f)
(define passed-180-1 #f)
(define passed-360-1 #f)

(define had-neg-values-2 #f)
(define passed-180-2 #f)
(define passed-360-2 #f)


(define (had-neg-values? fig)
  (if (= fig 1) had-neg-values-1 had-neg-values-2))

(define (passed-180? fig)
  (if (= fig 1) passed-180-1 passed-180-2))

(define (passed-360? fig)
  (if (= fig 1) passed-360-1 passed-360-2))


(define (set-had-neg-values-true! fig)
  (if (= fig 1) (set! had-neg-values-1 #t) (set! had-neg-values-2 #t)))

(define (set-passed-180-true! fig)
  (if (= fig 1) (set! passed-180-1 #t) (set! passed-180-2 #t)))

(define (set-passed-360-true! fig)
  (if (= fig 1) (set! passed-360-1 #t) (set! passed-360-2 #t)))


(define (initialize-angle-params-fig-1!)
  (set! had-neg-values-1 #f)
  (set! passed-180-1 #f)
  (set! passed-360-1 #f))

(define (initialize-angle-params-fig-2!)
  (set! had-neg-values-2 #f)
  (set! passed-180-2 #f)
  (set! passed-360-2 #f))




(define (unwarp-angle-simple! ang w fig)
  
  (if (and (eq? (had-neg-values? fig) #t) (> ang (/ pi 2)))
      (- ang (* 2 pi))
      (if (or (< ang 0) (and (= ang 0) (> w w-min)))
          (begin
            (set-had-neg-values-true! fig)
            ang)
          ang))
  )


(define (unwarp-angle-elaborate! ang w fig)
  
  (cond ((and (eq? (had-neg-values? fig) #t) (eq? (passed-360? fig) #t) 
              (> (+ ang (* 2 pi)) 0)
              )
         (- ang (* 2 pi)))
        
        ;#|
        ((and (eq? (had-neg-values? fig) #t) (not (eq? (passed-180? fig) #t))
              (> ang 0) 
              (< ang (/ pi 2)))
         ;works for pid:
         ang
         ;works for delay:
         ;(- ang (* 2 pi))
         )
        ;|#
        
        ((and (eq? (had-neg-values? fig) #t) ;(eq? (get-passed-180 id) #t)
              (> ang 0))
         (when ;(< (- ang (* 2 pi)) (* (- 1.8) pi))
             (and (eq? (passed-180? fig) #t) (< (- ang (* 2 pi)) 0))
           (set-passed-360-true! fig))
         
         ;change:
         (when (> (abs (- ang (* 2 pi))) (* 0.8 pi))
           (set-passed-180-true! fig))
         
         (- ang (* 2 pi)))
        
        (else
         (when (or (< ang 0) (and (= ang 0) (> w w-min)))
           (set-had-neg-values-true! fig))
         (when (> (abs ang) (* 0.8 pi))
           (set-passed-180-true! fig))
         ang))
  )




;Characteristic numbers computation & text display functions [SHOULD BE IMPROVED]

(define (display-filter-type AR-at-freq-min AR-at-freq-max bandwidth-threshold w-bandwidth-2)
  (cond ((and (> (/ AR-at-freq-min AR-at-freq-max) 1.5)
              (> AR-at-freq-min bandwidth-threshold)
              (not (> AR-at-freq-max (* 3 bandwidth-threshold))))
         (display "Low-pass filter")
         (newline)
         (newline))    
        ((and (> (/ AR-at-freq-max AR-at-freq-min) 1.5)
              (> AR-at-freq-max bandwidth-threshold)
              (not (> AR-at-freq-min (* 3 bandwidth-threshold))))
         (display "High-pass filter")
         (newline)
         (newline))
        ((and (< AR-at-freq-min bandwidth-threshold)
              (< AR-at-freq-max bandwidth-threshold)
              (not (eq? w-bandwidth-2 #f)))
         (display "Band-pass filter")
         (newline)
         (newline))))
      


(define (display-bandwidth w-bandwidth-1 w-bandwidth-2 bandwidth-threshold bandwidth-inverted?)          
  (cond ((eq? w-bandwidth-1 #f) 
         (display "bandwidth    = (0,∞) [rad/s]")
         (display ", thresh. = ")
         (display (round-decimal bandwidth-threshold 3)))
        ((eq? w-bandwidth-2 #f)
         (if (eq? (round-decimal w-bandwidth-1 2) 0)
             (begin 
               (display "bandwidth    = (0,∞) [rad/s]"))            
             (begin
               (display "bandwidth    = (0,")
               (display (round-decimal w-bandwidth-1 2))
               (display "] [rad/s]")))
         (display ", thresh. = ")
         (display (round-decimal bandwidth-threshold 3)))
        ((> (round-decimal w-bandwidth-2 2) (round-decimal w-bandwidth-1 2))
         (if (eq? (round-decimal w-bandwidth-1 2) 0)
             (begin 
               (display "bandwidth    = (0,")
               (display (round-decimal w-bandwidth-2 2))
               (display "] [rad/s]")
               (display ", thresh. = ")
               (display (round-decimal bandwidth-threshold 3)))             
             (begin
               (if (eq? bandwidth-inverted? #t)
                   (begin (display "bandwidth    = (0,")
                          (display (round-decimal w-bandwidth-1 2))
                          (display "] ∪ [")
                          (display (round-decimal w-bandwidth-2 2))
                          (display ",∞) [rad/s]")
                          (display ","))
                   (begin
                     (display "bandwidth    = [")
                     (display (round-decimal w-bandwidth-1 2))
                     (display ",")
                     (display (round-decimal w-bandwidth-2 2))
                     (display "] [rad/s]")
                     (display ",")))
               (newline)
               (display "threshold    = ")
               (display (round-decimal bandwidth-threshold 3)))))
        (else
         (if (eq? (round-decimal w-bandwidth-1 2) 0)
             (begin 
               (display "bandwidth    = (0,∞) [rad/s]"))
             (begin
               (display "bandwidth    = (0,")
               (display (round-decimal w-bandwidth-1 2))
               (display "] [rad/s]")))
         (display ", thresh. = ")
         (display (round-decimal bandwidth-threshold 3))))
  (newline))



(define (display-roll-off AR-at-freq-min AR-at-freq-max AR-at-100 AR-at-001 w-bandwidth-2)
           
  (cond ((> (/ AR-at-freq-min AR-at-freq-max) 2)
             
         ;Low-pass filter:
         (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-at-100 AR-at-freq-max))
                                                            (log (/ 100 w-max)))) 3))
         (display "roll-off     = ")
         (display roll-off)
         (display " (log(ΔAR)/log(Δw))")
         (newline))    
            
        ((> (/ AR-at-freq-max AR-at-freq-min) 2)
             
         ;High-pass filter:
         (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-at-freq-min AR-at-001))
                                                            (log (/ w-min 0.01)))) 3))
         (display "roll-off     = ")
         (display roll-off)
         (display " (log(ΔAR)/log(Δw))")
         (newline))
            
        ((and (< AR-at-freq-min 0.05) (< AR-at-freq-max 0.05) (not (eq? w-bandwidth-2 #f)))
             
         ;Band-pass filter:         
         (define roll-off-low (round-decimal (exact->inexact (/ (log (/ AR-at-freq-min AR-at-001))
                                                                (log (/ w-min 0.01)))) 3))
         (define roll-off-high (round-decimal (exact->inexact (/ (log (/ AR-at-100 AR-at-freq-max))
                                                                 (log (/ 100 w-max)))) 3))
         (display "roll-off (low)  = ")
         (display roll-off-low)
         (display " (log(ΔAR)/log(Δw))")
         (newline)
         (display "roll-off (high) = ")
         (display roll-off-high)
         (display " (log(ΔAR)/log(Δw))")
         (newline)))
  )



(define (display-gain-phase-margins gain-margin phase-margin)
   
  (if (not (eq? gain-margin #f))
      (begin (display "gain margin  = ")
             (display (round-decimal gain-margin 2))
             (newline))
      (begin (display "gain margin  = ∞")
             (newline)))
      
  (if (not (eq? phase-margin #f))
      ;(if (> phase-margin 0)
      (begin (display "phase margin = ")
             (display (round-decimal phase-margin 2))
             (display " [deg]")
             (newline)
             (newline))
      ;   (begin (newline)))
      (begin (display "phase margin = ∞")
             (newline)
             (newline)))
  )







;///// Basic Bode plot

(define (bode block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) ; 4 slots for f(w) functions provided here 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))
    
    
    (let* ((AR-at-freq-min (magnitude (tfs w-min)))
           (AR-at-freq-max (magnitude (tfs w-max)))
           (AR-min-value (min AR-at-freq-min AR-at-freq-max 0.707 0.099))
           (AR-at-001 (magnitude (tfs 0.01)))
           (AR-at-100 (magnitude (tfs 100)))
           

           ;[SHOULD BE IMPROVED]
           
           ;bandwidth parameters computation
           (bandwidth-threshold (min 0.707 chebyshev-threshold))
           
           (f-bandwidth (λ (w) (- (magnitude (tfs w)) bandwidth-threshold)))

           ;orig
           ;(w-bandwidth-init (half-interval-method                            
           ;                   f-bandwidth
           ;                   w-min
           ;                   w-max))

           (w-bandwidth-init (or (half-interval-method                            
                                  f-bandwidth
                                  w-min
                                  w-max)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  0.011)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  0.101)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  1.001)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  10.01)))
           
           (w-bandwidth-1 (if (not (eq? w-bandwidth-init #f))
                              (half-interval-method                            
                               f-bandwidth
                               w-min
                               (+ w-bandwidth-init w-min))
                              #f))
           
           (w-bandwidth-2 (if (not (eq? w-bandwidth-1 #f))
                              (half-interval-method                            
                               f-bandwidth
                               (+ w-bandwidth-init w-min)
                               w-max)
                              #f))

           (bandwidth-inverted? (if (not (eq? (and w-bandwidth-1 w-bandwidth-2) #f))
                                    (if (< (f-bandwidth (average w-bandwidth-1 w-bandwidth-2))
                                           (f-bandwidth w-bandwidth-1))
                                        #t
                                        #f)
                                    #f))
           
           
           ;gain margin parameters computation
           (f-gain-margin (λ (w) (- (let ((f1 (angle (tfs w))))
                             
                                      (unwarp-angle-simple! f1 w 1)
                             
                                      ) (- pi))))
           
           (w-gain-margin (half-interval-method                            
                           f-gain-margin
                           w-min
                           w-max))
           
           (gain-margin (if (not (or (eq? w-gain-margin #f) (eq? w-gain-margin 0)))
                            (begin ;(display wc-g)
                              (/ 1 (magnitude (tfs w-gain-margin))))
                            (begin ;(display wc-g)
                              #f)))
           
           
           ;phase margin parameters computation
           (f-phase-margin (λ (w) (- (magnitude (tfs w)) 1)))
           
           (w-phase-margin (half-interval-method                            
                            f-phase-margin
                            w-min
                            w-max))
           
           
           (f1 (if (not (eq? w-phase-margin #f)) (angle (tfs w-phase-margin)) #f))
           (phase-margin (if (not (eq? w-phase-margin #f))
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
      
      
      
      
      (set-chebyshev-threshold! 1000)
      (initialize-angle-params-fig-1!)          
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
                                w-min w-max ;#:color 3
                                )
                      ;|#                    
                      
                      #|
                    (function-interval
                     (λ (w) 1)
                     (λ (w) (magnitude (tfs-value-evaluation (make-rectangular 0 w))))
                     freq_min freq_max #:color 3 #:line2-color 3 #:line1-style 'transparent)
                    |#                    
                      
                      (function (λ (x) 1) #:color 3 #:style 'dot)
                      (function (λ (x) bandwidth-threshold) #:color 3 #:style 'dot #:y-min 0.099 #:y-max 10.001)
                      
                      ;#|
                      (if (not (or (eq? w-gain-margin #f) (eq? w-gain-margin 0)))
                          (error-bars (list 
                                       (vector w-gain-margin
                                               (/ (+ 1 (magnitude (tfs w-gain-margin))) 2)
                                               (/ (- 1 (magnitude (tfs w-gain-margin))) 2))))
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
                      (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot)
                      ;#|
                      (function (λ (w) (* 180 (/ 1 pi)
                                          (let ((f1 (angle (tfs w))))
                                            
                                            f1
                                            
                                            )))
                                w-min w-max  #:color 3 #:style 'dot
                                )
                      ;|#
                      (function (λ (w) (* 180 (/ 1 pi)
                                          (let ((f1 (angle (tfs w))))
                                            
                                            ;f1
                                            ;(unwarp-angle-simple f1 w 1)
                                            (unwarp-angle-elaborate! f1 w 1)
                                            
                                            )))
                                w-min w-max  ;#:color 3
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
                                       freq_min freq_max  #:color 1 #:line2-color 1 #:line1-style 'transparent)
                    |#
                      
                      (if (not (eq? w-phase-margin #f)) 
                          (error-bars (list 
                                       (vector w-phase-margin (+ (- 180) (/ phase-margin 2)) (/ phase-margin 2))))
                          '())
                      
                      
                      (function (λ (x) 0) #:color 0 #:style 'dot))))
             
             (make-space-line 10)))
      
      
      
      
      (initialize-angle-params-fig-1!)
      
      ;filter type computation & text display
      (display-filter-type AR-at-freq-min AR-at-freq-max bandwidth-threshold w-bandwidth-2)
      
      ;bandwidth computation & text display
      (display-bandwidth w-bandwidth-1 w-bandwidth-2 bandwidth-threshold bandwidth-inverted?)
      
      ;roll-off computation & text display
      (display-roll-off AR-at-freq-min AR-at-freq-max AR-at-100 AR-at-001 w-bandwidth-2)
           
      ;gain & phase margins computation & text display
      (display-gain-phase-margins gain-margin phase-margin)

         
      ))
  
  )







;///// Compare two blocks Bode plot

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
                         w-min w-max
                         #:label "tf1")
               (function (λ (w) (magnitude (tfs2 w)))
                         w-min w-max
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
                            (unwarp-angle-elaborate! f1 w 1)
                            
                            )))                        
                
                w-min w-max
                #:label "tf1")
               (function
                (λ (w) (* 180 (/ 1 pi)
                          (let ((f1 (angle (tfs2 w))))
                            
                            ;f1
                            (unwarp-angle-elaborate! f1 w 2)
                            
                            )))              
                
                w-min w-max
                #:color 3 
                #:label "tf2")
               (function (λ (x) 0) #:color 0 #:style 'dot)
               (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
      
      (make-space-line 10))
     
     )
    

    (initialize-angle-params-fig-1!)
    (initialize-angle-params-fig-2!)

    
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
                                             w-min w-max
                                             #:label "tf")
                                   (function (λ (x) 1) #:color 0 #:style 'dot)
                                   (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                          
                          
                          (parameterize ([plot-y-label      "Phase [deg]"]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)])
                            
                            (plot (list ;(axes)
                                   (tick-grid)
                                   (function (λ (w) (* 180 (/ 1 pi) (angle (tfs1 w))))
                                             w-min w-max
                                             #:label "tf")
                                   (function (λ (x) 0) #:color 0 #:style 'dot)
                                   (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
                          
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
                                        w-min w-max
                                        #:label "tf")
                              (function (λ (w) (magnitude (tfs-temp w)))
                                        w-min w-max
                                        #:color 0 #:style 'dot
                                        #:label "previous tf")
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
                                        w-min w-max
                                        #:label "tf")
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs-temp w))))
                                        w-min w-max
                                        #:color 0 #:style 'dot
                                        #:label "previous tf")
                              (function (λ (x) 0) #:color 0 #:style 'dot)
                              (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
                     
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
                                        w-min w-max)
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
                                        w-min w-max)
                              
                              (function (λ (x) 0) #:color 0 #:style 'dot)
                              (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
                     
                     
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
                                                   w-min w-max)
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
                                                   w-min w-max)
                                         (function (λ (x) 0) #:color 0 #:style 'dot)
                                         (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
                                
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
    
    
    (let* ((AR-at-freq-min (magnitude (tfs w-min)))
           (AR-at-freq-max (magnitude (tfs w-max)))
           (AR-min-value (min AR-at-freq-min AR-at-freq-max 0.707 0.099))
           (AR-at-001 (magnitude (tfs 0.01)))
           (AR-at-100 (magnitude (tfs 100)))
           

           ;[SHOULD BE IMPROVED]
           
           ;bandwidth parameters computation
           (bandwidth-threshold (min 0.707 chebyshev-threshold))
           
           (f-bandwidth (λ (w) (- (magnitude (tfs w)) bandwidth-threshold)))

           ;orig
           ;(w-bandwidth-init (half-interval-method                            
           ;                   f-bandwidth
           ;                   w-min
           ;                   w-max))

           (w-bandwidth-init (or (half-interval-method                            
                                  f-bandwidth
                                  w-min
                                  w-max)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  0.011)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  0.101)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  1.001)
                                 (newton-meth-for-solv-eq                            
                                  f-bandwidth
                                  10.01)))
           
           (w-bandwidth-1 (if (not (eq? w-bandwidth-init #f))
                              (half-interval-method                            
                               f-bandwidth
                               w-min
                               (+ w-bandwidth-init w-min))
                              #f))
           
           (w-bandwidth-2 (if (not (eq? w-bandwidth-1 #f))
                              (half-interval-method                            
                               f-bandwidth
                               (+ w-bandwidth-init w-min)
                               300)
                              #f))

           (bandwidth-inverted? (if (not (eq? (and w-bandwidth-1 w-bandwidth-2) #f))
                                    (if (< (f-bandwidth (average w-bandwidth-1 w-bandwidth-2))
                                           (f-bandwidth w-bandwidth-1))
                                        #t
                                        #f)
                                    #f))
           
           
           ;gain margin parameters computation
           (f-gain-margin (λ (w) (- (let ((f1 (angle (tfs w))))
                             
                                      (unwarp-angle-simple! f1 w 1)
                             
                                      ) (- pi))))
           
           (w-gain-margin (half-interval-method                            
                           f-gain-margin
                           w-min
                           w-max))
           
           (gain-margin (if (not (or (eq? w-gain-margin #f) (eq? w-gain-margin 0)))
                            (begin ;(display wc-g)
                              (/ 1 (magnitude (tfs w-gain-margin))))
                            (begin ;(display wc-g)
                              #f)))
           
           
           ;phase margin parameters computation
           (f-phase-margin (λ (w) (- (magnitude (tfs w)) 1)))
           
           (w-phase-margin (half-interval-method                            
                            f-phase-margin
                            w-min
                            w-max))
           
           
           (f1 (if (not (eq? w-phase-margin #f)) (angle (tfs w-phase-margin)) #f))
           (phase-margin (if (not (eq? w-phase-margin #f))
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
      
      
      
      
      (set-chebyshev-threshold! 1000)
      (initialize-angle-params-fig-1!)
      
      
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
                             #:label "0 < w < ∞"
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
                             #:label "-∞ < w ≤ 0"
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
      
      
      
      
      (initialize-angle-params-fig-1!)
            
      ;filter type computation & text display
      (display-filter-type AR-at-freq-min AR-at-freq-max bandwidth-threshold w-bandwidth-2)
      
      ;bandwidth computation & text display
      (display-bandwidth w-bandwidth-1 w-bandwidth-2 bandwidth-threshold bandwidth-inverted?)
      
      ;roll-off computation & text display
      (display-roll-off AR-at-freq-min AR-at-freq-max AR-at-100 AR-at-001 w-bandwidth-2)
           
      ;gain & phase margins computation & text display
      (display-gain-phase-margins gain-margin phase-margin)
      
      
      ))
  
  )




