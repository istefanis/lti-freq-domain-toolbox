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
(require "../math_library/general.rkt")
(require "../math_library/symbolic_algebra.rkt")
(require "../math_library/numerical_analysis.rkt")
(require "../elements/general.rkt")
(require "../util/display_modes.rkt")
(require "total_tf_parsing.rkt")
(require "characteristic_numbers.rkt")
(provide (all-defined-out))








; //////////   G. Frequency domain plot-generating functions  //////////


;The Plot library, by Neil Toronto, is used in all the following plot-creating fuctions

(plot-font-size 10)
(plot-width 420)


;min & max frequency [rad/s]
(define w-min (/ 1 1000)) ;only when writen in this form - instead of 0.001 - the freq domain plots are generated correctly by the Plot library 
(define w-max 1000) ;must be large enough so that angle adjustment based on w-max -> +inf works


;magnitude & phase curves computation precision factor
(define curves-precision-factor 100) ;increase for smoother curves


;bandwidth thresholds
(define half-power-threshold 0.707)
(define chebyshev-threshold 1000)
(define (set-chebyshev-threshold! x) (set! chebyshev-threshold x))






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
                          [plot-y-far-tick-labels? #t]
                          ;the log transform is not defined on negative arguments, so:
                          ;[plot-x-transform cbrt-transform])
                          [plot-x-transform  (stretch-transform -5 5 10)])
             
             (plot (list (axes)
                         (tick-grid)
                         (function (λ (s) (tfs s)) -100 100))
                   #:y-min -20
                   #:y-max 20))
           
           (make-space-line 10)))))






;///// Bode plot util functions

;Common plot parameters for all Bode plot-creating functions

(define (bode-plot-parameters)
  (plot-height 200)
  (plot-title (string-append (make-space-line 7) "Bode plot"))
  (plot-x-label "Frequency [rad/s]"))





;///// Computation of zeros & poles

(define zeros-1 '())
(define zeros-2 '())

(define poles-1 '())
(define poles-2 '())

(define expected-phase-value-at-w-max-1 null)
(define expected-phase-value-at-w-max-2 null)


(define (compute-zeros-poles! tf fig)
  (let ((a (get-numer tf)) ;'(poly-dense s 5 8)
        (b (get-denom tf)))
    (let ((numer-term-list (cdr (cdr a))) ;'(5 8)
          (denom-term-list (cdr (cdr b))))

      (if (or (contains-symbols?-tree numer-term-list)
              (contains-symbols?-tree denom-term-list))
          (begin 
            ;no reduction in this case - so numbers must be rounded
            (set! numer-term-list
                  (make-poly-dense 's (map (λ (x) (if (list? x)
                                                      (if (contains-symbols?-tree x)
                                                          (round-decimal-tree x)
                                                          (round-decimal (eval x anchor) 3))
                                                      (if (not (symbol? x))
                                                          (round-decimal (eval x anchor) 3)
                                                          x))) 
                                           numer-term-list)))

            (set! denom-term-list
                  (make-poly-dense 's (map (λ (x) (if (list? x)
                                                      (if (contains-symbols?-tree x)
                                                          (round-decimal-tree x)
                                                          (round-decimal (eval x anchor) 3))
                                                      (if (not (symbol? x))
                                                          (round-decimal (eval x anchor) 3)
                                                          x)))
                                           denom-term-list))))

          ;reduction
          (let ((reduction (reduce (make-poly-dense 's (map (λ(x) (eval x anchor)) numer-term-list))
                                   (make-poly-dense 's (map (λ(x) (eval x anchor)) denom-term-list)))))
            ;(displayln reduction)
            (set! numer-term-list (map (λ(x) (if (< (imag-part x) 0.000001) (real-part x) x)) (cdr (cdr (car reduction)))))
            (set! denom-term-list (map (λ(x) (if (< (imag-part x) 0.000001) (real-part x) x)) (cdr (cdr (cadr reduction)))))

     
            (if (eq? fig 1)
                (begin
                  (set! zeros-1 (find-complex-roots-of-polynomial numer-term-list))
                  (set! poles-1 (find-complex-roots-of-polynomial denom-term-list))
                  
                  (display "zeros: ")(displayln (replace-empty-list-for-display (merge-complex-conjugates-for-display zeros-1)))
                  (display "poles: ")(displayln (replace-empty-list-for-display (merge-complex-conjugates-for-display poles-1)))

                  ;computation of expected phase value at w-max, according to zeros & poles
                  (let* ((zeros-at-positive-halfplane (length (filter (λ(x) (> (real-part x) 0)) zeros-1)))
                         (zeros-at-negative-halfplane-or-y-axis (length (filter (λ(x) (<= (real-part x) 0)) zeros-1))))
                    (set! expected-phase-value-at-w-max-1
                          (* -90 (+ (length denom-term-list) -1 zeros-at-positive-halfplane (- zeros-at-negative-halfplane-or-y-axis))))
                    ;(display "expected phase value at w-max: ")(displayln expected-phase-value-at-w-max-1)
                    ))
                (begin
                  (set! zeros-2 (find-complex-roots-of-polynomial numer-term-list))
                  (set! poles-2 (find-complex-roots-of-polynomial denom-term-list))
                  
                  (display "zeros: ")(displayln (replace-empty-list-for-display (merge-complex-conjugates-for-display zeros-2)))
                  (display "poles: ")(displayln (replace-empty-list-for-display (merge-complex-conjugates-for-display poles-2)))

                  ;computation of expected phase value at w-max, according to zeros & poles
                  (let* ((zeros-at-positive-halfplane (length (filter (λ(x) (> (real-part x) 0)) zeros-2)))
                         (zeros-at-negative-halfplane-or-y-axis (length (filter (λ(x) (<= (real-part x) 0)) zeros-2))))
                    (set! expected-phase-value-at-w-max-2
                          (* -90 (+ (length denom-term-list) -1 zeros-at-positive-halfplane (- zeros-at-negative-halfplane-or-y-axis))))
                    ;(display "expected phase value at w-max: ")(displayln expected-phase-value-at-w-max-2)
                    )))
            (newline)
            )))))


(define (merge-complex-conjugates-for-display complex-numbers-list)
  (map (λ(x) (if (< (imag-part x) 0)
                 (string-append (number->string (real-part x))
                                "±"
                                (number->string (abs (imag-part x)))
                                "i")
                 x))
       (filter (λ(x) (<= (imag-part x) 0)) complex-numbers-list)))

(define (replace-empty-list-for-display complex-numbers-list)
  (if (> (length complex-numbers-list) 0)
      complex-numbers-list
      "N/A"))





;///// Computation of magnitude points

(define magnitude-points-1 '())
(define magnitude-points-2 '())

(define min-magnitude-1 1e8)
(define max-magnitude-1 0)
(define min-magnitude-2 1e8)
(define max-magnitude-2 0)

(define (compute-magnitude-points! w-min w-max tfs fig)
  (define (loop points w) 
    (if (< w w-max)
        (let ((new-magnitude-value (magnitude (tfs w))))
          (if (eq? fig 1)
              (begin (cond ((and (number? new-magnitude-value) (< new-magnitude-value min-magnitude-1)) (set! min-magnitude-1 new-magnitude-value)))
                     (cond ((and (number? new-magnitude-value) (> new-magnitude-value max-magnitude-1)) (set! max-magnitude-1 new-magnitude-value))))
              (begin (cond ((and (number? new-magnitude-value) (< new-magnitude-value min-magnitude-2)) (set! min-magnitude-2 new-magnitude-value)))
                     (cond ((and (number? new-magnitude-value) (> new-magnitude-value max-magnitude-2)) (set! max-magnitude-2 new-magnitude-value)))))
          (loop (append points (list (list w new-magnitude-value)))      
                (+ w (/ (sqrt w) curves-precision-factor))))
        points))

  (if (eq? fig 1)
      (begin (set! min-magnitude-1 1e8)
             (set! max-magnitude-1 0))
      (begin (set! min-magnitude-2 1e8)
             (set! max-magnitude-2 0)))
  (let ((points (loop '() w-min)))
    #|
    (if (eq? fig 1)
        (begin (displayln min-magnitude-1)
               (displayln max-magnitude-1))
        (begin (displayln min-magnitude-2)
               (displayln max-magnitude-2)))
    |#
    (if (eq? fig 1)
        (set! magnitude-points-1 points)
        (set! magnitude-points-2 points))
    points))





;///// Computation of phase points

(define phase-points-1 '())
(define phase-points-2 '())


;Angle unwrapping based on the comparison of consecutive phase values

(define angle-adjustment-total-1 0) ;Figure 1
(define angle-adjustment-total-2 0) ;Figure 2

(define last-angle-value-1 0)
(define last-angle-value-2 0)

(define (unwrap-angle-value! new-angle-value w fig)
  (set! new-angle-value (+ new-angle-value
                           (if (eq? fig 1) angle-adjustment-total-1 angle-adjustment-total-2)));
  (let ((new-adjustment 0)
        (diff (- new-angle-value (if (eq? fig 1) last-angle-value-1 last-angle-value-2))))
    (cond ((> diff 3.5)
           ;(displayln new-angle-value)
           ;(displayln (if (eq? fig 1) last-angle-value-1 last-angle-value-2))
           ;(display "Angle unwrap adjustment by -360, at w=")(displayln (round-decimal w 5))
           (set! new-adjustment (* -2 pi)))
          ((< diff -3.5)
           ;(displayln new-angle-value)
           ;(displayln (if (eq? fig 1) last-angle-value-1 last-angle-value-2))
           ;(display "Angle unwrap adjustment by +360, at w=")(displayln (round-decimal w 5))
           (set! new-adjustment (* 2 pi))))
    (if (eq? fig 1)
        (begin (set! diff (+ diff new-adjustment))
               (set! angle-adjustment-total-1 (+ angle-adjustment-total-1 new-adjustment))
               (set! last-angle-value-1 (+ last-angle-value-1 diff))
               ;(displayln last-angle-value-1)
               last-angle-value-1)
        (begin  (set! diff (+ diff new-adjustment))
                (set! angle-adjustment-total-2 (+ angle-adjustment-total-2 new-adjustment))
                (set! last-angle-value-2 (+ last-angle-value-2 diff))
                ;(displayln last-angle-value-2)
                last-angle-value-2))))



(define (compute-phase-points! w-min w-max tfs fig)
  (define (loop points w) 
    (if (< w w-max)
        (let ((new-phase-value (* 180 (/ 1 pi) (unwrap-angle-value! (angle (tfs w)) w fig))))
          ;(display w)(display ": ")
          ;(displayln new-phase-value)
          (loop (append points (list (list w new-phase-value)))
                (+ w (/ (sqrt w) curves-precision-factor))))
        points))
  
  (if (eq? fig 1)
      (begin (set! angle-adjustment-total-1 0)
             (set! last-angle-value-1 (angle (tfs w-min))))
      (begin (set! angle-adjustment-total-2 0)
             (set! last-angle-value-2 (angle (tfs w-min)))))

  ;adjustment of phase based on expected phase value at w-max according to zeros & poles
  (let* ((points (loop '() w-min))
         (last-phase-value (cadr (last points)))
         (expected-phase-value-at-w-max (if (eq? fig 1) expected-phase-value-at-w-max-1 expected-phase-value-at-w-max-2)))
    
    (cond ((not (eq? expected-phase-value-at-w-max '()))
          
           (cond ((> last-phase-value (+ expected-phase-value-at-w-max 20))
                  (let ((factor (ceiling (/ (- (abs (- last-phase-value expected-phase-value-at-w-max)) 20) 180))))
                    #|
                    (display "Current phase value at w-max: ") (displayln last-phase-value)
                    (display "Expected phase value at w-max: ") (displayln expected-phase-value-at-w-max)
                    (display "Phase adjustment via w-max: ") (displayln (- (* factor 180)))
                    (newline)
                    |#
                    (log-messages (list "[CP-88] Current phase value at w-max: " last-phase-value
                                        "Expected phase value at w-max: " expected-phase-value-at-w-max
                                        "Phase adjustment via w-max: " (- (* factor 180)))
                                  'checkpoints)
                    (set! points (map (λ(x) (list (car x) (- (cadr x) (* factor 180)))) points))))
                 
                 ((< (+ last-phase-value 20) expected-phase-value-at-w-max)
                  (let ((factor (ceiling (/ (- (abs (- expected-phase-value-at-w-max last-phase-value)) 20) 180))))
                    #|
                    (display "Current phase value at w-max: ") (displayln last-phase-value)
                    (display "Expected phase value at w-max: ") (displayln expected-phase-value-at-w-max)
                    (display "Phase adjustment via w-max: ") (displayln (* factor 180))
                    (newline)
                    |#
                    (log-messages (list "[CP-89] Current phase value at w-max: " last-phase-value
                                        "Expected phase value at w-max: " expected-phase-value-at-w-max
                                        "Phase adjustment via w-max: " (* factor 180))
                                  'checkpoints)
                    (set! points (map (λ(x) (list (car x) (+ (cadr x) (* factor 180)))) points)))))))

    (if (eq? fig 1)
        (set! phase-points-1 points)
        (set! phase-points-2 points))
    points))







;///// Basic Bode plot

(define (bode block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is possible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) ; 4 slots for f(w) functions provided here 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))

    ;(display (get-simplified-block-value block))

      
    (set-chebyshev-threshold! 1000)        

    (compute-zeros-poles! (get-simplified-block-value block) 1)

    ;reset angle unwrapping
    (set! angle-adjustment-total-1 0)
    (set! last-angle-value-1 (angle (tfs w-min)))

    ;compute points
    (compute-magnitude-points! w-min w-max tfs 1)
    (compute-phase-points! w-min w-max tfs 1)
    
    
    (let* ((magnitude-at-w-min (magnitude (tfs w-min)))
           (magnitude-at-w-max (magnitude (tfs w-max)))
           (magnitude-at-0005 (magnitude (tfs 0.005)))
           (magnitude-at-900 (magnitude (tfs 900)))

           
           ;bandwidth parameters computation
           (bandwidth-threshold (min half-power-threshold chebyshev-threshold))
           
           (bandwidth-function (λ (w) (- (magnitude (tfs w)) bandwidth-threshold)))

           (w-cutoff-root-intervals (find-curve-root-intervals
                                     (map (λ(x) (list (car x) (- (cadr x) bandwidth-threshold))) magnitude-points-1)))

           (w-cutoff-roots (map (λ(interval) (half-interval-method bandwidth-function (car interval) (cadr interval)))
                                w-cutoff-root-intervals))

           ;[SHOULD BE IMPROVED]
           
           #|           
           ;gain margin parameters computation

           (f-gain-margin (λ (w) (- (let ((f1 (angle (tfs w))))
                                      (unwrap-angle-simple! f1 w 1))                            
                                    (- pi))))
           
           (w-gain-margin (half-interval-method f-gain-margin w-min w-max))
           
           (gain-margin (if (not (or (eq? w-gain-margin #f) (eq? w-gain-margin 0)))
                            (/ 1 (magnitude (tfs w-gain-margin)))
                            #f))
           
           
           ;phase margin parameters computation
           
           (f-phase-margin (λ (w) (- (magnitude (tfs w)) 1)))
           
           (w-phase-margin (half-interval-method f-phase-margin w-min w-max))
           
           (f1 (if (not (eq? w-phase-margin #f))
                   (angle (tfs w-phase-margin)) #f))

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
           |#           
           )
      
      
      (bode-plot-parameters)
      
      (for-each 
       displayln
       
       (list (parameterize ([plot-y-label "Magnitude"]
                            [plot-y-transform log-transform]
                            [plot-y-ticks (log-ticks)]
                            [plot-y-far-label "[dB]"]
                            [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                            (invertible-compose
                                                             (linear-scale 8.68589) ;Neper to dB conversion factor
                                                             (invertible-inverse (invertible-function exp log))))]
                            [plot-x-transform log-transform]
                            [plot-x-ticks (log-ticks)]
                            ;[plot-x-far-label "[Hz]"]
                            ;[plot-x-far-ticks  (ticks-scale (plot-x-ticks) (linear-scale (/ 1 (* 2 pi))))]
                            )
               
               (plot (append
                      (list 
                       ;(axes)
                       (tick-grid)
                      
                       ;(function (λ (w) (magnitude (tfs w))) w-min w-max) ;#:color 3
                       
                       (lines magnitude-points-1)

                       (function (λ (x) 1) #:color 3 #:style 'dot)
                       (function (λ (x) bandwidth-threshold) #:color 3 #:style 'dot #:y-min 0.099 #:y-max 10.001)
                      
                       #|
                       (if (not (or (eq? w-gain-margin #f) (eq? w-gain-margin 0)))
                           (error-bars (list 
                                        (vector w-gain-margin
                                                (/ (+ 1 (magnitude (tfs w-gain-margin))) 2)
                                                (/ (- 1 (magnitude (tfs w-gain-margin))) 2))))
                           '())
                       |#
                       )

                      ;cutoff frequences vertical lines
                      (map (λ(x) (vrule x #:color 3 #:style 'dot #:width 0.5)) w-cutoff-roots)
                      )))
             
             
             (parameterize ([plot-y-label "Phase [deg]"]
                            [plot-x-transform log-transform]
                            [plot-x-ticks (log-ticks)]
                            [plot-y-far-label "[rad]"]
                            [plot-y-far-ticks (ticks-scale (plot-y-ticks)
                                                           (linear-scale (/ pi 180)))]
                            )
               
               (plot (list 
                      ;(axes)
                      (tick-grid)
                      (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot)

                      ;wrapped angle:
                      #|
                      (function (λ (w) (* 180 (/ 1 pi) (angle (tfs w))))
                                w-min w-max  #:color 3 #:style 'dot #:width 0.5)
                      |#
                      
                      (lines phase-points-1)

                      #|
                      (if (not (eq? w-phase-margin #f)) 
                          (error-bars (list 
                                       (vector w-phase-margin (+ (- 180) (/ phase-margin 2)) (/ phase-margin 2))))
                          '())
                      |#
                      
                      (function (λ (x) 0) #:color 0 #:style 'dot))))
             
             (make-space-line 10)))
      
      
      ;filter type computation & text display
      (compute-filter-type! min-magnitude-1 max-magnitude-1 magnitude-at-w-min magnitude-at-w-max bandwidth-threshold)
      
      ;bandwidth computation & text display
      (compute-bandwidth w-cutoff-roots magnitude-points-1 bandwidth-function bandwidth-threshold half-power-threshold w-min)
      
      ;roll-off computation & text display
      (compute-roll-off magnitude-at-w-min magnitude-at-w-max magnitude-at-0005 magnitude-at-900 w-min w-max)
           
      ;gain & phase margins text display
      ;(display-gain-phase-margins gain-margin w-gain-margin phase-margin w-phase-margin)

      ))
  )







;///// Compare two blocks Bode plot

(define (compare block1 block2)  ; tf1 and tf2 appropriate functions so that after the simplification two functions will remain 
  
  ;(set-logger-mode! 'nil)
  
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


    (set-chebyshev-threshold! 1000)        
    
    (compute-zeros-poles! (get-simplified-block-value block1) 1)
    (compute-zeros-poles! (get-simplified-block-value block2) 2)
    
    ;reset angle unwrapping
    (set! angle-adjustment-total-1 0)
    (set! angle-adjustment-total-2 0)
    (set! last-angle-value-1 (angle (tfs1 w-min)))
    (set! last-angle-value-2 (angle (tfs2 w-min)))
    
    ;compute points
    (compute-magnitude-points! w-min w-max tfs1 1)
    (compute-magnitude-points! w-min w-max tfs2 2)
    (compute-phase-points! w-min w-max tfs1 1)
    (compute-phase-points! w-min w-max tfs2 2)

    
    (bode-plot-parameters)
    
    (for-each
     displayln
     
     (list 
      
      (parameterize ([plot-y-label      "Magnitude"]
                     [plot-y-transform  log-transform]
                     [plot-y-ticks      (log-ticks)]
                     [plot-y-far-label "[dB]"]
                     [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                     (invertible-compose
                                                      (linear-scale 8.68589) ;Neper to dB conversion factor
                                                      (invertible-inverse (invertible-function exp log))))]
                     [plot-x-transform  log-transform]
                     [plot-x-ticks      (log-ticks)]
                     )
        
        (plot (list ;(axes)
               (tick-grid)

               (lines magnitude-points-1 #:label "tf1")
               (lines magnitude-points-2 #:color 3 #:label "tf2")
               
               (function (λ (x) 1) #:color 0 #:style 'dot)
               (function (λ (x) half-power-threshold) #:color 0 #:style 'dot))
              ))
      
      
      (parameterize ([plot-y-label      "Phase [deg]"]
                     [plot-x-transform  log-transform]
                     [plot-x-ticks      (log-ticks)]
                     [plot-y-far-label "[rad]"]
                     [plot-y-far-ticks (ticks-scale (plot-y-ticks)
                                                    (linear-scale (/ pi 180)))])
        
        (plot (list ;(axes)
               (tick-grid)
               
               (lines phase-points-1 #:label "tf1")
               (lines phase-points-2 #:color 3 #:label "tf2")
             
               (function (λ (x) 0) #:color 0 #:style 'dot)
               (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
      
      (make-space-line 10))
     
     )
    
    ))






;///// Evolve system Bode plot

(define evolve
  (let ((last-value '())
        (last-simplified-block-value '()))
    
    (lambda (block1)
      
      ;(newline)
      ;(set-logger-mode! 'nil)
      
      (let* ((total-tfs-value1 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block1))))))
             (tfs-value-evaluation1 (eval total-tfs-value1 anchor)))
        
        (define (tfs1 w) (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                (fw1-func w) 
                                                (fw2-func w)
                                                (fw3-func w) 
                                                (fw4-func w)))
        

        (compute-zeros-poles! (get-simplified-block-value block1) 1)
        
        ;reset angle unwrapping
        (set! angle-adjustment-total-1 0)
        (set! last-angle-value-1 (angle (tfs1 w-min)))

        ;compute points
        (compute-magnitude-points! w-min w-max tfs1 1)
        (compute-phase-points! w-min w-max tfs1 1)

        
        (bode-plot-parameters)
        
        (if (null? last-value)
            
            (begin (set! last-value total-tfs-value1)
                   (set! last-simplified-block-value (get-simplified-block-value block1))
                   
                   (for-each
                    displayln
                    
                    (list (parameterize ([plot-y-label      "Magnitude"]
                                         [plot-y-transform  log-transform]
                                         [plot-y-ticks      (log-ticks)]
                                         [plot-y-far-label "[dB]"]
                                         [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                                         (invertible-compose
                                                                          (linear-scale 8.68589) ;Neper to dB conversion factor
                                                                          (invertible-inverse (invertible-function exp log))))]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)])
                            
                            (plot (list ;(axes)
                                   (tick-grid)

                                   (lines magnitude-points-1 #:label "tf")
                                   
                                   (function (λ (x) 1) #:color 0 #:style 'dot)
                                   (function (λ (x) half-power-threshold) #:color 0 #:style 'dot))))
                          
                          
                          (parameterize ([plot-y-label      "Phase [deg]"]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)]
                                         [plot-y-far-label "[rad]"]
                                         [plot-y-far-ticks (ticks-scale (plot-y-ticks)
                                                                        (linear-scale (/ pi 180)))])
                            
                            (plot (list ;(axes)
                                   (tick-grid)
                                   
                                   (lines phase-points-1 #:label "tf")
                                   
                                   (function (λ (x) 0) #:color 0 #:style 'dot)
                                   (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
                          
                          (make-space-line 10))
                    ))
            
            
            (let* ((temp last-value)
                   (temp-simplified-block-value last-simplified-block-value)
                   (temp-value-evaluation (eval temp anchor)))
              
              (define (tfs-temp w) (temp-value-evaluation (make-rectangular 0 w) 
                                                          (fw1-func w) 
                                                          (fw2-func w)
                                                          (fw3-func w) 
                                                          (fw4-func w)))


              (compute-zeros-poles! temp-simplified-block-value 2)

              ;reset angle unwrapping
              (set! angle-adjustment-total-2 0)
              (set! last-angle-value-2 (angle (tfs-temp w-min)))
       
              ;compute points
              (compute-magnitude-points! w-min w-max tfs-temp 2)
              (compute-phase-points! w-min w-max tfs-temp 2)
              
              
              (set! last-value total-tfs-value1)
              
              (for-each
               displayln
               
               (list (parameterize ([plot-y-label      "Magnitude"]
                                    [plot-y-transform  log-transform]
                                    [plot-y-ticks      (log-ticks)]
                                    [plot-y-far-label "[dB]"]
                                    [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                                    (invertible-compose
                                                                     (linear-scale 8.68589) ;Neper to dB conversion factor
                                                                     (invertible-inverse (invertible-function exp log))))]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              
                              (lines magnitude-points-1 #:label "tf")
                              (lines magnitude-points-2 #:color 0 #:style 'dot #:label "previous tf")
                             
                              (function (λ (x) 1) #:color 0 #:style 'dot)
                              (function (λ (x) half-power-threshold) #:color 0 #:style 'dot))
                             
                             ))
                     
                     (parameterize ([plot-y-label      "Phase [deg]"]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)]
                                    [plot-y-far-label "[rad]"]
                                    [plot-y-far-ticks (ticks-scale (plot-y-ticks)
                                                                   (linear-scale (/ pi 180)))])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              
                              (lines phase-points-1 #:label "tf")
                              (lines phase-points-2 #:color 0 #:style 'dot #:label "previous tf")
                              
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
  
  ;(set-logger-mode! 'nil)
  
  ;change:
  ;(let ((simplification-result (simplify block)))
  ;(display simplification-result)
  
  ;change:
  ;(if (void? simplification-result)
  
  ;change:
  ;(newline)
  
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
                   
                   (define tfs-value-evaluation1
                     (eval (list (cons 'λ (cons '(y) (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (caddr total-tfs-value))))))) m1) anchor))
                   
                   (display "y = ")
                   (display (round-decimal m1 3)) 
                   (newline)
                   (newline)


                   ;substitute the y value aproximated, to compute the zeros & poles:
                   
                   ;(display (get-simplified-block-value block))
                   (let* ((tf (get-simplified-block-value block))
                          (a (get-numer tf)) ;'(poly-dense s 5 8)
                          (b (get-denom tf))
                          (numer-term-list (cdr (cdr a))) ;'(5 8)
                          (denom-term-list (cdr (cdr b)))
                          (numer-term-list-substituted (map (λ(x) (if (eq? x 'y) m1 x)) numer-term-list))
                          (denom-term-list-substituted (map (λ(x) (if (eq? x 'y) m1 x)) denom-term-list))
                          (get-simplified-block-value-substituted (list 'ratio
                                                                        (append (list 'poly-dense 's) numer-term-list-substituted)
                                                                        (append (list 'poly-dense 's) denom-term-list-substituted))))

                     ;(display get-simplified-block-value-substituted)
                     (compute-zeros-poles! get-simplified-block-value-substituted 1)
                   
                     ;reset angle unwrapping
                     (set! angle-adjustment-total-1 0)
                     (set! last-angle-value-1 0)
  

                     (bode-plot-parameters)
                   
                     (for-each
                      displayln
                    
                      (list 
                     
                       (parameterize ([plot-y-label      "Magnitude"]
                                      [plot-y-transform  log-transform]
                                      [plot-y-ticks      (log-ticks)]
                                      [plot-y-far-label "[dB]"]
                                      [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                                      (invertible-compose
                                                                       (linear-scale 8.68589) ;Neper to dB conversion factor
                                                                       (invertible-inverse (invertible-function exp log))))]
                                      [plot-x-transform  log-transform]
                                      [plot-x-ticks      (log-ticks)])
                       
                         (plot (list ;(axes)
                                (tick-grid)
                                (point-label (vector (* 1.0 w) (* 1.0 target)))

                                (lines (compute-magnitude-points!
                                        w-min w-max (λ(w)(tfs-value-evaluation1 (make-rectangular 0 w) 
                                                                                (fw1-func w) 
                                                                                (fw2-func w)
                                                                                (fw3-func w)
                                                                                (fw4-func w))) 1))
                              
                                (function (λ (x) 1) #:color 0 #:style 'dot)
                                (function (λ (x) half-power-threshold) #:color 0 #:style 'dot))))
                     
                     
                       (parameterize ([plot-y-label      "Phase [deg]"]
                                      [plot-x-transform  log-transform]
                                      [plot-x-ticks      (log-ticks)]
                                      [plot-y-far-label "[rad]"]
                                      [plot-y-far-ticks (ticks-scale (plot-y-ticks)
                                                                     (linear-scale (/ pi 180)))])
                       
                         (plot (list ;(axes)
                                (tick-grid)

                                (lines (compute-phase-points!
                                        w-min w-max (λ(w)(tfs-value-evaluation1 (make-rectangular 0 w) 
                                                                                (fw1-func w) 
                                                                                (fw2-func w)
                                                                                (fw3-func w)
                                                                                (fw4-func w))) 1))
                              
                                (function (λ (x) 0) #:color 0 #:style 'dot)
                                (function (λ (w) (- 180)) w-min w-max  #:color 3 #:style 'dot))))
                     
                     
                       (make-space-line 10))
                    
                      )))
                  
                  
                  #|
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
                              
                              
                              
                              (bode-plot-parameters)
                              
                              (for-each
                               displayln
                               
                               (list 
                                
                                (parameterize ([plot-y-label      "Magnitude"]
                                               [plot-y-transform  log-transform]
                                               [plot-y-ticks      (log-ticks)]
                                               [plot-y-far-label "[dB]"]
                                               [plot-y-far-ticks  (ticks-scale (plot-y-ticks)
                                                                               (invertible-compose
                                                                                (linear-scale 8.68589) ;Neper to dB conversion factor
                                                                                (invertible-inverse (invertible-function exp log))))]
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
                                         (function (λ (x) half-power-threshold) #:color 0 #:style 'dot))))
                                
                                
                                (parameterize ([plot-y-label      "Phase [deg]"]
                                               [plot-x-transform  log-transform]
                                               [plot-x-ticks      (log-ticks)]
                                               [plot-y-far-label "[rad]"]
                                               [plot-y-far-ticks (ticks-scale (plot-y-ticks)
                                                                              (linear-scale (/ pi 180)))])
                                  
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
                  |#
                  
                  (else (error "Unrecognized condition"))))
          
          (error "Unrecognized condition"))
      
      (error "Unrecognized condition"))
  
  ;)
  )






;///// Nyquist plot

(define (nyquist block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is possible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))

    
    (compute-zeros-poles! (get-simplified-block-value block) 1)
    
    
    (let* ((magnitude-at-w-min (magnitude (tfs w-min)))
           (magnitude-at-w-max (magnitude (tfs w-max)))
           (magnitude-at-0005 (magnitude (tfs 0.005)))
           (magnitude-at-900 (magnitude (tfs 900)))
           
           
           ;bandwidth parameters computation
           (bandwidth-threshold (min half-power-threshold chebyshev-threshold))
           
           (bandwidth-function (λ (w) (- (magnitude (tfs w)) bandwidth-threshold)))

           (w-cutoff-root-intervals (find-curve-root-intervals
                                     (map (λ(x) (list (car x) (- (cadr x) bandwidth-threshold))) magnitude-points-1)))

           (w-cutoff-roots (map (λ(interval) (half-interval-method bandwidth-function (car interval) (cadr interval)))
                                w-cutoff-root-intervals))


           ;[SHOULD BE IMPROVED]
           
           #|           
           ;gain margin parameters computation

           (f-gain-margin (λ (w) (- (let ((f1 (angle (tfs w))))
                                      (unwrap-angle-simple! f1 w 1))
                                    (- pi))))
           
           (w-gain-margin (half-interval-method f-gain-margin w-min w-max))

           (gain-margin (if (not (or (eq? w-gain-margin #f) (eq? w-gain-margin 0)))
                            (/ 1 (magnitude (tfs w-gain-margin)))
                            #f))
           
           
           ;phase margin parameters computation

           (f-phase-margin (λ (w) (- (magnitude (tfs w)) 1)))
           
           (w-phase-margin (half-interval-method f-phase-margin w-min w-max))
           
           (f1 (if (not (eq? w-phase-margin #f))
                   (angle (tfs w-phase-margin)) #f))
           
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
           |#
           )
      
      
      (set-chebyshev-threshold! 1000)
      
      
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
                       [plot-y-transform  (stretch-transform -1.5 1.5 20)]
                       [plot-y-far-tick-labels? #t])
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
                 )))
        
        
        
        
        (make-space-line 10)))
      
            
      ;filter type computation & text display
      (compute-filter-type! min-magnitude-1 max-magnitude-1 magnitude-at-w-min magnitude-at-w-max bandwidth-threshold)
      
      ;bandwidth computation & text display
      (compute-bandwidth w-cutoff-roots magnitude-points-1 bandwidth-function bandwidth-threshold half-power-threshold w-min)
      
      ;roll-off computation & text display
      (compute-roll-off magnitude-at-w-min magnitude-at-w-max magnitude-at-0005 magnitude-at-900 w-min w-max)
           
      ;gain & phase margins text display
      ;(display-gain-phase-margins gain-margin w-gain-margin phase-margin w-phase-margin)     
      
      ))
  )




