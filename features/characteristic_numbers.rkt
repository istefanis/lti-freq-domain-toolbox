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
(provide (all-defined-out))








; //////////   I. Characteristic numbers computation & text display functions  //////////


(define all-pass-filter #f)
(define no-pass-filter #f)
(define band-pass-filter #f)
(define band-stop-filter #f)
(define low-pass-filter #f)
(define high-pass-filter #f)


(define (compute-filter-type! min-magnitude max-magnitude magnitude-at-w-min magnitude-at-w-max bandwidth-threshold)

  (set! all-pass-filter #f)
  (set! no-pass-filter #f)
  (set! band-pass-filter #f)
  (set! band-stop-filter #f)
  (set! low-pass-filter #f)
  (set! high-pass-filter #f)
  
  (cond ((> min-magnitude bandwidth-threshold)
         (display "All-pass filter")
         (set! all-pass-filter #t)
         (newline)
         (newline))
        ((< max-magnitude bandwidth-threshold)
         (display "No-pass filter")
         (set! no-pass-filter #t)
         (newline)
         (newline))
        ((and (< magnitude-at-w-min bandwidth-threshold)
              (< magnitude-at-w-max bandwidth-threshold)
              (> max-magnitude bandwidth-threshold))
         (display "Band-pass filter")
         (set! band-pass-filter #t)
         (newline)
         (newline))
        ((and (> magnitude-at-w-min bandwidth-threshold)
              (> magnitude-at-w-max bandwidth-threshold)
              (< min-magnitude bandwidth-threshold))
         (display "Band-stop filter")
         (set! band-stop-filter #t)
         (newline)
         (newline))
        ((and (> magnitude-at-w-min magnitude-at-w-max)
              (> magnitude-at-w-min bandwidth-threshold)
              (< magnitude-at-w-max (* 3 bandwidth-threshold))
              (> (/ magnitude-at-w-min magnitude-at-w-max) 1.5))
         (display "Low-pass filter")
         (set! low-pass-filter #t)
         (newline)
         (newline))    
        ((and (> magnitude-at-w-max magnitude-at-w-min)
              (> magnitude-at-w-max bandwidth-threshold)
              (< magnitude-at-w-min (* 3 bandwidth-threshold))
              (> (/ magnitude-at-w-max magnitude-at-w-min) 1.5))
         (display "High-pass filter")
         (set! high-pass-filter #t)
         (newline)
         (newline))
        ))





(define (compute-bandwidth w-cutoff-roots magnitude-points bandwidth-function bandwidth-threshold half-power-threshold w-min)

  (let ((bandwidth-text ""))

    (if (eq? (length w-cutoff-roots) 0)
        (set! bandwidth-text (if (> (bandwidth-function 1) 0)
                                 "(0, ∞) [rad/s]"
                                 "N/A"))
        (let ((bandwidth-text-prefix "")
              (bandwidth-text-infix (λ(i) (if (odd? i)
                                              "] ∪ ["
                                              ", ")))
              (bandwidth-text-suffix "")
              (z 0)
              (i -1))

          (if (> (bandwidth-function w-min) 0)
              (begin (set! bandwidth-text-prefix "(0, ")
                     (set! bandwidth-text-suffix (if (odd? (length w-cutoff-roots))
                                                     "] [rad/s]"
                                                     ", ∞) [rad/s]"))
                     (set! z (+ z 1)))
              (begin (set! bandwidth-text-prefix "[")
                     (set! bandwidth-text-suffix (if (odd? (length w-cutoff-roots))
                                                     ", ∞) [rad/s]"
                                                     "] [rad/s]"))))
          
          (set! bandwidth-text (string-append
                                bandwidth-text-prefix 
                                (string-join (map 
                                              (λ(x)
                                                (set! i (+ i 1))
                                                (string-append (number->string (round-decimal x 3))
                                                               (if (not (eq? i (- (length w-cutoff-roots) 1)))
                                                                   (bandwidth-text-infix (+ i z))
                                                                   "")))
                                              w-cutoff-roots)
                                             "")
                                bandwidth-text-suffix))
          ))
    
    (display "bandwidth    = ")(displayln bandwidth-text)

    (display "threshold    = ")
    (let ((bandwidth-threshold-rounded (round-decimal bandwidth-threshold 3)))   
      (if (= bandwidth-threshold-rounded half-power-threshold)
          (begin (display bandwidth-threshold-rounded) (display " = -3 [dB]"))
          (display bandwidth-threshold-rounded)))
    (newline)
    ))





(define (compute-roll-off magnitude-at-w-min magnitude-at-w-max magnitude-at-0005 magnitude-at-900 w-min w-max) 
  (cond ((or all-pass-filter no-pass-filter band-pass-filter
             band-stop-filter low-pass-filter high-pass-filter)
         (let* ((roll-off-low (round-decimal
                               (/ (log (/ magnitude-at-w-min magnitude-at-0005)) (log (/ w-min 0.005))) ;log(AR2/AR1)/log(w2/w1))
                               3))
                (roll-off-high (round-decimal
                                (/ (log (/ magnitude-at-900 magnitude-at-w-max)) (log (/ 900 w-max)))
                                3))
                (roll-off-text (string-append (if (eq? roll-off-low 0)
                                                  "0 (low)"
                                                  (string-append (number->string (round-decimal (* 20 roll-off-low) 3))
                                                                 " [dB/dec] (low)"))
                                              ", "
                                              (if (eq? roll-off-high 0)
                                                  "0 (high)"
                                                  (string-append (number->string (round-decimal (* 20 roll-off-high) 3))
                                                                 " [dB/dec] (high)")))))         
           (display "roll-off     = ")
           (displayln roll-off-text)))))





;[SHOULD BE IMPROVED]
             
#|
(define (display-gain-phase-margins gain-margin w-gain-margin phase-margin w-phase-margin)

  (if (not (eq? gain-margin #f))
      (begin (display "gain margin  = ")
             (display (round-decimal gain-margin 2))
             (display " at ")
             (display (round-decimal w-gain-margin 2))
             (display " [rad/s]")
             (newline))
      (begin (display "gain margin  = ∞")
             (newline)))
  
  (if (not (eq? phase-margin #f))
      (begin (display "phase margin = ")
             (display (round-decimal phase-margin 2))
             (display " [deg] at ")
             (display (round-decimal w-phase-margin 2))
             (display " [rad/s]")
             (newline)
             (newline))
      (begin (display "phase margin = ∞")
             (newline)
             (newline)))
  )
|#




