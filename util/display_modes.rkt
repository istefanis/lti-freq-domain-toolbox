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

(provide (all-defined-out))







; //////////   A2. Display logger modes   //////////


; logger-mode adjusts the volume of information printed, in terms of the following levels:
;
; 'nil, 'algorithms (AL), 'simplifications (SF), 'checkpoints (CP)
;
;       more information printed -->


(define logger-mode 'nil)

(define (set-logger-mode! mode)
  (set! logger-mode mode))




(define display-width 52)
(pretty-print-columns display-width)




(define (log-messages lst attr)

  (define (print-messages l c)
    (newline)
    (newline)
    (for-each (lambda (x) (pretty-display x) (newline))       ; pretty display 
              ;(lambda (x) (displayln x) (newline))   ; regular display
              l)
    (display (make-string display-width c)))

  
  (cond ((eq? logger-mode 'nil) (display ""))
        
        ((eq? attr 'algorithms)
         (cond ((or (eq? logger-mode 'checkpoints)
                    (eq? logger-mode 'simplifications)
                    (eq? logger-mode 'algorithms))
                (print-messages lst #\=))))
        
        ((eq? attr 'simplifications)
         (cond ((or (eq? logger-mode 'checkpoints)
                    (eq? logger-mode 'simplifications))
                (print-messages lst #\-))))
        
        ((eq? attr 'checkpoints)
         (cond ((eq? logger-mode 'checkpoints)
                (print-messages lst #\-))))
        
        (else (error "Unrecognized display logger mode - LOG-MESSAGES" logger-mode))))



#|
(define (log-cached-simplification-message)
  (newline)
  (displayln "- cached simplification")
  (newline))
|#
