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

(require "../math_library/symbolic_algebra.rkt")
(require "../util/display_modes.rkt")
(require "general.rkt")
(provide (all-defined-out))







; //////////   B1. Transfer function (tf) element representation   //////////



; the elements of circuits (tfs, adders and blocks) are implemented as objects 
; using the message-passing style




; /////////  tf  /////////

; implementation assumption:
; each tf has only one input and multiple outputs
;
; multiple inputs can be achieved by adding in front of it an adder


(define (make-tf value block)
  (let ((i-am-block #f)
        (i-am-tf #t)
        (i-am-adder #f)
        (input '())
        (outputs-list '()))
    
    
    (define (process-set-input i)
      (if (not (null? input))
          (log-messages (list "[CP-00] input substitution - tf") 'checkpoints)
          'do-nothing)
      (set! input i))
    
    
    (define (process-add-output o)
      (set! outputs-list (cons o outputs-list)))
    
    
    (define (process-remove-output o)
      (define (remove-output outputs)
        (if (null? outputs)
            '()
            (if (eq? (car outputs) o)
                (cdr outputs)
                (cons (car outputs) (remove-output (cdr outputs)))))) 
      (set! outputs-list (remove-output outputs-list)))
    
    
    (define (me request)
      (cond ((eq? request 'get-value) value)
            ((eq? request 'set-value!) (lambda (x) (set! value x)))

            ((eq? request 'is-block?) i-am-block)
            ((eq? request 'is-tf?) i-am-tf)
            ((eq? request 'is-adder?) i-am-adder)
            
            ((eq? request 'has-input?) (not (null? input)))
            ((eq? request 'get-input) input)
            ((eq? request 'set-input!) (lambda (x) (process-set-input x)))
            ((eq? request 'print-input) (display input))
            
            ((eq? request 'has-outputs?) (not (null? outputs-list)))
            ((eq? request 'single-output?) (eq? (length outputs-list) 1))
            ((eq? request 'get-outputs) outputs-list)            
            ((eq? request 'add-output!) (lambda (x) (process-add-output x)))
            ((eq? request 'remove-output!) (lambda (x) (process-remove-output x)))
            ((eq? request 'print-outputs) (display outputs-list))
            
            (else (error "Unknown request - MAKE-TF" request))))
    
    
    ((adjoin-tfs! block) me)
    
    me))


(define (tf n d block)
  (define tf1 (make-tf (make-ratio (make-poly-dense 's n)
                                   (make-poly-dense 's d))
                       block))
  tf1)





