#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#






#lang racket

(require "general.rkt")
(provide (all-defined-out))








; //////////   B1. Adder element representation   //////////



; the elements of circuits (tfs, adders and blocks) are implemented as objects 
; using the message-passing style





; /////////  adder  /////////

; implementation assumption:
; each adder has multiple inputs and multiple outputs


(define (make-adder block)
  (let ((i-am-block #f)
        (value 'adder)
        (i-am-adder #t)
        (inputs-list '())     ; '((input1 sign1) (input2 sign2) ...)
        (outputs-list '()))
    
    
    (define (process-add-input i)
      (set! inputs-list (cons i inputs-list)))
    
    
    (define (process-remove-input i)    
      (define (remove-input inputs)
        (if (null? inputs)
            '()
            (if (eq? (car inputs) i)
                (cdr inputs)
                (cons (car inputs) (remove-input (cdr inputs)))))) 
      (set! inputs-list (remove-input inputs-list)))
    
    
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
            ((eq? request 'is-adder?) i-am-adder)
            
            ((eq? request 'has-input?) (not (null? inputs-list)))
            ((eq? request 'single-input?) (eq? (length inputs-list) 1))
            ((eq? request 'get-input) inputs-list)
            ((eq? request 'set-input!) (lambda (x) (process-add-input x)))
            ((eq? request 'remove-input!) (lambda (x) (process-remove-input x)))
            ((eq? request 'print-input) (display inputs-list))
            
            ((eq? request 'has-outputs?) (not (null? outputs-list)))
            ((eq? request 'single-output?) (eq? (length outputs-list) 1))
            ((eq? request 'two-outputs?) (eq? (length outputs-list) 2))
            ((eq? request 'get-outputs) outputs-list)  
            ((eq? request 'add-output!) (lambda (x) (process-add-output x)))
            ((eq? request 'remove-output!) (lambda (x) (process-remove-output x)))
            ((eq? request 'print-outputs) (display outputs-list))
            
            ((eq? request 'is-block?) i-am-block)
            
            (else (error "Unknown request - MAKE-ADDER" request))))
    
    
    ((adjoin-adders! block) me)
    
    me))

(define (adder block)
  (make-adder block))


