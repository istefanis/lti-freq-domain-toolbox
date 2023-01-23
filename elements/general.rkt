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

(require "../auxiliary/display_modes.rkt")
(provide (all-defined-out))







; //////////   B0. Selectors & constructors, "contracts", and some general procedures   //////////


; the elements of circuits (tfs, adders and blocks) are implemented as objects 
; using the message-passing style



; For all elements:

(define (get-value element) (element 'get-value))
(define (set-value! element new-value) ((element 'set-value!) new-value))

(define (block? element) (element 'is-block?))
(define (tf? element) (element 'is-tf?))
(define (adder? element) (element 'is-adder?))
(define (element? element) (or (block? element)
                               (tf? element)
                               (adder? element)))

(define (has-adder-input? element) 
  (if (and (not adder?) (element 'has-input?)) (adder? (get-input element)) 'ok))

(define (has-input? element) (element 'has-input?))
(define (get-input element) (element 'get-input))
(define (set-input! element) (element 'set-input!))
(define (print-input element) (element 'print-input))

(define (has-outputs? element) (element 'has-outputs?))
(define (single-output? element) (element 'single-output?))
(define (get-outputs element) (element 'get-outputs))
(define (add-output! element) (element 'add-output!))
(define (remove-output! element) (element 'remove-output!))
(define (print-outputs element) (element 'print-outputs))



; For blocks:

(define (element-of-tfs? block) (block 'element-of-tfs?))
(define (element-of-adders? block) (block 'element-of-adders?))

(define (is-simplified? block) (block 'is-simplified?))
(define (simplify block) (block 'simplify))

(define (adjoin-tfs! block) (block 'adjoin-tfs!))
(define (adjoin-adders! block) (block 'adjoin-adders!))

(define (remove-from-tfs! block) (block 'remove-from-tfs!))
(define (remove-from-adders! block) (block 'remove-from-adders!))

(define (get-tfs block) (block 'get-tfs))
(define (get-adders block) (block 'get-adders))
(define (get-blocks block) (block 'get-blocks))

(define (get-simplified-block-value block)
  (if (is-simplified? block)
      (begin ;(display "already simplified")
        (block 'get-value))
      (begin (simplify block)
             (if (is-simplified? block)
                 (block 'get-value)
                 (begin (log-messages (list "[CP-99] Block not fully simplified - GET-BLOCK-VALUE") 'checkpoints)
                        (block 'get-value))))))

(define (re-initialize-block! block) (block 're-initialize!))



; For adders:

(define (remove-input! adder) (adder 'remove-input!))
(define (single-input? adder) (adder 'single-input?))
(define (two-outputs? adder) (adder 'two-outputs?))







; //////////   C. Connection of elements  //////////


; connect-serially:

(define (connect-serially element1 element2)
  ((set-input! element2) element1)
  ((add-output! element1) element2))


(define connect connect-serially)





