#|
lti-freq-domain-toolbox | Functions for studying LTI (linear time-invariant) dynamical systems 
Copyright (C) 2014-2022  Ioannis Stefanis

This file is part of lti-freq-domain-toolbox.

lti-freq-domain-toolbox is free software: you can redistribute it and/or modify it under the terms of 
the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of 
the License, or (at your option) any later version.

lti-freq-domain-toolbox is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; 
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along with lti-freq-domain-toolbox. 
If not, see <https://www.gnu.org/licenses/>.
|#







#lang racket

(provide (all-defined-out))







; //////////   A1. Metrics   //////////


; variables used as counters, so as to have some metrics about the number of simplifications done:

(define simplifications-done 0)
(define algorithms-run 0)
(define algorithms-run-since-last-simplification 0)

(define (update-simplifications-counters!)
  (set! simplifications-done (+ simplifications-done 1))
  (set! algorithms-run-since-last-simplification 0))

(define (update-algorithms-run-counters!)
  (set! algorithms-run (+ algorithms-run 1))
  (set! algorithms-run-since-last-simplification 
        (+ algorithms-run-since-last-simplification 1)))

(define (zero-counters!)
  (set! simplifications-done 0)
  (set! algorithms-run 0)
  (set! algorithms-run-since-last-simplification 0))



(define (update-and-run! function arg)
  (update-algorithms-run-counters!)
  (if (eq? arg 'no-arg)
      (function)
      (function arg)))




