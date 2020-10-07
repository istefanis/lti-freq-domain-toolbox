#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the "COPYING.LESSER" file.

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
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




