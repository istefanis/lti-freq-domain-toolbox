#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#







#lang racket

(provide (all-defined-out))







; //////////   A2. Display logger modes   //////////


; logger-mode adjusts the volume of information printed, in terms of the following levels:
;
; 'nil, 'results, 'simplifications, 'tests
;
;       more information printed -->



(define logger-mode 'nil)

(define (set-logger-mode! mode)
  (set! logger-mode mode))



(define (log-messages proc attr)
  (cond ((eq? logger-mode 'nil) (display ""))
        ((eq? attr 'results)
         (cond ((or (eq? logger-mode 'tests)
                    (eq? logger-mode 'simplifications)
                    (eq? logger-mode 'results))
                (begin (newline)
                       (newline)
                       (proc)
                       (display (make-string 52 #\=))))))
        ((eq? attr 'simplifications)
         (cond ((or (eq? logger-mode 'tests)
                    (eq? logger-mode 'simplifications))
                (begin (newline)
                       (newline)
                       (proc)
                       (display (make-string 52 #\-))))))
        ((eq? attr 'tests)
         (cond ((eq? logger-mode 'tests)
                (begin (newline)
                       (newline)
                       (proc)
                       (display (make-string 52 #\-))))))
        (else (error "Unrecognized display logger mode - LOG-MESSAGES" logger-mode))))


(define (log-single-message m attr)
  (log-messages (lambda () 
                    (display m)
                    (newline)
                    (newline))
                  attr))


#|
(define (log-cached-simplification-message)
  (newline)
  (display "- cached simplification")
  (newline)
  (newline))
|#
