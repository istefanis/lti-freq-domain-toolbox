#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#







#lang racket

(provide (all-defined-out))







; //////////   A2. Display preferences   //////////


; display-mode adjusts the volume of information printed, in terms of the following levels:

; 'test, 'simplifications, 'results, 'nil
;      <-- more information printed

(define display-mode 'nil) ; the functions "bode" etc. automatically set it to nil

(define (handle-display proc attr)
  (cond ((eq? display-mode 'test) (proc))
        ((eq? display-mode 'simplifications) (when (or (eq? attr 'simplifications)
                                                       (eq? attr 'results)) (proc)))
        ((eq? display-mode 'results) (when (eq? attr 'results) (proc)))
        ((eq? display-mode 'nil) (display ""))
        (else (error "Unrecognized display-mode - HANDLE-DISPLAY" display-mode))))


(define (handle-single-display m attr)
  (handle-display (lambda () 
                    (display m)
                    (newline)
                    (newline))
                  attr))


(define (display-mode-nil!)
  (set! display-mode 'nil))


(define (display-cached-simplification-msg)
  (newline)
  (display "- cached simplification")
  (newline)
  (newline))



