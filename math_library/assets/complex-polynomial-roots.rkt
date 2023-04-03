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

(provide find-roots)







; //////////  Math library assets  //////////



; COMPLEX UNIVARIATE POLYNOMIAL ROOTS FINDING METHOD


; Weierstrass / Durand-Kerner method

#|
A porting from JavaScript to Racket of the 'durand-kerner' (https://github.com/scijs/durand-kerner) library
and its 'next-pow-2' (https://github.com/mikolalysenko/next-pow-2) dependency,
for the numerical computation of the complex roots of a univariate polynomial using the
Weierstrass / Durand-Kerner method (https://en.wikipedia.org/wiki/Durand%E2%80%93Kerner_method).

Assumptions & limitations:
- The original JavaScript version has been assumed a correct implementation and ported as is
- General convergence for any polynomial and initial guess of the roots is not guaranteed
  For practical cases however, the method (and this particular implementation of it) is often successful & fast

The original 'durand-kerner' & 'next-pow-2' libraries ported, are licensed under the MIT License,
and their licences are included in respective 'math_library\assets' subdirectories.
|#



(define epsilon 1e-8)

(define pr (build-list 1024 (λ(x) 0)))
(define pim (build-list 1024 (λ(x) 0)))

(define (solve n n-iters tolerance zr zi) 
  (let ((m (length zr))
        (a 0) (b 0)
        (na 0) (nb 0)
        (pa 0) (pb 0)
        (qa 0) (qb 0)
        (k1 0) (k2 0) (k3 0)
        (s1 0) (s2 0)
        (d 1))

    ;consecutive updates of approximations for all roots, until
    ;max iterations number or convergence is reached 
    (define (loop1! i)
      (cond ((and (< i n-iters) (>= d tolerance))
             (set! d 0)

             ;single update of approximations for all roots
             (define (loop2! j)
               (cond ((< j m)
                      ;read root zj
                      (set! pa (list-ref zr j))
                      (set! pb (list-ref zi j))

                      ;compute denominator for root zj,
                      ;using the other root values
                      ;(zj - z0) * (zj - z1) * ... * (zj - z(n-1))
                      (set! a 1)
                      (set! b 0)
                      
                      (define (loop3! k)
                        (cond ((< k m)
                               (if (eq? k j)
                                   (loop3! (+ k 1))
                                   (begin 
                                     (set! qa (- pa (list-ref zr k)))
                                     (set! qb (- pb (list-ref zi k)))
                                     (if (< (+ (* qa qa) (* qb qb)) tolerance) 
                                         (loop3! (+ k 1)) 
                                         (begin 
                                           (set! k1 (* qa (+ a b)))
                                           (set! k2 (* a (- qb qa)))
                                           (set! k3 (* b (+ qa qb)))
                                           (set! a (- k1 k3))
                                           (set! b (+ k1 k2))
                                           (loop3! (+ k 1)))))))))
                      (loop3! 0)
                      
                      ;compute numerator
                      (set! na (list-ref pr (- n 1)))
                      (set! nb (list-ref pim (- n 1)))
                      (set! s1 (- pb pa))
                      (set! s2 (+ pa pb))
                      
                      (define (loop4! k)
                        (cond ((>= k 0)
                               (set! k1 (* pa (+ na nb)))
                               (set! k2 (* na s1))
                               (set! k3 (* nb s2))
                               (set! na (+ k1 (- k3) (list-ref pr k)))
                               (set! nb (+ k1 k2 (list-ref pim k)))
                               (loop4! (- k 1)))))
                      
                      (loop4! (- n 2))

      
                      ;compute reciprocal
                      (set! k1 (+ (* a a) (* b b)))
                      (if (> (abs k1) epsilon)
                          (begin (set! a (/ a k1))
                                 (set! b (/ b (- k1))))
                          (begin (set! a 1)
                                 (set! b 0)))
                      
                      ;multiply and accumulate
                      (set! k1 (* na (+ a b)))
                      (set! k2 (* a (- nb na)))
                      (set! k3 (* b (+ na nb)))

                      (set! qa (- k1 k3))
                      (set! qb (+ k1 k2))

                      (set! zr (list-set zr j (- pa qa)))
                      (set! zi (list-set zi j (- pb qb)))
      
                      (set! d (max d (max (abs qa) (abs qb))))
                      
                      (loop2! (+ j 1)))))
             (loop2! 0)
             
             (loop1! (+ i 1)))))
    (loop1! 0)

    ;post process: combine any repeated roots
    (define counter 0)

    (define (loop5! i)
      (cond ((< i m)
             (set! counter 1)
             (set! a (list-ref zr i))
             (set! b (list-ref zi i))
    
             (define (loop6! j)
               (cond ((< j m)
                      (if (eq? i j)
                          (loop6! (+ j 1))
                          (begin 
                            (cond ((near (list-ref zr i) (list-ref zi i)
                                         (list-ref zr j) (list-ref zi j) tolerance)
                                   (set! counter (+ counter 1))
                                   (set! a (+ a (list-ref zr j)))
                                   (set! b (+ b (list-ref zi j)))))
                            (loop6! (+ j 1)))))))
             
             (loop6! 0)
             
             (cond ((> counter 1) 
                    (set! a (/ a counter))
                    (set! b (/ b counter))

                    (define (loop7! j)
                      (cond ((< j m)
                             (if (eq? i j)
                                 (loop7! (+ j 1))
                                 (begin
                                   (cond ((near (list-ref zr i) (list-ref zi i)
                                                (list-ref zr j) (list-ref zi j) tolerance)
                                          (set! zr (list-set zr j a))
                                          (set! zi (list-set zi j b))))
                                   (loop7! (+ j 1)))))))
                    
                    (loop7! 0)
                    
                    (set! zr (list-set zr i a))
                    (set! zi (list-set zi i b))))

             (loop5! (+ i 1)))))
    (loop5! 0)

    (map (λ(r i) (make-rectangular r i)) zr zi) 
    ;(list zr zi)
    ))

(define (bound n)
  (define (loop i b)
    (if (< i n)
        (loop (+ i 1) (max b (+ (* (list-ref pr i) (list-ref pr i))
                                (* (list-ref pim i) (list-ref pim i)))))
        b))
  (+ 1 (sqrt (loop 0 0))))

(define (find-roots r-coeff i-coeff n-iters tolerance zr zi)

  ;addition: drop any trailing zeros from r-coeff
  (define (drop-r-coeff-trailing-zero!)
    (cond ((and r-coeff
                (eq? (last r-coeff) 0)
                (> (if r-coeff (length r-coeff) 0) (if i-coeff (length i-coeff) 0)))
           (set! r-coeff (drop-right r-coeff 1))
           (drop-r-coeff-trailing-zero!))))
  (drop-r-coeff-trailing-zero!)
  
  (let ((n (if r-coeff (length r-coeff) 0)))

    ;addition: append zeros to i-coeff if its length is smaller than r-coeff
    (let ((ni (if i-coeff (length i-coeff) 0)))
      (cond ((> n ni)
             (set! i-coeff
                   (append (if i-coeff i-coeff '())
                           (build-list (- n ni) (λ(x) 0)))))))
    
    (if (<= n 1)
        '()
        (begin
          (cond ((< (length pr) n)
                 (let ((nl (next-power-of-2 n)))
                   (set! pr (build-list nl (λ(x) 0)))
                   (set! pim (build-list nl (λ(x) 0))))))

          (set! pr (append (take r-coeff n) (list-tail pr n)))
          (cond (i-coeff
                 (set! pim (append (take i-coeff n) (list-tail pim n)))))

          ;rescale coefficients
          (let* ((a (list-ref pr (- n 1)))
                 (b (list-ref pim (- n 1)))
                 (d (+ (* a a) (* b b))))
            (set! a (/ a d))
            (set! b (/ b (- d)))
  
            (let ((k1 0)
                  (k2 0)
                  (k3 0)
                  (s (- b a))
                  (t (+ a b)))

              (define (loop! i)
                (cond ((< i (- n 1))
                       (set! k1 (* a (+ (list-ref pr i) (list-ref pim i))))
                       (set! k2 (* (list-ref pr i) s))
                       (set! k3 (* (list-ref pim i) t))
                       (set! pr (list-set pr i (- k1 k3)))
                       (set! pim (list-set pim i (+ k1 k2)))
                       (loop! (+ i 1)))))
                    
              (loop! 0))

            (set! pr (list-set pr (- n 1) 1))
            (set! pim (list-set pim (- n 1) 0))

            ;set default n-iters & tolerance if unspecified
            (cond ((not n-iters)
                   (set! n-iters (* 100 n))))
            (cond ((not tolerance)
                   (set! tolerance 1e-6)))
             
            ;pick default initial guess if unspecified
            (cond ((not zr)
                   (set! zr (build-list (- n 1) (λ(x) 0)))
                   (set! zi (build-list (- n 1) (λ(x) 0)))
                   (let ((r (bound n)))
                     
                     (set! zr (map (λ(x)
                                     (let ((t (* (random) r))
                                           (c (cos(* (random) 2 pi))))
                                       (* t c)))
                                   zr)) 

                     (set! zi (map (λ(x)
                                     (let ((t (* (random) r))
                                           (c (cos(* (random) 2 pi))))
                                       (* t (sqrt (- 1 (* c c))))))
                                   zi))
                     ))
                  ((not zi)
                   (set! zi (build-list (length zr) (λ(x) 0)))))

            #|
            (displayln n)
            (displayln n-iters)
            (displayln tolerance)
            (displayln zr)
            (displayln zi)            
            |#
            (solve n n-iters tolerance zr zi)
            )))))


;Utils

(define (near a b c d tolerance)
  (let* ((qa (- a c))
         (qb (- b d))
         (r  (+ (* qa qa) (* qb qb))))
    
    (if (< (* r r) tolerance) #t #f)))

;compute the first number that is >= n & is a power of 2
(define (next-power-of-2 v)
  (cond ((eq? v 0)
         (set! v (+ v 1))))
  (set! v (- v 1))
  (cond ((> (arithmetic-shift v -1) 0)
         (set! v (bitwise-ior v (arithmetic-shift v -1)))))
  (cond ((> (arithmetic-shift v -2) 0)
         (set! v (bitwise-ior v (arithmetic-shift v -2)))))
  (cond ((> (arithmetic-shift v -4) 0)
         (set! v (bitwise-ior v (arithmetic-shift v -4)))))
  (cond ((> (arithmetic-shift v -8) 0)
         (set! v (bitwise-ior v (arithmetic-shift v -8)))))
  (cond ((> (arithmetic-shift v -16) 0)
         (set! v (bitwise-ior v (arithmetic-shift v -16)))))
  (+ v 1)
  )
;(for-each (λ(x) (displayln (next-power-of-2 x))) (build-list 20 values)) 


;Tests
#|
(find-roots '(1) #f #f #f #f #f)
(find-roots '(1 0) #f #f #f #f #f)
(find-roots #f '(1 0) #f #f #f #f)
(find-roots '(0 1) #f #f #f #f #f)

(find-roots '(1 2) #f #f #f #f #f)
(find-roots '(1 3 5) #f #f #f #f #f)
(find-roots '(1 -3 0 2) #f #f #f #f #f)

(find-roots '(1 4) '(1 2) #f #f #f #f)
(find-roots '(1 7) '(1 3 5) #f #f #f #f)
(find-roots '(1 5) '(1 -3 0 2) #f #f #f #f)

(find-roots '(1 5 4) '(1 2) #f #f #f #f)
(find-roots '(7 8 0) '(1 3 5) #f #f #f #f)
(find-roots '(-10 0 3 4 5) '(1 -3 2 0 2) #f #f #f #f)
|#

