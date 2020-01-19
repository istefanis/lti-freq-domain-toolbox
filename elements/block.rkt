#|
lti-freq-domain-toolbox
Copyright (C) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#






#lang racket

(require "../math_library/symbolic_algebra.rkt")
(require "../functionality/metrics.rkt")
(require "../functionality/display_modes.rkt")
(require "general.rkt")
(require "tf.rkt")
(provide (all-defined-out))







; //////////   B1. Block of elements representation   //////////




; the elements of circuits (tfs, adders and blocks) are implemented as objects 
; using the message-passing style




; /////////  block  /////////


; the block of elements is a means for achieving abstraction:
; circuit elements such as tfs, adders and blocks can be "stored" inside a block 
; and form a whole that can be handled as an element itself

; implementation assumption:
; each block of elements has only one input (another tf, adder or block) and multiple outputs
;
; multiple inputs can be achieved by adding in front of it an adder


(define (make-block . block) ; block is the parent block - optional
  (let ((value '())   ; block obtains a value only when simplified
        (blocks '())  ; blocks is a list of all block elements of the block
        (tfs '())     ; tfs is a list of all tf elements of the block
        (adders '())  ; adders is a list of all adder elements of the block
        (i-am-block #t)
        (i-am-adder #f)
        (input '())
        (outputs-list '())
        (i-am-simplified #t))
    
    
    
    
    (define (process-set-input i)
      (if (not (null? input))
          (handle-display (lambda () (display "- input substitution") (newline))
                          'test)
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
    
    
    
    
    
    ; //// blocks, tfs and adders lists operators:
    
    
    
    ; blocks operators:
    
    (define (process-element-of-blocks? x) (element? x blocks))
    
    
    (define (process-adjoin-blocks! x)
      (if (process-element-of-blocks? x)
          'adjoin-ok
          (begin (set! i-am-simplified #f)
                 (set! blocks (cons x blocks)))))
    
    
    (define (process-remove-from-blocks! x)      
      ; set! treates its second argument explicitly
      (define (remove! x set)
        (cond ((null? set) '())
              ((and (equal? set blocks) (eq? x (car set))) (set! blocks (cdr set)))
              ((eq? x (car set)) (cdr set))
              (else (set! blocks (cons (car set) (remove x (cdr set)))))))
      
      (set! i-am-simplified #f)
      (remove! x blocks))
    
    
    
    
    ; tfs operators:
    
    (define (process-element-of-tfs? x) (element? x tfs))
    
    
    (define (process-adjoin-tfs! x)
      (if (process-element-of-tfs? x)
          'adjoin-ok
          (begin (set! i-am-simplified #f)
                 (set! tfs (cons x tfs)))))
    
    
    (define (process-remove-from-tfs! x)      
      ; set! treates its second argument explicitly
      (define (remove! x set)
        (cond ((null? set) '())
              ((and (equal? set tfs) (eq? x (car set))) (set! tfs (cdr set)))
              ((eq? x (car set)) (cdr set))
              (else (set! tfs (cons (car set) (remove x (cdr set)))))))
      
      (set! i-am-simplified #f)
      (remove! x tfs))
    
    
    
    
    ; adders operators:
    
    (define (process-element-of-adders? x) (element? x adders))
    
    
    (define (process-adjoin-adders! x)
      (if (process-element-of-adders? x)
          'adjoin-ok
          (begin (set! i-am-simplified #f)
                 (set! adders (cons x adders)))))
    
    
    (define (process-remove-from-adders! x) 
      (define (remove! x set)
        (cond ((null? set) '())
              ((and (equal? set adders) (eq? x (car set))) (set! adders (cdr set)))
              ((eq? x (car set)) (cdr set))
              (else (set! adders (cons (car set) (remove x (cdr set)))))))
      
      (set! i-am-simplified #f)
      (remove! x adders))
    
    
    
    
    
    ; //// simplify (the operation handling the simplification process):
    
    (define (simplify-the-block)
      
      
      (define (simplify-loop)
        (if (or (and (eq? (length (append tfs blocks)) 1) (null? adders)) ; full simplification
                (> algorithms-run-since-last-simplification 9)) ; partial simplification
            
            
            
            
            (begin (if (and (eq? (length (append tfs blocks)) 1) (null? adders))
                       
                       (handle-display (lambda () 
                                         (newline)
                                         (display "FULL"))
                                       'results)
                       
                       (handle-display (lambda () 
                                         (newline)
                                         (display "PARTIAL"))
                                       'results))
                   
                   (handle-display (lambda () 
                                     (display " SIMPLIFICATION COMPLETED")
                                     (newline)
                                     (newline)
                                     (display "simplifications done: ")
                                     (display simplifications-done)
                                     (newline)
                                     (display "algorithms run: ")
                                     (display algorithms-run)
                                     (newline)
                                     (newline)
                                     (newline)
                                     ;(display "adders:")
                                     ;(newline)
                                     ;(display adders)
                                     ;(newline)
                                     ;(newline)                                 
                                     ;(display "simplified tf:")
                                     ;(newline)
                                     )
                                   'results)
                   
                   
                   (set! i-am-simplified #t)
                   #|
                   (newline)
                   (newline)
                   (display tfs)
                   (newline)
                   (display blocks)
                   (newline)
                   (map (λ (x) 
                          (newline)
                          (display (get-value x))) tfs)
                   (map (λ (x) 
                          (newline)
                          (display (get-value x))) blocks)
                   |#
                   (set! value ((car (append tfs blocks)) 'get-value))
                   (zero-counters!))
            
            
            
            
            (begin (update-and-run! serial-merging-tfs (append tfs blocks))
                   (update-and-run! serial-merging-adders adders)
                   ;(display "problem2")
                   
                   (let ((elements-now (append tfs blocks)))
                     (update-and-run! feedback-loop-merging (append tfs blocks))
                     (update-and-run! remove-unused-adders 'no-arg)
                     ;(display "problem3")
                     
                     (if (and (not (equal? elements-now (append tfs blocks))) (not (null? (append tfs blocks))))
                         
                         (simplify-loop)
                         
                         (begin (update-and-run! create-single-output-tfs (append tfs blocks))
                                (update-and-run! parallel-merging-tfs adders)
                                ;(display "problem4")
                                
                                (if (and (not (equal? elements-now (append tfs blocks))) (not (null? (append tfs blocks))))
                                    
                                    (begin (update-and-run! feedback-loop-merging (append tfs blocks))
                                           (update-and-run! remove-unused-adders 'no-arg)
                                           ;(display "problem5")
                                           (simplify-loop))
                                    
                                    (begin (update-and-run! parallel-merging-tfs adders)
                                           (simplify-loop)))))))
            ))
      
      
      
      (map simplify blocks)
      (if (or (null? (append tfs blocks)) (and (eq? (length (append tfs blocks)) 1) (null? adders)))
          
          (begin (set! i-am-simplified #t)
                 (set! value ((car (append tfs blocks)) 'get-value))
                 ;(newline)
                 (handle-single-display " NO SIMPLIFICATIONS TO BE DONE" 'results))
          
          (begin ;(handle-display (lambda () 
            ;                  (newline)
            ;                  )
            ;                'results)
            ;(display "problem")
            ;(map simplify blocks)
            ;(newline)
            ;(update-and-run! remove-unconnected-tfs 'no-arg)
            (simplify-loop))))
    
    
    
    
    
    
    
    
    
    
    ; //////////   B.1. simpification algorithms   //////////
    
    
    ; //// remove-unused-adders
    
    (define (remove-unused-adders)
      (define (remove set so-far)
        (if (null? set)
            
            (begin (when (not (equal? (length adders) (length so-far)))
                     (begin (update-simplifications-counters!)
                            (set! adders so-far)
                            (handle-display (lambda () (display "adders now:")
                                              (newline)
                                              (display (map (lambda (x) (get-value x)) adders))
                                              (newline)
                                              (newline))
                                            'simplifications)))
                   (handle-single-display "DONE - UNUSED ADDERS REMOVED" 'results))
            
            (if (or (has-input? (car set))
                    (has-outputs? (car set)))
                
                ; test
                (cond ((single-input? (car set)) ; remove adder
                       (map (lambda (x) (connect-serially (car (get-input (car set))) x))
                            (get-outputs (car set)))
                       ((remove-output! (car (get-input (car set)))) (car set))
                       (map (lambda (x) (when (adder? x)
                                          ((remove-input! x) (car set)))) (get-outputs (car set)))
                       (remove (cdr set) so-far))
                      
                      ((and (eq? (length tfs) 1) (not (has-input? (car set)))) ; no inputs
                       (map (lambda (x) (if (adder? x)
                                            ((remove-input! x) (car set))
                                            ((set-input! x) '())))
                            (get-outputs (car set)))
                       (remove (cdr set) so-far))
                      
                      ((and (eq? (length tfs) 1) (not (has-outputs? (car set)))) ; no outputs
                       (map (lambda (x) ((remove-output! x) (car set)))
                            (get-input (car set)))
                       (remove (cdr set) so-far))
                      
                      (else (remove (cdr set) (cons (car set) so-far)))) ; retain adder
                
                (begin (handle-display (lambda () (display (get-value (car set)))
                                         (display " removed")
                                         (newline)
                                         (newline))
                                       'test)
                       (remove (cdr set) so-far))))) ; remove adder - not connected
      (remove adders '()))
    
    
    
    
    
    ; //// create-single-output-tfs
    
    ;#|
    (define (create-single-output-tfs current-pair)
      (if (null? current-pair)
          
          (handle-single-display "DONE - SINGLE OUTPUT TFS CREATED" 'results)
          
          (check-tf-for-outputs (get-tf-from-pair current-pair) current-pair)))
    
    
    (define (check-tf-for-outputs tf current-pair)
      (let ((outputs (get-outputs tf)))
        (if (null? outputs)
            
            (handle-single-display "no-outputs-to-separate" 'test)
            
            (create-separate-tfs-from-tf tf outputs))
        
        (create-single-output-tfs (get-next-pair current-pair))))
    
    
    (define (create-separate-tfs-from-tf tf outputs)
      (if (null? outputs)
          
          'move-on
          
          (if (null? (cdr outputs))
              
              'move-on
              
              (let ((previous-block (get-input tf))
                    (first-output (car outputs)))
                (let ((new-block (make-tf (get-value tf) me)))
                  
                  ; tf. remove output to first-output:
                  ((remove-output! tf) first-output)
                  
                  ; first-output. remove input from tf:
                  (if (adder? first-output)
                      ((remove-input! first-output) tf)
                      ((set-input! first-output) '()))
                  
                  (connect-serially previous-block new-block)
                  (connect-serially new-block first-output)
                  
                  (update-simplifications-counters!)
                  (handle-display (lambda () 
                                    (display "separate single output tf created:")
                                    (newline)
                                    (display "tfs now:")
                                    (newline)
                                    (display (map (lambda (x) (get-value x)) tfs))
                                    (newline)
                                    (display "adders now:")
                                    (newline)
                                    (display adders)
                                    (newline)
                                    (newline))
                                  'simplifications)
                  
                  (create-separate-tfs-from-tf tf (cdr outputs)))
                
                ))))
    ;|#
    
    
    
    
    
    ; //// parallel-merging-tfs
    
    ;#|
    (define (parallel-merging-tfs current-pair)
      (if (null? current-pair)
          
          (handle-single-display "DONE - PARALLEL TFS MERGED" 'results)
          
          (check-adder (get-adder-from-pair current-pair) current-pair)))
    
    
    (define (check-adder adder current-pair)
      (let ((inputs (get-input adder)))
        (if (null? inputs)
            
            (handle-single-display "no-inputs-for-this-adder" 'test)
            
            (check-adder-inputs adder inputs))
        
        (parallel-merging-tfs (get-next-pair current-pair))))
    
    
    (define (check-adder-inputs adder inputs)
      (if (null? inputs)
          
          'move-on
          
          (let ((input1 (car inputs)))
            
            (if (and (not (adder? input1)) (single-output? input1))
                
                (begin (handle-display (lambda () 
                                         (display "check-input1")
                                         (newline)
                                         (newline))
                                       'test)
                       (check-input1 adder input1 (cdr inputs)))
                
                (begin (handle-display (lambda () 
                                         (display "no-single-output: ")
                                         (display (get-value input1))
                                         (newline)
                                         (newline))
                                       'test)
                       (check-adder-inputs adder (cdr inputs)))))))
    
    
    (define (check-input1 adder input1 inputs)
      (if (null? inputs)
          
          'move-on
          
          (if (has-input? input1)
              
              (if (adder? (get-input input1))
                  
                  (let ((adder1 (get-input input1))
                        (input2 (car inputs)))
                    (if (single-output? input2)
                        
                        (if (has-input? input2)
                            
                            (if (adder? (get-input input2))
                                
                                (let ((adder2 (get-input input2)))
                                  (if (eq? adder1 adder2)
                                      
                                      (begin (set-value! input2 (add (get-value input1) (get-value input2)))
                                             
                                             ((remove-output! adder1) input1)
                                             ((remove-input! adder) input1)
                                             
                                             ;(process-remove-from-tfs! input1)
                                             (if (input1 'is-block?)
                                                 (((input1 'get-block) 'remove-from-blocks!) input1)
                                                 (process-remove-from-tfs! input1))
                                             
                                             
                                             (update-simplifications-counters!)
                                             (handle-display (lambda () 
                                                               (display "parallel tfs merging done:")
                                                               (newline)
                                                               (display "tfs now:")
                                                               (newline)
                                                               (display (map (lambda (x) (get-value x)) tfs))
                                                               (newline)
                                                               (newline))
                                                             'simplifications)
                                             
                                             (check-adder-inputs adder inputs))
                                      
                                      (begin (handle-single-display "eq? adder1 adder2" 'test)
                                             (check-input1 adder input1 (cdr inputs)))))
                                
                                (begin (handle-single-display "(adder? (get-input input2))" 'test)
                                       (check-input1 adder input1 (cdr inputs))))
                            
                            (begin (handle-single-display "(has-input? input2)" 'test)
                                   (check-input1 adder input1 (cdr inputs))))
                        
                        (begin (handle-single-display "(single-output? input2)" 'test)
                               (check-input1 adder input1 (cdr inputs)))))
                  
                  (begin (handle-single-display "(adder? (get-input input1))" 'test)
                         (check-adder-inputs adder (cdr inputs))))
              
              (begin (handle-single-display "(has-input? input1)" 'test)
                     (check-adder-inputs adder (cdr inputs))))))
    ;|#
    
    
    
    
    
    ; //// feedback-loop-merging
    
    ;#|
    (define (feedback-loop-merging current-pair)
      (if (null? current-pair)
          
          (handle-single-display "DONE - FEEDBACK LOOPS SIMPLIFIED" 'results)
          
          (check-tf (get-tf-from-pair current-pair) current-pair)))
    
    
    (define (check-tf tf current-pair)
      (let ((outputs (get-outputs tf)))
        (if (null? outputs)
            
            (handle-single-display "no-outputs" 'test)
            
            (check-tf-output tf outputs))
        
        (feedback-loop-merging (get-next-pair current-pair))))
    
    
    (define (check-tf-output tf outputs)
      (if (null? outputs)
          
          'move-on
          
          (if (adder? (car outputs))
              
              ; no feedback tf
              (let ((first-adder (car outputs)))
                (if (single-output? first-adder)
                    
                    (begin 
                      ;(display (get-outputs first-adder))
                      (let ((adder-output (car (get-outputs first-adder))))
                        (if (eq? adder-output tf)
                            
                            (begin
                              ; tf 1. store the merged value at tf:
                              (set-value! tf (make-ratio (mul (get-numer (get-value tf))
                                                              (get-denom (get-value tf)))
                                                         (mul (sub (get-denom (get-value tf)) 
                                                                   (get-numer (get-value tf)))
                                                              (get-denom (get-value tf)))))
                              
                              ; tf 2. remove output to adder:
                              ((remove-output! tf) first-adder)
                              
                              ; adder 1. remove tf input:
                              ((remove-input! first-adder) tf)
                              
                              ; adder 2. remove adder from adders if it has no input or only one input left:
                              (if (has-input? first-adder)
                                  
                                  (if (single-input? first-adder)
                                      
                                      (begin
                                        (let ((previous-block (car (get-input first-adder))))
                                          
                                          ; remove output from previous block to adder:
                                          ((remove-output! previous-block) first-adder)
                                          
                                          ; transfer its outputs to the previous block:
                                          (map (lambda (x) ((add-output! previous-block) x))
                                               (get-outputs first-adder))
                                          
                                          ; remove from adders:
                                          (process-remove-from-adders! first-adder)
                                          
                                          ; substitute tf input:
                                          ((set-input! tf) previous-block)))
                                      
                                      'do-nothing)
                                  
                                  (begin 
                                    ; tf 2. remove output to adder:
                                    ((set-input! tf) '())
                                    
                                    ; remove from adders:
                                    (process-remove-from-adders! first-adder))
                                  
                                  )
                              
                              (update-simplifications-counters!)
                              (handle-display (lambda () 
                                                (display "feedback loop merging with no feedback tf done:")
                                                (newline)
                                                (display "tfs now:")
                                                (newline)
                                                (display (map (lambda (x) (get-value x)) tfs))
                                                (newline)
                                                (display "adders now:")
                                                (newline)
                                                (display adders)
                                                (newline)
                                                (newline))
                                              'simplifications)
                              
                              (check-tf-output tf (cdr outputs)))
                            
                            (begin (handle-single-display "no-loop-here (eq? adder-output tf) - no feedback tf" 'test)
                                   (check-tf-output tf (cdr outputs))))
                        
                        ))
                    
                    
                    (begin (handle-single-display "no-loop-here (single-output? first-adder)" 'test)
                           (check-tf-output tf (cdr outputs)))
                    
                    ))
              
              ; feedback tf
              (let ((feedback-tf (car outputs)))
                (if (single-output? feedback-tf)
                    
                    (let ((feedback-tf-output (car (get-outputs feedback-tf))))
                      (if (adder? feedback-tf-output)
                          
                          ; as before:
                          (let ((first-adder feedback-tf-output))
                            (if (single-output? first-adder)
                                
                                (begin 
                                  ;(display (get-outputs first-adder))
                                  (let ((adder-output (car (get-outputs first-adder))))
                                    (if (eq? adder-output tf)
                                        
                                        (begin
                                          ; tf 1. store the merged value at tf:
                                          
                                          (let ((num-tf (get-numer (get-value tf)))
                                                (denom-tf (get-denom (get-value tf)))
                                                (num-feedtf (mul (get-numer (get-value tf)) (get-numer (get-value feedback-tf))))
                                                (denom-feedtf (mul (get-denom (get-value tf)) (get-denom (get-value feedback-tf)))))
                                            
                                            
                                            
                                            
                                            (set-value! tf (make-ratio (mul num-tf
                                                                            denom-feedtf)
                                                                       (mul (sub denom-feedtf
                                                                                 num-feedtf)
                                                                            denom-tf)))
                                            
                                            )
                                          
                                          
                                          ; tf 2. remove output to feedback-tf:
                                          ((remove-output! tf) feedback-tf)
                                          
                                          ; remove feedback-tf from tfs:
                                          ;(process-remove-from-tfs! feedback-tf)
                                          (if (feedback-tf 'is-block?)
                                              (((feedback-tf 'get-block) 'remove-from-blocks!) feedback-tf)
                                              (process-remove-from-tfs! feedback-tf))
                                          
                                          ; adder 1. remove feedback-tf input:
                                          ((remove-input! first-adder) feedback-tf)
                                          
                                          ; adder 2. remove adder from adders if it has no input or only one input left:
                                          (if (has-input? first-adder)
                                              
                                              (if (single-input? first-adder)
                                                  
                                                  (begin
                                                    (let ((previous-block (car (get-input first-adder))))
                                                      
                                                      ; remove output from previous block to adder:
                                                      ((remove-output! previous-block) first-adder)
                                                      
                                                      ; transfer its outputs to the previous block:
                                                      (map (lambda (x) (add-output! previous-block x)) (get-outputs first-adder))
                                                      
                                                      ; remove from adders:
                                                      (process-remove-from-adders! first-adder)
                                                      
                                                      ; substitute tf input:
                                                      ((set-input! tf) previous-block)))
                                                  
                                                  'do-nothing)
                                              
                                              (begin 
                                                ; tf 2. remove output to adder:
                                                ((set-input! tf) '())
                                                
                                                ; remove from adders:
                                                (process-remove-from-adders! first-adder))
                                              
                                              )
                                          
                                          (update-simplifications-counters!)
                                          (handle-display (lambda () 
                                                            (display "feedback loop merging with feedback tf done:")
                                                            (newline)
                                                            (display "tfs now:")
                                                            (newline)
                                                            (display (map (lambda (x) (get-value x)) tfs))
                                                            (newline)
                                                            (display "adders now:")
                                                            (newline)
                                                            (display adders)
                                                            (newline)
                                                            (newline))
                                                          'simplifications)
                                          
                                          (check-tf-output tf (cdr outputs)))
                                        
                                        (begin (handle-single-display "no-loop-here (eq? adder-output tf)" 'test)
                                               (check-tf-output tf (cdr outputs)))
                                        
                                        )))
                                
                                (begin (handle-single-display "no-loop-here (single-output? first-adder)" 'test)
                                       (check-tf-output tf (cdr outputs)))
                                
                                ))          
                          
                          
                          (begin (handle-single-display "no-loop-here (adder? feedback-tf-output)" 'test)
                                 (check-tf-output tf (cdr outputs)))
                          
                          ))
                    
                    (begin (handle-single-display "no-loop-here (single-output? feedback-tf)" 'test)
                           (check-tf-output tf (cdr outputs)))
                    
                    ))
              
              )))
    ;|#
    
    
    
    
    ; //// serial-merging-tfs
    
    (define (serial-merging-tfs current-pair)
      (if (null? current-pair)
          
          (handle-single-display "DONE - SERIAL TFS MERGED" 'results)
          
          (let ((tf (get-tf-from-pair current-pair))
                (next-pair (get-next-pair current-pair)))  ; current, next and previous pairs of tfs
            
            #|
            (display "hdgjjdghj")
            (newline)
            (display (get-value tf))
            (newline)
            (newline)
            (display "outputs: ")
            (display (get-outputs tf))
                                  (newline)
            (newline)
            |#
            
            (if (single-output? tf)
                
                (let ((second-tf (car (get-outputs tf))))
                  (if (adder? second-tf)
                      
                      (begin (handle-display (lambda () (display "no-serial-merging-tfs-here (adder?)")
                                               (newline)
                                               (newline))
                                             'test)
                             (serial-merging-tfs next-pair))
                      
                      (begin (update-simplifications-counters!)
                             (handle-display (lambda () 
                                               (display (get-value tf))
                                               (display " and")
                                               (newline)
                                               (display (get-value second-tf))
                                               (display " merged")
                                               (newline)
                                               (newline))
                                             'simplifications)
                             
                             
                             ; store the merged value at second-tf:
                             #|
                             (newline)
                             (display "display (get-value tf))")
                             (display (get-value tf))
                             (newline)
                             (display (get-value second-tf))
                             (newline)
                             |#
                             (set-value! second-tf (mul (get-value tf) (get-value second-tf)))
                             
                             #|
                             (display "new value")
                             (newline)
                             (display (get-value second-tf))
                             (newline)
                             |#
                             
                             ; rearrange connections:
                             (if (has-input? tf)
                                 (begin ((remove-output! (get-input tf)) tf)
                                        (connect-serially (get-input tf) second-tf)) ; input to second tf removed
                                 ((set-input! second-tf) '()))
                             
                             
                             
                             ; delete tf from tfs:
                             (if (tf 'is-block?)
                                 (((tf 'get-block) 'remove-from-blocks!) tf)
                                 (process-remove-from-tfs! tf))
                             
                             #|
                             (newline)
                             (display (get-blocks a))
                             (newline)
                             (display (get-value a))
                             |#
                             
                             (handle-display (lambda () 
                                               (display "tfs now:")
                                               (newline)
                                               (display (map (lambda (x) (get-value x)) tfs))
                                               (newline)
                                               (newline))
                                             'simplifications)
                             
                             (serial-merging-tfs next-pair))))
                
                (begin (handle-display (lambda () (display "no-serial-merging-tfs-here (single-output?) ")
                                         (display (get-value tf))
                                         (newline)
                                         (newline))
                                       'test)
                       (serial-merging-tfs next-pair))))
          
          ))
    
    
    
    
    ; //// serial-merging-adders
    
    (define (serial-merging-adders current-pair)
      (if (null? current-pair)
          
          (handle-single-display "DONE - SERIAL ADDERS MERGED" 'results)
          
          (let ((adder (get-adder-from-pair current-pair))
                (next-pair (get-next-pair current-pair)))  ; current, next and previous pairs of adders
            (if (single-output? adder)
                
                (let ((second-adder (car (get-outputs adder))))
                  (if (not (adder? second-adder))
                      
                      (begin (handle-single-display "no-serial-merging-adders-here (adder?)" 'test)
                             (serial-merging-adders next-pair))
                      
                      (if (not (single-input? second-adder))
                          
                          (begin (handle-single-display "no-serial-merging-adders-here (single-input?)" 'test)
                                 (serial-merging-adders next-pair))
                          
                          
                          (begin (update-simplifications-counters!)
                                 (handle-display (lambda () 
                                                   (display (get-value adder))
                                                   (display " and")
                                                   (newline)
                                                   (display (get-value second-adder))
                                                   (display " merged")
                                                   (newline)
                                                   (newline))
                                                 'simplifications)
                                 
                                 
                                 ; rearrange connections:
                                 (if (has-input? adder)
                                     (begin (map (lambda (x) ((remove-output! x) adder)) (get-input adder))
                                            (map (lambda (x) (connect-serially x second-adder)) (get-input adder))
                                            ((remove-input! second-adder) adder))
                                     ((remove-input! second-adder) adder))
                                 
                                 
                                 ; delete adder from adders:
                                 (process-remove-from-adders! adder)
                                 
                                 (handle-display (lambda () (display "adders now:")
                                                   (newline)
                                                   (display (map (lambda (x) (get-value x)) adders))
                                                   (newline)
                                                   (newline))
                                                 'simplifications)
                                 
                                 (serial-merging-adders next-pair))
                          
                          )))
                
                (begin (handle-display (lambda () (display "no-serial-merging-adders-here (single-output?) ")
                                         (display (get-value adder))
                                         (newline)
                                         (newline))
                                       'test)
                       (serial-merging-adders next-pair))))
          
          ))
    
    
    
    
    
    
    ; //// message-handling:
    
    (define (me request)
      (cond ((eq? request 'is-block?) i-am-block)
            ((eq? request 'is-adder?) i-am-adder)

            ((eq? request 'is-simplified?) i-am-simplified)
            ((eq? request 'simplify) (simplify-the-block))
            
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
            
            ((eq? request 'element-of-blocks?) (lambda (x) (process-element-of-blocks? x)))
            ((eq? request 'element-of-tfs?) (lambda (x) (process-element-of-tfs? x)))
            ((eq? request 'element-of-adders?) (lambda (x) (process-element-of-adders? x)))
            ((eq? request 'adjoin-blocks!) (lambda (x) (process-adjoin-blocks! x)))
            ((eq? request 'adjoin-tfs!) (lambda (x) (process-adjoin-tfs! x)))
            ((eq? request 'adjoin-adders!) (lambda (x) (process-adjoin-adders! x)))
            ((eq? request 'remove-from-blocks!) (lambda (x) (process-remove-from-blocks! x)))
            ((eq? request 'remove-from-tfs!) (lambda (x) (process-remove-from-tfs! x)))
            ((eq? request 'remove-from-adders!) (lambda (x) (process-remove-from-adders! x)))
            
            ((eq? request 'get-value) value)
            ((eq? request 'set-value!) (lambda (x) (set! value x)))
            
            ((eq? request 'get-block) (car block))
            ((eq? request 'get-blocks) blocks)
            ((eq? request 'get-tfs) tfs)
            ((eq? request 'get-adders) adders)
            
            (else (error "Unknown request - MAKE-TF" request))))
    
    (when (not (null? block))
      (((car block) 'adjoin-blocks!) me))
    
    me))






