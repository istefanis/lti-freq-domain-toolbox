

#|
lti-freq-domain-toolbox
Copyright (C) 2014 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
|#







#lang racket

(require "lib.rkt")
(require "arithmetic_analysis.rkt")
(require "symbolic_algebra.rkt")
(require math/base)
(provide (all-defined-out))



(define-namespace-anchor n_anchor)
(define anchor (namespace-anchor->namespace n_anchor))







; //////////   A. metrics and display preferences   //////////


; variables used as counters, so as to have some metrics about the simplifications done:

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




; display-mode adjusts the volume of information printed, in terms of the following levels:

; 'test, 'simplifications, 'results, 'nil
;      <-- more information printed

(define display-mode 'nil) ; the functions bode etc automatically set it to nil

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










; //////////   B. element (tf, adder and block of elements) representations   //////////



; the elements of circuits (tfs, adders and blocks) are implemented as objects 
; using the message-passing style


(define (get-value element) (element 'get-value))
(define (set-value! element new-value) ((element 'set-value!) new-value))

(define (is-adder? element) (element 'is-adder?))
(define (has-adder-input? element) 
  (if (and (not is-adder?) (element 'has-input?)) (is-adder? (get-input element)) 'ok))

(define (has-input? element) (element 'has-input?))
(define (single-input? adder) (adder 'single-input?))
(define (get-input element) (element 'get-input))
(define (set-input! element) (element 'set-input!))
(define (remove-input! adder) (adder 'remove-input!))
(define (print-input element) (element 'print-input))

(define (has-outputs? element) (element 'has-outputs?))
(define (single-output? element) (element 'single-output?))
(define (two-outputs? adder) (adder 'two-outputs?))
(define (get-outputs element) (element 'get-outputs))
(define (add-output! element) (element 'add-output!))
(define (remove-output! element) (element 'remove-output!))
(define (print-outputs element) (element 'print-outputs))




(define (element-of-tfs? block) (block 'element-of-tfs?))
(define (element-of-adders? block) (block 'element-of-adders?))

(define (adjoin-tfs! block) (block 'adjoin-tfs!))
(define (adjoin-adders! block) (block 'adjoin-adders!))

(define (remove-from-tfs! block) (block 'remove-from-tfs!))
(define (remove-from-adders! block) (block 'remove-from-adders!))


(define (block? element) (element 'is-block?))
(define (is-simplified? block) (block 'is-simplified?))
(define (simplify block) (block 'simplify))

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
                 (begin (handle-display "Block not fully simplified - GET-BLOCK-VALUE" 'test)
                        (block 'get-value))))))






; list operators for handling tfs and adders elements (they share the same list representation):

(define (get-next-pair pair) (cdr pair))
(define (get-tf-from-pair pair) (car pair))
(define (get-adder-from-pair pair) (car pair))




(define (element? x set)
  (cond ((null? set) #f)
        ((eq? x (car set)) #t)
        (else (element? x (cdr set)))))






; one-dimensional tables:

(define (lookup key table)
  (let ((record (m-assoc key (mcdr table))))
    (if record
        (mcdr record)
        #f)))

(define (m-assoc key records)
  (cond ((null? records) #f)
        ((equal? (mcar (mcar records)) key) (mcar records))
        (else (m-assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (m-assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table (mcons (mcons key value) (mcdr table))))))

(define (make-table)
  (mcons '*table* '()))







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
                       (map (lambda (x) (when (is-adder? x)
                                          ((remove-input! x) (car set)))) (get-outputs (car set)))
                       (remove (cdr set) so-far))
                      
                      ((and (eq? (length tfs) 1) (not (has-input? (car set)))) ; no inputs
                       (map (lambda (x) (if (is-adder? x)
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
                  (if (is-adder? first-output)
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
            
            (if (and (not (is-adder? input1)) (single-output? input1))
                
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
              
              (if (is-adder? (get-input input1))
                  
                  (let ((adder1 (get-input input1))
                        (input2 (car inputs)))
                    (if (single-output? input2)
                        
                        (if (has-input? input2)
                            
                            (if (is-adder? (get-input input2))
                                
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
                                
                                (begin (handle-single-display "(is-adder? (get-input input2))" 'test)
                                       (check-input1 adder input1 (cdr inputs))))
                            
                            (begin (handle-single-display "(has-input? input2)" 'test)
                                   (check-input1 adder input1 (cdr inputs))))
                        
                        (begin (handle-single-display "(single-output? input2)" 'test)
                               (check-input1 adder input1 (cdr inputs)))))
                  
                  (begin (handle-single-display "(is-adder? (get-input input1))" 'test)
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
          
          (if (is-adder? (car outputs))
              
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
                      (if (is-adder? feedback-tf-output)
                          
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
                          
                          
                          (begin (handle-single-display "no-loop-here (is-adder? feedback-tf-output)" 'test)
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
                  (if (is-adder? second-tf)
                      
                      (begin (handle-display (lambda () (display "no-serial-merging-tfs-here (is-adder?)")
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
                  (if (not (is-adder? second-adder))
                      
                      (begin (handle-single-display "no-serial-merging-adders-here (is-adder?)" 'test)
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
      (cond ((eq? request 'is-adder?) i-am-adder)
            
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
            
            
            ((eq? request 'simplify) (simplify-the-block))
            
            ((eq? request 'get-value) value)
            ((eq? request 'set-value!) (lambda (x) (set! value x)))
            
            ((eq? request 'get-block) (car block))
            ((eq? request 'get-blocks) blocks)
            ((eq? request 'get-tfs) tfs)
            ((eq? request 'get-adders) adders)
            
            
            ((eq? request 'is-block?) i-am-block)
            ((eq? request 'is-simplified?) i-am-simplified)
            
            (else (error "Unknown request - MAKE-TF" request))))
    
    (when (not (null? block))
      (((car block) 'adjoin-blocks!) me))
    
    me))








; /////////  tf  /////////

; implementation assumption:
; each tf has only one input and multiple outputs
;
; multiple inputs can be achieved by adding in front of it an adder


(define (make-tf value block)
  (let ((i-am-block #f)
        (i-am-adder #f)
        (input '())
        (outputs-list '()))
    
    
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
    
    
    (define (me request)
      (cond ((eq? request 'get-value) value)
            ((eq? request 'set-value!) (lambda (x) (set! value x)))
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
            
            ((eq? request 'is-block?) i-am-block)
            
            (else (error "Unknown request - MAKE-TF" request))))
    
    
    ((adjoin-tfs! block) me)
    
    me))


(define (tf n d block)
  (define tf1 (make-tf (make-ratio (make-poly-dense 's n)
                                   (make-poly-dense 's d))
                       block))
  tf1)








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








; //// initialize a, b, c, d blocks

(define a (make-block))
(define b (make-block))
(define c (make-block))
(define d (make-block))









; //////////   C. operations on the circuit level  //////////


; connect-serially:

(define (connect-serially block1 block2)
  ((set-input! block2) block1)
  ((add-output! block1) block2))


(define connect connect-serially)










; //////////   D. circuit examples and elementary blocks  //////////


(define (feedback-loop-test1 block) ;parent block
  
  (define tf1 (make-tf (make-ratio (make-poly-dense 's '(1 0 1 1 0))
                                   (make-poly-dense 's '(1)))
                       block))
  
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (make-adder block))
  (define add2 (adder block))
  
  (connect-serially tf1 tf2)
  (connect tf2 add1)
  (connect add1 tf1)
  (connect tf1 add2)
  (connect add2 tf3)
  (connect tf3 add2)
  
  block)  ; return the block where the elements were stored so as to be passed to simplify




(define (multiple-outputs-test1 block)
  
  (define tf1 (tf '(1 0 1 1 0) '(1) block))  
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  
  (connect add1 tf2)
  (connect tf2 tf3)
  (connect tf2 tf4)
  (connect tf2 tf5)
  
  block)




(define (serial-adders-test1 block)
  
  (define tf1 (tf '(1 0 1 1 0) '(1) block))  
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  
  (connect add1 add2)
  (connect add2 tf5)
  (connect tf5 add1)
  
  block)




(define (parallel-tfs-test1 block)
  
  (define tf1 (tf '(1 0 1 1 0) '(1) block))
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  
  (connect add1 tf1)
  (connect add1 tf2)
  (connect add1 tf3)
  (connect tf1 add2)
  (connect tf2 add2)
  (connect tf3 add2)
  
  block)




(define (circuit1 block)
  
  (define tf1 (tf '(1 0 0 0 0) '(1 1 0) block))
  (define tf2 (tf '(1 0) '(2 1 0) block))
  (define tf3 (tf '(1 0) '(1 0 1 0) block))
  (define tf4 (tf '(1) '(1 0) block))
  (define tf5 (tf  '(1) '(1 0 0) block))
  (define tf6 (tf '(2 0) '(3 0 1) block))
  (define tf7 (tf '(1) '(4 0 0) block))
  
  (define add1 (adder block))
  (define add2 (adder block))
  (define add3 (adder block))
  (define add4 (adder block))
  
  (connect add1 tf2)
  (connect add1 tf3)
  (connect add1 tf4)
  
  (connect tf2 add2)
  (connect tf3 add2)
  
  (connect tf3 add3)
  (connect tf4 add3)
  
  (connect add2 tf1)
  (connect tf1 add1)
  
  (connect add2 tf5)
  (connect tf5 add4)
  (connect add3 add4)
  
  (connect add4 tf6)
  (connect tf6 add4)
  (connect tf6 tf7)
  (connect tf7 add1)
  
  block)




(define (circuit2 block)
  (define tf1 (make-tf (make-ratio (make-poly-dense 's '(2)) 
                                   (make-poly-dense 's '(1 1)))
                       block)) 
  (define tf2 (tf '(1 2 5) '(8) block))
  (connect tf1 tf2)
  block)




(define (circuit3 block)  
  (define tf1 (tf '(-10 0 1) '(1 2 1) block))
  block)







; //// elementary blocks:


(define (phase-delay-circuit block)  
  (define tf1 (tf '(5 1) '(8 1) block))
  block)




(define (integrator block)
  (define tf1 (tf '(1) '(1 0) block))
  block)




(define (sine block)
  (define tf1 (tf '(1) '(1 0 1) block))
  block)




; //// pid controllers:


(define (pi-controller kc ti block)
  (define tf1 (tf (list (list '* kc ti) kc)  ; writen this way so as to accept symbols (and so work with tune)
                  (list ti 0)
                  block))
  block)




(define (pd-controller kc td block) 
  (define tf1 (tf (list (list '* td kc) kc)
                  '(1)
                  block))
  block)




(define (pid-controller kc ti td block)
  (define tf1 (tf (list (list '* kc ti td) (list '* kc ti) kc)
                  (list ti 0)
                  block))
  block)




; //// Cheb filters:

(define cheb-threshold 1000)

(define (cheb-t1 n e w0 block)
  
  (define (theta m)
    (/ (* pi (- (* 2 m) 1)) (* 2 n)))
  
  (define (spm m)
    (+ (- (* (sinh (/ (asinh (/ 1 e)) n))
             (sin (theta m))))
       (* (make-rectangular 0 1.0) (cosh (/ (asinh (/ 1 e)) n)) (cos (theta m)))))
  
  
  (set! cheb-threshold (/ 1 (sqrt (+ 1 (* e e)))))
  
  ;(define b (make-block))
  
  
  ;(define tf0 (tf '(1) (list (* (expt 2 (- n 1)) e)) b))
  (define tf0 (tf '(1) (list (* (expt 2 (- n 1)) e)) block))
  ;(define tf1 (tf '(1) (list 1 (- (spm 1))) b))
  ;(define tf2 (tf '(1) (list 1 (- (spm 2))) b))
  ;(define tf3 (tf '(1) (list 1 (- (spm 3))) b))
  
  
  (define (connection-function tfm m)
    (when (< m n)
      (let ;((f1 (tf '(1) (list 1 (- (spm (+ m 1)))) b)))
          ((f1 (tf '(1) (list 1 (- (spm (+ m 1)))) block)))
        (connect tfm f1)
        (connection-function f1 (+ m 1)))))
  
  (connection-function tf0 0)
  
  block)






; //// time delay blocks:


; approximating e-sT using pade functions 
; (the arithmetic methods fail when trying to model delay by just adding e-sT as a tf):

#|
(define (pade5 T)
  (tf (list (* T T T T)
            (* -120 T T T)
            (* 1260 T T)
            (* -6720 T)
            15120)
      (list (* T T T T T)
            (* 25 T T T T)
            (* 300 T T T)
            (* 2100 T T)
            (* 8400 T)
            15120)))
|#



(define (pade m n T block)
  
  ; the values returned for the greater values of i are rather small,
  ; so must be multiplied by an appropriate coefficient:
  (define coeff (/ 1000000 (expt T 5)))
  
  (define (p i)
    ; 100
    (/ (* coeff
          (expt -1 i)
          (factorial (+ m n (- 0 i)))
          (factorial m))
       (* (factorial (+ m n))
          (factorial i)
          (factorial (- m i)))))
  
  (define (q i)
    (/ (* coeff
          (factorial (+ m n (- 0 i)))
          (factorial n))
       (* (factorial (+ m n))
          (factorial i)
          (factorial (- n i)))))
  
  #|
  (displayln (p 0))
  (displayln (p 1))
  (displayln (p 2))
  (displayln (p 3))
  (displayln (p 4))
  (displayln (p 5))
  |#
  
  ;(displayln (map (λ (x) (* (p x) (expt T x))) (integers m)))
  
  
  (define tf1 (tf (map (λ (x) (* (p x) (expt T x))) (integers m))  ; integers from 0 to m
                  (map (λ (x) (* (q x) (expt T x))) (integers n))
                  block))
  
  tf1)




; T<7
(define (delay block T)
  
  (newline)
  (displayln "delay approximated using pade(4,5)")
  
  (define tf2 (pade 4 5 T block))
  
  (connect tf2 (cadr (get-tfs block)))
  
  block)














; //////////   E. transforming the tf value expression to the string format, for display  //////////



; a function that tranforms a list structure to a simple list of strings of each element:

(define (list-to-list-of-strings lst)
  
  
  (define (list-to-string coeff-init rest)
    
    (define (list-to-string2 coeff)
      (if (null? coeff)
          (cons ")"
                rest)
          (if (= (length coeff) 1)
              (cond 
                ;((list? (car coeff)) (cons "f1" (list-to-string2 (cdr coeff))))
                ((list? (car coeff)) (list-to-string (car coeff) (list-to-string2 (cdr coeff))))
                
                ((symbol? (car coeff)) (cons (symbol->string (car coeff)) (list-to-string2 (cdr coeff))))
                ((number? (car coeff)) (cons (number->string (car coeff)) (list-to-string2 (cdr coeff)))))
              (cond 
                ;((list? (car coeff)) (cons "f1" (cons " " (list-to-string2 (cdr coeff)))))
                ((list? (car coeff)) (list-to-string (car coeff) (cons " " (list-to-string2 (cdr coeff)))))
                
                
                ((symbol? (car coeff)) (cons (symbol->string (car coeff)) 
                                             (cons " " (list-to-string2 (cdr coeff)))))
                ((number? (car coeff)) (cons (number->string (car coeff)) 
                                             (cons " " (list-to-string2 (cdr coeff)))))))))
    
    (if (null? coeff-init)
        rest
        (cons "("
              (cond 
                ;((list? (car coeff-init)) (cons "f1" 
                ;                                  (cons " " (list-to-string2 (cdr coeff-init)))))
                ((list? (car coeff-init)) (cons "f1" 
                                                (cons " " (list-to-string2 (cdr coeff-init)))))
                
                ((symbol? (car coeff-init)) (cons (symbol->string (car coeff-init)) 
                                                  (cons " " (list-to-string2 (cdr coeff-init)))))
                ((number? (car coeff-init)) (cons (number->string (car coeff-init)) 
                                                  (cons " " (list-to-string2 (cdr coeff-init)))))))))
  
  
  
  
  ; for all the list elements except the first one:
  (define (list-to-printable-list-2 l)
    
    ;(newline)
    ;(display "coeff1: ")
    ;(display coeff1)
    ;(newline)
    
    ;cheb
    (let* ((coeff1 (car (cdr (car (cdr l)))))
           (coeff (if (number? coeff1) 
                      (real-part coeff1)
                      coeff1)))
      (let ((sign (if (number? coeff)
                      (if (> coeff 0)
                          " + "
                          " - ")
                      " + ")))
        
        
        ; simpifications for coeff 0 and 1:
        (cond ((eq? coeff 0)
               (if (not (pair? (car (cddr l))))
                   '()
                   (list-to-printable-list-2 (car (cddr l)))))
              
              ((or (eq? coeff 1) (eq? coeff -1))
               (if (not (pair? (car (cddr l))))
                   (list sign (number->string (abs coeff)))
                   (cons sign
                         (cons "s^"
                               (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                     (list-to-printable-list-2 (car (cddr l))))))))
              
              (else
               (if (not (pair? (car (cddr l))))
                   (cond ((list? coeff) (cons sign (list-to-string coeff '())))
                         ((symbol? coeff) (list sign (symbol->string coeff)))
                         (else (list sign (number->string (abs coeff)))))
                   (cond ((list? coeff) 
                          (cons sign
                                (list-to-string coeff
                                                (cons "*s^"
                                                      (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                                            (list-to-printable-list-2 (car (cddr l))))))))
                         ((symbol? coeff) 
                          (cons sign
                                (cons (symbol->string coeff)
                                      (cons "*s^"
                                            (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                                  (list-to-printable-list-2 (car (cddr l))))))))
                         (else 
                          (cons sign
                                (cons (number->string (abs coeff))
                                      (cons "*s^"
                                            (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr l)))))))))
                                                  (list-to-printable-list-2 (car (cddr l)))))))))
                   ))
              ))))
  
  
  
  ; for the first list element (special handling so as not to add its sign if that positive):
  (let ((coeff (car (cdr (car (cdr lst))))))
    
    ;(newline)
    ;(display "coeff: ")
    ;(display coeff)
    ;(newline)
    
    (cond ((eq? coeff 0) 
           (if (not (pair? (car (cddr lst))))
               '()
               (list-to-printable-list-2 (car (cddr lst)))))
          
          ((or (eq? coeff 1) (eq? coeff -1))
           (if (not (pair? (car (cddr lst))))
               (cond ((list? coeff) (list-to-string coeff '()))
                     ((symbol? coeff) (list (symbol->string coeff)))
                     (else (list (number->string (abs coeff)))))
               (cons "s^"
                     (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                           (list-to-printable-list-2 (car (cddr lst)))))))
          
          (else
           (if (not (pair? (car (cddr lst))))
               (cond ((list? coeff) (list-to-string coeff '()))
                     ((symbol? coeff) (list (symbol->string coeff)))
                     (else (list (number->string (abs coeff)))))
               (cond ((list? coeff) 
                      (list-to-string coeff
                                      (cons "*s^"
                                            (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                                                  (list-to-printable-list-2 (car (cddr lst)))))))
                     ((symbol? coeff) 
                      (cons (symbol->string coeff)
                            (cons "*s^"
                                  (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                                        (list-to-printable-list-2 (car (cddr lst)))))))
                     (else 
                      (cons (number->string coeff)
                            (cons "*s^"
                                  (cons (number->string (car (cdr (cdr (car (cdr (cdr (car (cdr lst)))))))))
                                        (list-to-printable-list-2 (car (cddr lst))))))))                                  
               ))
          )))





; a function that appends a list of strings to a string:

(define (list-of-strings-to-string l)
  (if (> (length l) 1)
      (apply string-append l) ;string-append takes as input a list of strings
      (car l)))


(define (make-space-line length) (make-string length #\ ))

(define (make-ratio-line length) (make-string length #\-))





; display tf handles the display process - after all the format transformations are done:

(define (display-tf nom-string den-string)
  (let ((l1 (string-length nom-string))
        (l2 (string-length den-string)))
    (cond ((or (> l1 40) (> l2 40))
           (newline)
           (displayln "tf:")
           (newline)
           (displayln nom-string)
           (displayln (make-ratio-line 46))
           (displayln den-string)
           (newline))
          (else  
           (let ((max-length (max l1 l2)))
             (let ((max-l1 (- max-length l1))
                   (max-l2 (- max-length l2)))
               (display "      ")
               (display (make-space-line (round (/ max-l1 2))))
               (displayln nom-string)
               (display "tf:   ")
               (displayln (make-ratio-line max-length))
               (display "      ")                      
               (display (make-space-line (round (/ max-l2 2))))
               (displayln den-string)
               (newline)))))))






; //// display tests:

#|
(define test '(/ (+ (* 6 (expt s 10)) 
                    (+ (* 8 (expt s 9)) 
                       (+ (* 6 (expt s 8)) 
                          (+ (* -4 (expt s 7)) 
                             (+ (* 4 (expt s 6)) 
                                (+ (* 2 (expt s 5)) 
                                   (+ (* 5 (expt s 4)) 
                                      (+ (* 7 (expt s 3)) 
                                         (+ (* 7 (expt s 2)) 
                                            (+ (* 5 (expt s 1)) 
                                               (+ (* 2 (expt s 0)) 0)))))))))))
                 (+ (* -6 (expt s 10)) 
                    (+ (* 4 (expt s 9)) 
                       (+ (* 4 (expt s 8)) 
                          (+ (* 14 (expt s 7)) 
                             (+ (* 8 (expt s 6)) 
                                (+ (* -2 (expt s 5)) 
                                   (+ (* -3 (expt s 4)) 
                                      (+ (* -5 (expt s 3)) 
                                         (+ (* -7 (expt s 2)) 
                                            (+ (* -5 (expt s 1)) 
                                               (+ (* -2 (expt s 0)) 0)))))))))))
                 ))


(define test2 '(/ (+ (* 1 (expt s 0)) 0) 
                  
                  (+ (* 1 (expt s 1)) 
                     (+ (* 0 (expt s 0)) 0)))
  )





(define n-l (list-to-printable-list (cadr test2)))
(define d-l (list-to-printable-list (caddr test2)))

(newline)
(displayln n-l)
(newline)
(displayln d-l)



(define n-s (printable-list-to-string n-l))
(define d-s (printable-list-to-string d-l))

(newline)
;(displayln n-s)
(newline)
;(displayln d-s)
;|#












; //////////   F. modifying the format of the circuit's tf value for the interpreter  //////////



; fast-display procedure displays a circuit's simplified tf in three formats - for testing:

(define (fast-display block)
  (let ((res (get-simplified-block-value block)))
    (display res)
    (newline)
    (newline)
    (display (ratio-to-list res))
    (newline)
    (newline)))




; map-with-s expands lists of the poly format, to polynomials:

(define (expand-polynomial list-orig)
  
  (define (loop lst)
    (let ((l (if (null? lst)
                 0
                 (length lst))))
      (cond ((= l 0) 0)
            #|
            ((= l 1)
             (list '+ (car lst)
                   0))
            ((= l 2)
             (list '+ 
                   (list '* (car lst) 's)
                   (loop (cdr lst))))
|#
            ((= l (length list-orig))
             (list '+ 
                   (list '* (car lst) (list 'expt 's (- l 1))) 
                   (loop (cdr lst))))
            (else
             (list '+ (list '* (car lst) (list 'expt 's (- l 1)))
                   (loop (cdr lst)))))))
  
  (loop list-orig))







; before a ratio of polynomials is reduced by ratio-to-list,
; the coefficient lists must be evaluated so that the coefficients are simplified 
; - no evaluations must be done on symbols:

(define (contains-symbols?-tree l)
  (if (null? l)
      #f
      (if (list? (car l))
          (or (contains-symbols?-tree (car l))
              (contains-symbols?-tree (cdr l)))    
          (or (and (symbol? (car l)) (not (or (eq? (car l) '+)
                                              (eq? (car l) '-)
                                              (eq? (car l) '*)
                                              (eq? (car l) '/))))
              (contains-symbols?-tree (cdr l))))))



(define (round-decimal-tree l) 
  (if (null? l)
      '()
      (if (list? (car l))
          (cons (round-decimal-tree (car l))
                (round-decimal-tree (cdr l)))
          (if (symbol? (car l))
              (cons (car l) (round-decimal-tree (cdr l)))
              (cons (round-decimal (car l) 3) (round-decimal-tree (cdr l)))))))






; ratio-to-list:
; i)   gets as input the simplified value in the poly format
; ii)  performs the reduction of the polynomial using reduce from the symbolic algebra package
; iii) expands the result using expand-polynomial
; iv)  transforms the latter to the string format ready for display using
;      list-to-list-of-strings and then list-of-strings-to-string
; v)   displays the tf string
; vi)  returns the expanded value to the calling function, ready for evaluation


(define (ratio-to-list value . disp)
  (let ((a (get-numer value))
        (b (get-denom value)))
    (let ((a-term-list (cdr (cdr a)))
          (b-term-list (cdr (cdr b))))
      ;(newline)
      ;(newline)
      ;(display a-term-list)
      ;(newline)
      ;(display b-term-list)
      ;(newline)
      (let* ((res (or (contains-symbols?-tree a-term-list)
                      (contains-symbols?-tree b-term-list)
                      ))
             
             (division (if res
                           ; no reduction in this case - so numbers bust be rounded
                           (list (make-poly-dense 's (map (λ (x) (if (list? x)
                                                                     (if (contains-symbols?-tree x)
                                                                         (round-decimal-tree x)
                                                                         (round-decimal (eval x anchor) 3))
                                                                     (if (not (symbol? x))
                                                                         (round-decimal (eval x anchor) 3)
                                                                         x))) 
                                                          a-term-list))
                                 (make-poly-dense 's (map (λ (x) (if (list? x)
                                                                     (if (contains-symbols?-tree x)
                                                                         (round-decimal-tree x)
                                                                         (round-decimal (eval x anchor) 3))
                                                                     (if (not (symbol? x))
                                                                         (round-decimal (eval x anchor) 3)
                                                                         x)))
                                                          b-term-list)))
                           (reduce (make-poly-dense 's (map (λ(x) (eval x anchor)) a-term-list))
                                   (make-poly-dense 's (map (λ(x) (eval x anchor)) b-term-list)))))
             (nom (expand-polynomial (cdr (cdr (car division)))))
             (den (expand-polynomial (cdr (cdr (cadr division))))))
        
        ;(let ((nom (map-with-s a-term-list))
        ;      (den (map-with-s b-term-list)))
        
        ;(newline) 
        ;(display a-term-list)
        ;(display division)
        ;(display nom)
        ;(newline)
        ;(display den)
        ;(newline)
        
        ; cheb:
        ;#|
        ;(newline)
        ;(display den)
        ;(newline)
        ;(display (list-to-printable-list nom))
        ;(newline)
        ;(display (list-to-printable-list den))
        ;(newline)
        (when (null? disp)
          (display-tf (list-of-strings-to-string (list-to-list-of-strings nom))
                      (list-of-strings-to-string (list-to-list-of-strings den))))
        ;|#
        
        ;(newline)
        (list '/ nom den)))))






; a lite version of the same procedure:

(define (ratio-to-list-lite value)
  (let ((a (get-numer value))
        (b (get-denom value)))
    (list '/ 
          (expand-polynomial (cdr (cdr a)))
          (expand-polynomial (cdr (cdr b))))
    ))











; //////////   G. plot-creating functions  //////////


; the PLoT library, by Neil Toronto <neil.toronto@gmail.com>, 
; is used for generating the following plots

; in all the following plot-creating fuctions, block is a circuit building expression 
; that returns the block to be simplified, such as: (circuit1 a) or: (pi-controller 7 5 a)




(require plot)
(newline)






(define (get-total-tfs-value block)
  
  (display-mode-nil!)
  (newline)
  
  ;(let ((cached-simplification-result (simplify block)))
  (cons 'λ (cons '(s fw1 fw2 fw3 fw4) 
                 (list (ratio-to-list (get-simplified-block-value block))))))
;)








(define (F block)  ; tf is evaluated here
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         (tfs-value-evaluation (eval total-tfs-value anchor)))
    
    (define (tfs s) (tfs-value-evaluation s 0 0 0 0))
    ;(fw1-func 0) 
    ;(fw2-func w)
    ;(fw3-func w)
    ;(fw4-func w)))
    
    
    (plot-height 300)
    (plot-title "F(s) of the complete circuit")
    
    
    (for-each 
     displayln
     
     (list (parameterize ([plot-x-label      "s"]
                          [plot-y-label      "F(s)"]
                          ; the log transform is not defined on negative arguments, so:
                          ;[plot-x-transform cbrt-transform])
                          [plot-x-transform  (stretch-transform -5 5 10)])
             
             (plot (list (axes)
                         (tick-grid)
                         (function (λ (s) (tfs s)) -100 100))
                   #:y-min -20
                   #:y-max 20))
           
           (make-space-line 10)))))










(define (bode-plot-parameters)
  (plot-height 200)
  ;(plot-title "Bode diagram")
  ;(plot-title (string-append (make-space-line 10) "Bode diagram"))
  (plot-title (string-append (make-space-line 7) "Bode diagram"))
  (plot-x-label "Frequency [rad/s]")
  )








; angle unwarping - from (-180 < angle < 180) to (-540 < angle < 180):

(define had-neg-values-1 #f)
(define passed-180-1 #f)
(define passed-360-1 #f)

(define had-neg-values-2 #f)
(define passed-180-2 #f)
(define passed-360-2 #f)



(define (get-had-neg-values id)
  (if (= id 1)
      had-neg-values-1
      had-neg-values-2))
(define (set-had-neg-values-t id)
  (if (= id 1)
      (set! had-neg-values-1 #t)
      (set! had-neg-values-2 #t)))


(define (get-passed-180 id)
  (if (= id 1)
      passed-180-1
      passed-180-2))
(define (set-passed-180-t id)
  (if (= id 1)
      (set! passed-180-1 #t)
      (set! passed-180-2 #t)))


(define (get-passed-360 id)
  (if (= id 1)
      passed-360-1
      passed-360-2))
(define (set-passed-360-t id)
  (if (= id 1)
      (set! passed-360-1 #t)
      (set! passed-360-2 #t)))




(define (unwarp-angle-simple ang w id)
  
  (if (and (eq? (get-had-neg-values id) #t) (> ang 0)) ;cheb: (> ang (/ pi 2)))
      (- ang (* 2 pi))
      (if (or (< ang 0) (and (= ang 0) (> w 0.001)))
          (begin
            (set-had-neg-values-t id)
            ang)
          ang))
  )


(define (unwarp-angle-simple-2 ang w id)
  
  (if (and (eq? (get-had-neg-values id) #t) (> ang (/ pi 2)))
      (- ang (* 2 pi))
      (if (or (< ang 0) (and (= ang 0) (> w 0.001)))
          (begin
            (set-had-neg-values-t id)
            ang)
          ang))
  )


(define (unwarp-angle ang w id)
  
  (cond ((and (eq? (get-had-neg-values id) #t) (eq? (get-passed-360 id) #t) 
              (> (+ ang (* 2 pi)) 0)
              )
         (- ang (* 2 pi)))
        
        ;#|
        ((and (eq? (get-had-neg-values id) #t) (not (eq? (get-passed-180 id) #t))
              (> ang 0) 
              (< ang (/ pi 2)))
         ;works for pid:
         ang
         ;works for delay:
         ;(- ang (* 2 pi))
         )
        ;|#
        
        ((and (eq? (get-had-neg-values id) #t) ;(eq? (get-passed-180 id) #t)
              (> ang 0))
         (when ;(< (- ang (* 2 pi)) (* (- 1.8) pi))
             (and (eq? (get-passed-180 id) #t) (< (- ang (* 2 pi)) 0))
           (set-passed-360-t id))
         
         ;change:
         (when (> (abs (- ang (* 2 pi))) (* 0.8 pi))
           (set-passed-180-t id))
         
         (- ang (* 2 pi)))
        
        (else
         (when (or (< ang 0) (and (= ang 0) (> w 0.001)))
           (set-had-neg-values-t id))
         (when (> (abs ang) (* 0.8 pi))
           (set-passed-180-t id))
         ang))
  )









; fw-functions - for adding functions of w or s to the tf poly
; - better to use them only in the s-domain:

(define (simple-delay w T) (exp (* (make-rectangular 0 (- w)) T)))
(define (simple-delay-s s T) (/ 1 (exp (* s T))))

(define (comb-filter w a) (+ 1 (* a (simple-delay w 0.05))))

(define (fw1-func w) (comb-filter w 0.5))
(define (fw2-func w) (comb-filter w 0.7))
(define (fw3-func w) (simple-delay w 1))
(define (fw4-func s) (simple-delay-s s 1))

;(compare (tf '(0.02 fw1) '(1)) (tf '(0.02 fw2) '(1)))










(define (bode block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))
    
    
    (let* ((AR-low (magnitude (tfs (/ 1 1000))))
           (AR-high (magnitude (tfs 500)))
           (AR-min-value (min AR-low AR-high 0.707 0.099))
           (AR-100 (magnitude (tfs 100)))
           (AR-001 (magnitude (tfs 0.01)))
           
           
           ; bandwidth
           (b-threshold (min (max AR-low AR-high) 0.707 cheb-threshold))
           
           (fb (λ (w) (- (magnitude (tfs w)) b-threshold)))
           
           (wb-init (half-interval-method                            
                     fb
                     0.001 500))
           
           (wb1 (if (not (eq? wb-init #f))
                    (half-interval-method                            
                     fb
                     0.001 (+ wb-init 0.001))
                    #f))
           
           (wb2 (if (not (eq? wb1 #f))
                    (half-interval-method                            
                     fb
                     (+ wb-init 0.001) 300)
                    #f))
           
           
           ;gain margin
           (fc-g (λ (w) (- (let ((f1 (angle (tfs w))))
                             
                             (unwarp-angle-simple-2 f1 w 1)
                             ;(unwarp-angle f1 w 1)
                             
                             ) (- pi))))
           
           (wc-g (half-interval-method                            
                  fc-g
                  0.001 500))
           
           (gain-margin (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                            (begin ;(display wc-g)
                              (/ 1 (magnitude (tfs wc-g))))
                            (begin ;(display wc-g)
                              #f)))
           
           
           ; phase margin
           (fc-p (λ (w) (- (magnitude (tfs w)) 1)))
           
           (wc-p (half-interval-method                            
                  fc-p
                  0.001 500))
           
           
           (f1 (if (not (eq? wc-p #f)) (angle (tfs wc-p)) #f))
           (phase-margin (if (not (eq? wc-p #f))
                             (+ 180 
                                (* 180 (/ 1 pi)
                                   
                                   ; no searching is being done here, just one substitution
                                   ;#|
                                   (if (and (eq? had-neg-values-1 #t) (> f1 0)) ; old:(> f1 (/ pi 2))
                                       (- f1 (* 2 pi))
                                       (if (< f1 0)
                                           (begin
                                             (set! had-neg-values-1 #t)
                                             f1)
                                           f1))
                                   ;|#                                
                                   
                                   ))
                             #f))
           
           )
      
      
      
      
      (set! cheb-threshold 1000)
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      (bode-plot-parameters) 
      
      (for-each 
       displayln
       
       (list (parameterize ([plot-y-label "Magnitude (abs)"]
                            [plot-y-transform log-transform]
                            [plot-y-ticks (log-ticks)]
                            [plot-x-transform log-transform]
                            [plot-x-ticks (log-ticks)]
                            )
               
               (plot (list 
                      ;(axes)
                      (tick-grid)
                      
                      ;#|
                      (function (λ (w) (magnitude (tfs w)))
                                (/ 1 1000) 500 ;#:color 3
                                )
                      ;|#                    
                      
                      #|
                    (function-interval
                     (λ (w) 1)
                     (λ (w) (magnitude (tfs-value-evaluation (make-rectangular 0 w))))
                     (/ 1 1000) 500 #:color 3 #:line2-color 3 #:line1-style 'transparent)
                    |#                    
                      
                      (function (λ (x) 1) #:color 3 #:style 'dot)
                      (function (λ (x) b-threshold) #:color 3 #:style 'dot #:y-min 0.099 #:y-max 10.001)
                      
                      ;#|
                      (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                          (error-bars (list 
                                       (vector wc-g
                                               (/ (+ 1 (magnitude (tfs wc-g))) 2)
                                               (/ (- 1 (magnitude (tfs wc-g))) 2))))
                          '())
                      ;|#
                      
                      )))
             
             
             (parameterize ([plot-y-label "Phase [deg]"]
                            [plot-x-transform log-transform]
                            [plot-x-ticks (log-ticks)]
                            )
               
               (plot (list 
                      ;(axes)
                      (tick-grid)
                      (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot)
                      ;#|
                      (function (λ (w) (* 180 (/ 1 pi)
                                          (let ((f1 (angle (tfs w))))
                                            
                                            f1
                                            
                                            )))
                                (/ 1 1000) 500  #:color 3 #:style 'dot
                                )
                      ;|#
                      (function (λ (w) (* 180 (/ 1 pi)
                                          (let ((f1 (angle (tfs w))))
                                            
                                            ;f1
                                            ;(unwarp-angle-simple f1 w 1)
                                            (unwarp-angle f1 w 1)
                                            
                                            )))
                                (/ 1 1000) 500  ;#:color 3
                                )
                      #|
                    (function-interval (λ (w) 0)
                                       (λ (w) (* 180 (/ 1 pi)
                                                 ;#|
                                                 (let ((f1 (angle (tfs-value-evaluation (make-rectangular 0 w)))))
                                                   
                                                   ;f1
                                                   ;(unwarp-angle-simple f1 w 1)
                                                   (unwarp-angle f1 w 1)
                                                   
                                                   )))
                                       ;|#
                                       (/ 1 1000) 500  #:color 1 #:line2-color 1 #:line1-style 'transparent)
                    |#
                      
                      (if (not (eq? wc-p #f)) 
                          (error-bars (list 
                                       (vector wc-p (+ (- 180) (/ phase-margin 2)) (/ phase-margin 2))))
                          '())
                      
                      
                      (function (λ (x) 0) #:color 0 #:style 'dot))))
             
             (make-space-line 10)))
      
      
      
      
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      
      
      (cond ((> (/ AR-low AR-high) 2)
             (display "Low-pass filter:")
             (newline)
             (newline))    
            ((> (/ AR-high AR-low) 2)
             (display "High-pass filter:")
             (newline)
             (newline))
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             (display "Band-pass filter:")
             (newline)
             (newline)))    
      
      
      
      (cond ((eq? wb1 #f) 
             (display "bandwidth    = (0,inf)"))
            ((eq? wb2 #f)
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]"))))
            ((> (round-decimal wb2 2) (round-decimal wb1 2))
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))
                 (begin
                   (display "bandwidth    = [")
                   (display (round-decimal wb1 2))
                   (display ",")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))))
            (else
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]")))))
      (display " - thresh.: ")
      (display (round-decimal b-threshold 3))
      (newline)
      
      
      
      ; roll-off:
      
      (cond ((> (/ AR-low AR-high) 2)
             
             ;Low-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))    
            
            ((> (/ AR-high AR-low) 2)
             
             ;High-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))
            
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             
             ;Band-pass filter:         
             (define roll-off-low (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (define roll-off-high (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off (low)  = ")
             (display roll-off-low)
             (display " (log(ΔAR)/log(Δw))")
             (newline)
             (display "roll-off (high) = ")
             (display roll-off-high)
             (display " (log(ΔAR)/log(Δw))")
             (newline)))  
      
      
      
      (if (not (eq? gain-margin #f))
          (begin (display "gain margin  = ")
                 (display (round-decimal gain-margin 2))
                 (newline))
          (begin (display "gain margin  = inf")
                 (newline)))
      
      
      
      (if (not (eq? phase-margin #f))
          ;(if (> phase-margin 0)
          (begin (display "phase margin = ")
                 (display (round-decimal phase-margin 2))
                 (display " [deg]")
                 (newline)
                 (newline))
          ;   (begin (newline)))
          (begin (display "phase margin = inf")
                 (newline)
                 (newline)))
      
      ))
  
  )








(define (compare block1 block2)  ; tf1 and tf2 appropriate functions so that after the simplification two functions will remain 
  
  (display-mode-nil!)
  
  (let* ((total-tfs-value1 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block1))))))
         (tfs-value-evaluation1 (eval total-tfs-value1 anchor))                               ;change: cadr
         (total-tfs-value2 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block2))))))
         (tfs-value-evaluation2 (eval total-tfs-value2 anchor)))                                                      ;change: tf1 
    
    
    (define (tfs1 w) (tfs-value-evaluation1 (make-rectangular 0 w) 
                                            (fw1-func w) 
                                            (fw2-func w)
                                            (fw3-func w) 
                                            (fw4-func w)))
    (define (tfs2 w) (tfs-value-evaluation2 (make-rectangular 0 w) 
                                            (fw1-func w) 
                                            (fw2-func w)
                                            (fw3-func w) 
                                            (fw4-func w)))
    
    
    
    (bode-plot-parameters)
    
    (for-each
     displayln
     
     (list 
      
      (parameterize ([plot-y-label      "Magnitude (abs)"]
                     [plot-y-transform  log-transform]
                     [plot-y-ticks      (log-ticks)]
                     [plot-x-transform  log-transform]
                     [plot-x-ticks      (log-ticks)]
                     )
        
        (plot (list ;(axes)
               (tick-grid)
               (function (λ (w) (magnitude (tfs1 w)))
                         (/ 1 1000) 500
                         #:label "tf1")
               (function (λ (w) (magnitude (tfs2 w)))
                         (/ 1 1000) 500
                         #:color 3 
                         #:label "tf2")
               (function (λ (x) 1) #:color 0 #:style 'dot)
               (function (λ (x) 0.707) #:color 0 #:style 'dot))
              ))
      
      
      (parameterize ([plot-y-label      "Phase [deg]"]
                     [plot-x-transform  log-transform]
                     [plot-x-ticks      (log-ticks)])
        
        (plot (list ;(axes)
               (tick-grid)
               (function
                (λ (w) (* 180 (/ 1 pi)
                          (let ((f1 (angle (tfs1 w))))
                            
                            ;f1
                            (unwarp-angle f1 w 1)
                            
                            )))                        
                
                (/ 1 1000) 500
                #:label "tf1")
               (function
                (λ (w) (* 180 (/ 1 pi)
                          (let ((f1 (angle (tfs2 w))))
                            
                            ;f1
                            (unwarp-angle f1 w 2)
                            
                            )))              
                
                (/ 1 1000) 500
                #:color 3 
                #:label "tf2")
               (function (λ (x) 0) #:color 0 #:style 'dot)
               (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
      
      (make-space-line 10))
     
     )
    
    (set! had-neg-values-1 #f)
    (set! passed-180-1 #f)
    (set! passed-360-1 #f)
    
    (set! had-neg-values-2 #f)
    (set! passed-180-2 #f)
    (set! passed-360-2 #f)
    
    ))








(define evolve
  (let ((last-value '()))
    
    (lambda (block1)
      
      (newline)
      (display-mode-nil!)
      
      (let* ((total-tfs-value1 (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (get-simplified-block-value block1))))))
             (tfs-value-evaluation1 (eval total-tfs-value1 anchor)))
        
        (define (tfs1 w) (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                (fw1-func w) 
                                                (fw2-func w)
                                                (fw3-func w) 
                                                (fw4-func w)))
        
        
        
        (bode-plot-parameters)
        
        (if (null? last-value)
            
            (begin (set! last-value total-tfs-value1)
                   
                   (for-each
                    displayln
                    
                    (list (parameterize ([plot-y-label      "Magnitude (abs)"]
                                         [plot-y-transform  log-transform]
                                         [plot-y-ticks      (log-ticks)]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)])
                            
                            (plot (list ;(axes)
                                   (tick-grid)
                                   (function (λ (w) (magnitude (tfs1 w)))
                                             (/ 1 1000) 500
                                             #:label "tf")
                                   (function (λ (x) 1) #:color 0 #:style 'dot)
                                   (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                          
                          
                          (parameterize ([plot-y-label      "Phase [deg]"]
                                         [plot-x-transform  log-transform]
                                         [plot-x-ticks      (log-ticks)])
                            
                            (plot (list ;(axes)
                                   (tick-grid)
                                   (function (λ (w) (* 180 (/ 1 pi)
                                                       (angle (tfs1 w))))
                                             (/ 1 1000) 500
                                             #:label "tf1")
                                   (function (λ (x) 0) #:color 0 #:style 'dot)
                                   (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                          
                          (make-space-line 10))
                    ))
            
            
            (let* ((temp last-value)
                   (temp-value-evaluation (eval temp anchor)))
              
              (define (tfs-temp w) (temp-value-evaluation (make-rectangular 0 w) 
                                                          (fw1-func w) 
                                                          (fw2-func w)
                                                          (fw3-func w) 
                                                          (fw4-func w)))
              
              (set! last-value total-tfs-value1)
              
              (for-each
               displayln
               
               (list (parameterize ([plot-y-label      "Magnitude (abs)"]
                                    [plot-y-transform  log-transform]
                                    [plot-y-ticks      (log-ticks)]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (function (λ (w) (magnitude (tfs1 w)))
                                        (/ 1 1000) 500
                                        #:label "tf")
                              (function (λ (w) (magnitude (tfs-temp w)))
                                        (/ 1 1000) 500
                                        #:color 0 #:style 'dot
                                        #:label "last-tf")
                              (function (λ (x) 1) #:color 0 #:style 'dot)
                              (function (λ (x) 0.707) #:color 0 #:style 'dot))
                             
                             ))
                     
                     (parameterize ([plot-y-label      "Phase [deg]"]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs1 w))))
                                        (/ 1 1000) 500
                                        #:label "tf1")
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs-temp w))))
                                        (/ 1 1000) 500
                                        #:color 0 #:style 'dot
                                        #:label "last-tf")
                              (function (λ (x) 0) #:color 0 #:style 'dot)
                              (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                     
                     (make-space-line 10))
               ))
            ))    
      )))









(define (tune block condition w)
  
  (define total-tfs-value '())
  (define m1 0)
  (define m2 0)
  
  (set! display-mode 'nil)
  
  ;change:
  ;(let ((simplification-result (simplify block)))
  ;(display simplification-result)
  
  ;change:
  ;(if (void? simplification-result)
  
  ;change:
  (newline)
  
  (set! total-tfs-value (cons 'λ (cons '(y) (list (ratio-to-list (get-simplified-block-value block))))))
  ;ratio-to-list-lite
  ;change:
  #|
        (begin (newline)
               (display "- cached simplification")
               (newline)
               (newline)
               (set! total-tfs-value (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list-lite simplification-result)))))))
        |#
  
  
  (if (= (length condition) 3)
      
      (if (eq? (car condition) '=)
          
          (let ((target (caddr condition)))    
            
            (cond ((eq? (cadr condition) 'AR)
                   
                   (set! m1 (newton-meth-for-solv-eq                                         
                             (λ (y) (- (magnitude ((eval (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list total-tfs-value)))
                                                               (make-rectangular 0 w) 
                                                               (fw1-func w) 
                                                               (fw2-func w)
                                                               (fw3-func w)
                                                               (fw4-func w)) anchor) y)) target))
                             1))
                   
                   (define tfs-value-evaluation1 (eval (list (cons 'λ (cons '(y) (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (caddr total-tfs-value))))))) m1) anchor))
                   
                   ;(newline)
                   (display "y = ")
                   (display (round-decimal m1 3)) 
                   (newline)
                   (newline)
                   (newline)
                   
                   
                   (bode-plot-parameters)
                   
                   (for-each
                    displayln
                    
                    (list 
                     
                     (parameterize ([plot-y-label      "Magnitude (abs)"]
                                    [plot-y-transform  log-transform]
                                    [plot-y-ticks      (log-ticks)]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (point-label (vector (* 1.0 w) (* 1.0 target)))
                              (function (λ (w) (magnitude (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                                                 (fw1-func w) 
                                                                                 (fw2-func w)
                                                                                 (fw3-func w)
                                                                                 (fw4-func w))))
                                        (/ 1 1000) 500)
                              (function (λ (x) 1) #:color 0 #:style 'dot)
                              (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                     
                     
                     (parameterize ([plot-y-label      "Phase [deg]"]
                                    [plot-x-transform  log-transform]
                                    [plot-x-ticks      (log-ticks)])
                       
                       (plot (list ;(axes)
                              (tick-grid)
                              (function (λ (w) (* 180 (/ 1 pi)
                                                  (angle (tfs-value-evaluation1 (make-rectangular 0 w) 
                                                                                (fw1-func w) 
                                                                                (fw2-func w)
                                                                                (fw3-func w)
                                                                                (fw4-func w)))))
                                        (/ 1 1000) 500)
                              
                              (function (λ (x) 0) #:color 0 #:style 'dot)
                              (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                     
                     
                     (make-space-line 10))
                    
                    ))
                  
                  
                  
                  ((eq? (cadr condition) 'ph)
                   
                   (set! m2 (newton-meth-for-solv-eq
                             (λ (y) (- (* 180 (/ 1 pi) (angle ((eval (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list total-tfs-value))) (make-rectangular 0 w) 
                                                                           (fw1-func w) 
                                                                           (fw2-func w)
                                                                           (fw3-func w)
                                                                           (fw4-func w)) anchor) y))) target))
                             1))
                   
                   (define tfs-value-evaluation2 (eval (list (cons 'λ (cons '(y) (list (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (caddr total-tfs-value))))))) m2) anchor))                     
                   
                   ;(newline)
                   
                   (if (eq? m2 #f)
                       (begin (display "no such y")
                              (newline)
                              (newline))
                       
                       (begin (display "y = ")
                              (display (round-decimal m2 3))                                
                              (newline)
                              (newline)
                              (newline)
                              
                              
                              
                              (bode-plot-parameters)
                              
                              (for-each
                               displayln
                               
                               (list 
                                
                                (parameterize ([plot-y-label      "Magnitude (abs)"]
                                               [plot-y-transform  log-transform]
                                               [plot-y-ticks      (log-ticks)]
                                               [plot-x-transform  log-transform]
                                               [plot-x-ticks      (log-ticks)])
                                  
                                  (plot (list ;(axes)
                                         (tick-grid)
                                         (function (λ (w) (magnitude (tfs-value-evaluation2 (make-rectangular 0 w) 
                                                                                            (fw1-func w) 
                                                                                            (fw2-func w)
                                                                                            (fw3-func w)
                                                                                            (fw4-func w))))
                                                   (/ 1 1000) 500)
                                         (function (λ (x) 1) #:color 0 #:style 'dot)
                                         (function (λ (x) 0.707) #:color 0 #:style 'dot))))
                                
                                
                                (parameterize ([plot-y-label      "Phase [deg]"]
                                               [plot-x-transform  log-transform]
                                               [plot-x-ticks      (log-ticks)])
                                  
                                  (plot (list ;(axes)
                                         (tick-grid)
                                         (point-label (vector (* 1.0 w) (* 1.0 target)))
                                         (function (λ (w) (* 180 (/ 1 pi)
                                                             (angle (tfs-value-evaluation2 (make-rectangular 0 w) 
                                                                                           (fw1-func w) 
                                                                                           (fw2-func w)
                                                                                           (fw3-func w)
                                                                                           (fw4-func w)))))
                                                   (/ 1 1000) 500)
                                         (function (λ (x) 0) #:color 0 #:style 'dot)
                                         (function (λ (w) (- 180)) (/ 1 1000) 500  #:color 3 #:style 'dot))))
                                
                                (make-space-line 10))
                               
                               ))
                       ))
                  
                  
                  (else (error "Unrecognized condition"))))
          
          (error "Unrecognized condition"))
      
      (error "Unrecognized condition"))
  
  ;)
  
  )








(define (nyquist block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs w) (tfs-value-evaluation (make-rectangular 0 w) 
                                          (fw1-func w) 
                                          (fw2-func w)
                                          (fw3-func w)
                                          (fw4-func w)))
    
    
    (let* ((AR-low (magnitude (tfs (/ 1 1000))))
           (AR-high (magnitude (tfs 500)))
           (AR-min-value (min AR-low AR-high 0.707 0.099))
           (AR-100 (magnitude (tfs 100)))
           (AR-001 (magnitude (tfs 0.01)))
           
           
           ; bandwidth
           (b-threshold (min (max AR-low AR-high) 0.707 cheb-threshold))
           
           (fb (λ (w) (- (magnitude (tfs w)) b-threshold)))
           
           (wb-init (half-interval-method                            
                     fb
                     0.001 500))
           
           (wb1 (if (not (eq? wb-init #f))
                    (half-interval-method                            
                     fb
                     0.001 (+ wb-init 0.001))
                    #f))
           
           (wb2 (if (not (eq? wb1 #f))
                    (half-interval-method                            
                     fb
                     (+ wb-init 0.001) 300)
                    #f))
           
           
           ;gain margin
           (fc-g (λ (w) (- (let ((f1 (angle (tfs w))))
                             
                             (unwarp-angle-simple-2 f1 w 1)
                             ;(unwarp-angle f1 w 1)
                             
                             ) (- pi))))
           
           (wc-g (half-interval-method                            
                  fc-g
                  0.001 500))
           
           (gain-margin (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                            (begin ;(display wc-g)
                              (/ 1 (magnitude (tfs wc-g))))
                            (begin ;(display wc-g)
                              #f)))
           
           
           ; phase margin
           (fc-p (λ (w) (- (magnitude (tfs w)) 1)))
           
           (wc-p (half-interval-method                            
                  fc-p
                  0.001 500))
           
           
           (f1 (if (not (eq? wc-p #f)) (angle (tfs wc-p)) #f))
           (phase-margin (if (not (eq? wc-p #f))
                             (+ 180 
                                (* 180 (/ 1 pi)
                                   
                                   ; no searching is being done here, just one substitution
                                   ;#|
                                   (if (and (eq? had-neg-values-1 #t) (> f1 0)) ; old:(> f1 (/ pi 2))
                                       (- f1 (* 2 pi))
                                       (if (< f1 0)
                                           (begin
                                             (set! had-neg-values-1 #t)
                                             f1)
                                           f1))
                                   ;|#                                
                                   
                                   ))
                             #f))
           
           )
      
      
      
      
      (set! cheb-threshold 1000)
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      (plot-height 400)
      (plot-title "Nyquist diagram")
      (plot-x-label "Real part")
      (plot-y-label "Imaginary part")
      
      
      (for-each 
       displayln
       
       (list 
        (parameterize (;[plot-x-transform cbrt-transform]
                       ;[plot-y-transform cbrt-transform])
                       [plot-x-transform  (stretch-transform -1.5 1.5 20)]
                       [plot-y-transform  (stretch-transform -1.5 1.5 20)])
          (plot (list 
                 (axes)
                 (tick-grid)
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             0.0000001 10
                             #:x-min -10 #:x-max 10 #:y-min -10 #:y-max 10
                             #:color 3
                             #:label "0<=w<oo"
                             )
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             10 (expt 10 3)
                             ;#:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                             #:color 3
                             )
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             -10 0.0000001
                             ;#:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                             #:color 2
                             #:label "-oo<w<=0"
                             )
                 (parametric (λ (w) (vector (real-part (tfs w))
                                            (imag-part (tfs w))
                                            ))
                             (- (expt 10 3)) -10
                             ;#:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
                             #:color 2
                             )
                 (points (list (vector -1 0))
                         ;#:sym 'fullcircle2
                         #:sym 'times
                         #:color 1
                         )
                 
                 ; error bars are vertical - can't be used here
                 #|
               (if (not (or (eq? wc-g #f) (eq? wc-g 0)))
                          (error-bars (list 
                                       (vector wc-g
                                               (/ (+ 1 (magnitude (tfs wc-g))) 2)
                                               (/ (- 1 (magnitude (tfs wc-g))) 2))))
                          '())
               |#
                 
                 )))
        
        
        
        
        (make-space-line 10)))
      
      
      
      
      (set! had-neg-values-1 #f)
      (set! passed-180-1 #f)
      (set! passed-360-1 #f)
      
      
      
      
      (cond ((> (/ AR-low AR-high) 2)
             (display "Low-pass filter:")
             (newline)
             (newline))    
            ((> (/ AR-high AR-low) 2)
             (display "High-pass filter:")
             (newline)
             (newline))
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             (display "Band-pass filter:")
             (newline)
             (newline)))    
      
      
      
      (cond ((eq? wb1 #f) 
             (display "bandwidth    = (0,inf)"))
            ((eq? wb2 #f)
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]"))))
            ((> (round-decimal wb2 2) (round-decimal wb1 2))
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))
                 (begin
                   (display "bandwidth    = [")
                   (display (round-decimal wb1 2))
                   (display ",")
                   (display (round-decimal wb2 2))
                   (display "] [rad/s]"))))
            (else
             (if (eq? (round-decimal wb1 2) 0)
                 (begin 
                   (display "bandwidth    = (0,inf)"))
                 (begin
                   (display "bandwidth    = (0,")
                   (display (round-decimal wb1 2))
                   (display "] [rad/s]")))))
      (display " - thresh.: ")
      (display (round-decimal b-threshold 3))
      (newline)
      
      
      
      ; roll-off:
      
      (cond ((> (/ AR-low AR-high) 2)
             
             ;Low-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))    
            
            ((> (/ AR-high AR-low) 2)
             
             ;High-pass filter:
             (define roll-off (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (display "roll-off     = ")
             (display roll-off)
             (display " (log(ΔAR)/log(Δw))")
             (newline))
            
            ((and (< AR-low 0.05) (< AR-high 0.05) (not (eq? wb2 #f)))
             
             ;Band-pass filter:         
             (define roll-off-low (round-decimal (exact->inexact (/ (log (/ AR-low AR-001)) (log (/ 0.001 0.01)))) 3))
             (define roll-off-high (round-decimal (exact->inexact (/ (log (/ AR-100 AR-high)) (log (/ 100 500)))) 3))
             (display "roll-off (low)  = ")
             (display roll-off-low)
             (display " (log(ΔAR)/log(Δw))")
             (newline)
             (display "roll-off (high) = ")
             (display roll-off-high)
             (display " (log(ΔAR)/log(Δw))")
             (newline)))  
      
      
      
      (if (not (eq? gain-margin #f))
          (begin (display "gain margin  = ")
                 (display (round-decimal gain-margin 2))
                 (newline))
          (begin (display "gain margin  = inf")
                 (newline)))
      
      
      
      (if (not (eq? phase-margin #f))
          ;(if (> phase-margin 0)
          (begin (display "phase margin = ")
                 (display (round-decimal phase-margin 2))
                 (display " [deg]")
                 (newline)
                 (newline))
          ;   (begin (newline)))
          (begin (display "phase margin = inf")
                 (newline)
                 (newline)))
      
      ))
  
  )












; //////////   H. t-domain response  //////////


(define tmax 30)




; Talbot algorithm for Laplace inversion




(define shift 0.0)




(define (Talbot F t N)
  
  ; stepsize initialization:
  (let ((h (/ (* 2 pi) N)));
    
    
    (when (= t 0) 
      (display "error - Inverse transform can not be calculated for t=0 - TALBOT"))
    
    
    
    ; loop is evaluating the Laplace inversion at each point theta which is based on the trapezoidal rule:
    (define (loop k ans)
      (if (> k N)
          
          (* (/ h (* 2 (make-rectangular 0 1) pi)) ans)
          
          (let* ((theta (+ (* -1 pi) (* (+ k 0.5) h)))
                 
                 (z (+ shift (* (/ N t) (+ (* 0.5017 theta (cot (* 0.6407 theta))) -0.6122 
                                           (* 0.2645 (make-rectangular 0 1) theta)))))
                 
                 (dz (* (/ N t) (+ (* -0.5017 0.6407 theta (expt (csc (* 0.6407 theta)) 2)) 
                                   (* 0.5017 (cot (* 0.6407 theta))) (* 0.2645 (make-rectangular 0 1))))))
            
            
            (loop (+ k 1) (+ ans (* (exp (* z t)) (F z) dz))))))
    
    (loop 0 0)))




; the trig functions cot(phi) and csc(phi):

(define (cot phi) (/ 1.0 (tan phi)))
(define (csc phi) (/ 1.0 (sin phi)))









; L{d(t)}=1

(define (impulse block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs s) (tfs-value-evaluation s 
                                          0 
                                          0
                                          0
                                          0))    
    
    
    
    (for-each
     displayln
     
     (list 
      
      (parameterize ([plot-title "t - f diagram"]
                     [plot-width    400]
                     [plot-height   200]
                     [plot-x-label  "t"]
                     [plot-y-label  "f"])
        
        (plot (list
               ;(axes)
               (tick-grid)
               (function (λ (t) (real-part (Talbot tfs t 150))) (/ 1 1000) tmax)
               (function (λ (t) -10.0) (/ 1 1000) tmax #:color 0 #:style 'dot)
               (function (λ (t) 10.0) (/ 1 1000) tmax #:color 0 #:style 'dot)
               (function (λ (t) 0) (/ 1 1000) tmax #:color 0 #:style 'dot))))
      
      
      (make-space-line 10)))))









(define (impulse-deriv block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs s) (tfs-value-evaluation s 
                                          0 
                                          0
                                          0
                                          0)) 
    
    
    
    (for-each
     displayln
     
     (list
      
      (parameterize ([plot-title "t - df/dt diagram"]
                     [plot-width    400]
                     [plot-height   200]
                     [plot-x-label  "t"]
                     [plot-y-label  "df/dt"])
        
        (plot (list
               ;(axes)
               (tick-grid)
               (function (deriv (λ (t) (real-part (Talbot tfs t 150)))) (/ 1 1000) tmax)
               (function (λ (t) -10.0) (/ 1 1000) tmax #:color 0 #:style 'dot)
               (function (λ (t) 10.0) (/ 1 1000) tmax #:color 0 #:style 'dot))))
      
      
      (make-space-line 10)))))










(define (trajectory block)
  
  (let* ((total-tfs-value (get-total-tfs-value block))
         
         ; it must be evaluated here and not inside the function of the plot procedure,
         ; so that zooming on the figure is plausible:
         (tfs-value-evaluation (eval total-tfs-value anchor))) ;tfs-value-evaluation is a function of s and w
    
    (define (tfs s) (tfs-value-evaluation s 
                                          0 
                                          0
                                          0
                                          0)) 
    
    
    
    (let ((t-list (cdr (build-list 1000 (λ (x) (* x 0.01)))))
          (f (λ (t) (real-part (Talbot tfs t 150)))))
      
      (for-each
       displayln
       
       (list 
        (parameterize ([plot-title "df/dt - f diagram"]
                       [plot-width    400]
                       [plot-height   200]
                       [plot-x-label  "df/dt"]
                       [plot-y-label  "f"])
          
          (plot (list
                 (axes)
                 (tick-grid)
                 (point-label (vector (f 0.01) ((deriv f) 0.01)) "initial value" #:anchor 'top-left)
                 (lines (map vector (map f t-list) (map (deriv f) t-list))))))
        
        (make-space-line 10))))))










; stp<40

(define (step block gain)
  
  (define total-tfs-value '())
  
  (newline)
  (set! display-mode 'nil)
  
  
  (let ((num (get-numer (get-simplified-block-value block)))
        (den (get-denom (get-simplified-block-value block))))
    
    
    #|
          (displayln (get-value (car tfs)))
          (newline)
          (displayln num)
          (displayln den)
          |#          
    
    
    ; adding the step tf ([gain]/[1 0]):
    
    (let ((new-num (mul num (make-poly-dense 's (list gain))))
          (new-den (mul den (make-poly-dense 's (list 1 0)))))
      
      #|
            (newline)
            (displayln new-num)
            (displayln new-den)
            |#
      
      
      ; just for displaying:
      (ratio-to-list (get-simplified-block-value block))
      (newline)
      (displayln "with gain:")
      (newline)
      (ratio-to-list (make-ratio new-num new-den))
      
      #|
            (newline)
            (displayln (make-ratio new-num new-den))
            (newline)
            (newline)
            (ratio-to-list (make-ratio new-num new-den) 'do-not-display)
            (newline)
            (displayln 'aaaaaaa)
            |#
      
      (set! total-tfs-value (cons 'λ (cons '(s fw1 fw2 fw3 fw4) (list (ratio-to-list (make-ratio new-num new-den) 'do-not-display)))))
      ))
  
  
  (define tfs-value-evaluation (eval total-tfs-value anchor))
  (define (tfs s) (tfs-value-evaluation s
                                        0
                                        0
                                        0
                                        (fw4-func s)))
  
  
  (for-each
   displayln
   
   (list 
    
    (parameterize ([plot-title "t - f diagram"]
                   [plot-width    400]
                   [plot-height   200]
                   [plot-x-label  "t"]
                   [plot-y-label  "f"])
      
      (plot (list
             (axes)
             (tick-grid)
             (function (λ (t) (real-part (Talbot tfs t 150))) (/ 1 1000) tmax)
             (function (λ (t) -10.0) (/ 1 1000) tmax #:color 0 #:style 'dot)
             (function (λ (t) 10.0) (/ 1 1000) tmax #:color 0 #:style 'dot)
             (function (λ (t) gain) (/ 1 1000) tmax #:color 0 #:style 'dot)
             )))
    
    
    (make-space-line 10)))
  
  )




