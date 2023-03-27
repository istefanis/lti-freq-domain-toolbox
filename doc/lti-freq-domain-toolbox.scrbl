#lang scribble/manual

@(require (for-label racket))


@title{lti-freq-domain-toolbox}
@author{@(author+email "Ioannis Stefanis" "iastefan@outlook.com")}


@defmodule[lti-freq-domain-toolbox]


The @bold{lti-freq-domain-toolbox} is a collection of functions written in Racket, which lets Control Systems engineers study LTI (linear time-invariant) dynamical systems - from PID controllers to more complex systems.

It can perform tasks such as:

@itemlist[@item{computation of the overall transfer function (tf) of a system that is modeled by interconnected tfs in the s-domain}
          @item{generation of its Bode and Nyquist plots}
          @item{numerical computation of its time domain response}]

Known issues & limitations: check the package's README.md file.

@table-of-contents[]







@;//////////   Section 1   //////////


@section[#:tag "representation"]{Representing a dynamical system}


@subsection[#:tag "introduction"]{Introduction}

A linear time-invariant (LTI) dynamical system is represented as a circuit of interconnected elements: transfer functions (tfs), adders and blocks.

A block of elements is a means for achieving abstraction. Circuit elements such as transfer functions (tfs), adders and even other blocks
can be "stored" inside a block, and form a whole that can be handled as an element itself.

@subsection[#:tag "elements"]{Elements}




@defproc*[([(block) block?]
           [(block [parent-block block?]) block?])]{
                 
 @margin-note{
  @bold{Implementation assumption:}
 
  @italic{Each block of elements has only one input (another tf, adder or block) and multiple outputs.}

  @italic{Multiple inputs can be achieved by adding in front of it an adder.}
 }

 A new block is defined using the @racket[block] procedure. A parent block inside which the block is stored can be optionally specified.

 @racketblock[(define new-block1 (block))
              (define new-block-inside-block-a (block a))]

 Notes:
 @itemlist[@item{The four blocks a, b, c, d are predefined for speed}
           @item{Build new blocks only when needed and if there are elements to be stored inside}
           @item{If all the elements inside a block are @bold{not} connected, the value of the block's overall tf is that of the latest tf stored}
           @item{Re-run the program before defining new circuits in already used blocks}]
}


 

@defproc[(tf [nom-coeffs (listof real?)] [denom-coeffs (listof real?)] [b1 block?])
         tf?]{

 @margin-note{
  @bold{Implementation assumption:}
 
  @italic{Each tf has only one input (another tf, adder or block) and multiple outputs.}

  @italic{Multiple inputs can be achieved by adding in front of it an adder.}
 }

 To define a @bold{transfer function}, the lists of the coefficients of its nominator and denominator polynomials, as well as
 the block it belongs to must be specified, using the @racket[tf] procedure.

 @racketblock[(define new-tf1 (tf '(4 3 6) '(2 1) b))]
}




@defproc[(adder [b1 block?])
         adder?]{

 @margin-note{
  @bold{Implementation assumption:}
 
  @italic{Each adder has multiple inputs and multiple outputs}
 }

 An adder is an element used to add multiple input signals, and provide one or multiple outputs.
 It can be placed in front of a block or tf, to provide them with multiple inputs. An adder is created and placed inside a block using the
 @racket[adder] procedure.

 @racketblock[(define tf1 (tf '(1 0) '(2 1 0) b))
              (define tf2 (tf '(5 0) '(5 1 0) b))
              (define new-adder1 (adder b))
              (connect tf1 new-adder1)
              (connect tf2 new-adder1)]
}




@subsection[#:tag "connecting-elements"]{Connecting elements}

Elements (tfs, adders and blocks) are connected @bold{serially} using the @racket[connect] procedure:

@defproc[(connect [e1 element?] [e2 element?])
         element?]{

 @racketblock[(connect tf1 tf2)             
              (connect tf2 adder1)
              (connect tf3 adder1)
              (connect a b)]

 The connection of two elements has no effect if they are not elements of the same block.

 Complex connection designs can be achieved by defining all serial connections between elements.
}





@;//////////   Section 2   //////////


@section[#:tag "plot-functions"]{Plot functions}

@margin-note{ 
 @italic{All plot functions listed below take as inputs blocks, not transfer functions.}
}

@subsection[#:tag "frequency-domain"]{Frequency domain}

@defproc[(bode [b1 block?])
         void?]{

 The @racket[bode] function takes as input a block. It computes and prints its overall tf, Bode magnitude and phase plots,
 and characteristic numbers.

 @racketblock[(bode (pd-controller 5 8 a))]

 This example works because the @racket[pd-controller] function is defined so that it returns as result the block in which it is installed in.
}




@defproc[(compare [b1 block?] [b2 block?])
         void?]{

 The @racket[compare] function takes as inputs two @bold{different} blocks. It computes and prints comparatively their overall tfs,
 Bode magnitude and phase plots.

 @racketblock[(define c1 (block))
              (define c2 (block))
              (define tf1 (tf '(1) '(1 0 1) c1))
              (define tf2 (tf '(5) '(1 0 1) c2))
              (compare c1 c2)
              (compare (pid-controller 6 7 3 a) (pi-controller 7 8 b))]

 The second example works because the @racket[pid-controller] and @racket[pi-controller] functions return as result the block they are installed in.
}




@defproc[(evolve [b1 block?])
         void?]{

 The @racket[evolve] function takes as input a block. It computes and prints its overall tf and its Bode magnitude and phase plots,
 storing the results. The next time it is called, it prints the current block's Bode plot on top of the last one stored. 

 @racketblock[(evolve (pid-controller 6 7 3 a))
              (evolve (pid-controller 5 20 4 a))]
}




@defproc[(tune [b1 block?] [condition list?] [freq positive?])
         void?]{

 The @racket[tune] function can be used in order to compute the value of a single system's parameter, so that the system achieves a specific performance.
 It takes as inputs a block, an equality condition regarding the @bold{amplitude response AR} or the @bold{phase shift ph},
 and the frequency at which this equality condition must be satisfied.

 The function prints the value of the parameter computed, and the Bode magnitude and phase plots of the system.

 @racketblock[(tune (pi-controller 5 'y a) '(= AR 140) 0.01)
              (tune (pi-controller 5 'y a) '(= ph -50) 0.01)]
}




@defproc[(nyquist [b1 block?])
         void?]{

 The @racket[nyquist] function takes as input a block. It computes and prints its overall tf, Nyquist plot, and characteristic numbers.

 @racketblock[(define tf1 (tf '(1) '(1 1 1) a))
              (nyquist a)]
}




@subsection[#:tag "time-domain"]{Time domain}

@defproc[(step [b1 block?] [gain real?])
         void?]{

 The @racket[step] function takes as inputs a block and a gain number. It computes and prints the block's tf, the block's tf with gain added,
 and the step time response of the system.

 @racketblock[(step (sine a) 5)
              (define tf2 (tf '(1) '(0.3 0.1 1) b))
              (step b 5)]
}




@defproc[(impulse [b1 block?])
         void?]{

 The @racket[impulse] function takes as input a block. It computes and prints the block's tf, and its impulse time response.

 @racketblock[(impulse (sine a))
              (define tf2 (tf '(1) '(0.3 0.1 1) b))
              (impulse b)]
}




@defproc[(trajectory [b1 block?])
         void?]{

 The @racket[trajectory] function takes as input a block. It computes and prints the block's tf,
 and its time domain df(t)/dt - f(t) trajectory plot.

 @racketblock[(trajectory (sine a))
              (define tf2 (tf '(1) '(0.3 0.1 1) b))
              (trajectory b)]
}






@;//////////   Section 3   //////////


@section[#:tag "predefined-circuits"]{Predefined circuits}

Circuits can be defined so that they return as result the block they are stored in. Therefore, they can be used directly as inputs to functions:

@margin-note{ 
 @italic{All plot functions listed below take as inputs blocks, not transfer functions.}
}
   
@racketblock[(bode (feedback-loop-test1 a))]

instead of:

@racketblock[(feedback-loop-test1 a)
             (bode a)]

That isn't the case when using tfs, because a tf does not return the block in which it is installed.



@subsection[#:tag "Basic components"]{Basic components}

@defproc[(integrator [b1 block?])
         block?]

@racketblock[(bode (integrator a))]




@defproc[(sine [b1 block?])
         block?]

@racketblock[(bode (sine a))]




@defproc[(phase-delay-circuit [b1 block?])
         block?]

@racketblock[(bode (phase-delay-circuit a))]




@subsection[#:tag "ControllerS"]{Controllers}

The PI, PD and PID controllers are defined by specifying the proportional (Kp), integral (Ki) and/or derivative (Kd) gains respectively,
as well as the block to be stored in.

@defproc[(pi-controller [Kp real?] [Ki real?] [b1 block?])
         block?]

@racketblock[(bode (pi-controller 5 8 a))]




@defproc[(pd-controller [Kp real?] [Kd real?] [b1 block?])
         block?]

@racketblock[(bode (pd-controller 5 8 a))]




@defproc[(pid-controller [Kp real?] [Ki real?] [Kd real?] [b1 block?])
         block?]

@racketblock[(bode (pid-controller 5 8 4 a))]




@subsection[#:tag "Filters"]{Filters} 

A Chebyshev Type I filter is defined by specifying the polynomial order (n), ripple factor (e) and cutoff frequency (w0) parameters,
along with the block to be stored in.

@defproc[(chebyshev-type1 [n exact-positive-integer?] [e real?] [w0 positive?] [b1 block?])
         block?]

@racketblock[(bode (chebyshev-type1 6 1 1 a))]




@subsection[#:tag "Delay components"]{Delay components}

Delay can be modeled in two ways:
@itemlist[@item{by approximating it using a Padé polynomial}
          @item{by adding delay as a function f(w) of the frequency w in the overall block's tf (only for the s-domain)}]

The @racket[pade-delay] procedure adds a time delay component to a block, modeled by a Padé polynomial of order [6/6] approximating the function e^(-t).

@defproc[(pade-delay [t positive?] [b1 block?])
         block?]

@racketblock[(step (pade-delay 5 (sine a)) 1)]









@;//////////   Section 4   //////////


@section[#:tag "circuit-simplifications"]{Circuit simplifications}

In order to compute the overall transfer function (tf) of a system modeled by interconnected elements inside a block, a simplification of its structure
must be performed.

This simplification is performed by running a set of algorithms inside the block. Each algorithm may run more than once.
During each algorithm run, a simplification may or may not be performed. 

To be able to test and review this whole process, logging checkpoints have been implemented, with respective messages available to be displayed.


@subsection[#:tag "display-modes"]{Display modes}


@defproc[(set-logger-mode! [logger-mode symbol?]) void?]

@racketblock[(set-logger-mode! 'checkpoints)]

The @racket[set-logger-mode!] procedure can adjust the volume of simplifications-related messages displayed, in terms of the four following levels:

@itemlist[@item{@bold{'nil}: no messages are displayed (default)}
          @item{@bold{'algorithms}: only messages regarding the algorithms run are displayed}
          @item{@bold{'simplifications}: only messages regarding the algorithms run and the simplifications performed are displayed}
          @item{@bold{'checkpoints}: all messages at all checkpoints are displayed}]





