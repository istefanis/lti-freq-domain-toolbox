lti-freq-domain-toolbox
=======================

A library written in [Racket](https://racket-lang.org), which lets Control Systems engineers study LTI (linear time-invariant) dynamical systems - from PID controllers to multipart systems of complex design.

It can perform tasks such as:
* the analytical computation of the overall transfer function (tf) of a system modeled by interconnected elements in the s-domain
* the generation of its Bode and Nyquist plots
* the numerical computation of its time domain response

> *Predefined circuits are also included: controllers (PI, PD and PID), filters (Chebyshev type I), delay components (Padé) etc.*

> *This is a command-line tool. For a GUI one, check the newer [Controllio](https://github.com/istefanis/controllio) app.*

## Known issues & limitations

* bandwidth computation in more complicated cases (ex. double band pass filters)
* phase adjustment in some cases of steep phase shifts

## Installation

1. Get Racket from: https://racket-lang.org and install

2. Run DrRacket, open: ```File``` -> ```Install Package``` and paste: ```lti-freq-domain-toolbox``` in the "Package Source" field, to automatically download the package

## Usage

1. Type in the definitions (left/upper part of the screen):
   ```
   #lang racket
   (require lti-freq-domain-toolbox)
   ```
   to load the package, and run with ```Ctrl+R```

2. Type in the interactions / REPL (right/lower part of the screen):
   ```
   (run-examples)
   ```
   and press ```Enter``` to test all the examples of "main.rkt", and get a feeling of how the program works

## Documentation

### Online

* The documentation is available online [here](https://docs.racket-lang.org/lti-freq-domain-toolbox/index.html)

### Locally (after installation)

* In DrRacket, open: ```Help``` -> ```Racket Documentation``` and paste: ```lti-freq-domain-toolbox``` in the "search manuals" field

## Plot examples

![plot examples](https://github.com/iastefan/lti-freq-domain-toolbox/blob/master/plots.png)

## Code dependencies diagram

![dependencies](https://github.com/iastefan/lti-freq-domain-toolbox/blob/master/dependencies.png)


## License

Copyright (C) 2014-2023  Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU General Public License Version 3 (GPLv3). 
The GPLv3 license text is included in the "COPYING.TXT" file.

See http://www.gnu.org/licenses/gpl-3.0.txt for more information.
