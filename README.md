lti-freq-domain-toolbox
=======================

The lti-freq-domain-toolbox is a collection of functions written in [Racket](http://racket-lang.org), which lets Control Systems engineers study LTI (linear time-invariant) dynamical systems - from PID controllers to more complex systems.

It can perform tasks such as:
* computation of the overall transfer function (tf) of a system that is modeled by interconnected tfs in the s-domain
* generation of its Bode and Nyquist plots
* numerical computation of its time domain response

=======================

To use:

1. Get Racket from: http://racket-lang.org and install

2. Run DrRacket, open: ```File``` -> ```Install Package``` and paste: ```lti-freq-domain-toolbox``` in the "Package Source" field, to automatically download the package

3. Type in the definitions (left/upper part of the screen):
   ```
   #lang racket
   (require lti-freq-domain-toolbox)
   ```
   to load the package, and run with ```Ctrl+R```

4. Type in the interactions / REPL (right/lower part of the screen):
   ```
   (run-examples)
   ```
   and press ```Enter``` to test all the examples of "main.rkt", and get a feeling of how the program works.

5. To read the documentation of the package, open: ```Help``` -> ```Racket Documentation``` and paste: ```lti-freq-domain-toolbox``` in the "search manuals" field
   
=======================

Some plot examples:

![plot examples](https://github.com/iastefan/lti-freq-domain-toolbox/blob/master/plots.png)


=======================

Code dependencies diagram:

![dependencies](https://github.com/iastefan/lti-freq-domain-toolbox/blob/master/dependencies.png)


=======================

lti-freq-domain-toolbox (c) 2014-2020 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the "COPYING.LESSER" file.

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
