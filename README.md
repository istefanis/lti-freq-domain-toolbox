lti-freq-domain-toolbox
=======================

lti-freq-domain-toolbox is a collection of functions for lti dynamical systems written in [Racket](http://racket-lang.org) that deal with tasks as:
* computation of the transfer function (tf) of a system modeled by interconnected tf blocks 
in the s-domain
* generation of bode & nyquist plots
* arithmetic computation of its time-domain response.

=======================
#####To use:

1. Get Racket from: http://racket-lang.org and install

2. Run DrRacket, open: ```File``` -> ```Install Package``` and paste: ```lti-freq-domain-toolbox``` in the "Package Source" field, to automatically download the package

3. Type in the definitions (left/upper part of the screen):
   ```
   #lang racket
   (require lti-freq-domain-toolbox)
   ```
   to load the package, and run with ```Ctrl+R```

5. Type in the interactions (right/lower part of the screen):
   ```
   (run-examples)
   ```
   and press ```Enter``` to test all the examples of "main.rkt".

Read "USER GUIDE.txt" for more guidelines.

=======================
#####Some plot examples:
![plot examples](https://github.com/iastefan/lti-freq-domain-toolbox/blob/master/plots.png)


=======================
lti-freq-domain-toolbox (c) 2014 Ioannis Stefanis <iastefan@outlook.com>

lti-freq-domain-toolbox is distributed under the GNU Lesser General Public License Version 3 (LGPLv3). 
The LGPLv3 license text is included in the file "LICENSE_LESSER.txt".

See http://www.gnu.org/licenses/lgpl-3.0.txt for more information.
