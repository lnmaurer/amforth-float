amforth-float -- a partly complete floating point wordset for AmForth,
available at <http://amforth.sourceforge.net>. The basic floating point
wordset is almost complete. The extension wordset is mostly incomplete. See
the check list below.

This is a personal project -- I am not involved with the development of
AmForth. However, this library has been included in amforth since version 4.2
(along with some related libraries like assembly versions of some of the words).

To use this library as is, you need to include some words when assembling
AmForth. Add the following to 'dict_appl.inc':

; needed for place-rec
.include "words/n_to_r.asm"
.include "words/n_r_from.asm"

; for maker
.include "dict_wl.inc"

; needed for recognizer
.include "words/get-recognizer.asm"
.include "words/set-recognizer.asm"

; other files for amforth-float
.include "words/d-equal.asm"
.include "words/2r_from.asm"
.include "words/2to_r.asm"

If you really want to avoid doing this, amforth-float can be modified to work
while still retaining much of its functionality; see previous versions of the
readme.

Once AmForth is installed on the microcontroller, upload

lib/ans94/core/postpone.frt
lib/ans94/core-ext/marker.frt
float.fth

in that order.

This floating point implementation is inspired by the IEEE 754-2008 binary32
format -- your standard single precision float. I adapted their format to fit
with the way amforth handles double length integers (henceforth 'doubles'),
with the most significant cell higher on the stack. The most significant bit
is the sign bit, followed by a byte for the exponent, and the remaining 23
bits for the significant. See the basics at:
http://en.wikipedia.org/wiki/Single_precision_floating-point_format

These floats are stored on the data stack -- not on their own separate stack.

ANS94 Floating-point words check list
Words with 'yes' next to them have been implemented. 
see <http://lars.nocrew.org/dpans/dpans12.htm>
or <http://galileo.phys.virginia.edu/classes/551.jvn.fall01/dpans12.htm>

Floating-Point words:

>FLOAT yes
D>F yes
F! yes
F* yes
F+ yes
F- yes
F/ yes
F0< yes
F0= yes
F< yes
F>D yes
F@ yes
FALIGN
FALIGNED
FCONSTANT yes
FDEPTH yes
FDROP yes
FDUP yes
FLITERAL yes
FLOAT+ yes
FLOATS yes
FLOOR yes
FMAX yes
FMIN yes
FNEGATE yes
FOVER yes
FROT yes
FROUND yes
FSWAP yes
FVARIABLE yes
REPRESENT

Floating-Point extension words:

DF!
DF@
DFALIGN
DFALIGNED
DFLOAT+
DFLOATS
F**
F. yes
FABS yes
FACOS
FACOSH
FALOG
FASIN
FASINH
FATAN
FATAN2
FATANH
FCOS
FCOSH
FE. yes
FEXP
FEXPM1
FLN
FLNP1
FLOG
FS. yes
FSIN
FSINCOS
FSINH
FSQRT
FTAN
FTANH
F~ yes
PRECISION yes
SET-PRECISION yes
SF! 
SF@
SFALIGN
SFALIGNED
SFLOAT+
SFLOATS

Other words in the library that you may find useful but aren't part of the standard:
-stack manipulation words:
  FNIP ( f1 f2 -- f2 )
  FTUCK ( f1 f2 -- f2 f1 f2 )
  NFSWAP ( f n -- n f )
  FNSWAP ( n f -- f n )
  NFOVER ( f n -- f n f )
  FNOVER ( n f -- n f n )
    (Note that The way to remember the FN/NF words is that FN/NF shows the top two items on the stack after executing the word. E.g. for FNSWAP, after execution the stack will be F N.)
-float comparison words:
  F= ( f1 f2 -- flag )
  F< ( f1 f2 -- flag )
  F> ( f1 f2 -- flag )
  F>= ( f1 f2 -- flag )
  F<= ( f1 f2 -- flag )
-float constants:
  F0 ( -- f-0 ) returns a float equal to zero
  F1 ( -- f-1 )
  F10 ( -- f-10 )
-float operators:
  F10^N  ( n -- f-10^n )
  CEIL ( f -- f ) returns the ceiling of f
-float <-> single conversion:
  S>F ( n -- f )
  F>S ( f -- n )
-float display:
  FSD. ( f -- ) prints the float in scientific notation using the dragon2 algorithm, which automatically decides how many digits to print
  FEE. ( f -- n) prints out the mantissa in engineering notation but leaves the exponent on the stack
  SI. ( n -- ) prints the SI prefixes for the exponent on the stack
    Note: these last two can be used together to print out numbers with SI prefixes for example:
      > 555.66e5 fee. si.
      55.57M ok
-double words
  D0= ( d -- flag )
  D= ( d1 d2 -- flag )
  D0< ( d -- flag )
  D10* ( d -- d*10 )
-other:
  FALSE ( -- n-0 )
  >= ( n1 n2 -- flag ) the flag is true if n1 >= n2 and false otherwise

There are many words in the library that are helper words and you'll probably have no use for.

All the files that make up this library are copyright 2013 by Leon Maurer.

  This program is free software; you can redistribute it and/or
  modify it under the terms of the GNU General Public License
  as published by the Free Software Foundation; using version 2
  of the License.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
