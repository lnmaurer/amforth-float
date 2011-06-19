amforth-float -- a partially complete floating point wordset for amforth,
available at <http://amforth.sourceforge.net>. The basic floating point
wordset is mostly complete. The extension wordset is mostly incomplete. See
the check list below.

This is just a personal project -- I am not involved with the development of
amforth. However, this library has been included in amforth since version 4.2
(along with some related libraries like assemly versions of some of the words).

The code requires 'd=', which isn't automatically included in amforth. So
either add it to 'dict_appl.inc' or use the code in 'float.fth'.

Having the word 'marker' isn't strictly needed, but it can be useful. If you
want it, add

.include "words/set-current.asm"
.include "words/set-order.asm"

to 'dict_appl.inc' when assembling amforth. Then upload 'lib/ans94/marker.frt'.
If you don't want to use marker, then comment it out in 'float.fth'.

Finially, if you want the ability to enter floating point numbers just like
integers, you need the recognizer to work; add

.include "words/n_to_r.asm"
.include "words/n_r_from.asm"

to 'dict_appl_core.inc'. Then upload 'lib/ans94/postpone.frt'. If you don't
want the recognizer, then comment the section out in 'float.fth'.

This floating point implementation is inspired by the IEEE 754-2008 binary32
format -- your standard single precision float. I adapted their format to fit
with the way amforth handles double length integers (henceforth 'doubles'),
with the most significant cell higher on the stack. The most significant bit
is the sign bit, followed by a byte for the exponent, and the remaining 23
bits for the significant. See the basics at:
http://en.wikipedia.org/wiki/Single_precision_floating-point_format

These floats are stored on the data stack -- not on their own seperate stack.

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
-double words
  D0= ( d -- flag )
  D= ( d1 d2 -- flag )
  D0< ( d -- flag )
  D10* ( d -- d*10 )
-other:
  FALSE ( -- n-0 )
  >= ( n1 n2 -- flag ) the flag is true if n1 >= n2 and false otherwise

There are many words in the library that are helper words and you'll probably have no use for.

All the files that make up this library are copyright 2011 by Leon Maurer.

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