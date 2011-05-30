amforth-float -- a partially complete floating point wordset for amforth

amforth.sourceforge.net

This is just a personal project -- I am not involved with the development of
amforth. However, this library has been included in amforth since version 4.2
(along with some related libraries like assemly versions of some of the words).

The code requires 'd=', which doesn't seem to be compiled automatically. So
either add it to 'dict_appl.inc' or uncomment the code in 'float.fth'.

Having the word 'marker' isn't strictly needed, but it can be useful. If you
want it, add

.include "words/set-current.asm"
.include "words/set-order.asm"

to 'dict_appl.inc' when assembling amforth. Then upload 'lib/ans94/marker.frt'.

Finially, if you want the recognizer to work (the code is at the end of
'float.fth'; it allows you to enter floats just like normal numbers), add

.include "words/n_to_r.asm"
.include "words/n_r_from.asm"

to 'dict_appl_core.inc'. Then upload 'lib/ans94/postpone.frt'.

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
see http://lars.nocrew.org/dpans/dpans12.htm

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
F.
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
FE.
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
F~
PRECISION
SET-PRECISION 
SF! 
SF@
SFALIGN
SFALIGNED
SFLOAT+
SFLOATS

Other words in the library:
FNIP ( f1 f2 -- f2 )
FTUCK ( f1 f2 -- f2 f1 f2 )
NFSWAP ( f n -- n f )
FNSWAP ( n f -- f n )
NFOVER ( f n -- f n f )
FNOVER ( n f -- n f n )

The way to remember the FN/NF words is that FN/NF shows the top two items on
the stack after executing the word. E.g. for FNSWAP, after execution the stack
will be F N.

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