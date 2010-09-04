amforth-float -- a partially complete floating point wordset for amforth

amforth.sourceforge.net

This is just a personal project -- I am not involved with the development of
amforth.

The code requires 'd=', which doesn't seem to be compiled automatically. So
either add it to dict_appl.inc or uncomment the code for it below.

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

>FLOAT
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
FDEPTH
FDROP yes
FDUP yes
FLITERAL
FLOAT+ 
FLOATS
FLOOR
FMAX yes
FMIN yes
FNEGATE yes
FOVER yes
FROT yes
FROUND
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