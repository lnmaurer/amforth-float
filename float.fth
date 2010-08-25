\ This floating point implimentation is inspired by the IEEE 754-2008 binary32
\ format -- your standard single precision float. I adapted their format to fit
\ with the way amforth handles double length integers (henceforth 'doubles'),
\ with the most significant cell higher on the stack. The most significant bit
\ is the sign bit, followed by a byte for the exponent, and the remaining 23
\ bits for the significand. See the basics at:
\ http://en.wikipedia.org/wiki/Single_precision_floating-point_format

\ These floats are stored on the data stack -- not on their own seperate stack.

\ ANS94 Floating-point words check list
\ Words with 'yes' next to them have been implimented. 
\ see http://lars.nocrew.org/dpans/dpans12.htm

\ >FLOAT
\ D>F yes
\ F! yes
\ F* yes
\ F+ yes
\ F- yes
\ F/
\ F0< yes
\ F0= yes
\ F< yes
\ F>D yes
\ F@ yes
\ FALIGN
\ FALIGNED
\ FCONSTANT yes
\ FDEPTH
\ FDROP yes
\ FDUP yes
\ FLITERAL
\ FLOAT+ 
\ FLOATS
\ FLOOR
\ FMAX yes
\ FMIN yes
\ FNEGATE yes
\ FOVER yes
\ FROT yes
\ FROUND
\ FSWAP yes
\ FVARIABLE yes
\ REPRESENT

\ MISC

true not constant false

\ return +0
0 0 fconstant f0

\ STACK MANIPULATION WORDS

\ DOESN'T WORK?
: f>r ( f -- , R: -- f )
    >r >r ;

\ DOESN'T WORK?
: fr> ( -- f, R: f -- )
    r> r> ;

: fdrop ( f -- )
 drop drop ;

: fdup ( f -- f f )
 over over ;

: fover ( f1 f2 -- f1 f2 f1 )
  >r >r
  over over
  r>
  rot rot
  r>
  rot rot ;
  
: fswap ( f1 f2 -- f2 f1 )
 rot >r rot r> ;

: frot ( f1 f2 f3 -- f2 f3 f1 )
 >r >r fswap r> r> fswap ;

: fnip ( f1 f2 -- f2 )
 fswap fdrop ;

: ftuck ( f1 f2 -- f2 f1 f2 )
 fswap fover ;

: nfswap ( f n -- n f )
  rot rot ;

: fnswap ( n f -- f n )
  rot ;

: nfover ( f n -- f n f )
  >r fdup r> nfswap ;

: nip ( n1 n2 -- n2 )
  swap drop ;

\ WORDS FOR STORING FLOATS

: fconstant ( f -- )
    create , ,
    does>
    dup i@ swap 1+ i@ swap \ SHOULD BE CELL+?
;

: fvariable ( -- )
  here constant 4 allot ;

: f@ ( addr -- f )
  dup cell+ @ swap @ ;
  
: f! ( f addr -- )
  swap over ! cell+ ! ;

\ OPERATORS FOR DOUBLES

: d0= ( d -- flag )
  0= swap 0= and ;

: d0< ( d -- flag )
  nip 0< ;

\ negates d if it's negative and returns a flag saying whether it was negated
\ or not
: dnegateifneg ( d -- d flag )
  fdup ( ddup ) d0< if dnegate true else false then ;

\ splits a 24 bit double (e.g. the significand) in to two 12 bit singles --
\ an upper and a lower (nU, nL) keeping the signs
: dsplit ( d -- nU nL )
  \ get the upper half by just shifting it 12 times to the right
  fdup 12 0 do d2/ loop d>s nfswap
  \ get the lower half using a mask and keeping track of the sign
  dnegateifneg >r drop 4095 and
  r> if negate then ;

\ HELPER WORDS FOR SEPERATING OUT AND PUTTING BACK TOGETHER THE DIFFERENT
\ PARTS OF FLOATS

\ FROM WIKIPEDIA: "The true significand includes an implicit leading bit with
\ value 1 unless the exponent is stored with all zeros." So we must test that
\ both significand and exponent are all zeros (making the exponent -127). Also
\ note that we can have +0 and -0

\ returns the exponent, properly biased
: fexponent ( f -- n )
  nip
  32640 and ( 0111111110000000 )
  7 rshift
  127 - ;

\ returns +/- 1 for positive or negative, counts 0 as postive
: fsign ( f -- n )
  nip 0< if -1 else 1 then ;

\ returns the significand including sign and implicit 1 in the 24th place
\ if it should be there (i.e. unless fexponent = -127 ). Note that the -0 and
\ +0 both return d 0
: fsignificand ( f -- d )
  fdup 127 and ( 0000000001111111 )
  fover fexponent -127 = not if 128 + then
  fover fsign 0< if dnegate then fnip ;

\ n is exponent in range [-127,127]
\ handles exponent error checking
: fsetexponent ( f n -- f )
  dup -127 < abort" exponent < -127 "
  dup 127 > abort" exponent > 127 "
  >r -32641 and ( 1000000001111111 ) r>
  127 + 7 lshift or
  ;

: faddtoexponent ( f n -- f )
  >r fdup fexponent r> + fsetexponent ;

\ stores the sign of n in to f at addr
: fsetsign ( f n -- )
  0< if ( make negative )
    32768 or ( 1000000000000000 ) \ -32768
  else ( make positive )
    32767 and ( 0111111111111111 )
  then
  ;

\ only keeps the first 24 digits (23 explicitly), so significand must be in
\ proper form beforehand aborts if out of range
: fmakesignificand ( d -- f-with-exponent=-127 )
  dnegateifneg >r ( f d, R: flag )
  dup 255 > abort" |significand| > 16777215 "
  127 and
  r> if -1 fsetsign then
  ;

: fsetsignificand ( f d -- f )
  fmakesignificand fswap fexponent fsetexponent ;

\ IT'S HANDY TO BREAK A FLOAT IN TO A D (SIGNIFICAND) AND N (EXPONENT)
\ THE FOLLOWING FUNCTIONS MANIPULATE THAT PAIR

\ performs a 'right shift' on a float split in to an d and n
\ d is shifted right (halved) and n is incrimented
: frshift ( d-significand n-exponent -- d n )
  1+ >r d2/ r> ;

\ rshifts n times
: frshiftn ( d-significand n-exponent n-times -- d n )
  dup 0> if 0 do frshift loop else drop then ;

\ like rshift, but in other direction
: flshift ( d-significand n-exponent -- d n )
  1- >r d2* r> ;

: flshiftn ( d-significand n-exponent n-times -- d n )
  dup 0> if 0 do flshift loop else drop then ;

\ shifts until the exponent is the desired value
: fshiftto ( d-significand n-exponent n-desired-exponent -- d n )
  over - dup abs swap 0> ( d n n-abs-diff n-diff )
  if 
    frshiftn
  else
    flshiftn
  then ;

\ shifts out all trailing zeros
: fremovetrailing ( d n -- d n )
  begin
    nfover ( really ndover ) drop 1 and 0=
  while
    frshift
  repeat ;

: sigexp>f ( d-significand n-exponent -- f )
  nfover ( ndover ) d0=
  if \ take care of zero seperately
    drop fdrop f0
  else \ nonzero
  \ the plan is to first make the signficand positive and then shift it so that
  \ it has a one in the 24th place (and handle the exponent accordingly) then
  \ take care of the sign and stick the significand and exponent together to
  \ make a float
  
    \ take off sign
    nfswap ( ndswap ) dnegateifneg >r fnswap ( d n, R: flag )

    \ if it's too large, shift it right
    begin
      nfover ( really dfover ) 0 128 d>
    while
      frshift
    repeat

    \ if it's too small, shift it left
    \ however, make sure to keep the exponent >=-127
    \ to allow for subnormal numbers
    begin
      nfover ( really ndover ) 0 128 d< ( d n flag )
      over -127 > and
    while
      flshift
    repeat

    \ restore sign
    r> swap >r if dnegate then ( d, R: n )

    fmakesignificand r> fsetexponent
  then ;

: f>sigexp ( f -- d-significand n-exponent )
  fdup fexponent >r fsignificand r> ;

\ MATHEMATICAL OPERATORS

\ fsignificand takes care of the +/- 0 problem
: f0= ( f -- flag )
  fsignificand d0=  ;

\ works because 0 is postive for doubles
: f0< ( f -- flag)
  fsignificand d0< ;

: fnegate ( f -- -f )
  fdup fsign negate fsetsign ;

: fabs ( f -- |f| )
  fdup f0< if fnegate then ;

\ NOTE: might run in to trouble subtracting two numbers that are close in
\ magnitude. We shift 6 extra spaces to the left before adding to get some
\ additional significant digits, have to think about this more
\ THE PLAN: break both in to d n pairs, find the larger of the two exponents,
\ and align the both pairs to that exponent minus 6 (see above). Once aligned,
\ we can just add the significands then make the resulting float from the d n
\ pair
: f+ ( f1 f2 -- f1+f2 )
  fover fexponent >r fdup fexponent r> max 6 - ( f1 f2 n-max-exp )
  >r f>sigexp i fshiftto drop ( f1 d2, R: n-max )
  fswap ( dfswap ) f>sigexp i fshiftto drop ( d2 d1, R: n-max)
  d+ r> sigexp>f ;

: f- ( f1 f2 -- f1-f2 )
  fnegate f+ ;

\ The basic idea is to break the significand of each float in to two 12bit
\ pieces. These are nice to work with, because we can use m* to multiply them
\ in to 24bit significands, turn them in to floats, and add them all up.
\ The exponents for the different parts have to be taken in to account -- they
\ differ by 12. When adding up at the end, we sum from smallest to largest to
\ collect as many significant figures as we can. The actual implimentation is
\ kind of messy, but here goes...
: f* ( f1 f2 -- f1*f2 )
  f>sigexp >r fswap ( d2 f1, R: n2 )
  f>sigexp r> + >r ( d2 d1, R: exp )
  dsplit fswap dsplit ( n1u n1l n2u n2l, R: exp )
  fover fover ( n1u n1l n2u n2l n1u n1l n2u n2l , R: exp )
  rot m* i 23 - sigexp>f r> nfswap >r >r >r
  ( n1u n1l n2u n2l n1u n2u, R: fll exp )
  m* i 1+ sigexp>f r> nfswap >r >r >r ( n1u n1l n2u n2l, R: fll fuu exp )
  rot rot ( n1u n2l n1l n2u, R: fll fuu exp )
  m* i 11 - sigexp>f r> nfswap >r >r >r ( n1u n2l, R: fll fuu flu exp )
  m* r> 11 - sigexp>f r> r> r> r> r> r> ( ful flu fuu fll )
  frot f+ frot f+ f+ ;

: f< ( f1 f2 -- flag )
  f- f0< ;

: fmax ( f1 f2 -- f )
  fover fover f- f0<
  if fswap then
  fdrop ;

: fmin ( f1 f2 -- f )
  fover fover f- f0< not
  if fswap then
  fdrop ;

\ CONVERSION

: d>f ( d -- f )
  0 sigexp>f 23 faddtoexponent ;

: s>f ( n -- f )
  s>d d>f ;

: f>d ( f -- d )
  f>sigexp 23 fshiftto drop ;

: f>s ( f -- n )
  f>d d>s ;

