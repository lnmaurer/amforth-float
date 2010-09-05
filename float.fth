\ you can skip the next line if you'd like, but it makes is easier to erase
\ and reload these floating point words
marker ->clean

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

\ MISC

true not constant false

\ return +0
0 0 fconstant f0

: emitdigit ( n -- )
  48 + emit ;

\ OPERATORS FOR DOUBLES

: d0= ( d -- flag )
  0= swap 0= and ;

\ wasn't installed by default, so add it if you haven't included it
\ : d= ( d1 d2 -- flag )
\   d- d0= ;

: d0< ( d -- flag )
  nip 0< ;

\ negates d if it's negative and returns a flag saying whether it was negated
\ or not
: dnegateifneg ( d -- d flag )
  fdup ( ddup ) d0< if dnegate true else false then ;

\ splits a 24 bit double (e.g. the significand) in to two 12 bit singles --
\ an upper and a lower (nU, nL) keeping the signs
\ we remove the sign at the beginning and add it back at the end because you
\ can run in to inconsistancies otherwise. For example:
\ > -13176795. d2/ d.
\ -6588398  ok
\ > 13176795. d2/ d.
\ 6588397  ok
: dsplit ( d -- nU nL )
  dnegateifneg >r fdup ( ddup )
  \ get the upper half by shifting it 12 times to the right
  12 0 do d2/ loop d>s
  r@ if negate then nfswap ( ndswap )
  \ get the lower half using a mask
  drop 4095 ( 0000111111111111 ) and
  r> if negate then ;

\ HELPER WORDS FOR SEPERATING OUT AND PUTTING BACK TOGETHER THE DIFFERENT
\ PARTS OF FLOATS

\ FROM WIKIPEDIA: "The true significand includes an implicit leading bit with
\ value 1 unless the exponent is stored with all zeros." So we must test that
\ both significand and exponent are all zeros (making the exponent -127). Also
\ note that we can have +0 and -0. This exponent of -127 really means -126,
\ it's just a way to indicate the implicit leading bit should be left out.

\ returns the exponent, even if -127
: frawexponent ( f -- n )
  nip
  32640 and ( 0111111110000000 )
  7 rshift
  127 - ;

\ remember that an exponent of -127 really means subnormal so the actual
\ exponent is -126
: fexponent ( f -- n )
  frawexponent -126 max ;

\ returns +/- 1 for positive or negative, counts 0 as postive
: fsign ( f -- n )
  nip 0< if -1 else 1 then ;

\ returns the significand including sign and implicit 1 in the 24th place
\ if it should be there (i.e. unless frawexponent = -127 ). Note that the -0
\ and +0 both return d 0
: fsignificand ( f -- d )
  fdup 127 and ( 0000000001111111 )
  fover frawexponent -127 = not if 128 + then
  fover fsign 0< if dnegate then fnip ;

\ n is exponent in range [-127,127]
\ handles exponent error checking
: fsetexponent ( f n -- f )
  dup -127 < abort" exponent < -127 "
  dup 127 > abort" exponent > 127 "
  >r -32641 and ( 1000000001111111 ) r>
  127 + 7 lshift or
  ;

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
  \ only need to look at upper half of double to see if it's too large
  \ the upper half can only use the first 8 digits
  dup 255 > abort" |significand| > 16777215 "
  127 and
  r> if -1 fsetsign then
  ;

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

: sigexp>f ( d-significand n-exponent -- f )
\ the plan is to first make the signficand positive and then shift it so that
\ it has a one in the 24th place (and handle the exponent accordingly) then
\ take care of the sign and stick the significand and exponent together to
\ make a float
  
  \ take off sign
  nfswap ( ndswap ) dnegateifneg >r fnswap ( d n, R: flag )

  \ if the significand is too large, shift it right
  \ OR
  \ shift it right if the exponent is too small
  begin
    \ only need to look at upper half of double to see if it's too large
    \ the upper half should use the first 8 digits
    over 255 > \ 255 is all ones in the first 8 places -- b11111111
    over -126 < or
  while
    frshift
  repeat

  \ if the significand is too small, shift it left
  \ however, make sure to keep the exponent >=-126
  \ or zero would cause it to shift left forever
  begin
    over 128 < \ 128 is one followed by seven zeros -- b10000000
    over -126 > and
  while
    flshift
  repeat

  \ check to see if it's a subnromal number by checking if the significand is
  \ less than 24 digits
  nfover 0 128 d<
  if 1- then \ if it is, change the exponent from -126 to -127

  \ restore sign
  r> swap >r if dnegate then ( d, R: n )

  fmakesignificand r> fsetexponent ;

: f>sigexp ( f -- d-significand n-exponent )
  fdup fexponent >r fsignificand r> ;

: faddtoexponent ( f n -- f )
  >r f>sigexp r> + sigexp>f ;
\  >r fdup fexponent r> + fsetexponent ;

\ CONVERSION

: d>f ( d -- f )
  0 sigexp>f 23 faddtoexponent ;

: s>f ( n -- f )
  s>d d>f ;

: f>d ( f -- d )
  f>sigexp 23 fshiftto drop ;

: f>s ( f -- n )
  f>d d>s ;  

\ MATHEMATICAL OPERATORS

\ fsignificand takes care of the +/- 0 problem
: f0= ( f -- flag )
  fsignificand d0=  ;

\ works because 0 is postive for doubles
: f0< ( f -- flag)
  fsignificand d0< ;

: fnegate ( f -- -f )
  fdup fsign negate fsetsign ;

: fnegateifneg ( d -- d flag )
  fdup f0< if fnegate true else false then ;

: fabs ( f -- |f| )
  fdup f0< if fnegate then ;

\ strict equality -- no rangle for wiggle room
: f= ( f1 f2 -- flag )
  f>sigexp >r fswap f>sigexp >r d= r> r> = and ;

\ sigexp>f will take care of changing significand if nescessary --
\ such as for subnormal numbers
: f2/ ( f -- f/2 )
  f>sigexp 1- sigexp>f ;

: f2* ( f -- f/2 )
  f>sigexp 1+ sigexp>f ;

\ NOTE: might run in to trouble subtracting two numbers that are close in
\ magnitude. We shift 6 extra spaces to the left before adding to get some
\ additional significant digits, have to think about this more
\ THE PLAN: break both in to d n pairs, find the larger of the two exponents,
\ and align the both pairs to that exponent minus 6 (see above). Once aligned,
\ we can just add the significands then make the resulting float from the d n
\ pair
: f+ ( f1 f2 -- f1+f2 )
  fover fexponent >r fdup fexponent r> max 6 - ( f1 f2 n-max-exp )
  >r f>sigexp r@ fshiftto drop ( f1 d2, R: n-max )
  fswap ( dfswap ) f>sigexp r@ fshiftto drop ( d2 d1, R: n-max)
  d+ r> sigexp>f ;

: f- ( f1 f2 -- f1-f2 )
  fnegate f+ ;

: f< ( f1 f2 -- flag )
  f- f0< ;

: f> ( f1 f2 -- flag )
  fswap f< ;

: f>= ( f1 f2 -- flag )
  f< not ;

: f<= ( f1 f2 -- flag )
  f> not ;

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
  rot m* r@ 23 - sigexp>f r> nfswap >r >r >r
  ( n1u n1l n2u n2l n1u n2u, R: fll exp )
  m* r@ 1+ sigexp>f r> nfswap >r >r >r ( n1u n1l n2u n2l, R: fll fuu exp )
  rot rot ( n1u n2l n1l n2u, R: fll fuu exp )
  m* r@ 11 - sigexp>f r> nfswap >r >r >r ( n1u n2l, R: fll fuu flu exp )
  m* r> 11 - sigexp>f r> r> r> r> r> r> ( ful flu fuu fll )
  frot f+ frot f+ f+ ;

: fmax ( f1 f2 -- f )
  fover fover f- f0<
  if fswap then
  fdrop ;

: fmin ( f1 f2 -- f )
  fover fover f- f0< not
  if fswap then
  fdrop ;

\ makes the exponent zero and returns the old exponent or what the exponent
\ would have been for subnormal numbers.
: fzeroexponent ( f -- f n )
  fdup f0=
  if \ it's zero, not much to do
    0
  else
    0 nfswap \ will store the number of shifts ( n f )

    \ if it's subnormal, shift it left until it isn't
    begin
      fdup frawexponent -127 =
    while
      f2* fnswap 1+ nfswap
    repeat

    fnswap >r ( f, R: n )  
    fdup fexponent >r 0 fsetexponent r> r> - 
  then ;

: f/ ( f1 f2 -- f1/f2 )
  fdup f0= abort" division by zero "

  fnegateifneg >r fswap fnegateifneg >r fswap
  r> r> xor >r ( f1 f2, R: flag-negative )

  fzeroexponent >r ( f1 f2, R: negative n2 )
  fswap fzeroexponent r> - >r fswap ( f1 f2, R: negative n1-n2 )
  \ f1 will be known as remainder, f2 as divisor, and n1-n2 as exponent
  f0 frot frot ( sum remainder divisor, R: negative exponent )
  \ [ 0 128 0 sigexp>f ] is better than 0 16256
  \ but it gets bit by the double length number in colon definition bug
  0 16256 frot frot ( sum toadd remainder divisor, R: negative exponent )

  \ floats only have 24 significant digits, but if f2>f1 then first digit is
  \ insignificant, so do 25 to be safe
  25 0 do 
    fover f0= if leave then \ no need to continue if remainder is zero
    fover fover f< not
    if \ remainder >= than divisor
      ftuck f- fswap
      >r >r >r >r ( sum toadd, R: negative exponent divisor remainder )
      ftuck f+ fswap
      r> r> r> r>
    then
    \ either way, half toadd and divisor
    f2/ >r >r >r >r f2/ r> r> r> r>
  loop

  fdrop fdrop fdrop r> faddtoexponent
  r> if fnegate then ;

\ the greatest integer <= the float
\ e.g. the floor of 3.5 is 3
\ and the floor of -3.5 is -4
\ the division in fshiftto gets rid of the fractional part
: ffloor ( f -- f )
  f>sigexp 23 fshiftto sigexp>f ;

\ the ceiling of x is -floor(-x)
: fceil ( f -- f )
  fnegate ffloor fnegate ;

\ returns f mod 1 -- basically the fractional part of f
\ fmod1(f) = f - ffloor(f)
: fmod1 ( f -- f )
  fdup ffloor f- ;

\ print f using scientific notation
\ this uses the dragon2 algorithm from
\ "How to print floating point numbers accurately"
\ by Steele and White
\ in our implimentation, their P=25
: fs. ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    fdrop 48 46 48 emit emit emit \ prints "0.0"
  else
    \ first, let's take care of the sign
    fdup f0<
    if
      fnegate \ make it positive
      45 emit \ print a "-"
    then

    \ next, we scale the number to be in [1,10)
    0 nfswap \ the 0 is the x in dragon2 algorithm

    \ if it's too large, make it smaller
    \ THREE VERSIONS, PICK YOUR POISON
\ SLOW BUT CLEAN:
\    begin
\      fdup [ 10 s>f swap ] literal literal f>=
\    while
\      [ 10 s>f swap ] literal literal f/ 
\      fnswap 1+ nfswap
\    repeat
\ FAST AND CLEAN BUT WITH MORE ROUNDING ERRORS:
\    begin
\      fdup [ 10 s>f swap ] literal literal f>=
\    while
\      [ 1 s>f 10 s>f f/ swap ] literal literal f* 
\      fnswap 1+ nfswap
\    repeat
\ GOOD COMPROMISE:
    [ 1 s>f swap ] literal literal ( s-x f-v f-1 )
    begin
      fover fover [ 10 s>f swap ] literal literal f* ( s-x f-v f-powerof10 f-v f-10*powerof10 )
      fdup >r >r ( s-x f-v f-powerof10 f-v f-10*powerof10, R: f-10*powerof10 )
      f>=
    while
      fdrop
      fnswap 1+ nfswap
      r> r>
    repeat
    r> r> fdrop ( s-x f-v f-powerof10 )
    f/

    \ if it's too small, make it bigger
    begin
      fdup [ 1 s>f swap ] literal literal f<
    while
      [ 10 s>f swap ] literal literal f*
      fnswap 1- nfswap
    repeat

    \ the float on the stack is called v' in the dragon2 algorithm

    fdup ffloor f>s emitdigit \ now we can print the first digit
    46 emit \ the decimal point

    \ calculate n = p - floor(log2(v')) - 1
    24 \ that's p - 1 since p is 25
    nfover [ 2 s>f swap ] literal literal f> if 1-
    nfover [ 4 s>f swap ] literal literal f> if 1-
    nfover [ 8 s>f swap ] literal literal f> if 1- then then then
    
    \ now the stack is ( s-x f-v' s-n )

    \ let's construct M = 2^(-n)/2 = 2^(-n-1)
    negate 1- >r
    [ 1 s>f swap ] literal literal r> fsetexponent ( s-x f-v' f-M )

    \ don't care about k in algorithm since we'll just print immediatly
    \ R in algorithm is fractional part of v'
    fswap fmod1 ( s-x f-M f-R )

    \ in dragon2, use B=10 because that's the base we want
    begin
      \ calculate the digit (their U) and move to return stack
      fdup [ 10 s>f swap ] literal literal f* ffloor f>s >r ( s-x f-M f-R, R: s-U )
      \ update R
      [ 10 s>f swap ] literal literal f* fmod1
      \ update M
      fswap [ 10 s>f swap ] literal literal f* fswap
      fover fover f<= >r
      fover [ 1 s>f swap ] literal literal f- fover fnegate f<=
      r> and
    while
      \ output the digit (their U )
      r> emitdigit
    repeat

    \ take care of the final digit (their case statement)
    r> nfover [ 1 s>f f2/ swap ] literal literal f>
    if 1+ then
    emitdigit

    fdrop fdrop \ M and R are no longer of use to us

    \ x has been waiting patiently at the bottom of the stack this whole time
    ?dup if
      69 emit . \ if it's non-zero, print "E" then print x
    else
      bl emit \ otherwise, just print a space
    then
  then ;

\ again, the next line is for convienence, not nescessity
marker ->afterfloat

