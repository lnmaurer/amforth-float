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

: (fliteral) ( -- f )
  r@ 1+ i@ r@ i@ r> 2 + >r ;

: fliteral ( f -- )
  compile (fliteral) , , ; immediate

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

: d10* ( d -- d*10 )
  10 * ( n-lower n-uppper*10 )
  swap 10 um* ( n-uppper*10 d-lower*10 )
  fnswap + ;

: dreversedigits ( dinitial -- dfinal n-digits )
  0 0 0 >r ( di df, R: digits )
  begin
    fover d0= not
  while
    fswap 10 ud/mod fnswap ( df di/10 rem, R: digits )
    >r fswap d10* r> s>d d+ ( di/10 df*10+rem, R: digits )
    r> 1+ >r \ updated digits
  repeat
  r> ;

: dreversedigits2 ( dinitial n-digits -- dfinal )
  dup 0= if
    drop drop 0 0 \ 0 digits means dfinal = 0
  else
    0 0 fnswap 0 do ( dinitial dfinal )
      fswap 10 ud/mod fnswap ( dfinal dinitial/10 rem )
      >r fswap d10* r> s>d d+
   loop
  then ;

: dtransferdigit ( d1 d2 -- d1*10+rem d2/10 )
  10 ud/mod ( d1 rem d2/10 )
  >r >r >r ( d1 R: d2/10 rem )
  d10* r> s>d d+
  r> r> ;

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
: fshifttoexp ( d-significand n-exponent n-desired-exponent -- d n )
  over - dup abs swap 0> ( d n n-abs-diff n-diff )
  if 
    frshiftn
  else
    flshiftn
  then ;

\ shifts significand until there is a 1 in the 'n-digit' digit -- the first
\ digit corresponds to n-digit = 0
\ INCOMPLETE
: fshifttopos ( d-significand n-exponent n-digit -- d n )
  >r 0 0 r> 0 ( d-significand n-exponent d-0 n-digit n-0 -- d n )

  ;

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
  23 sigexp>f ;

: s>f ( n -- f )
  s>d d>f ;

: f>d ( f -- d )
  f>sigexp 23 fshifttoexp drop ;

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
  >r f>sigexp r@ fshifttoexp drop ( f1 d2, R: n-max )
  fswap ( dfswap ) f>sigexp r@ fshifttoexp drop ( d2 d1, R: n-max)
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
\ : fzeroexponent ( f -- f n )
\  fdup f0=
\  if \ it's zero, not much to do
\    0
\  else
\    0 nfswap \ will store the number of shifts ( n f )
\
\    \ if it's subnormal, shift it left until it isn't
\    begin
\      fdup frawexponent -127 =
\    while
\      f2* fnswap 1+ nfswap
\    repeat
\
\    fnswap >r ( f, R: n )  
\    fdup fexponent >r 0 fsetexponent r> r> - 
\  then ;

\ : f/ ( f1 f2 -- f1/f2 )
\  fdup f0= abort" division by zero "
\
\  fnegateifneg >r fswap fnegateifneg >r fswap
\  r> r> xor >r ( f1 f2, R: flag-negative )
\
\  fzeroexponent >r ( f1 f2, R: negative n2 )
\  fswap fzeroexponent r> - >r fswap ( f1 f2, R: negative n1-n2 )
\  \ f1 will be known as remainder, f2 as divisor, and n1-n2 as exponent
\  f0 frot frot ( sum remainder divisor, R: negative exponent )
\  \ [ 0 128 0 sigexp>f ] is better than 0 16256
\  \ but it gets bit by the double length number in colon definition bug
\  0 16256 frot frot ( sum toadd remainder divisor, R: negative exponent )
\
\  \ floats only have 24 significant digits, but if f2>f1 then first digit is
\  \ insignificant, so do 25 to be safe
\  25 0 do 
\    fover f0= if leave then \ no need to continue if remainder is zero
\    fover fover f< not
\    if \ remainder >= than divisor
\      ftuck f- fswap
\      >r >r >r >r ( sum toadd, R: negative exponent divisor remainder )
\      ftuck f+ fswap
\      r> r> r> r>
\    then
\    \ either way, half toadd and divisor
\    f2/ >r >r >r >r f2/ r> r> r> r>
\  loop
\
\  fdrop fdrop fdrop r> faddtoexponent
\  r> if fnegate then ;

: fpreparefordivide ( f -- d n )
  f>sigexp

  \ get d so that it has a one in the 25th place
  begin
    >r dup 4096 < r> swap \ only need to look at upper part of d
  while
    flshift
  repeat ;

: f/ ( f1 f2 -- f1/f2 )
  fdup f0= abort" division by zero "

  \ should the result be negative
  fnegateifneg >r fswap fnegateifneg >r fswap
  r> r> xor >r ( f1 f2, R: flag-negative )

  fpreparefordivide >r ( f1 d2, R: flag-negative n2 )

  fswap fpreparefordivide ( d2 d1 n1, R: negative n2 )

  \ also subtract 5 because everything shifted 5 to left
  r> - 5 - >r fswap ( d1 d2, R: negative n1-n2 )

  \ d1 will be known as remainder, d2 as divisor, and n1-n2 as exponent
  \ put sum on the stack, initialized to zero
  0 0 frot frot ( sum remainder divisor, R: negative exponent )
  \ now, put in the thing we'll add
  0 4096 frot frot ( sum toadd remainder divisor, R: negative exponent )

  \ floats only have 24 significant digits, but if d2>d1 then first digit is
  \ insignificant, so do 26 to be safe
  26 0 do 
    fover d0= if leave then \ no need to continue if remainder is zero
    fover fover d< not
    if \ remainder >= than divisor
      ftuck d- fswap
      >r >r >r >r ( sum toadd, R: negative exponent divisor remainder )
      ftuck d+ fswap
      r> r> r> r>
    then
    \ either way, half toadd and divisor
    d2/ >r >r >r >r d2/ r> r> r> r>
  loop

  fdrop fdrop fdrop r> sigexp>f
  r> if fnegate then ;

\ the greatest integer <= the float
\ e.g. the floor of 3.5 is 3
\ and the floor of -3.5 is -4
\ the division in fshifttoexp gets rid of the fractional part
: floor ( f -- f )
  f>sigexp 23 fshifttoexp sigexp>f ;

\ the ceiling of x is -floor(-x)
: ceil ( f -- f )
  fnegate floor fnegate ;

\ returns f mod 1 -- basically the fractional part of f
\ fmod1(f) = f - floor(f)
: fmod1 ( f -- f )
  fdup floor f- ;

\ round to nearest integer
: fround ( f -- f )
  fdup fmod1 [ 1 s>f f2/ ] fliteral f<
  if
    floor
  else
    ceil
  then ;

\ returns d/10^n where n is the first integer such that 10^n > d
\ for example, if d is 1234, then this returns .1234
: d>fraction ( d -- f )
  d>f
  begin
    fdup [ 1 s>f ] fliteral f>
  while
    [ 10 s>f ] fliteral f/
  repeat ;


\ print f using scientific notation
\ this uses the dragon2 algorithm from
\ "How to print floating point numbers accurately"
\ by Steele and White
\ in our implimentation, their P=25
: fs. ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    fdrop 32 48 46 48 32 emit emit emit emit emit \ prints "0.0"
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
    begin
      fdup [ 10 s>f ] fliteral f>=
    while
      [ 10 s>f ] fliteral f/ 
      fnswap 1+ nfswap
    repeat

    \ if it's too small, make it bigger
    begin
      fdup [ 1 s>f ] fliteral f<
    while
      [ 10 s>f ] fliteral f*
      fnswap 1- nfswap
    repeat

    \ the float on the stack is called v' in the dragon2 algorithm

    fdup floor f>s emitdigit \ now we can print the first digit
    46 emit \ the decimal point

    \ calculate n = p - floor(log2(v')) - 1
    24 \ that's p - 1 since p is 25
    nfover [ 2 s>f ] fliteral f> if 1-
    nfover [ 4 s>f ] fliteral f> if 1-
    nfover [ 8 s>f ] fliteral f> if 1- then then then
    
    \ now the stack is ( s-x f-v' s-n )

    \ let's construct M = 2^(-n)/2 = 2^(-n-1)
    negate 1- >r
    [ 1 s>f ] fliteral r> fsetexponent ( s-x f-v' f-M )

    \ don't care about k in algorithm since we'll just print immediatly
    \ R in algorithm is fractional part of v'
    fswap fmod1 ( s-x f-M f-R )

    \ in dragon2, use B=10 because that's the base we want
    begin
      \ calculate the digit (their U) and move to return stack
      fdup [ 10 s>f ] fliteral f* floor f>s >r ( s-x f-M f-R, R: s-U )
      \ update R
      [ 10 s>f ] fliteral f* fmod1
      \ update M
      fswap [ 10 s>f ] fliteral f* fswap
      fover fover f<= >r
      fover [ 1 s>f ] fliteral f- fover fnegate f<=
      r> and
    while
      \ output the digit (their U )
      r> emitdigit
    repeat

    \ take care of the final digit (their case statement)
    r> nfover [ 1 s>f f2/ ] fliteral f>
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

\ print f using scientific notation with n digits after the decimal point
: fsn. ( f n -- )
  >r ( f, R: n )
  \ handle zero seperately
  fdup f0=
  if
    fdrop 46 48 32 emit emit emit \ prints " 0."
    \ the leading space makes it aligned with any "-" printed
    r> 0 do 48 emit loop 32 emit \ prints "0" n times then a space
  else
    \ first, let's take care of the sign
    fdup f0<
    if
      fnegate \ make it positive
      45 emit \ print a "-"
    else
      32 emit \ print a " " so that it's aligned with any "-" printed
    then

    \ next, we scale the number to be in [1,10)
    0 nfswap \ the 0 is the x in dragon2 algorithm

    \ if it's too large, make it smaller
    begin
      fdup [ 10 s>f ] fliteral f>=
    while
      [ 10 s>f ] fliteral f/ 
      fnswap 1+ nfswap
    repeat

    \ if it's too small, make it bigger
    begin
      fdup [ 1 s>f ] fliteral f<
    while
      [ 10 s>f ] fliteral f*
      fnswap 1- nfswap
    repeat

    ( s-x f-v , R: n )

    \ the float on the stack is called v' in the dragon2 algorithm

    fdup floor f>s emitdigit \ now we can print the first digit
    46 emit \ the decimal point
    
    \ now remove the integer part of v', yeilding M
    fmod1 ( s-x f-M, R:n )

    r> 0 do
      [ 10 s>f ] fliteral f* \ multiply by 10 get make a new integer
      fdup floor f>s emitdigit \ emit the integer
      fmod1 \ get rid of it, leaving the remainder
    loop

    fdrop \ get rid of remainder

    \ x has been waiting patiently at the bottom of the stack this whole time
    ?dup if
      69 emit . \ if it's non-zero, print "E" then print x
    else
      bl emit \ otherwise, just print a space
    then
  then ;

\ COME UP WITH BETTER NAMES FOR NEXT TWO
\ returns the number that occupies the part of the string from n-location + 1 to the end
: partnumber ( n-adr n-length n-location -- n )
  over over - 1- rot drop rot rot + swap ( new_adr new_length )
  over dup
  c@ >r ( store the value that was at n-location to the return stack )
  c! ( store the length there to make a counted string )
  dup number ( new_adr num, R: previous_value )
  r> rot c! ; ( return the value to its previous place )

: extract ( n-adr n-length c-char -- n-adr n-new-length n-extracted )
  >r over over r> cscan nip ( adr count loc )
  over over = 
  if \ character not found
    drop 0
  else \ character found, note that loc becomes new-length
    swap >r ( adr loc, R: length )
    over over r> swap ( adr loc adr length loc )
    partnumber 
  then ;

\ string of form 'integer'.'fractioal'e'exp'
: string>float ( c-addr u-length -- f )
  \ get exponent first -- this is the number that follows e, E, d, or D
  101 extract dup 0= if drop \ 'e'
  69 extract dup 0= if drop  \ 'E'
  100 extract dup 0= if drop \ 'd'
  68 extract dup 0=          \ 'D'
  then then then

  >r ( adr length, R: exp )
  
  \ next get fractional part -- 46 is '.'
  46 extract >r ( adr length, R: exp fractional )
  -1 partnumber ( integer, R: exp fractional )
  s>f r> s>f ( f-integer f-fractional, R: exp )

  \ make f-fractional a fraction
  begin
    fdup [ 1 s>f ] fliteral f>
  while
    [ 10 s>f ] fliteral f/
  repeat
  
  \ combine fractional and integer parts
  fover f0< if
    f- \ integer part is negative, so fractional part should be too
  else
    f+ \ integer part is positive, so fractional part should be too
  then

  \ now, shift according to exp
  r> dup 0=
  if
    drop
  else
    dup 0<
    if
      negate
      0 do [ 10 s>f ] fliteral f/ loop
    else
      0 do [ 10 s>f ] fliteral f* loop
    then
  then ;

: >float ( n-c-addr u-length -- f true | false)
  ['] string>float catch
  0=
  if
    true \ no error encounter -- we have a float
  else
    drop drop false \ couldn't make a float, clear the two inputs off the stack
  then ;

\ returns the current number of possible FP numbers on the data stack
: fdepth ( -- n )
  sp0 sp@ - 4 / ;

\ Add the size in address units of a floating-point number to f-addr1,
\ giving f-addr2
: float+ ( f-addr1 -- f-addr2 )
  4 + ;

\ n2 is the size in address units of n1 floating-point numbers
: floats ( n1 -- n2 )
  4 * ;

\ again, the next line is for convienence, not nescessity
marker ->afterfloat

