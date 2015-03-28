\ you can skip the next line if you'd like, but it makes is easier to erase
\ and reload these floating point words
marker ->clean

\ STACK MANIPULATION WORDS

: f>r ( f -- , R: -- f )
  2>r ;

: fr> ( -- f, R: f -- )
  2r> ;

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

: fnover ( n f -- n f n )
  >r over r> swap ;

: nip ( n1 n2 -- n2 )
  swap drop ;

\ WORDS FOR STORING FLOATS

: fconstant ( f -- )
    create , ,
    does>
    dup @i swap 1+ @i swap \ SHOULD BE CELL+?
;

: fvariable ( -- )
  here constant 4 allot ;

: f@ ( addr -- f )
  dup cell+ @ swap @ ;
  
: f! ( f addr -- )
  swap over ! cell+ ! ;

: (fliteral) ( -- f )
  r@ 1+ @i r@ @i r> 2 + >r ;

: fliteral ( f -- )
  compile (fliteral) , , ; immediate

\ USEFUL CONSTANTS

true not constant false

0 0 fconstant f0 \ 0.0
0 16256 fconstant f1 \ 1.0
0 16672 fconstant f10 \ 10.0
0 16128 fconstant f0.5

\ OPERATORS FOR SINGLES

: >= ( n1 n2 -- f)
  over over > >r = r> or ;

\ OPERATORS FOR DOUBLES

: d0= ( d -- flag )
  0 0  d= ;

: d0< ( d -- flag )
  nip 0< ;

: d10* ( d -- d*10 )
  10 * ( n-lower n-uppper*10 )
  swap 10 um* ( n-uppper*10 d-lower*10 )
  fnswap + ;

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
  ?dup 0> if 0 do frshift loop then ;

\ like rshift, but in other direction
: flshift ( d-significand n-exponent -- d n )
  1- >r d2* r> ;

: flshiftn ( d-significand n-exponent n-times -- d n )
  ?dup 0> if 0 do flshift loop then ;

\ shifts until the exponent is the desired value
: fshifttoexp ( d-significand n-exponent n-desired-exponent -- d n )
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

: fnegateifneg ( f -- f flag )
  fdup f0< if fnegate true else false then ;

: negateiftrue ( f flag -- f )
  if fnegate then
;

: fabs ( f -- |f| )
  fdup f0< negateiftrue ;

\ strict equality -- no range for wiggle room
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

  fover f0= if \ if f1 is zero, fpreparefordivide will get in an infinite loop
    f0 \ so just return 0
  else
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
    r> negateiftrue
  then ;

\ from the standard:
\ If f3 is positive, flag is |f1-f2| < f3
\ If f3 is zero, this is the same as f1 f2 f=
\ If f3 is negative, flag is |f1-f2| < |f3|*(|f1|+|f2|)
: f~ ( f1 f2 f3 -- flag )
  fdup f0=
  if
    fdrop f=
  else
    fdup f0<
    if
      fabs >r >r ( f1 f2, R: |f3|)
      fover fabs fover fabs f+ r> r> f* ( f1 f2 |f3|*[|f1|+|f2|] )
    then
    >r >r f- fabs r> r> f<
  then
;

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
  fdup fmod1 f0.5 f<
  if
    floor
  else
    ceil
  then ;

: f10^n ( n -- f-10^n )
  f1 ( n f )
  fnover 0= if \ n is zero, so just return the 1.0 we have
    fnswap drop
  else
    fnover abs 0 do f10 f* loop ( n f-10^|n| )
    fnswap 0 < if
      f1 fswap f/
    then
  then
;

\ finds the integer n-steps with the smallest magnitude such that
\ f > 10^n-steps where n-steps = n * n-stepsize for some integer n
\ for exaple "3 .123 smallerpowerof10" yeilds n-steps=3 and f-10^n-steps=10^-3
\ note that f must be strictly greather than zero
: smallerpowerof10 ( n-stepsize f  -- n-steps f-10^n-steps )
  0 nfswap f1 fswap ( n-stepsize n-steps f-comparison f )

  \ first, we increase comparison until it's too large
  begin
    fover fover f< \ is f-comparison < f
  while
    >r >r >r >r ( n-stepsize n-steps, R: f f-comparison )
    over + \ add n-stepsize to n-steps
    over f10^n r> r> f* \ multiply f-comparison by 10^n-stepsize
    r> r>
  repeat
    
  \ then we divide until it's too small
  begin
    fover fover f> \ is f-comparison > f
  while
    >r >r >r >r ( n-stepsize n-steps, R: f f-comparison )
    over - \ subtract n-stepsize from n-steps
    over f10^n r> r> fswap f/ \ divide f-comparison by 10^n-stepsize
    r> r>
  repeat

  fdrop >r >r >r drop r> r> r>
;

\ INPUT AND OUTPUT


\ FIRST, SOME OUTPUT HELPER WORDS
\ if f is negative, negate it and emit a minus sign
: takecareofsign ( f -- |f|)
  fdup f0<
  if
    fnegate \ make it positive
    45 emit \ print a "-"
  then
;

: emitdigit ( n -- )
  48 + emit ;

\ f is a float in [1.0,10); this word emits the digit in the
\ ones place and returns the fractional part
: femitdigit ( f -- f-remainder )
  fdup floor f>s emitdigit \ emit the integer
  fmod1 \ get rid of it, leaving the remainder
;

\ f is a float in [1.0,10); this word emits 'n' digits and returns the remainder
: fprintdigits ( f n -- f-remainder )
  0 do femitdigit f10 f* loop
;

\ prints out 'n' zeros
: emitnzeros ( n -- )
  ?dup if \ only go in to loop if we don't want to print zero '0's
    0 do 48 emit loop
  then
;

\ this stores how many digits will be printed by F., FE., and FS.
\ (EVENTUALLY) if it's set to zero, then the word will choose how many digits to print
\ using the dragon2 algorithm, for now, FSD. does that
6 constant precision

: set-precision ( u -- )
  abs [ ' precision 1+ ] literal !i ;

\ returns f such that the PRECISIONth digit has been rounded
: roundtoprecision ( f -- f )
  fnegateifneg >r \ remove sign and store it on the stack
  1 nfover smallerpowerof10 fnswap >r f/ \ scale f to be in the range the range [1.0, 10.0)
  precision 1- f10^n f* \ shift so that PRECISION digits are in the integer part
  fround \ round the number
  f0.5 fover f0<
  if f- else f+ then \ put something in the next digit to get around later rounding errors
  precision 1- f10^n f/ \ shift the number back to where it was initially
  r> f10^n f* \ scale f back to its original size
  r> negateiftrue \ restore sign
;

\ NOW, FOR THE REAL OUTPUT WORDS

\ TODO: WRITE FD.NO-SPACE WORD AND THEN WRITE FSD. USING IT; ALSO WRITE FED. AND FD.
\ AND INTEGRATE THEM WITH FS., FE., AND F. SO THAT THE 'D' VERSIONS ARE CALLED IF
\ PRECESION IS ZERO
\ print f using scientific notation
\ using the dragon2 algorithm from
\ "How to print floating point numbers accurately"
\ by Steele and White
\ in our implimentation, their P=25
: fsd. ( f -- )
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
      fdup f10 f>=
    while
      f10 f/ 
      fnswap 1+ nfswap
    repeat

    \ if it's too small, make it bigger
    begin
      fdup f1 f<
    while
      f10 f*
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
    f1 r> fsetexponent ( s-x f-v' f-M )

    \ don't care about k in algorithm since we'll just print immediatly
    \ R in algorithm is fractional part of v'
    fswap fmod1 ( s-x f-M f-R )

    \ in dragon2, use B=10 because that's the base we want
    begin
      \ calculate the digit (their U) and move to return stack
      fdup f10 f* floor f>s >r ( s-x f-M f-R, R: s-U )
      \ update R
      f10 f* fmod1
      \ update M
      fswap f10 f* fswap
      fover fover f<= >r
      fover f1 f- fover fnegate f<=
      r> and
    while
      \ output the digit (their U )
      r> emitdigit
    repeat

    \ take care of the final digit (their case statement)
    r> nfover f0.5 f>
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

\ note that this doesn't do any rounding
: f.no-space ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    fdrop 46 48 emit emit \ prints "0."
    precision 1- emitnzeros \ prints "0" precision-1 times
  else
    takecareofsign \ we're now working with a positive number
    1 nfover smallerpowerof10 fnswap ( f f-10^n-steps n-steps )
    \ f/10^n-steps is a number in [1.0,10)
    >r f/ r> ( f/10^n-steps n-steps )
    dup 0 <
    if \ the number is less than 1.0, so print "0." and then enough leading zeros
      46 48 emit emit \ prints "0."
      abs 1- emitnzeros \ print n-steps - 1 leading zeros
      \ print out the right number of digits
      precision fprintdigits fdrop
    else \ f is greather than 1.0
      dup 1+ precision >=
      if \ everything we need to print is in front of the decimal place
	>r ( f/10^n-steps, R: n-steps )
	precision fprintdigits fdrop r> ( n-steps )
	precision - 1+ emitnzeros \ print n-steps - precision + 1 trailing zeros
	46 emit \ finially, print a '.' for good measure
      else \ last case: we have to print some before and some after the decimal place
	>r ( f/f-10^n-steps, R: n-steps )
	r@ 1+ fprintdigits \ print the digits before the decimal place
	46 emit \ print a '.'
	precision r> - 1- fprintdigits fdrop \ print digits after decimal place
      then
    then
  then
;

: f. ( f -- )
  roundtoprecision f.no-space bl emit ;

\ print a float with engineering notation
: fe. ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    f. \ f. prints zero the same way fe. would
  else
    takecareofsign roundtoprecision
    fdup 3 nfswap smallerpowerof10 ( f n-steps f-10^n-steps )
    fnswap >r f/ f.no-space \ normalize the number and print it
    69 emit r> . \ print the exponent
  then
;

\ print f using scientific notation
: fs. ( f -- )
  \ handle zero seperately
  fdup f0=
  if
    f. \ f. prints zero the same way fs. would
  else
    takecareofsign roundtoprecision
    fdup 1 nfswap smallerpowerof10 ( f n-steps f-10^n-steps )
    fnswap >r f/ f.no-space \ normalize the number and print it
    69 emit r> . \ print the exponent
  then
;

\ prints out the mantissa in engineering notation but leaves the exponent on the stack
: fee. ( f -- n-exponent )
  \ handle zero seperately
  fdup f0=
  if
    f. \ f. prints zero the same way fe. would
    0 \ leave zero on TOS
  else
    takecareofsign roundtoprecision
    fdup 3 nfswap smallerpowerof10 ( f n-steps f-10^n-steps )
    fnswap >r f/ f.no-space \ normalize the number and print it
    r> \ leave the exponent on TOS
  then
;

\ NOTE: THE FOLLOWING WILL BE REWRITTEN MORE CLEANLY LATER
\ prints the SI prefixes for the exponent on the stack
\ so 3 -> k, -6 -> u, etc.
\ if exponent isn't a multiple of three, it's just printed
: si.  ( n-si_pref -- )
dup 3 = if ." k" else
dup 6 = if ." M" else
dup 9 = if ." G" else
dup 12 = if ." T" else
dup 15 = if ." P" else
dup 18 = if ." E" else
dup 21 = if ." Z" else
dup 24 = if ." Y" else
dup -3 = if ." m" else
dup -6 = if ." u" else
dup -9 = if ." n" else
dup -12 = if ." p" else
dup -15 = if ." f" else
dup -18 = if ." a" else
dup -21 = if ." z" else
dup -24 = if ." y" else

dup 0 = if ." " else

dup 69 emit .

then then then then then
then then then then then then
then then then then then then drop ;

\ NOW, FOR INPUT, HELPER WORDS FIRST

\ COME UP WITH BETTER NAMES FOR NEXT TWO
\ returns the number that occupies the part of the string from n-location + 1 to the end
: partnumber ( n-adr n-length n-location -- n )
  swap over - 1- rot rot + 1+ swap ( new_adr new_length )
  number nip 0= if -13 throw then ;

\ the last returned value is true if the charcter was found, and false if not
: extract ( n-adr n-length c-char -- n-adr n-new-length n-extracted true|false )
  >r over over r> cscan nip ( adr count loc )
  over over = 
  if \ character not found
    false
  else \ character found, note that loc becomes new-length
    swap >r ( adr loc, R: length )
    over over r> swap ( adr loc adr length loc )
    partnumber true
  then ;

\ the plan is to first get the exponent (if there is one)
\ then we take care of the sign, if any
\ next, we start storing the digits in to a double while keeping track
\ of the exponent. For example 12.34e will get turned in to the double
\ 1234, so we need to decriment the exponent by two (one for each digit
\ after the decimal place) to get the right answer. If the double fills
\ up, then we stop since the double has more significant digits than a
\ float has. If the float fills up before we get to the decimal place,
\ then we have to add one to the exponent for every digit before the
\ decimal place we miss.
\ the double is then converted to a float, which we divide or multiply
\ by the appropriate power of 10 to get the exponent right
\ finially, we restore the sign

\ string of form 'integer'.'fractioal'e'exp'
: string>float ( c-addr u-length -- f )
  \ get exponent first -- this is the number that follows e, E, d, or D
  101 extract not if drop \ 'e'
  69  extract not if drop \ 'E'
  100 extract not if drop \ 'd'
  68  extract not if drop \ 'D'
    0 \ if you can't find anything, then it's zero
  then then then then

  >r ( adr length, R: exp )

  over c@ 45 = if \ starts with negative sign
    r> true >r >r ( adr length, R: bool-isneg exp)
    1- swap 1+ swap ( adr+1 length-1, R: bool-isneg exp)
  else
    r> false >r >r
  then ( adr length, R: bool-isneg exp)

  over c@ 43 = if \ if it's a plus sign, just ignore it, but reduce string size
    1- swap 1+ swap ( adr+1 length-1, R: bool-isneg exp)
  then

  r> rot rot ( exp adr length, R: bool-isneg )

  [ 0. ] fliteral fnswap ( exp adr d-0 length, R: bool-isneg )
  >r false nfswap r> ( exp adr bool-after_decimal d-0 length, R: bool-isneg )

  0 do ( exp adr bool-after_decimal d-0 )
    \ get next character
    fover drop i + c@ ( exp adr bool-after_decimal d-sum char )
    dup 46 = if \ it's a '.'
      drop \ get rid of character
      fnover if \ we've already encountered a decimal point
        abort
      then
      \ we're after the decimal now, so make bool-after_decimal true
      fnswap drop true nfswap ( exp adr bool-after_decimal d-sum )
    else
      nfover [ 214748364. ] fliteral d> if \ d-sum can't hold any more
        drop \ don't care about character
        fnover not if
          \ we're before the decimal place, so add one to the exponent
          >r >r >r >r 1+ r> r> r> r>
        else
          leave \ there's nothing left to do if we're after the decimal place
        then
      else
        48 - ( exp adr bool-after_decimal d-sum possible-digit )
        dup 0 < ( exp adr bool-after_decimal d-sum pd bool )
        over 9 > ( exp adr bool-after_decimal d-sum pd bool bool )
        or if abort then \ it's not a digit, abort
        ( exp adr bool-after_decimal d-sum digit )
        >r d10* r> s>d d+ ( exp adr bool-after_decimal d-sum )
        fnover if ( exp adr bool-after_decimal d-sum )
          >r >r >r >r 1- r> r> r> r> \ decriment the exponent by one
        then
      then
    then
  loop ( exp adr bool-after_decimal d-sum, R: bool-isneg )

  >r >r drop drop r> r> ( exp d-sum, R: bool-isneg )

  d>f fnswap ( f-sum exp, R: bool-isneg )

  \ next, take care of the exponent
  f10^n f* \ take care of the exponent
  ( f, R: bool-isneg )

  \ take care of negative sign
  r> negateiftrue ;


: >float ( n-c-addr u-length -- f true | false)
  ['] string>float catch
  0= if
    true \ no error encounter -- we have a float
  else
    drop drop false \ couldn't make a float, clear the two inputs off the stack
 then ;

\ OTHER WORDS FROM THE ANS94 STANDARD

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
 
\ WORDS TO SET UP THE RECOGNIZER
: rec-float >float 
  if
    state @ if
      postpone fliteral
    then
    -1
  else
    0
  then 
;

: place-rec ( xt -- )
  get-recognizer
  dup >r
  1-  n>r
  swap
  nr> drop r> 1+
  set-recognizer
;

' rec-float place-rec

\ again, the next line is for convienence, not nescessity
marker ->afterfloat

