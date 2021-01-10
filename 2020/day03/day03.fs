0 Value fd-in
: open-input ( addr u -- )  r/o open-file throw to fd-in ;
: close-input ( -- )  fd-in close-file throw ;

\ 32 characters should be enough, increase if lines are longer
32 Constant max-line
Create line-buffer max-line 2 + allot

: tree ( -- t ) s" #" ;

\ Check if row is the toboggan's row
: is-toboggan-row ( ty row w -- ty row w b )
  { ty row w }
  ty row w
  ty row = ;

\ Given ith index in line-buffer, return string of length 1 at that position
: get-char ( ith -- addr 1 )
  line-buffer swap +
  1 ;

\ Move toboggan to the next position on the slope
: move-toboggan ( dx dy tx ty w -- tx ty )
  { dx dy tx ty w }
  tx dx + w mod ty dy + ;

\ dx delta x; dy delta y; n trees so far; toboggan x; y; row; w row width;
: slide-to-pos ( dx dy n tx ty row w -- dx dy n tx ty row )
  { dx dy n tx ty row w }
  dx dy
  tx get-char tree compare 0= if
    n 1+
  else
    n
  endif
  dx dy tx ty w move-toboggan
  row ;

\ Move to next row
: next-row ( row -- row ) 1+ ;

\ Return number of crashes with trees
: count-crashes ( dx dy -- dx dy n )
  0   \ n - trees num
  0 0 \ tx ty toboggan pos
  0   \ row
  begin
    line-buffer max-line fd-in read-line throw
  while
    is-toboggan-row if
      slide-to-pos
    else
      \ remove w produced by read-line
      drop
    endif
    next-row
  repeat
  \ drop everything until n
  2drop 2drop ;

\ TODO save whole input file in the memory before processing
: solve ( dx dy file -- result )
  { dx dy addr x }
  addr x open-input
  dx dy count-crashes
  close-input
  \ drop dx dy
  rot rot 2drop ;

: solve-a ( addr x -- result ) 3 1 2swap solve ;

: solve-b ( addr x -- result )
  { addr x }
  1 1 addr x solve
  3 1 addr x solve *
  5 1 addr x solve *
  7 1 addr x solve *
  1 2 addr x solve *
  ;

: sample
  s" Solving sample A... " type
  s" sample.txt" solve-a .
  s" Solving sample B... " type
  s" sample.txt" solve-b . ;

: main
  \ 151
  s" Solving Day03A... " type
  s" input.txt" solve-a .
  \ 7540141059
  s" Solving Day03B... " type
  s" input.txt" solve-b .
  ;
