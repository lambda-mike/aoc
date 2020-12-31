/*
mainA('sample.txt', Result).
mainA('input.txt', Result).
*/

/* 888331 */
solveA(Nums, Result) :-
  member(X, Nums),
  member(Y, Nums),
  X =\= Y,
  X + Y =:= 2020,
  X * Y =\= 0,
  Result is X * Y.

/* Test for sample; should be: 514579
solveA([ 1721, 979, 366, 299, 675, 1456 ], Result).
*/

/* 130933530 */
solveB(Nums, Result) :-
  member(X, Nums),
  member(Y, Nums),
  member(Z, Nums),
  X =\= Y,
  Z =\= Y,
  X + Y + Z =:= 2020,
  X * Y * Z =\= 0,
  Result is X * Y * Z.

read_file(Stream, []) :-
  at_end_of_stream(Stream).
read_file(Stream, [X|L]) :-
  \+ at_end_of_stream(Stream),
  catch((read_integer(Stream, X), read_file(Stream, L)), E, (X = 0, L = [])).

mainA(Name, Result) :-
  open(Name, read, File, [eof_action(eof_code)]),
  read_file(File, Nums),
  close(File),
  solveA(Nums, Result).

mainB(Name, Result) :-
  open(Name, read, File, [eof_action(eof_code)]),
  read_file(File, Nums),
  close(File),
  solveB(Nums, Result).

main :-
  open('input.txt', read, File, [eof_action(eof_code)]),
  read_file(File , Nums),
  close(File),
  solveA(Nums, A),
  solveB(Nums, B),
  write('A: '),
  write(A),
  write('\n'),
  write('B: '),
  write(B),
  write('\n'),
  halt.

/* So executable works */
top_level :-
  main.
