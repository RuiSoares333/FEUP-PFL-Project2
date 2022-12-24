:-use_module(library(lists)).
:-use_module(library(between)).

/**
 * replace(+Idx, +L1, +Val, -L2)
 */
replace(Idx, L1, Val, L2) :-
    length(L1, Length),
    Length > Idx,
    replace(Idx, L1, Val, L2, []).

replace(0, [_ | T], Val, L2, Acc) :-
    append(Acc, [Val], Acc1),
    append(Acc1, T, L2), !.

replace(Idx, [H | T], Val, L2, Acc) :-
    Idx > 0,
    append(Acc, [H], Acc1),
    Idx1 is Idx - 1,
    replace(Idx1, T, Val, L2, Acc1).

/**
 * betweenAndEven(+Lower, +Upper, ?X)
 *
 * Generator/verifier of even numbers between lower and upper
 */
betweenAndEven(Lower, Upper, X) :-
    between(Lower, Upper, X),
    X mod 2 =:= 0.

/**
 * betweenAndEven(+Lower, +Upper, ?X)
 *
 * Generator/verifier of odd numbers between lower and upper
 */
betweenAndEven(Lower, Upper, X) :-
    between(Lower, Upper, X),
    X mod 2 =:= 0.


firstNletters(N, Letters) :-
    findall(Letter, isLetterAndBounded(Letter, N), Letters).

isLetterAndBounded(Letter, N) :-
    UpperBound is N+65-1, % code('A') = 65
    between(65, UpperBound, Code),
    char_code(Letter, Code).

isValidDirection(t, _).
isValidDirection(b, _).
isValidDirection(l, b).
isValidDirection(r, r).

directionToOffsets(t, 0, -1).
directionToOffsets(b, 0, 1).
directionToOffsets(l, -1, 0).
directionToOffsets(r, 1, 0).

/**
 * countOcurrences(List, Elem, Res)
 *
 * Counts the ocurrences of an element in a list
 */
countOcurrences(List, Elem, Res) :- countOcurrences(List, Elem, Res, 0).
countOcurrences([], _, Res, Res) :- !.
countOcurrences([Elem | T], Elem, Res, Acc) :-
    Acc1 is Acc + 1,
    countOcurrences(T, Elem, Res, Acc1).
countOcurrences([_ | T], Elem, Res, Acc) :- countOcurrences(T, Elem, Res, Acc).

/**
 * mapsublist(Pred, List, Result)
 *
 * Applies map to the sublists of a matrix
 */
mapsublist(Pred, List, Result) :- mapsublist(Pred, List, Result, []).
mapsublist(_, [], Result, Result).
mapsublist(Pred, [Sub | T], Result, Acc) :-
    maplist(Pred, Sub, NewSub),
    append(Acc, [NewSub], Acc1),
    mapsublist(Pred, T, Result, Acc1).