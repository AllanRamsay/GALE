/*
	IDIOMS.PL
*/

idiom(X, L) :-
    +fixed@X,
    plantIdiom(L, args@X).
    
plantIdiom([], []).
plantIdiom([W | L0], [X | L1]) :-
    atom(W),
    X <> [word, no_case_default],
    -moved:dir@displaced@X,
    !,
    underlying@X <-> W,
    plantIdiom(L0, L1).
plantIdiom([X | L0], [X | L1]) :-
    plantIdiom(L0, L1).