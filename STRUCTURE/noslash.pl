
/*
	NOSLASH.PL

	Various common feature/value specifications.

	This MUST appear before pureslash and classes in the top-level file.
*/

intransitive(X) :-
    args@X <-> [].

saturated(X) :-
    args@X <-> [].

unknown(X) :-		% NOT to be used inline
    \+ \+ var(index@X).

setNoWH(X) :-
    wh@X <-> [].
testNoWH(X) :-
    wh@X <-> [].
setWHMarked(X) :-
    wh@X <-> [_ | _].
testWHMarked(X) :-
    wh@X <-> [_ | _].
checkWHMarked(X, WH) :-
    wh@X <-> [WH].
setWHMarked(X, WH) :-
    wh@X <-> [WH].

/*
setNoWH(X) :-
    wh@X <-> WH/WH.

setWHMarked(X) :-
    wh@X <-> WH/[_ | WH].

testNoWH(X) :-
    wh@X <-> WH,
    trigger(WH, (WH = W0/W1, W0 == W1)).

testWHMarked(X) :-
    \+ \+ wh@X <-> []/[_WH].
*/

