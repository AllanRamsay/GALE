
/*
	FOCUS.PL

	If something of type X is included between the focussing brackets [...] 
	then create an object of type focus(X), to be used by focussing 
	operators like "only".

*/


focus(X, start) :-
    cat@X <-> focus,
    target@X <> x,
    target@X <> word,
    mod@target@X <-> mod@result@X,
    -moved:dir@displaced@X,
    modified@result@X <-> 5,
    X <> [adjunct, saturated],
    modifier@X 
     <-> lambda(_A, 
           lambda(B, qq(0.25, focus1(B, qq(0.3, lambda(C, C)))))),
    semantics@X <-> yyy.

focus(X, end) :-
    cat@X <-> focus,
    X <> saturated,
    -modifier@X.
