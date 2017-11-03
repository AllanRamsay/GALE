
/*
	LEXICALFUNCTIONS.PL
	
	Rearranged to use WHEN rather than constraints, 31/07/01
	
*/

softzero(OBJECT, X) :-
    theta@OBJECT <-> arg(object),
    softzero1(OBJECT, X).

softzero1(OBJECT, X) :-
    OBJECT <> np,
    -usedef:def@zero@OBJECT,
    trigger(index@OBJECT, extendWithX(missing_arg(theta@OBJECT, index@OBJECT, index@X), failures@X)).

vpcomp(X, SCOPE) :-
    -value:def@zero@X,
    args@X <-> [SUBJECT],
    X <> vp,
    theta@X <-> arg(event(theta@SUBJECT, mood@X, SCOPE)).

vpcomp(X) :-
    X <> vpcomp(0).
    
scomp(X, SCOPE) :-
    -value:def@zero@X,
    X <> s,
    ZV <-> value:def@zero@subject@X,
    MV <-> semantics@subject@X,
    trigger(index@X, (ZV == + -> MV = scompZeroSubj; true)),
    theta@X <-> arg(event(mood@X, ZV, SCOPE)).

scomp(X) :-
    X <> scomp(1).

gnoun(X) :-
    X <> nominal,
    -usedef:def@generic@X,
    -date@X,
    default(predicative@X <-> *(_)),
    ptag:tag@tag@X <> tag(noun).

basic_noun(X) :-
    X <> noun,
    ptag:tag@tag@X <> tag(noun).

pconstraints(X, pconstraints@X).