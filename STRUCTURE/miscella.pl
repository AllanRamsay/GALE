/*
	MISCELLANY.PL

	Various bits and pieces. What else could it be?

*/

complementiser(X, comp@X) :-
    Y <> [setNoWH, s, tensed_form, genuine],
    -modifier@X,
    -comp@Y,
    [cat, finite]@X <-> [cat, finite]@Y,
    semantics@X <-> identity,
    theta@Y <-> arg(dummy),
    aspect@X <-> lambda(A, lambda(_B, A)),
    dir@Y <> xafter,
    args@X <-> [Y].

complementiser(X) :-
    complementiser(X, *text@X).

basicArabicComplementiser(X, Y) :-
    X <> inflected1,
    -moved:dir@displaced@Y,
    irreal@X <-> irreal@Y,
    +pause@X,
    pitchMark:phoneme@phon@X <-> l,
    -comp@Y,
    +compact@Y,
    [cat, finite]@X <-> [cat, finite]@Y,
    arg@X <-> identity,
    +fixed@X,
    definite@X <-> dummy,
    semantics@X <-> comp.

arabicComplementiser(X, Y) :-
    basicArabicComplementiser(X, Y),
    +realised@subject@Y,
    theta@Y <-> arg(dummy),
    args@X <-> [Y].

'?anOr?anna'(X, Y) :-
    % verb initial
    comp@X <-> *COMP,
    softParse(COMP,
	      start@core@Y = xstart@Y,
	      compSubjPos(index@core@Y, start@core@Y, xstart@Y, COMP),
	      X),
    subject@Y <> reallyNom,
    -main@Y,
    Y <> subjunctive,
    text@X <-> {'?an'}.

'?anOr?anna'(X, Y) :-
    comp@X <-> *COMP,
    softParse(COMP,
	      start@subject@Y = xstart@Y,
	      compSubjPos(index@core@Y, start@subject@Y, xstart@Y, COMP),
	      X),
    +realised@subject@Y,
    subject@Y <> acc,
    -main@Y,
    Y <> indicative,
    % commented out to allow for Hanady's 15/02/05 set of examples
    % core@Y <> verb,
    text@X <-> {'?anna'}.

checkPredCase(PRED) :-
    reallyNom(PRED).
checkPredCase(PRED) :-
    pcase(PRED).

checkPredPosition(INDEF, PRED) :-
    (INDEF = - ->
	( % PRED <> reallyNom, % commented out for Hanady's 15/02/05 examples
	    -moved:dir@displaced@PRED);
	(PRED <> [pcase, np], +moved:dir@displaced@PRED)).
