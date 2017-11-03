/*
      SOMALIMISC.PL
*/

somaliDualForm(X, Z) :-
    dir@Y <> xbefore,
    somaliDualForm(X, Z, Y).

somaliDualForm(X, X, _Y).
somaliDualForm(X, Z, Y) :-
    somaliCliticForm(X, Z, Y).

somaliCliticForm(X, Z, Y) :-
    cat@X <-> compound(Y, ORDER),
    (dir@Y <> xbefore -> ORDER = [Y, Z]; ORDER = [Z, Y]),
    X <> [saturated],
    language@X <-> language@Y,
    language@X <-> language@Z,
    affixes@X <-> [Y],
    Y <> [word, inflected1], %!!!!!!!!!!!!!!!!!!!!
    -affix@Y.
    
somaliWHPro(X) :-
    +clitic@X,
    % +clitic@NP,
    % NP <> notcasemarked,
    NP <> no_case_default,
    wh@X <-> W0/W1,
    [agree, positions]@NP <-> [agree, positions]@X,
    when(nonvar(W1),
	 somaliWH(W0/W1, index@X, NP)).

somaliWH(W/W, _I, _NP).
somaliWH(W0/[NP | W0], _I, NP) :-
    NP <> [np, adjunct],
    target@NP <> np,
    target@NP <-> TARGET,
    dir@TARGET <> xafter,
    % xend@TARGET <-> start@NP,
    % to stop,WH-clauses picking up case markers, 12-01-06
    -modifiable@NP,
    true. % xend@TARGET <-> start@NP.

somaliSmallPron(X) :-
    +clitic@X,
    specifier@X <-> pro_ref.

somaliSmallSubjPron(X, Z) :-
    % X = Z,
    somaliDualForm(X, Z, Y),
    dir@Y <> xbefore,
    somaliSmallPron(Z),
    Z <> reallyNom,
    -after:dir@displaced@Z.

somaliSmallObjPron1(X) :-
    somaliWHPro(X),
    somaliSmallPron(X),
    -moved:dir@displaced@X, % This has to be commented out to allow alternate "swapping" args of compound adpositions
    X <> acc_or_dat.

somaliSmallObjPron2(X) :-
    somaliWHPro(X),
    somaliSmallPron(X),
    -moved:dir@displaced@X, % This has to be commented out to allow alternate "swapping" args of compound adpositions
    X <> acc_or_dat.

somaliPrepCompound(X) :-
    trigger(cat@X,
	    testSomaliPrepCompound(X)).

testSomaliPrepCompound(X) :-
    X <> prep.
testSomaliPrepCompound(X) :-
    cat@X <-> compound(Y, [Y, Z]),
    testSomaliPrepCompound(Z).

somaliDet(X, Z, GEN) :-
    cat@X <-> compound(Y, [Y, Z]),
    modifier@Z <-> *det,
    X <> [saturated],
    % need to catch the fact that the constituents are in the same language
    % at this point rather than leaving it to some later element to sort out
    language@X <-> language@Y,
    language@X <-> language@Z,
    affixes@X <-> [GEN, Y],
    dir@GEN <> xbefore,
    affix@GEN <-> *gender,
    Y <> [word, inflected1],
    dir@Y <> xbefore,
    % to stop it attempting to combine with something which looks like a clitic pronoun
    -clitic@Y,
    -affix@Y,
    agree@Z <-> agree@GEN,
    Z <> [det, saturated],
    target@Z <-> TARGET,
    TARGET <> [nn, fullyUnspecified],
    [cat, agree]@TARGET<-> [cat, agree]@result@Z,
    % Need to describe the result properly
    result@Z <> np,
    agree@Z <-> agree@TARGET,
    +fixed@target@Z,
    xend@TARGET <-> start@Z,
    [def, agree]@Z <-> [def, agree]@result@Z.

somaliComplementiser(X, Z, comp@result@Z) :-
    somaliDualForm(X, Z, Y),
    X = Z,
    dir@Y <> xafter,
    -clitic@Y,
    % +clitic@Y,
    cat@Z <-> focusMarker,
    Z <> [adjunct, inflected],
    target@Z <-> TARGET,
    TARGET <> s,
    -clitic@Z,
    -comp@TARGET,
    modifier@Z <-> *comp(text@Z),
    dir@TARGET <> xbefore,
    semantics@Z <-> identity,
    result@Z <> s.

somaliComplementiser(X, Z) :-
    somaliComplementiser(X, Z, modifier@Z).

zeroCopula :-
    -realised@NP,
    +clitic@NP,
    NP <> setNoWH,
    -predicative@ZERO,
    ZERO <> [xzero, setNoWH, pres_tense],
     % copula is nicer for debugging, printing parse trees, ...
     % But 0 works better for Mbrola!
     % text@ZERO <-> '(copula)',
    [assimilated, text]@core@ZERO <-> [assimilated, text]@ZERO,
    language@ZERO <> somali,
    +fixed@ZERO,
    +active@ZERO,
    args@ZERO <-> [PRED, NP],
    dir@NP <> xbefore,
    dir@PRED <> xafter,
    -moved:dir@displaced@NP,
    -moved:dir@displaced@PRED,
    post_defaults(ZERO),
    NP <> [np, reallyNom, no_case_default],
    dir@NP <> xbefore,
    ZERO <> [verbal, setNoWH, pres_tense],
    -comp@ZERO,
    agree@NP <-> agree@PRED,
    [syntax, meaning, morphology, structure]@subject@ZERO 
        <-> [syntax, meaning, morphology, structure]@NP,
    theta@NP <-> arg(topic),
    theta@PRED <-> arg(predication),
    semantics@ZERO <-> lambda(Z, equiv(Z)),
    PRED <> [np, acc],
    -clitic@PRED,
    -equivalence@PRED,
    agree@NP <-> agree@PRED,
    create_mutable(0, score@ZERO),
    newindex(index@ZERO),
    assertWhena(partial(ZERO)).    
waxa(X) :-
    somaliComplementiser(X, X, *waxa),
    subject@TARGET <-> SUBJ,
    trigger(index@TARGET,
	    (\+ text@core@TARGET = '0',
	      (+value:def@zero@SUBJ ->
		-fullForm@TARGET;
		(+fullForm@TARGET, end@X = start@SUBJ)))),
    target@X <-> TARGET,
    +compact@TARGET,
    args@X <-> [NP],
    NP <> np,
    -clitic@NP,
    dir@NP <> xafter,
    -value:def@zero@NP,
    displaced@NP <> xafter,
    theta@NP <-> arg(focus),
    xstart@target@X <-> end@X,
    modified@result@X <-> 9.9.

baa(X) :-
    somaliComplementiser(X, Z),
    text@Z <-> baa,
    subject@TARGET <-> SUBJ,
    trigger(index@TARGET,
	    (\+ text@core@TARGET = '0',
	      (+value:def@zero@SUBJ ->
		true;
		(+fullForm@TARGET, end@Z = start@SUBJ)))),
    target@Z <-> TARGET,
    args@Z <-> [NP],
    NP <> acc,
    theta@NP <-> arg(focus),
    cat@NP <-> topic,
    NP <> saturated,
    dir@NP <> xbefore,
    -moved:dir@displaced@NP,
    +realised@NP,
    modified@result@Z <-> 9.9.

/*
waxa(X) :-
    X <> [np, inflected1, notcasemarked],
    text@X <-> waxa,
    -caseDefault@X,
    -modifiable@X, % to stop "ka" or relative clauses attaching to it
    [agree, positions]@NP <-> [agree, positions]@X,
    -caseDefault@NP,
    cat@NP <-> topic,
    NP <> saturated,
    target@NP <-> TARGET,
    result@NP <-> RESULT,
    TARGET <> s,
    -comp@TARGET,
    text@core@TARGET <-> '0',
    RESULT <> s,
    comp@RESULT <-> *(waxa),
    wh@X <-> W0/[NP | W0].
*/


somaliPrepCluster(X, Y) :-
    trigger(cat@Y, checkSomaliPrepCluster(X, Y)).

checkSomaliPrepCluster(X, C) :-
    cat@C <-> compound(Y, [Y, _Z]),
    Y <> prep,
    modified@result@X < modified@result@Y.
checkSomaliPrepCluster(X, Y) :-
    Y <> prep,
    modified@result@X < modified@result@Y.
