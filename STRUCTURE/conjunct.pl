 /* 	
 	CONJUNCTIONS.PL  	
	This needs work: at the moment it embodies two rules, of the form
	X --> X & X and X --> both X and X, with a fairly specific set of 	
	features shared between the X's. It needs a better treatment of the 	
	way the various features behave for different categories and different 	
	conjunctions, and it also needs to be fixed to deal with non-constituent 	
	coordination (i.e. "he voted against and effectively doomed the plan")  
	
	Can't see the discrpancies between me and Helen @ 31/07/01

*/  

% because, although, etc.
connective(X, adv(w)):-
    cat@X <-> connective,
    -text@subject@X,
    X <> fullyUnspecified,
    discourse@X <-> DR,
    +fixed@X,
    modifier@X <-> discourse_rel(sent_mod,DR,specf@TARGET),
    semantics@X <-> lambda(J, (J)),
    target@X <-> TARGET,
    mood@X <-> mood@TARGET,
    TARGET <> [s, tensed_form],
    -ellipse@TARGET,
    +fixed@TARGET,
    dir@TARGET <> xafter,
    result@X <-> RESULT,
    modified@RESULT <-> 0.5,
    RESULT <> [s, fullySpecified],
    definite@RESULT <-> identity,
    -moved:dir@displaced@S,
    args@X <-> [S],
    S <> [s, tensed_form],
    -ellipse@S,
    dir@S <> xafter,
    +compact@S,
    theta@S <-> arg(identity),
    theta@X <-> modifier(sentential).

connective(X, adv(d)):-
    cat@X <-> connective,
    -text@subject@X,
    X <> fullyUnspecified,
    discourse@X <-> DR,
    +fixed@X,
    modifier@X <-> discourse_rel(sent_mod,DR,specf@TARGET,mood@TARGET),
    semantics@X <-> lambda(J, J),
    target@X <-> TARGET,
    mood@X <-> mood@TARGET,
    TARGET <> [s, tensed_form],
    -ellipse@TARGET,
    +fixed@TARGET,
    dir@TARGET <> xafter,
    result@X <-> RESULT,
    modified@RESULT <-> 0.5,
    RESULT <> [s, fullySpecified],
    definite@RESULT <-> identity,
    -moved:dir@displaced@S,
    args@X <-> [S],
    S <> [s, tensed_form],
    -ellipse@S,
    dir@S <> xafter,
    +compact@S,
    theta@S <-> arg(identity),
    theta@X <-> modifier(sentential).

connective(X, prefix(d)):-
    X <> [verb, tensed_form],
    -text@subject@X,
    args@X <-> [S2,S1],
    S1 <> [s, tensed_form],
    S2 <> [s, tensed_form],
    +fixed@X,
    dir@S1 <> xafter,
    dir@S2 <> xafter,
    X <> fullyUnspecified,
    theta@S1 <-> arg(identity),
    theta@S2 <-> arg(identity),
    -declarative@X,-interrogative@X,-imperative@X,
    %%mood@X <-> mood@S1,
    -comp@S2,
    discourse@X <-> DR,
    semantics@X <-> discourse_rel(prefix, DR, mood@S1, mood@S2).

% however, moreover, etc.
connective(X, ref):-
    X <> [verb, tensed_form],
    -text@subject@X,
    +fixed@X,
    X <> fullyUnspecified,
    -declarative@X,-interrogative@X,-imperative@X,
    %%mood@X <-> mood@S,
    semantics@X <-> 
    discourse_rel(ref, discourse@X, mood@S),
    args@X <-> [S],
    S <> [s, tensed_form].

connective(X, ref(w)):-
    X <> [verb, tensed_form],
    -text@subject@X,
    +fixed@X,
    X <> fullyUnspecified,
    mood@X <-> mood@S,
    semantics@X <-> 
    discourse_rel(ref, discourse@X),
    args@X <-> [S],
    S <> [s, tensed_form].

connective(X, ref(d)):-
    X <> [verb, tensed_form],
    -text@subject@X,
    +fixed@X,
    X <> fullyUnspecified,
    mood@X <-> mood@S,
    semantics@X <-> 
    discourse_rel(ref, discourse@X, mood@S),
    args@X <-> [S],
    S <> [s, tensed_form].


% but, so, etc.
connective(X, infix(d)):-
    X <> [verb, tensed_form],
    -text@subject@X,
    args@X <-> [S2 | REST],
    S1 <> [s, tensed_form, declarative1],
    S2 <> [s, tensed_form],
    -moved:dir@displaced@S1,
    -moved:dir@displaced@S2,
    +fixed@X,
    dir@S1 <> xbefore,
    dir@S2 <> xafter,
    X <> fullyUnspecified,
    theta@S1 <-> arg(identity),
    theta@S2 <-> arg(identity),
    mood@X <-> mood@S2,
    -comp@S2,
    discourse@X <-> DR,
    semantics@X <-> discourse_rel(infix, DR, mood@S1, mood@S2),
    when(nonvar(index@S2), (REST = []; REST = [S1])).

connective(X, infix(w)):-
    X <> [verb, tensed_form],
    -text@subject@X,
    args@X <-> [S1,S2],
    S1 <> [s, tensed_form],
    S2 <> [s, tensed_form],
    +fixed@X,
    dir@S1 <> xbefore,
    dir@S2 <> xafter,
    X <> fullyUnspecified,
    theta@S1 <-> arg(identity),
    theta@S2 <-> arg(identity),
    mood@X <-> mood@S2,
    %%mood@X <-> mood@S1,
    -comp@S2,
    discourse@X <-> DR,
    semantics@X <-> world_rel(DR).

connective(X, prefix):-
    X <> [verb, tensed_form],
    -text@subject@X,
    args@X <-> [S1,S2],
    S1 <> [s, tensed_form],
    S2 <> [s, tensed_form],
    +fixed@X,
    dir@S1 <> xafter,
    dir@S2 <> xafter,
    %%-moved:dir@dir@S1,
    %%+moved:dir@dir@S2,
    X <> fullyUnspecified,
    theta@S1 <-> arg(identity),
    theta@S2 <-> arg(identity),
    -declarative@X,
    -interrogative@X,
    -imperative@X.

coordinate_set(Y, C1, C2) :-
     C2 <> [no_case_default],
     case@C1 <-> case@C2,
     [cat, case, predicative, result, vform, agree, date, kspec]@Y
       	<-> [cat, case, predicative, result, vform, agree, date, kspec]@C2,
     -moved:dir@displaced@C2,
     %% xstart@C1 <-> end@core@Y,
     %% xend@C2 <-> start@core@Y,
     modifier@Y <-> conjmods(modifier@C2, modifier@C1),
     % arg@Y <-> conjarg(arg@C2, arg@C1),
     arg@Y <-> identity,
     [dir, syntax]@target@Y <-> [dir, syntax]@target@C2,
     syntax@result@Y <-> syntax@result@C2,
     trigger(index@target@Y, conjointModifiers(target@C1, target@C2)),
     -moved:dir@displaced@C1,
     -moved:dir@displaced@C2,
     [cat, compact, specified]@C1 <-> [cat, compact, specified]@C2,
     value:def@zero@subject@C1 <-> value:def@zero@subject@C2,
     trigger(index@C1, fineCoordination(span@Y, C1, C2)).  

conjointModifiers(C1, C2) :-
    [dir, syntax]@C1 <-> [dir, syntax]@C2.

fineCoordination(SPANY, C1, C2) :-
    0 is SPANY /\ span@C1,
    length(args@C1, L), 
    length(args@C2, L),
    !,
    ((verb(C1), unsaturated(C1)) ->
        [agree, finite]@C1 <-> [agree, finite]@C2;
        true).

checkConjointArgs([], []).
checkConjointArgs([H0 | T0], [H1 | T1]) :-
    head@H0 <-> head@H1,
    checkConjointArgs(T0, T1).

conjunction(X, infix(CONJ)) :-
     +fixed@X,
     coordinate_set(X, C1, C2),
     -moved:dir@displaced@C1,
     -moved:dir@displaced@C2,
     theta@C1 <-> arg(conjunct),
     theta@C2 <-> arg(conjunct),
     % so we only get A & (B & C)
     -conj@C2,
     dir@C2 <> xbefore,
     dir@C1 <> xafter,
     % Dead simple version restored by Allan, 5/10/01
     semantics@X <-> CONJ,
     conj@X <-> infix(CONJ),
     C1 <> genuine,
     C2 <> genuine,
     % arg@X <-> arg@C1,
     % Removed by Allan, 1/12/00
     % definite@X <-> identity,
     trigger(index@C1, nonvar(cat@C1)),
     COMMA <> comma3,	% Skip unnecessary comma
     dir@COMMA <> xbefore,
     theta@COMMA <-> arg(dummy),
     args@C2 <-> REST,
     -moved:dir@displaced@COMMA,
     trigger(index@C2, checkConjointArgs(args@C1, REST)),
     (args@X = [C1, C2 | REST];
	 args@X = [COMMA, C1, C2 | REST]),
     ptag:tag@tag@X <> tag(conj1).

conjunction(X, prefix(TYPE)) :-
     +fixed@X,
     -modifiable@X,
     coordinate_set(X, Y1, _Y2),
     args@X <-> [Y1],
     dir@Y1 <> xafter,
     Y1 <> genuine,
     semantics@X <> identity,
     conj@Y1 <-> infix(TYPE),
     conj@X <-> prefix(TYPE),
     ptag:tag@tag@X <> tag(conj2).   

nonConstituentCoordination :-
    conjunction(C, infix(_)),
    C <> word,
    partial(C),
    !,
    nonConstituentCoordination(C).
    
nonConstituentCoordination(C) :-
    start@C <-> end@P0,
    partial(P0),
    +compact@P0,
    args@P0 <-> [A0 | _ARGS0],
    pretty('Looking for second partial'),
    args@P1 <-> [A1 | _ARGS1],
    syntax@A1 <-> syntax@A0,
    [cat, agree, tensed]@P0 <-> [cat, agree, tensed]@P1,
    partial(P1),
    +compact@P1,
    start@P1 > start@C,
    pretty('Found partials'),
    [nonfoot, subcat]@A0 <-> [nonfoot, subcat]@X1,
    end@X1 <-> start@X1,
    X1 <> [saturated, word],
    [xstart, xend]@X1 <-> [start, end]@X1,
    text@X1 <-> dummy,
    underlying@X1 <-> dummy,
    -date@X1,
    semantics@X1 <-> dummyConjunct(index@X1),
    span@X1 <-> 0,
    +compact@X1,
    -moved:dir@displaced@X1,
    language@X1 <-> language@P0,
    [start, end, index, text, underlying]@core@X1
      	<-> [start, end, index, text, underlying]@X1,
    -realised@X1,
    -moved:dir@displaced@X1,
    syntax@X1 <-> syntax@X2,
    pretty(before),
    \+ \+ (complete(X2), start@X2 > start@C),
    (newDummy(X1, end@P0) -> true; true),
    newDummy(X1, end@P1).
    
newDummy(X1, start@X1) :-
    \+ complete(X1),
    p_combine(X1).
    
    
    
