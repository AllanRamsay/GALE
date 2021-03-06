
%%%% CHARTYPE.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***************************************************************
Convert a nice readable spelling rule into an FST. characters/2
maps the items that appear in a rule to \scare{characters}, where the
signature for characters is given below. Conventions for items in
spelling rules are

\begin{itemize}
\item
\texttt{a,b,c, \ldots}: the character itself
\item
\texttt{x0, x1, \ldots, c0, \ldots, v0, \ldots, lv0, \ldots, sv0,
\ldots, cv0, \ldots}: variables ranging over arbitrary characters,
consonants, vowels, long vowels, short vowels, semi-vowels (items that
are \emph{both} vowels and consonants). Could easily be extended to
cover other classes if necessary.
\item{m/n/s}: disjunction over \q{m}, \q{n} and \q{s}.
\item{t1:t2}: combination of \texttt{t1} and \texttt{t2}.
\end{itemize}
*******************************************************************/

vector2list(X, L) :-
    X =.. [{}, Y],
    commas2list(Y, L).

commas2list(V, [V]) :-
    var(V),
    !.
commas2list((A, B), [A | L]) :-
    !,
    commas2list(B, L).
commas2list(A, [A]).

set_context(V, V) :-
    var(V),
    !.
set_context([??? | A0], A2) :-
    !,
    reverse(A0, A1),
    append(A1, _A, A2).
set_context(A0, A1) :-
    reverse(A0, A1).

setLeftContext(L0, L1) :-
    set_context(L0, L1).

setRightContext(V, V) :-
    var(V),
    !.
setRightContext([???], _) :-
    !.
setRightContext([], []) :-
    !.
setRightContext([H | T0], [H | T1]) :-
    setRightContext(T0, T1).

enlist(A, A) :-
    (A==[]; var(A); islist(A)),
    !.
enlist(A, [A]).

markInserted(V, _I) :-
    var(V),
    !.
markInserted([], _I).
markInserted([H | T], X) :-
    inserted:char@H <-> I,
    (var(I) -> I = *(X); true),
    markInserted(T, X).

markUninserted(V) :-
    var(V),
    !.
markUninserted([]).
markUninserted([H | T]) :-
    inserted:char@H <-> I,
    (var(I) -> I = -; true),
    markUninserted(T).

make_fst((SURFACE0 ==> UNDERLYING0: LC0 ## RC0 : X :- G), FST) :-
    !,
    enlist(SURFACE0, SURFACE1),
    enlist(UNDERLYING0, UNDERLYING1),
    enlist(LC0, LC1),
    enlist(RC0, RC1),
    make_fst((LC1/SURFACE1/RC1 ==> UNDERLYING1 : X :- G), FST).

make_fst(((LEFT0/SURFACE0/RIGHT0 ==> UNDERLYING0:K0) :- G0), FST) :-
     !,
     (fst(K0) ->
 	K1 = K0;
 	K0 <-> current:fst@K1),
     setLeftContext(LEFT0, LEFT1),
     setRightContext(RIGHT0, RIGHT1),
     characters(LEFT1, LEFTN, [], A0, G0, G1),
     characters(SURFACE0, SURFACE1, A0, A1, G1, G2),
     markUninserted(SURFACE1),
     characters(RIGHT1, RIGHTN, A1, A2, G2, G3),
     characters(UNDERLYING0, UNDERLYING1, A2, _A3, G3, GN),
     (SURFACE1 = [K] ->
 	INSERTED = grapheme:char@K;
 	INSERTED = +),
     markInserted(UNDERLYING1, INSERTED),
     append(SURFACE1, RIGHTN, SURFACEN),
     append(UNDERLYING1, RIGHTN, UNDERLYINGN),
     HEAD = fst(SURFACEN, LEFTN, K1, UNDERLYINGN),
     (GN = true -> FST = HEAD; FST = (HEAD :- GN)).

make_fst(((LEFT0/SURFACE0/RIGHT0 ==> UNDERLYING0:K0)), FST) :-
     !,
     (fst(K0) ->
 	K1 = K0;
 	K0 <-> current:fst@K1),
     setLeftContext(LEFT0, LEFT1),
     setRightContext(RIGHT0, RIGHT1),
     characters(LEFT1, LEFTN, [], A0, true, G1),
     characters(SURFACE0, SURFACE1, A0, A1, G1, G2),
     characters(RIGHT1, RIGHTN, A1, A2, G2, G3),
     characters(UNDERLYING0, UNDERLYING1, A2, _A3, G3, GN),
     append(SURFACE1, RIGHTN, SURFACEN),
     append(UNDERLYING1, RIGHTN, UNDERLYINGN),
     HEAD = fst(SURFACEN, LEFTN, K1, UNDERLYINGN),
     (GN = true -> FST = HEAD; FST = (HEAD :- GN)).

make_fst((SURFACE ==> UNDERLYING: LEFT ## RIGHT :- G), RULE) :-
    makeImanRule(SURFACE, UNDERLYING, LEFT, RIGHT, G, RULE).
    
make_fst(((SURFACE0 ==> (SOURCE0:K0)) :- G0), FST) :-
     append(CONTEXT0, [STAR | SURFACE1], SURFACE0),
     STAR == ^,
     (fst(K0) ->
 	K1 = K0;
 	K0 <-> current:fst@K1),
     !,
     set_context(CONTEXT0, CONTEXT1),
     characters(CONTEXT1, CONTEXT, [], A0, G0, G1),
     characters(SURFACE1, SURFACE, A0, A1, G1, G2),
     characters(SOURCE0, SOURCE, A1, _A2, G2, G3),
     HEAD = fst(SURFACE, CONTEXT, K1, SOURCE),
     (G3 = true -> FST = HEAD; FST = (HEAD :- G3)).

makeOpen([], []).
makeOpen([??? | T], _L) :-
	!,
	(T = [] ->
	 true;
	 throw('??? should be at the end')).
makeOpen([H | T0], [H | T1]) :-
	makeOpen(T0, T1).

makeImanRule(SURFACE0, UNDERLYING0, LEFT0, RIGHT0, G0, (imanRule(LEFTN, TARGET, REPLACEMENT) :- GN)) :-
    !,
    enlist(SURFACE0, SURFACE1),
    enlist(UNDERLYING0, UNDERLYING1),
    enlist(LEFT0, LEFT1),
    enlist(RIGHT0, RIGHT1),
    characters(LEFT1, LEFT2, [], A0, G0, G1),
    reverse(LEFT2, LEFT3),
    makeOpen(LEFT3, LEFTN),
    characters(SURFACE1, SURFACEN, A0, A1, G1, G2),
    markUninserted(SURFACEN),
    characters(RIGHT1, RIGHT2, A1, A2, G2, G3),
    makeOpen(RIGHT2, RIGHTN),
    characters(UNDERLYING1, UNDERLYING2, A2, _A3, G3, GN),
    append(SURFACEN, RIGHTN, TARGET),
    append(UNDERLYING2, RIGHTN, REPLACEMENT).

inCharacterRange(C) :-
    ((C > 64, C < 91); (C > 96, C < 123); cmember(C, "`'$|<>*~}?@.{!^")).

lowerCase(X) :-
    atom_codes(X, [C | _]),
    (C > 96, C < 123).

setChoices(U, X/Y, (CX; CY)) :-
    !,
    setChoices(U, X, CX),
    setChoices(U, Y, CY).
setChoices(U, X, U=X).

setTriggeredChoice(U, C0) :-
    setChoices(U, C0, C1),
    trigger(U, C1).

character(V, C, A, A, G, G) :-
    var(V),
    !,
    grapheme:char@C <-> V.
character(???, ???, A, A, G, G) :-
    !.
character({}, C, A, A, G, G) :-
    !,
    C <> character.
character(X/Y, CHAR, A, A, G, G) :-
    string(Y),
    underlying:char@CHAR <-> U,
    +vowel:char@CHAR,
    -long:char@CHAR,
    !,
    setTriggeredChoice(U, X/Y).
character(X/Y, CHAR, A, A, G, (T, G)) :-
    grapheme:char@CHAR <-> C,
    !,
    plant_tests(X/Y, C, T).
character(P:C, C, A0, A1, G0, G1) :-
    var(C),
    !,
    character(P, C, A0, A1, G0, G1).
character(C:P, C, A0, A1, G0, G1) :-
    var(C),
    !,
    character(P, C, A0, A1, G0, G1).
character(V, C, A0, A1, G0, G1) :-
    vector2list(V, L),
    !,
    character(L, C, A0, A1, G0, G1).
character(CHAR, CHAR, A, A, G, G) :-
    CHAR <> character,
    !.
character(S, CHAR, A0, A1, G0, G1) :-
    string(S),
    !,
    addCharFeatures([S], CHAR, A0, A1, G0, G1).
character(Q, C, A0, A1, G0, G1) :-
    Q <-> [_ | _],
    !,
    addCharFeatures(Q, C, A0, A1, G0, G1).
character(?, CHAR, A, A, G, G) :-
    grapheme:char@CHAR <-> ?,
    !,
    chartype(CHAR).
character(X, CHAR, A0, A1, G, G) :-
    atom(X),
    atom_codes(X, CHARS),
    vowel:char@CHAR <-> VOWEL,
    underlying:char@CHAR <-> U,
    long:char@CHAR <-> LONG,
    (CHARS = [C] ->
      (grapheme:char@CHAR <-> X,
	  U = [C],
	  inCharacterRange(C),
	  chartype(CHAR),
	  A1 = A0);
      CHARS = [0'c, _N] ->
	(VOWEL = -,
	(cmember(X=CHAR, A0) ->
	    A1 = A0;
		A1 = [X=CHAR | A0]));
	CHARS = [0'v, _N] ->
	(VOWEL = +,
	    (cmember(X=CHAR, A0) ->
		A1 = A0;
		A1 = [X=CHAR | A0]));
	CHARS = [0'l, 0'v, _N] ->
	(cmember(X=CHAR, A0) ->
	    A1 = A0;
	    (VOWEL = +, LONG = +, A1 = [X=CHAR | A0]));
	CHARS = [0's, 0'v, _N] ->
	(cmember(X=CHAR, A0) ->
	    A1 = A0;
	    (VOWEL = +, LONG = -, A1 = [X=CHAR | A0]));
	CHARS = [0's, 0'c, _N] ->
	(cmember(X=CHAR, A0) ->
	    A1 = A0;
	    (A1 = [X=CHAR | A0]));
	CHARS = [0'x, _N] ->
	(cmember(X=CHAR, A0) ->
	    A1 = A0;
	    (A1 = [X=CHAR | A0]));
	CHARS = [_C1, _C2] ->
	(U = CHARS,
	    +multiple:char@CHAR,
	    A1 = A0)),
    !.
character(X, CHAR, A, A, G, G) :-
	atom_codes(X, [_C0, _C1, _C2 | _REST]),
	!,
	GOAL =.. [X, CHAR],
	GOAL.
character(X, X, A, A, G, G).

simpleCharacter(V, _C) :-
    var(V),
    !.
simpleCharacter(?, CHAR) :-
    grapheme:char@CHAR <-> ?,
    !,
    chartype(CHAR).
simpleCharacter(N, CHAR) :-
    integer(N),
    !,
    atom_codes(X, [N]),
    simpleCharacter(X, CHAR).
simpleCharacter(X, CHAR) :-
    atom(X),
    atom_codes(X, CHARS),
    underlying:char@CHAR <-> U,
    (CHARS = [C] ->
      (grapheme:char@CHAR <-> X,
	  U = [C],
	  chartype(CHAR),
	  A1 = A0);
	CHARS = [_C1, _C2] ->
	(U = CHARS,
	    +multiple:char@CHAR,
	    A1 = A0)).

character(X, C) :-
    simpleCharacter(X, C),
    default(-multiple:char@C).

characters(V, V) :-
    var(V),
    !.
characters(L0, L2) :-
    reverse(L0, L1),
    rcharacters(L1, L2).

rcharacters([], []).
rcharacters([H0 | T0], [H2 | T1]):-
    character(H0, H2),
    rcharacters(T0, T1).

addCharFeatures([], _C, A, A, G, G).
addCharFeatures([F0 | FF], C, A0, A1, G0, G1) :-
    string(F0),
    !,
    underlying:char@C <-> F0,
    ((nonvar(F0), cmember(F0, ["a", "i", "u", "o"])) ->
	(grapheme:char@C = '?',
	    +vowel:char@C,
	    -long:char@C);
	true),
    addCharFeatures(FF,C, A0, A1, G0, G1).
addCharFeatures([X | FF], C, A0, AN, G0, GN) :-
    (atom(X); X = _/_),
    !,
    character(X, C, A0, A1, G0, G1),
    addCharFeatures(FF, C, A1, AN, G1, GN).
addCharFeatures([c=U0 | FF], C, A0, AN, G0, GN) :-
    U1 <-> grapheme:char@C,
    !,
    U0 = U1,
    addCharFeatures(FF, C, A0, AN, G0, GN).
addCharFeatures([u=U0 | FF], C, A0, AN, G0, GN) :-
    U1 <-> underlying:char@C,
    !,
    ((nonvar(U0), U0 = X/Y) ->
	setTriggeredChoice(U1, X/Y);
	U0 = U1),
    addCharFeatures(FF, C, A0, AN, G0, GN).
addCharFeatures([F0=V | FF],  C, A0, AN, G0, GN) :-
    !,
    compoundFeature(F0, char, F1),
    TEST =.. [F1, C=>V],
    TEST,
    addCharFeatures(FF, C, A0, AN, G0, GN).
addCharFeatures([F0 | FF], C, A0, A1, G0, G1) :-
    ((nonvar(F0), F0 =.. [A, B]) ->
	(((A = +; A = -) -> (F1 = B, K = A); (F1 = A, K = B)),
	    compoundFeature(F1, char, F2),
	    TEST =.. [F2, C=>K],
	    TEST);
	true),
    addCharFeatures(FF,C, A0, A1, G0, G1).

plant_tests(X/Y, C, (TA;TB)) :-
    !,
    plant_tests(X, C, TA),
    plant_tests(Y, C, TB).
plant_tests(X, C, X=C).

characters(V, V, A, A, G, G) :-
    var(V),
    !.
characters([], [], A, A, G, G).
characters([H0 | T0], [H1 | T1], A0, A2, G0, G2) :-
    character(H0, H1, A0, A1, G0, G1),
    !,
    characters(T0, T1, A1, A2, G1, G2).

shortVowelChar(V) :-
    cmember(V, [a, e, i, o, u]).
longvowelChar(V) :-
    cmember(V, [w, y, '|', 'A', 'I', 'O', '}']).

vowelChar(V) :-
    shortVowelChar(V);
    longvowelChar(V).
consonantChar(C) :-
    cmember(C, 
           ['B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
	    'P', 'O', 'Q', 'R', 'S', 'T', 'V', 'W', 'X', 'Y', 'Z', 
	    '<', '>', '|', 'A', $, '}', b, c, d, f, g, h, j, k, l, m, n, p, q, r, s,
	    t, v, w, x, @, y, z, '`', '''', ~, *, ^]).

capital(CAP) :-
    cmember(CAP, ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z','�', '�', '�']).

sibilant([_ANY | "x"]).
sibilant("sh").
sibilant("ch").
sibilant("ss").

  /*
sound("o", S) :-
    sukun:char@S <-> *(phoneme:char@S).
sound("a", S) :-
    S <> [+open, -high, -rounded, -sukun, -long, +vowel]:char,
    +back:char@S,
    emph:char@S <-> back:char@S.
sound("A", S) :-
    S <> [+open, -high, -rounded, -sukun, +long, +vowel]:char,
    emph:char@S <-> back:char@S.
sound("|", S) :-
    S <> [+open, -high, -rounded, -sukun, +long]:char.
sound("i", S) :-
    S <> [+open, -back, +high, -rounded, -sukun, -long, +vowel]:char.
sound("y", S) :-
    S <> [+open, -back, +high, -rounded, -sukun, +long]:char.
sound("u", S) :-
    S <> [+open, +back, +high, +rounded, -sukun, -long, +vowel]:char.
sound("w", S) :-
    S <> [+open, +back, +high, +rounded, -sukun, +long]:char.
sound("e", S) :-
    S <> [-long, +vowel]:char.
sound("n", S) :-
    S <> [-vowel, -sukun]:char.
sound("^", S) :-
    S <> [-vowel, -sukun, manner=stop]:char.
sound("t", S) :-
    S <> [-vowel, -sukun, manner=stop]:char.
sound("W", S) :-
    S <> [-vowel, -sukun, manner=stop]:char.
sound("O", S) :-
    S <> [-vowel, -sukun, manner=stop]:char.
sound(_, S) :-
    S <> [-sukun, -high, -vowel]:char.
*/

chartype(CHAR) :-
    grapheme:char@CHAR <-> ?,
    !,
    +vowel:char@CHAR,
    +query:char@CHAR.
chartype(X) :-
    grapheme:char@X <-> CHAR,
    atom_codes(CHAR, underlying:char@X),
    sound(CHAR, X).

plantBanned :-
    retractall(banned(_, _)),
    retractall(banned(_, _, _)),
    ban(L, X),
    addToBanned(L, X),
    fail.
plantBanned.

addToBanned(B0, X) :-
    characters(B0, B1, [], _, _, _),
    addToBanned(0, B1, X).

addToBanned(N, [], X) :-
    assertWhen(banned(N, X)).
addToBanned(I, [H0 | T], X) :-
    H0\(grapheme:char) <-> H1\(grapheme:char),
    (grapheme:char@H0 == '?' ->
	true;
	grapheme:char@H0 = grapheme:char@H1),
    ((banned(I, J, H2), msubsume(H1, H2)) ->
	true;
	(newindex(J),
	    assertWhen(banned(I, J, H1)))),
    addToBanned(J, T, X).

checkBanned(L, X) :-
    checkBanned(0, L, X).
checkBanned([_ | L], X) :-
    checkBanned(L, X).

checkBanned(I, _, X) :-
    banned(I, X).
checkBanned(I, [H | T], X) :-
    (var(underlying:char@H) ->
	(+vowel:char@H,
	    -long:char@H/*,
	    underlying:char@H = "?"*/);
	true),
    default(-inserted:char@H),
    default(-final:char@H),
    default(-initial:char@H),
    banned(I, J, H),
    checkBanned(J, T, X).

showUnderlying(X0) :-
    (number(X0) ->
     retrieve(X0, X1);
     X1 = X0),
    reverse(underlying@X1, U0),
    tidySign(U0, U1),
    pretty(U1).