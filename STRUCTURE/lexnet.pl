
%%%% LEXNET.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**********************************************************************
\medpara
This manages the lexical trie. Not for the faint-hearted.

\medpara
lexnet/3 builds the tree, and isn't too bad. Walk through your
string: if there's a transition from the last position via the current
letter then follow it, otherwise add one, and when you get to the end
add an entry. The only complication arises when you have a
\scare{branch}, \eg \q{man}/\q{men}, where you want to rejoin after
the branch but you also want to know which route you followed. Deal
with by findBranches/5 and plantBranches/4.

\medpara
Looking something up in the trie is \underline{complicated}. Ought to
be straightforward, but it isn't. There are two problems. (i) We have
to do it very efficiently, so I've optimised various bits to the point
where I don't understand why I've done some things (\eg why there are
cuts at particular points), and (ii) we have to invoke spelling rules
\emph{as we go}, which makes things awkward. I really can't bear to
give a proper description of what's going on here. Anyone who really
needs to know for some reason should talk to me.
****************************************************************/

/*a2s(BRANCH, branch@X1)
	LEXNET.PL

	Deal with dictionaries by turning them into networks.

*/

/*** insist that item is/isn't right at the end of a word ***/
terminal(X, +) :-
    postfix:fst@fstIn@X <-> [].
terminal(X, -) :-
    postfix:fst@fstIn@X <-> [_ | _].

/*** insist that item is/isn't right at the start of a word ***/
initial(X, +) :-
    items:fst@fstIn@X <-> [].
initial(X, -) :-
    prefix:fst@fstIn@X <-> [_, _ | _].

/*** convert ASCII code to 'character' or atom ***/
s2a([], []).
s2a([H0 | T0], [H1 | T1]) :-
    inCharacterRange(H0),
    atom_codes(char:character@H1, [H0]),
    chartype(H1),
    !,
    s2a(T0, T1).
s2a([H0 | T0], [H1 | T1]) :-
    atom_codes(H1, [H0]),
    s2a(T0, T1).

/*** convert atom to 'character' ***/
a2s([], []).
a2s([H | T0], [X | T1]) :-
    C <-> char:character@H,
    !,
    atom_codes(C, [X]),
    a2s(T0, T1).
a2s([H | T0], [X | T1]) :-
    atom(H),
    !,
    atom_codes(H, [X]),
    a2s(T0, T1).
a2s([H0 | T0], [H0 | T1]) :-
    a2s(T0, T1).

replaceQueryTransitions :-
    format('Replacing zero transitions~n', []),
    char:character@C0 <-> ?,
    %% nonvar(underlying:character@C0),
    C1\(char:character) <-> C0\(char:character),
    retract(transition(N0, C0, N1, X)),
    assert(transition(N0, C1, N1, X)),
    fail.
replaceQueryTransitions.

/***
Used for finding all the branches inside a (.../.../...) element of
a lemma
***/
findBranches([')' | L], L, BRANCH, BRANCHES, [BRANCH | BRANCHES]) :-
    !.
findBranches(['/' | L0], LN, BRANCH, BRANCHES0, BRANCHESN) :-
    !,
    findBranches(L0, LN, [], [BRANCH | BRANCHES0], BRANCHESN).
findBranches([X | L0], L1, B0, BRANCHES0, BRANCHESN) :-
    append(B0, [X], B1),
    findBranches(L0, L1, B1, BRANCHES0, BRANCHESN).

/***
extend a route through a trie as necessary 
***/

lexnet(N0, ['(' | L0], K) :-
    !,
    newindex(N1),
    language@entry:lexentry@K <-> language@X,
    assert(diversion(N0, N1, <<, 1, language@X)),
    (findBranches(L0, L1, [], [], BRANCHES) ->
	(plantBranches(BRANCHES, N1, N2, K), lexnet(N2, L1, K));
	throw(unclosedBranch(L0))).
lexnet(N0, [H | T], K) :-
    retract(transition(N0, H, N1, language@M0)),
    language@entry:lexentry@K <-> language@M0,
    language@entry:lexentry@K <-> language@M1,
    mgu(entry:lexentry@K, M0, M1),
    assert(transition(N0, H, N1, language@M1)),    !,
    lexnet(N1, T, K).
lexnet(N0, [H | T], K) :-
    newindex(N1),
    language@entry:lexentry@K <-> language@E,
    assert(transition(N0, H, N1, language@E)),
    lexnet(N1, T, K).
lexnet(N0, [], K) :-
    ((entry(N0, K0), msubsume(K0, K)) ->
	true;
	assertWhen(entry(N0, K))).
plantBranches([], _N0, _N1, _K).
plantBranches([B | BB], N0, N1, K) :-
    plantBranch(B, N0, N1, K),
    plantBranches(BB, N0, N1, K).

plantBranch([], N0, N1, K) :-
    (var(N1) -> newindex(N1); true),
    language@entry:lexentry@K <-> language@X,
    assert(diversion(N0, N1, >>, -1, language@X)).
plantBranch([H | T], N0, N2, K) :-
    retract(transition(N0, H, N1, M0)),
    language@entry:lexentry@K <-> language@M0,
    mgu(entry:lexentry@K, M0, M1),
    assert(transition(N0, H, N1, language@M1)),
    !,
    plantBranch(T, N1, N2, K).
plantBranch([H | T], N0, N2, K) :-
    newindex(N1),
    language@entry:lexentry@K <-> language@M1,
    assert(transition(N0, H, N1, language@M1)),
    plantBranch(T, N1, N2, K).

mgu(X, Y, X) :-
    X == Y,
    !.
mgu(A, B, _) :-
    (atomic(A); atomic(B); var(A); var(B)),
    !.
mgu([H0 | T0], [H1 | T1], [H | T]) :-
    !,
    mgu(H0, H1, H),
    mgu(T0, T1, T).
mgu(X0, X1, X) :-
    functor(X0, F, N),
    functor(X1, F, N),
    !,
    X0 =.. [_ | L0],
    X1 =.. [_ | L1],
    mgu(L0, L1, L),
    X =.. [F | L].
mgu(_, _, _).

clearnet :-
    clearnet1(_).
clearnet(L) :-
    F =.. [L, language@X],
    F,
    clearnet1(X).

clearnet1(X) :-
    entry:lexentry@E <-> X,
    retractall(entry(_, E)),
    retractall(transition(_, _, _, language@X)),
    retractall(diversion(_, _, _, _, language@X)).

lexnet(LSPEC) :-
    LCONSTRUCT =.. [LSPEC, language@X],
    LCONSTRUCT,
    iformat('Converting dictionary ...~n', []),
    (ROOT $$ ENTRY0),
    atom_codes(N, ROOT), 
    iformat('~n~w ', [N]),
    ((nonvar(ENTRY0), ENTRY0 = (ENTRY1 delayed DELAYED0)) ->
	DELAYED0 = DELAYED;
	ENTRY1 = ENTRY0),
    ((nonvar(ENTRY1), ENTRY1 = (ENTRY2 immediate IMMEDIATE)) ->
	true;
	ENTRY2 = ENTRY1),
    ((nonvar(ENTRY2), ENTRY2 = (X lextype LEX)) ->
	true;
	(X = ENTRY2 -> 
	    true; 
	    (differences(X, ENTRY2), fail))),
    (nonvar(LEX) -> lextype@X = LEX; true),
    iformat(' OK', []),
    default(DELAYED = true),
    default(IMMEDIATE = true),
    immediate:lexentry@ENTRY <-> IMMEDIATE,
    /*
    plantGoals(IMMEDIATE, X, IMMEDIATE1),
    IMMEDIATE1,
    lexdefaults(X, ROOT),
    */
    s2a(ROOT, LIST),
    entry:lexentry@ENTRY <-> X,
    root:lexentry@ENTRY <-> ROOT,
    default(text@X=N),
    delayed:lexentry@ENTRY <-> DELAYED,
    lexnet(0, LIST, ENTRY),	    
    fail.
lexnet(LANGUAGE) :-
    iformat('~nDictionary of ~w OK~n', [LANGUAGE]).
  
newnet(LANGUAGE) :-
    clearnet,
    lexnet(LANGUAGE).

shownet :-
    shownet(0, 0).

shownet(I, N1) :-
    transition(N1, FIRSTX, FIRST, _),
    !,
    transition(N1, X, N2, _),
    \+ N1 = N2,
    (((X = FIRSTX, N2 = FIRST); I < 1) -> true; tab(I-1)),
    (character(X) -> W=char:character@X; X = W),
    (N1 = 0 -> true; write(-)), 
    write(W),
    shownet(I+2, N2),
    fail.
shownet(_I, _N) :-
    nl.

/***
This converts a trie, or part of a trie, to the iformat required by
LATEX/link2trees.pl

Once you've got it into this shape, makeTrie(N) produces the LaTeX
source for a network rooted at N, so you have recmember where you
started it from

trie2links does the whole thing, trie2links(LINK) starts from a specified
transition
***/

trie2links :-
    abolish(link/3),
    abolish(link/4),
    transition(0, C, N, X),
    trie2links(transition(0, C, N, X)),
    fail.
trie2links :-
    listing(link/3).

trie2links(LINK) :-
    makeLinks(LINK).

makeLinks(transition(N0, C0, N1, _)) :-
    U <-> underlying:character@C0,
    (ground(U) ->
	atom_codes(A, U);
	A = '?'),
    assert(link(N0, N1, '.', A)),
    transition(N1, C2, N2, _),
    makeLinks(transition(N1, C2, N2, _)),
    fail.

notInserted([]).
notInserted([H | T]) :-
    -inserted:character@H,
    notInserted(T).

lookup_word(N, X, language@X) :-
    number(N),
    !,
    text@X <-> N,
    make_number(N, X).
lookup_word(W, X, language@X) :-
    surface@X <-> W,
    atom_codes(W, [I0 | S]),
    initial_cap@X <-> ICAP,
    ((\+ LANGUAGE == arabic, uc([I0]), I1 is I0+32, ICAP = +);
     (I1 = I0, ICAP = -)),
    s2a([I1 | S], L),
    notInserted(L),
    % You can assert sublexical to see inside words when experimenting
    affixes@X <-> AFFIXES,
    affix@X <-> AFFIX,
    (flag(sublexical) -> 
	true;
    	(AFFIXES <-> [], AFFIX = -)),
    language@X <-> language@K,
    language@X <-> LANGUAGE,
    trail:fst@FST0 <-> [],
    prefix:fst@FST0 <-> [],
    postfix:fst@FST0 <-> L,
    items:fst@FST0 <-> [],
    depth:fst@FST0 <-> 0,
    postfix:fst@FST1 <-> [],
    items:fst@FST1 <-> [X],
    sr(0, K, FST0, FST1, SC),
    underlying@X <-> prefix:fst@FST1,
    (flag(showSpelling) ->
	pretty(foundOne(text@X));
	true),
    ((LANGUAGE <> german, X <> noun) -> ICAP = +; ICAP = -).

extractUnderlying([], []).
extractUnderlying([H0 | T0], [H1 | T1]) :-
	H1 <-> underlying:character@H0,
	extractUnderlying(T0, T1).

combineRootAndAffix(X, Y, Z, K1, AFFIXES, AFFIXES1, FST0, FSTN) :-
    [prefix:fst, postfix:fst]@FST0
    <-> [prefix:fst, postfix:fst]@FST1,
    items:fst@FST1 <-> [Z | items:fst@FST0],
    %% trail:fst@FST1 <-> [],
    trail:fst@FST1 <-> trail:fst@FST0,
    affixes@X <-> [Y | AFFIXES],
    affixes@Z <-> ZAFFIXES,
    %% diacritics@X <-> diacritics@Z,
    default(AFFIXES = []),
    default(AFFIXES1 = []),
    append(AFFIXES1, AFFIXES, ZAFFIXES),
    [diacritics, translation, sense]@X <-> [diacritics, translation, sense]@Z,
    [translation, sense]@X <-> [translation, sense]@Y,
    failures@X <-> failures@Z,
    %% trail:fst@FST1 <-> [],
    trail:fst@FST1 <-> trail:fst@FST0,
    items:fst@FST1 <-> items:fst@fstIn@K1,
    tag@X <-> TAG,
    tag@Y <-> TAG,
    tag@Z <-> TAG,
    current:fst@FST1 <-> CURRENT,
    extend(failures@Z, failures@Y),
    score@X <-> SCOREX,
    score@Y <-> SCOREY,
    score@Z <-> SCOREZ,
    (nonvar(SCOREX) ->
        (nonvar(SCOREY) ->
             (get_mutable(IX, SCOREX),
              get_mutable(IY, SCOREY),
              IZ is IX+IY,
              create_mutable(IZ, SCOREZ));
             (SCOREZ = SCOREX, SCOREZ = SCOREY));
         (SCOREZ = SCOREY, SCOREX = SCOREZ)),
    (ZAFFIXES = [CURRENT | _] -> true; true),
    (flag(showSpelling) ->
	(depth:fst@FST1 is depth:fst@FST0+1,
	    tab(depth:fst@FST1),
	    iformat('Combined ~w, ~w to make ~w: need ~w~n',
		   [text@X, text@Y, text@Z, affix@CURRENT]));
	true),
    next_link(0, K1, FST1, FSTN, _).

sr(0, _K, FST, FST, SC) :-
    var(SC),
    postfix:fst@FST <-> [],
    items:fst@FST <-> [_X],
    !.
sr(_N, _K, FST0, _FST1, _SC) :-
    items:fst@FST0 = [X],
    affixes@X = [Y],
    before:dir@dir@Y == +,
    !,
    fail.
sr(_N, _K, FST0, _FST1, _SC) :-
    items:fst@FST0 = [X, Y | _],
    affix@Y <-> *(_),
    before:dir@dir@Y == +,
    immediate@Y == +,
    affixes@X <-> XAFFIXES,
    \+ XAFFIXES = [Y | _],
    !, 
    fail.
sr(_N, _K, FST0, _FST1, _SC) :-
    items:fst@FST0 = [X, Y | _],
    -affix@X,
    -affix@Y,
    affixes@X <-> [],
    affixes@Y <-> [],
    !, 
    fail.
sr(0, K0, FST0, FSTN, SC) :-
    var(SC),
    language@X <-> language@K0,
    items:fst@FST0 <-> [Y, X | items:fst@FST1],
    trail:fst@FST1 <-> trail:fst@FST0,
    [prefix:fst, postfix:fst, depth:fst, trail:fst]@FST0
    <-> [prefix:fst, postfix:fst, depth:fst, trail:fst]@FST1,
    +after:dir@dir@Y, % Y is the affix, X is the root
    [index, language, realised, mother, displaced]@X <-> [index, language, realised, mother, displaced]@Y,
    [syntax, meaning, lextype, index, language, affix, history, positions, mother, realised, displaced]@X
    	<-> [syntax, meaning, lextype, index, language, affix, history, positions, mother, realised, displaced]@Z,
    affixes@Y <-> AFFIXES1,
    text@Z <-> {text@X, text@Y},
    language@K0 <-> language@K1,
    K1 <> initial(-),
    affixes@X <-> XAFFIXES,
    % Join their affixes, and fix them to be nil by default
    -affix@X,
    affix@Y <-> YAFFIX,
    %% Insist that inflectional suffixes are consumed immediately:
    %% derivational ones may have to wait
    depth:fst@FST0 <-> D0,
    (YAFFIX <-> *(_) ->
	(!,
	    XAFFIXES = [Y | AFFIXES],
	    (flag(showSpelling) ->
		(tab(D0),
		    iformat('Calling combineRootAndAffix after cut~n', []));
		true));
	(YAFFIX = -) ->
	(XAFFIXES = [Y | AFFIXES],
	    !,
	    (flag(showSpelling) ->
		((tab(D0),
		  iformat('Calling combineRootAndAffix NO cut~n', []));
		    (tab(D0),
			iformat('Backtracking~n', []),
			fail));
		true));
	fail),
    combineRootAndAffix(X, Y, Z, K1, AFFIXES, AFFIXES1, FST1, FSTN).
sr(0, K0, FST0, FSTN, SC) :-
    var(SC),
    language@X <-> language@K0,
    items:fst@FST0 <-> [Y, X | items:fst@FST1],
    trail:fst@FST1 <-> trail:fst@FST0,
    [prefix:fst, postfix:fst, depth:fst, trail:fst]@FST0
    <-> [prefix:fst, postfix:fst, depth:fst, trail:fst]@FST1,
    +before:dir@dir@X, % X is the affix, Y is the root
    -affix@Y,
    affixes@Y <->YAFFIXES,
    affixes@X <-> AFFIXES1,
    [index, language, realised, mother, displaced]@X <-> [index, language, realised, mother, displaced]@Y,
    [syntax, meaning, lextype, index, language, affix, history, positions, mother, realised, displaced]@Y 
    	<-> [syntax, meaning, lextype, index, language, affix, history, positions, mother, realised, displaced]@Z,
    text@Z <-> {text@X, text@Y},
    language@K0 <-> language@K1,
    K1 <> initial(-),
    (flag(showSpelling) ->
	(tab(depth:fst@FST0),
	    iformat("I think ~w is a prefix for ~w~n", [text@X, text@Y]));
	true),
    YAFFIXES <-> [X | AFFIXES],
    immediate@X <-> XIMMEDIATE,
    (XIMMEDIATE == + ->
	(!,
	    (flag(showSpelling) ->
		(tab(depth:fst@FST0),
		    iformat("and I think it's deterministic~n", []));
		true));
	true),
    combineRootAndAffix(Y, X, Z, K1, AFFIXES, AFFIXES1, FST1, FSTN).
%% This has to come AFTER the cuts in the previous rules, and BEFORE
%% the rule for transitions
sr(N0, K, FST0, FSTN, SC) :-
    \+ \+ SC = transition,
    L0 <-> postfix:fst@FST0,
    L1 <-> postfix:fst@FST1,
    L1 <-> [T | _],
    initial:character@T <-> TINITIAL,
    (N0 = 0 ->
	+ = TINITIAL;
	- = TINITIAL),
    [items:fst, prefix:fst, trail:fst, current:fst]@FST0
    <-> [items:fst, prefix:fst, trail:fst, current:fst]@FST1,
    \+ cmember(+, L0),
    \+ \+ (transition(N0, _T, _N1, language@K); entry(N0, _)),
    TRAIL <-> trail:fst@FST0,
    (TRAIL = [diversion(>>, _) | TRAIL1] -> true; TRAIL1 = TRAIL),
    K <-> current:fst@FST0,
    catch((fst(L0, TRAIL1, FST0, L1),
	   (flag(showSpelling) ->
	       (depth:fst@FST1 is depth:fst@FST0+1,
		   showSpellingRule(L0, L1, FST0));
	       true),
	   next_link(N0, K, FST1, FSTN, spellingChange)),
	  BLOCK, true),
    (BLOCK == blocked ->
	(!, fail);
	true).
sr(N0, K, FST0, FSTN, _SC) :-
    ICT1 <-> initial:character@T1,
    [position:character, syll:character, shadda:character, sound:character, sukun:character, underlying:character, initial:character, final:character, inserted:character]@T
    <-> [position:character, syll:character, shadda:character, sound:character, sukun:character, underlying:character, initial:character, final:character, inserted:character]@T1,
    K <-> current:fst@FST0,
    prefix:fst@FST1 <-> [T1 | prefix:fst@FST0],
    trail:fst@FST1 <-> [T | trail:fst@FST0],
    postfix:fst@FST0 <-> [T | postfix:fst@FST1],
    [items:fst, current:fst]@FST0 <-> [items:fst, current:fst]@FST1,
    transition(N0, T, N1, language@K),
    (N0 == 0 -> ICT1 = +; ICT1 = -),
    default(-inserted:character@T),
    (-query:character@T ->
	extendWithX(T1, actualConsonants@K);
	extendWithX(T1, actualVowels@K)),
    (flag(showSpelling) ->
	(depth:fst@FST1 is depth:fst@FST0+1,
	    tab(depth:fst@FST0),
	    iformat('transition(~w, ~w, ~w)~n', [N0, char:character@T, N1]),
	    tab(depth:fst@FST0),
	    iformat('~w-~w, ~w: current ~w~n', [N0, N1, T, affix@K]));
	true),
    next_link(N1, K, FST1, FSTN, transition).
sr(N0, K0, FST0, FSTN, _SC) :-
    diversion(N0, N1, D, I, language@K0),
    (flag(showSpelling) ->
	(tab(depth:fst@FST0),
	    (D = '<<' ->
		iformat('Starting diversion: ~w~n', [N0]);
		iformat('Finished diversion: ~w~n', [N0])));
	true),
    [prefix:fst, items:fst, postfix:fst, depth:fst]@FST0
    <-> [prefix:fst, items:fst, postfix:fst, depth:fst]@FST1,
    trail:fst@FST1 <-> [diversion(D, I) | trail:fst@FST0],
    (D = '>>' ->
	setBranch(trail:fst@FST0, [], branch@K0);
	true),
    next_link(N1, K0, FST1, FSTN, _).

setBranch([diversion(<<, _) | _TRAIL], B, B) :-
    !.
setBranch([H0 | T0], B0, B1) :-
    atom_codes(char:character@H0, [H1]),
    setBranch(T0, [H1 | B0], B1).

flattenSRule([], []).
flattenSRule([H0 | T0], [H1 | T1]) :-
    C <-> char:character@H0,
    U <-> underlying:character@H0,
    ((nonvar(U), atom_codes(X, U), \+X=C) ->
	H1 = C:X;
	H1 = C),
    !,
    flattenSRule(T0, T1).
flattenSRule([H0 | T0], [H0 | T1]) :-
    flattenSRule(T0, T1).
    
showSpellingRule(L0, L1, FST) :-
    TRAIL <-> trail:fst@FST,
    INDENT <-> depth:fst@FST,
    reverse(TRAIL, REV),
    append(REV, [^ | L0], L),
    append(REV, [^ | L1], R),
    tab(INDENT),
    flattenSRule(L, LN),
    flattenSRule(R, RN),
    flattenSRule(prefix:fst@FST, PFN),
    iformat('~w', [(LN ==> RN)]),
    iformat(':~w, Current ~w~n', [PFN, affix@current:fst@FST]),
    showItems(items:fst@FST, INDENT).

showItems([], _I).
showItems([H | T], I) :-
    tab(I),
    iformat('Item: ~w, affix: ~w, dir: ~w, immediate: ~w, score: ~w ', [text@H, affix@H, dir@H, immediate@H, score@H]),
    affixes@H <-> AFFIXES,
    (AFFIXES = [A | _] ->
	iformat('(looking for ~w, ~w)', [affix@A, dir@A]);
	true),
    nl,
    showItems(T, I).

deletebrackets(L0, L1) :-
    deletebrackets(L0, L1, 0, 0).
    
deletebrackets([], [], N, N).
deletebrackets([diversion(<<, D) | T0], T1, N0, N2) :-
    !,
    N1 is N0+D,
    deletebrackets(T0, T1, N1, N2).
deletebrackets([H | T0], [H | T1], N0, N1) :-
    deletebrackets(T0, T1, N0, N1).

/***
Make sure that you've got something (TEMP) which requires a suffix (X1)
which may be zero 
***/

check_zero(X1, [TEMP | _REST]) :-
    affixes@TEMP <-> [X1 | _],
    +empty@X1,
    +immediate@X1, 
    -affix@TEMP,
    -empty@TEMP,
    +after:dir@dir@X1.

% If you're trying to attach a zero affix, make sure it's acceptable
find_entry(0, ENTRY, _FST0, _FST1) :-
    S0 <-> items:fst@fstIn@X1,
    entry:lexentry@ENTRY <-> X1,
    !,
    check_zero(X1, S0),
    plantGoals(immediate:lexentry@ENTRY, X1, IMMEDIATE),
    IMMEDIATE,
    lexdefaults(X1, root:lexentry@ENTRY),
    entry(0, ENTRY),
    ((flag(showEntries); flag(showSpelling)) ->
	iformat('Found entry ~w~n', [0]);
	true).
find_entry(N, ENTRY, FST0, _FST1) :-
    S0 <-> items:fst@fstIn@X1,
    prefix:fst@FST0 <-> [T | _],
    +final:character@T,
    entry:lexentry@ENTRY <-> X1,
    entry(N, ENTRY),
    ((flag(showEntries); flag(showSpelling)) ->
	iformat('Found entry ~w~n', [N]);
	true),
    plantGoals(immediate:lexentry@ENTRY, X1, IMMEDIATE),
    IMMEDIATE,
    lexdefaults(X1, root:lexentry@ENTRY),
    ((immediate@X1 == +, dir@X1 <> xafter) ->
	(S0 = [TEMP | _REST], affixes@TEMP = [X1 | _]);
	true).

next_link(N1, X1, FST0, FSTN, SC) :-
    PREFIX <-> prefix:fst@FST0,
    final:character@T <-> FINAL,
    ((PREFIX = [T | _], N1 > 0) -> FINAL = '-'; true),
    sr(N1, X1, FST0, FSTN, SC).
next_link(N1, X1, FST0, FSTN, SPELLINGCHANGE) :-
    \+ \+ SPELLINGCHANGE = transition,
    L0 <-> postfix:fst@FST0,
    L0 <-> postfix:fst@fstIn@X1,
    X1 <-> entry:lexentry@ENTRY,
    fstIn@X1 <-> FST0,
    ITEMS0 <-> items:fst@FST0,
    -affix@A0,
    affixes@A0 <-> [A1 | _],
    affix@A1 <-> *(_),
    dir@A1 <> xbefore,
    \+ ITEMS0 = [A0, A1 | _],
    (flag(showSpelling) ->
	(tab(depth:fst@FST0),
	    iformat('Looking for entry at: ~w~n', [N1]),
	    showItems(ITEMS0, depth:fst@FST0+2));
	true),
    \+ L0 = [? | _],
    ITEMS1 <-> [PRE | _],
    affix@PRE <-> *(_),
    immediate@PRE <-> IP,
    affixes@X1 <-> X1AFFIXES,
    ((ITEMS0 = ITEMS1, IP == +) ->
	X1AFFIXES = [PRE | _];
	true),
    find_entry(N1, ENTRY, FST0, FST1),
    /*
    (retract(count(entries, COUNT0)) -> COUNT1 is COUNT0+1; COUNT1 = 1),
    assert(count(entries, COUNT1)),    iformat('entry ~w~n', [text@X1]),
    */
    (flag(showSpelling) ->
	(depth:fst@FST1 is depth:fst@FST0+1,
	    tab(depth:fst@FST0),
	    flattenSRule(L0, LX),
	    iformat('Entry found: ~w (text ~w, remainder ~w, empty ~w, immediate ~w, affix ~w, trail ~w)~n',
		    [N1, text@X1, LX, empty@X1, immediate@X1, affix@X1, postfix:fst@FST0]),
	    showItems(items:fst@FST0, depth:fst@FST0+2));
	true),
    K <> initial(-),
    items:fst@fstIn@K <-> [X1 | _],
    trail:fst@FST1 <-> trail:fst@FST0,
    PREFIX0 <-> prefix:fst@FST0,
    PREFIXN <-> prefix:fst@FST1,
    underlying@X1 <-> UNDERLYING0,
    (nonvar(UNDERLYING0) ->
	(UNDERLYING1 = UNDERLYING0, %% characters(UNDERLYING0, UNDERLYING1),
	 (N1 = 0 ->
	  PREFIX1 = PREFIX0;
	  (removeLastItem(PREFIX0, PREFIX1, LASTITEM),
	   %% This is to force any consonants that had features
	   %% specified in a rule to pass them on to the consonants
	   %% in the hand-coded underlying form. There is an assumption
	   %% here that hand-coded underlying forms do indeed have the
	   %% same consonants as the surface form: AR, 01/08/11
	   (matchConsonants(LASTITEM, UNDERLYING1, true) ->
	       true;
	       pretty(matchConsonants(LASTITEM, UNDERLYING1))),
	   markLast(UNDERLYING1))),
	 append(UNDERLYING1, PREFIX1, PREFIXN));
	PREFIXN = PREFIX0),
    [postfix:fst]@FST1 <-> [postfix:fst]@FST0,
    items:fst@FST1 <-> [X1 | items:fst@FST0],
    items:fst@FST1 <-> items:fst@fstIn@K,
    [syntax, affix, affixes]@K <-> [syntax, affix, affixes]@current:fst@fstOut@X1,
    language@X1 <-> language@K,
    K <-> current:fst@FST1,
    syntax@K <-> syntax@current:fst@fstOut@X1,
    next_link(0, K, FST1, FSTN, _),
    followUp(N1, L0-_L1, X1, trail:fst@FST0, ENTRY).

matchConsonants([], _, _) :-
    !.
matchConsonants(_, [], _) :-
    !.
matchConsonants([C0 | L0], [C1 | L1], FIRST) :-
    underlying:character@C1 == underlying:character@C0,
    initial:character@C1 <-> I,
    final:character@C1 <-> F,
    !,
    (FIRST -> F = +; F = -),
    C0 = C1\(position:character),
    matchConsonants(L0, L1, fail),
    (L1 = [] -> I = +; I = -).
matchConsonants(L0, [V | L1], FIRST) :-
    +vowel:character@V,
    -long:character@V,
    initial:character@V <-> I,
    final:character@V <-> F,
    +inserted:character@V,
    (FIRST -> F = +; F = -),
    matchConsonants(L0, L1, fail),
    (L1 = [] -> I = +; I = -).
    
markLast([]).
markLast([X | _]) :-
    +final:character@X.

removeLastItem([H | T0], T1, [H | L]) :-
    (initial:character@H == + ->
	T1 = T0;
	removeLastItem(T0, T1, L)).
removeLastItem([], [], []).

noDiversions([], []).
noDiversions([diversion(_, _) | L0], L1) :-
    !,
    noDiversions(L0, L1).
noDiversions([H | L0], [H | L1]) :-
    !,
    noDiversions(L0, L1).

followUp(_N1, _L0-_L1, X1, _TRAIL0, ENTRY) :-
    !,
    plantGoals(delayed:lexentry@ENTRY, X1, DELAYED),
    DELAYED.

findTransition(I, char:character@C, J) :-
    transition(I, C, J, _E).

findPath(I, [C1 | PATH]) :-
    transition(I, C0, J, _),
    (character(C0) ->
	C1 <-> char:character@C0;
	C1 = C0),
    findPath(J, PATH).
findPath(I, []) :-
    entry(I, _).

showAllPaths :-
    findallWithWhen(P, findPath(0, P), PATHS0),
    %% use qsort because sort deletes duplicates (which rather
    %% spoils the point of the exercise)
    qsort(PATHS0, PATHS1),
    showAllPaths(PATHS1, ********).

showAllPaths([H | T], LAST) :-
    ((T = [H | _], \+ H = LAST) -> iformat('*******************~n', []); true),
    pretty(H),
    showAllPaths(T, H).
