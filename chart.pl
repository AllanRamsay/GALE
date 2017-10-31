
makeQ(L0, I, TAGS, L1) :-
    makeQ(L0, I, TAGS, L1, _LANGUAGE).

makeQ([], _I, _TAGS, [], _LANGUAGE).
makeQ([H0 | T0], I, [TAG | TAGS], [H1 | T1], LANGUAGE) :-
    J is I+1,
    start@WORD -- I,
    end@WORD -- J,
    xstart@WORD -- I,
    xend@WORD -- J,
    span@WORD is 1 << I,
    dtrs@WORD -- [],
    (T0 = [] ->
     +terminal@WORD;
     -terminal@WORD),
    gensym(i, index@WORD),
    trigger(tag@WORD, (tag@WORD = TAG -> true; incCost(WORD, 10))),
    findall(WORD, lookup(H0, WORD, LANGUAGE), WORDS),
    (WORDS = [] ->
     (unknown(H0, WORD, LANGUAGE) ->
      H1 = WORD;
      throw('No such word'(H0)));
      disjoin(WORDS, H1)),
    makeQ(T0, J, TAGS, T1, LANGUAGE).

fixWhiteSpace([], []) :-
    !.
fixWhiteSpace(A0, A1) :-
    atomic(A0),
    !,
    atom_chars(A0, L0),
    fixWhiteSpace(L0, L1),
    atom_chars(A1, L1).
fixWhiteSpace(L0, LN) :-
    append("  ", L1, L0),
    !,
    fixWhiteSpace([32 | L1], LN).
fixWhiteSpace([X, Y], [X, 32, Y]) :-
    \+ [X] = " ",
    member(Y, ".?"),
    !.
fixWhiteSpace([H | T1], [H | T2]) :-
    fixWhiteSpace(T1, T2).

initialiseAgenda(TEXT0, TEXTN, LANGUAGE) :-
    fixWhiteSpace(TEXT0, TEXT1),
    makeList(TEXT1, TEXT2),
    (?useTagger -> tag(TEXT2, TAGS); true),
    makeQ(TEXT2, 0, TAGS, TEXTN, LANGUAGE).

chartParse(TEXT0, X, LANGUAGE, MAX) :-
    gensym(reset),
    D <> sign,
    retractall(D),
    retractall(packed(_, _)),
    initialiseAgenda(TEXT0, TEXT2, LANGUAGE),
    length(TEXT2, L),
    addZero,
    processAgenda(TEXT2, X, L, MAX).

chartParse(TEXT, X, LANGUAGE) :-
    chartParse(TEXT, X, LANGUAGE, i5000).

chartParse(TEXT, LANGUAGE) :-
    call_residue((chartParse(TEXT, _, LANGUAGE) -> true; fail), _).

parseOne(TEXT, dtree@X) :-
    call_residue((chartParse(TEXT, X, english),
		  nf(X, NF),
		  format("~n\\begin{figure}[ht]~n\\hspace*{\\fill}\\begin{minipage}[t]{0.45\\linewidth}~n\\begin{verbatim}", []),
		  format("\\hspace*{\\fill}~n\\end{verbatim}~n\\end{minipage}~n", []),
		  format("~n\\caption{\\q{~w}}\\label{NF: ~w}~n\\end{figure}~n", [TEXT, TEXT])),
		 _),
    !.

latexParse(TEXT) :-
    call_residue((chartParse(TEXT, X, english),
		  denest(dtree@X, T0),
		  %% pretty(denest(T0)),
		  nfTree(T0, TN),
		  pretty(nfTree(TN)),
		  (?latex ->
		   (format("~n\\begin{figure}[ht!]~n\\hbox{\\hspace*{\\fill}~n\\begin{minipage}[t]{0.45\\linewidth}", []),
		    npsTree(TN),
		    format("\\end{minipage}~n\\hspace*{\\fill}}~n\\caption{\\q{~w}}\\label{~w}~n\\end{figure}~n", [TEXT, TEXT]));
		   true)),
		 _),
    !.

nfParse(TEXT, TN) :-
    call_residue((chartParse(TEXT, X, english),
		  nf(X, TN),
		  (?latex ->
		   format("~n\\begin{figure}[ht!]~n\\hbox{\\hspace*{\\fill}~n\\begin{minipage}[t]{0.45\\linewidth}~n\\begin{verbatim}", []); true),
		  pretty(TN),
		  (?latex ->
		   format("~n\\end{verbatim}~n\\end{minipage}~n\\hspace*{\\fill}}~n\\caption{\\q{~w}}\\label{NF:~w}~n\\end{figure}~n", [TEXT, TEXT]); true)),
		 _),
    !.
nfParse(TEXT) :-
    nfParse(TEXT, _TN).

parseOne(TEXT) :-
    parseOne(TEXT, _).

parseAll(TEXT) :-
    call_residue((chartParse(TEXT, X, english),
		  denest(dtree@X, DN),
		  nfTree(DN, NFTREE),
		  pretty(nfTree(NFTREE)),
		  nf(X, NF),
		  pretty(nf(NF)),
		  fail),
		 _).

testFracas(START, END, THROWFAIL) :-
    retractall(fracasCounter(_)),
    fracas(X),
    (retract(fracasCounter(I)) -> J is I+1; J = 1),
    assert(fracasCounter(J)),
    trigger(START, J >= START),
    trigger(END, \+ J > END),
    format("~n~ntestFracas(~w). %%  '~w').~n parseOne('~w').~n", [J, X, X]),
    (catch(parseOne(X), CAUGHT, \+ format('caught ~w~n', [CAUGHT])) ->
     true;
     THROWFAIL ->
     throw('no analysis');
     format('** no analysis *****************************************~n', [])),
    fail.

testFracas(I, J) :-
    testFracas(I, J, fail).

testFracas(I) :-
    testFracas(I, I).

testFracas :-
    testFracas(_, _, fail).

addZero :-
    start@X -- end@X,
    xstart@X -- xend@X,
    start@X -- xstart@X,
    [start, end, modified, zero] :: [X, hd@X],
    span@X -- 0,
    index@X -- z0,
    root@X -- [('zero', start@X)],
    dtree@X -- zero,
    X <> [x, -target, notMoved, +zero, modified(0)],
    trigger(n:xbar@cat@X, (n(X) -> saturated(X); v(X))),
    setCost(X, 0),
    assert(X).

/**
  if I == 0:
      START is I
  elif 1 >> I & SPAN == 0:
      START is I+1
  else:
      findStart(I-1, SPAN)
  **/

findStart(0, _SPAN, 0) :-
    !.
findStart(I0, SPAN, J) :-
    I1 is I0-1,
    (0 is (1 << I1) /\ SPAN ->
     J = I0;
     findStart(I1, SPAN, J)).

/**
  Suppose our phrase is made of words 0-1, 1-2, 2-3.

  Then its span will be 1 \/ 2 \/ 4, i.e. 7.

  If we take 2 as the end of the hd, then we want the
  end of this one to be 3, i.e. the last number such that
  1 << N is 1, or the first such that 1 << (N+1) is 0.
  **/

findEnd(I0, SPAN, J) :-
    I1 is I0+1,
    (0 is (1 << I1) /\ SPAN ->
     J = I1;
     findEnd(I1, SPAN, J)).

setPosition(X, Y, Z) :-
    span@Z is span@X \/ span@Y,
    findStart(start@X, span@Z, start@Z),
    findEnd(start@X, span@Z, end@Z),
    (nonvar(xend@Y) ->
     max(xend@X, xend@Y, xend@Z);
     xend@X -- xend@Z),
    (nonvar(xstart@Y) ->
     min(xstart@X, xstart@Y, xstart@Z);
     xstart@X -- xstart@Z),
    ((+terminal@X; +terminal@Y) -> +terminal@Z; -terminal@Z).

view(WORD, WORD).
view(WORD, VIEW) :-
    [position\moved, language, hd, cost, dtree] :: [WORD, VIEW],
    disjunct(externalviews@WORD, VIEW).

find(X) :-
    [theta, structure\dtree] :: [X, Y],
    call(Y),
    view(Y, X),
    (dtree@X == dtree@Y -> true; dtree@X = (altview@X=dtree@Y)).

checkWHPosition(H, D) :-
    H <> [v],
    -zero@D,
    -zero@WH,
    checkNoWH(H),
    checkWHMarked(D, WH),
    !,
    catch(start@WH < xstart@H, E, throw(checkWHPosition1(E, index@H, index@D))).

checkWHPosition(H, D) :-
    H <> [v],
    -zero@D,
    -zero@WH,
    checkWHMarked(H, WH),
    checkNoWH(D),
    !,
    catch(xstart@D > start@WH, E, (pretty(D), throw(checkWHPosition2(E, index@H, index@D)))).

checkWHPosition(_, _).

/**
  linked(HD, DTR)
  **/

checkLinked(IDTR, HD, DTR) :-
    (IDTR == z0; linked(start@hd@HD, start@hd@DTR)).

plantCheckLinked(HD, DTR) :-
    index@DTR -- IDTR,
    start@hd@HD -- SHDHD,
    start@hd@DTR -- SHDDTR,
    !,
    trigger((IDTR, SHDHD, SHDDTR), checkLinked(IDTR, HD, DTR)).

breakOn(X, I) :-
    (index@X == I -> trace; true).

modPosition(M, T, R) :-
    [ST, ET, SM, EM, SR, ER, DT]
    -- [start@T, end@T, start@M, end@M, start@R, end@R, dir@T],
    (after(DT) ->
      (EM = ST ->
       notMoved(M);
       (SM < ET ->
	movedBefore(M);
	movedAfter(M)));
     (before(DT),
      (SM = ET ->
       notMoved(M);
       (SM < ET ->
	movedAfter(M);
	movedBefore(M))))),
    (set:position@moved@M = +).

checkFixedMod(M, T) :-
    T <> [notMoved],
    !,
    ((after(dir@M), start@M -- end@T);
     (before(dir@M), end@M -- start@T)).
checkFixedMod(_M, _T).

/**
  If X could have multiple views, then instead of doing assert(X)
  you could do assert((T :- T=X; vmember(T, externalviews@X))).

  Is that any different from
  
  
  **/

addExternalView(X, Y) :-
    (addDisjunct(externalviews@X, Y) -> true; true).

extendWH(X, Y) :-
    extend(wh@X, wh@Y),
    \+ (nonvar(wh@X),
	wh@Y = [WH1, WH2 | _],
	nonvar(zero@WH2),
	zero@WH1 == +,
	zero@WH2 == +).
    
combineModAndTarget(M, T, R) :-
    [complete, shifted, wh] :: [T, R],
    extend(shifted@R, shifted@M),
    target@M -- T,
    T <> [+modifiable],
    result@M -- R,
    \+ ((length(wh@T, L) -> true; true), L = 2),
    [surface, language, externalviews, hd] :: [T, R],
    dtrs@R -- [index@T, mod(index@M)],
    (?forced ->
     plantCheckLinked(T, M);
     true),
    trigger((span@M, span@T), 0 is span@T /\ span@M),
    modified@T -- MODT,
    modified@R -- MODR,
    (var(span@M) -> find(M); find(T)),
    \+ (nonvar(MODT), nonvar(MODR), (MODT > MODR; (MODT == MODR, \+ compact(R)))),
    %% (zero@T == + -> (after(dir@T) -> start@T = end@M; end@T = start@M); true),
    dtree@R -- [dtree@T, modifier(modifier@M, dtree@M)],
    setPosition(M, T, R),
    modPosition(M, T, R),
    checkWHPosition(T, M),
    extendWH(T, M),
    mergeCosts(T, M, R),
    incCost(R, 0.5),
    (nonvar(shifted@R) -> var(wh@R); true),
    (used@T = +).

argPosition(H0, A, H1) :-
    shifted :: [H0, H1],
    extend(shifted@H1, shifted@A),
    [EA, SA, SH0, EH0, DA] -- [end@A, start@A, start@H0, end@H0, dir@A],
    (before(DA) ->
      (EA -- SH0 ->
       notMoved(A);
       (-zero@A,
	%% compact(A),
	(EA < SH0 ->
	 movedBefore(A);
	 movedAfter(A))));
     (after(DA),
      (SA -- EH0 ->
       notMoved(A);
       (-zero@A,
	compact(A),
	(SA > EH0 ->
	 movedAfter(A);
	 movedBefore(A)))))),
    A\shifted -- X,
    ((movedBefore(A), var(wh@A)) -> add1(shifted@H1, X); true),
    (set:position@moved@A = + -> true),
    (notMoved(A) -> true; incCost(H0, 0.3)).

combineHdAndArg(H0, A, H1) :-
    [syntax\args, theta, externalviews, complete, moved, hd, modifier, case] :: [H0, H1],
    \+ ((length(wh@H0, L) -> true; true), L = 2),
    %% \+ \+ wh@H0 -- [_],
    args@H0 -- [A | args@H1],
    dtrs@H1 -- [arg(index@A), index@H0],
    [surface, language] :: [H0, H1],
    -zero@H0,
    (?forced ->
     plantCheckLinked(H0, A);
     true),
    trigger((span@A, span@H0), 0 is span@A /\ span@H0),
    (var(span@A) ->
     find(A);
     find(H0)),
    default(zero@A = -),
    checkWHPosition(H0, A),
    extendWH(H0, A),
    setPosition(H0, A, H1),
    argPosition(H0, A, H1),
    mergeCosts(H0, A, H1),
    %% A <> [specified],
    %% +specified@A,
    dtree@H1 -- [dtree@H0, arg(theta@A, specifier@A, dtree@A)],
    (args@H1 = [] -> complete@H1 = +; true),
    %% (nonvar(shifted@H1) -> var(wh@H1); true),
    (used@A = +).

findCombination(X, _Z) :-
    ?breakOn(I),
    breakOn(X, I),
    fail.
findCombination(X, Z) :-
    combineHdAndArg(X, _Y, Z).
findCombination(X, Z) :-
    combineHdAndArg(_Y, X, Z).
findCombination(M, R) :-
    target@M <> sign,
    +modifiable@T,
    combineModAndTarget(M, T, R).
findCombination(T, R) :-
    +modifiable@T,
    combineModAndTarget(_M, T, R).
findCombination(X, Z) :-
    index@Y -- ext(index@X),
    [position\moved, language, hd, cost] :: [X, Y],
    disjunct(externalviews@X, Y),
    dtree@Y -- (altview@Y=dtree@X),
    (used@X = +),
    findCombination(Y, Z).

spanningEdge(X, end@X) :-
    start@X -- 0,
    X <> [saturated, compact],
    var(wh@X).

spanningEdge(Y) :-
    start@Y -- 0,
    Y <> [compact, saturated],
    end@Y -- EY,
    end@Z -- EZ,
    retrieve(_I, Y),
    nonvar(EY),
    var(wh@X),
    \+ (retrieve(_, Z), nonvar(EZ), EZ > EY).

showThisEdge(X) :-
    (?showEdges ->
     pretty(index@X+root@X+dtree@X+dtrs@X);
     true).

checkNewEdge(X, AGENDA0, AGENDA2) :-
    %% This is a hack to have a feature that is known to be the last thing
    %% that will be unified, so that we can set triggers that can look to see
    %% whether things have been bound
    ((?packing, call_residue(D, true)) ->
     (assert(packed(index@D, X)),
      AGENDA2 = AGENDA0);
      assert(X)).

processAgenda(AGENDA, K, END) :-
    processAgenda(AGENDA, K, END, i1000).

processAgenda([X | AGENDA0], K, END, MAX) :-
    [span, syntax] :: [X, D],
    index@X -- IX,
    (var(IX) -> gensym(i, IX); true),
    (IX == MAX -> (!, fail); true),
    checkNewEdge(X, AGENDA0, AGENDA2),
    showThisEdge(X),
    ((spanningEdge(X, END),
      K = X,
      (?firstOneOnly -> !; true));
     ((AGENDA2 == AGENDA0 ->
       true;
       (findall(C, findCombination(X, C), AGENDA1),
	(true ->
	 (qsort(AGENDA1, SORTED1, [], cheaperQ), mergeSorted(SORTED1, AGENDA0, AGENDA2, cheaperM));
	 append(AGENDA1, AGENDA0, AGENDA2)))),
     processAgenda(AGENDA2, K, END, MAX))).

retrieve(index@X, X) :-
    call(X).

rpretty(I) :-
    retrieve(I, X),
    pretty(X).

fpretty(I) :-
    retrieve(I, X),
    cpretty(X).

fpretty(I, F) :-
    retrieve(I, X),
    extractFeature(F, X, K, fail),
    cpretty(K).

whyNot(I, J) :-
    retrieve(I, IX),
    args@IX -- [A | _],
    retrieve(J, JX),
    compare(JX, A).

whyNot(I, J) :-
    retrieve(I, IX),
    args@IX -- [A | _],
    retrieve(J, JX),
    disjunct(externalviews@JX, KX),
    compare(KX, A).

whyNotModify(I, J) :-
    retrieve(I, IX),
    target@IX -- T,
    retrieve(J, JX),
    compare(T, JX).

whyNotModifyAltView(I, J) :-
    retrieve(I, IX),
    position :: [IX, IZ],
    disjunct(externalviews@IX, IZ),
    target@IZ -- T,
    retrieve(J, JX),
    compare(T, JX).

arglength(V, ???) :-
    var(V),
    !.
arglength([], 0).
arglength([_ | T], L) :-
    arglength(T, L0),
    (L0 = ??? ->
     L = ???;
     L is L0+1).

showAllEdges :-
    call(X),
    arglength(args@X, L),
    pretty(index@X+dtree@X+L+dtrs@X),
    fail.

showParsedSegment(STREAM, K, COMPLETE) :-
    unset(quoteAtoms, QUOTED1),
    format(STREAM, "/*", []), 
    pretty(STREAM, COMPLETE),
    pretty(STREAM, index@K+root@K),
    format(STREAM, "~n*/~n", []),
    write_canonical(STREAM, root@K),
    format(STREAM, '.~n', []),
    QUOTED1.
    
parseSegment(STREAM, X, S, N, LANGUAGE, MAX) :-
    !,
    format(STREAM, 'parseSegment(~q, ~q, ~q).~n', [X, LANGUAGE, i500]),
    call_residue(((chartParse(X, _T, LANGUAGE, MAX) -> true; true),
		  (spanningEdge(K) ->
		   showParsedSegment(STREAM, K, complete);
		   ((biggest2(K, L), compact(K)) ->
		    (showParsedSegment(STREAM, K, incomplete),
		     (nonvar(index@L), compact(L)) ->
		     showParsedSegment(STREAM, L, incomplete)));
		   fail)), _).

parseSegment(STREAM, X, LANGUAGE, MAX) :-
    parseSegment(STREAM, X, _, _, LANGUAGE, MAX),
    !.

parseSegment(X, LANGUAGE, MAX) :-
    parseSegment(user_output, X, LANGUAGE, MAX).

parseSegment(X, MAX) :-
    parseSegment(X, arabic, MAX).

parseSegment(X) :-
    parseSegment(X, i100).

fracasSegments(I) :-
    tagged(SRC, TAGGED),
    format("%% SRC: tagger('~w')~n", [SRC]),
    parseSegment(TAGGED, english, I),
    fail.
fracasSegments :-
    fracasSegments(i30).

writeNumbered(_STREAM, [], _N).
writeNumbered(STREAM, [(A, E, P) | T], I) :-
    format(STREAM, '~w:~w:~w:~w, ', [I, E, A, P]),
    J is I+1,
    writeNumbered(STREAM, T, J).

parseSegments(OUT) :-
    getStream(OUT, STREAM),
    retractall(saved(_, _, _, _)),
    set(firstOneOnly, _),
    set(packing, _),
    segment(S, N, X),
    \+ member((_, _, 'VBD'), X),
    \+ member((_, _, 'NNS'), X),
    \+ member(('atin', _, _), X),
    unset(quoteAtoms, _QUOTED),
    nl(STREAM), print(STREAM, **************************),
    set(quoteAtoms, _),
    nl(STREAM), write(STREAM, segment(S, N)),
    %% reverse(X, Y), nl(STREAM), ((member((A, _, _), Y), write(STREAM, A), write(STREAM, ' '), fail); true),
    nl(STREAM), writeNumbered(STREAM, X, 0),
    nl(STREAM), nl(STREAM), print(STREAM, parseSegment(X)), write(STREAM, '.'),
    nl(STREAM),
    parseSegment(STREAM, X, S, N),
    nl(STREAM),
    fail.
parseSegments(_OUT) :-
    closeStream,
    unset(quoteAtoms, _).

parseSegments :-
    parseSegments(user_output).

getCost(X, I) :-
    get_mutable(I, cost@X),
    default(I=0).

setCost(X, I) :-
    create_mutable(I, cost@X).

incCost(X, I) :-
    catch(get_mutable(J, cost@X), _, (setCost(X, 0), J=0)),
    K is I+J,
    update_mutable(K, cost@X).

mergeCosts(X, Y, Z) :-
    getCost(X, CX),
    getCost(Y, CY),
    CZ is CX + CY,
    setCost(Z, CZ).

cheaperM(X, Y) :-
    getCost(X, CX),
    getCost(Y, CY),
    catch(CX =< CY, E, throw(cheaperM(E, index@X, index@Y))).

cheaperQ(X, Y) :-
    getCost(X, CX),
    getCost(Y, CY),
    catch(CX < CY, E, throw(cheaperQ(E, index@X, index@Y))).

parseDefns :-
    definition@Y -- D,
    word(X, Y),
    nonvar(D),
    format("~n~w: ~nchartParse('~w', X, english), pretty(index@X+root@X), fail.~n", [X, D]),
    chartParse(D, T, english, i200),
    pretty(root@T),
    nl,
    fail.

getUnknownWords :-
    definition@Y -- D,
    word(X, Y),
    nonvar(D),
    catch(initialiseAgenda(D, T, english), 'No such word'(ROOT), pretty('No such word'(ROOT))),
    fail.

    