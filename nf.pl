:- op(32, xfy, '\IN').
:- op(32, xfy, '\sub').

revpolarity(P0, P1) :-
    (P0 = + -> P1 = -; P1 = -).

/**
  It may be useful to order the dtrs of tree so that modifiers
  come at the end. It's hard to see it doing any damage, unless
  we start getting into situations in which the surface order is
  important for dealing with pronouns.
  **/
orderDtrs(_, modifier(_, _)).

/**
  The tree that comes out of the parser reflects the order of
  combination. That means that for "he loves her" we get

  baseTree([[love>s, arg(dobj, *(indefinite), [woman>, modifier(A, a)])],
            arg(subject, *(universal), [man>, modifier(B, every)])])

  where we have a two-element list made of ["loves a woman", "a man"]

  That's just an accident of the way the parser works. denest turns
  this into

  justRoots([love,
             arg(dobj, *(indefinite), [woman, modifier(A, a)]),
             arg(subject, *(universal), [man, modifier(B, every)])])

  It includes sorting dtrs so that modifiers come at the end. That
  will help when trying to deal with upward/downward matching and
  possibly with simplification.
  **/

denest([[H | T0] | T1], D) :-
    !,
    append(T0, T1, T),
    denest([H | T], D).
denest([H0 | T0], L) :-
    !,
    denest(H0, H1),
    denest(T0, T1),
    qsort([H1 | T1], L, orderDtrs).
denest(_A=B, C) :-
    !,
    denest(B, C).
denest(arg(A, B, D0), arg(A, B, D1)) :-
    !,
    denest(D0, D1).
denest(modifier(A, B0), modifier(A, B1)) :-
    !,
    denest(B0, B1).
denest(X, X).

/**
  Because I have this theory of modifiers and specifiers that says
  that any modifier *might* be a specifier and any specifier *might*
  be a modifier, I have to do some tidying up to get rid of cases
  where a modifier isn't actually a specifier and vice versa.
  **/

nfTree(X, X) :-
    (var(X); atomic(X)),
    !.
%% Ignore an argument if it is marked as an identity (or is undefined)
nfTree([_X, arg(identity, SPECX, [Y | ARGS])], N) :-
    !,
    nfTree(arg(Y, SPECX, ARGS), N).
%% Likewise if someone has an argument whose specifier is marked as an identity (or undefined)
nfTree([X, arg(THETAX, SPECX, [_I, arg(_SPECY, identity, D)])], N) :-
    !,
    nfTree([X, arg(THETAX, SPECX, D)], N).
nfTree(arg(A, B, D0), arg(A, B, D1)) :-
    !,
    nfTree(D0, D1).
%% Igore modifier nodes where the modifier is identity or undefined
nfTree([modifier(identity, _D0) | T0], T1) :-
    !,
    nfTree(T0, T1).
nfTree([modifier(A, D0) | T0], [modifier(A, D1) |T1]) :-
    !,
    nfTree(D0, D1),
    nfTree(T0, T1).
nfTree([X0 | T0], [X1 | T1]) :-
    !,
    nfTree(X0, X1),
    nfTree(T0, T1).
nfTree(X, X).

nfDtrs([], []).
nfDtrs([H0 | T0], [H1 | T1]) :-
    nfTree(H0, H1),
    nfDtrs(T0, T1).

%% Get rid of affixes. 
justRoots(X, X) :-
    (var(X); atomic(X)),
    !.
justRoots(A > _, A) :-
    !.
justRoots([H0 | T0], [H1 | T1]) :-
    !,
    justRoots(H0, H1),
    justRoots(T0, T1).
justRoots(X0, X1) :-
    X0 =.. L0,
    justRoots(L0, L1),
    X1 =.. L1.

%% qq(Q, T) means: add Q to the QSTACK and subsequently apply it to the
%% core, leave T where it is. We will write {P:X} for P(X) because we
%% can't write P(X).
qlf(X, X) :-
    (var(X); atomic(X)),
    !.
qlf(arg(claim, *(tense(TNS, REF)), [EVENT | ARGS]),
    claim(opaque(qq(tense(REF)::{TNS, V}, at(V, opaque(qq(indefinite :: E, {QLF, E}))))))) :-
    !,
    qlf([EVENT | ARGS], QLF).
qlf(arg(query, *(tense(TNS, REF)), [EVENT | ARGS]),
    query(opaque(qq(tense(REF)::{TNS, V}, at(V, opaque(qq(indefinite :: E, {QLF, E}))))))) :-
    !,
    qlf([EVENT | ARGS], QLF).
qlf(arg(xcomp, *SPEC, X0), {xcomp, opaque(X1)}) :-
    var(SPEC),
    !,
    qlf(X0, X1).
qlf(arg(THETA, *SPEC, X0), {THETA, X1}) :-
    var(SPEC),
    !,
    qlf(X0, X1).
qlf(arg(THETA, *generic, X0), {generic, {THETA, V}, {X1, V}}) :-
    !,
    qlf(X0, X1).
qlf(arg(THETA, *name, [X0:'NP']), QLF) :-
    !,
    qlf(arg(THETA, *(the), name(X0)), QLF).
qlf(arg(THETA, *lambda, _X0), qq(lambda::V, {THETA, V})) :-
    !.
qlf(arg(THETA, *SPEC, X0), qq(SPEC::{X1, V}, {THETA, V})) :-
    !,
    qlf(X0, X1).
qlf(['.', X0], X1) :-
    !,
    qlf(X0, X1).
qlf(['?', X0], X1) :-
    !,
    qlf(X0, X1).
qlf([H0 | T0], [H1 | T1]) :-
    !,
    qlf(H0, H1),
    qlf(T0, T1).
qlf(X0, X1) :-
    X0 =.. L0,
    qlf(L0, L1),
    X1 =.. L1.

extractQQ(X, X, Q, Q) :-
    (atomic(X); var(X)),
    !.
extractQQ(opaque(X0), X1, Q, Q) :-
    !,
    fixQuants(X0, X1).
extractQQ(qq(Q0, T0), T1, QS0, [Q1 | QS2]) :-
    !,
    extractQQ(Q0, Q1, QS0, QS1),
    extractQQ(T0, T1, QS1, QS2).
extractQQ([H0 | T0], [H1 | T1], Q0, Q2) :-
    !,
    extractQQ(H0, H1, Q0, Q1),
    extractQQ(T0, T1, Q1, Q2).
extractQQ(X0, X1, Q0, Q1) :-
    X0 =.. L0,
    extractQQ(L0, L1, Q0, Q1),
    X1 =.. L1.

extractQQ(X0, X1, QSTACK) :-
    extractQQ(X0, X1, [], QSTACK).

occursIn(X, Y) :-
    X == Y,
    !.
occursIn(X, _) :-
    (var(X); atomic(X)),
    !,
    fail.
occursIn([H | T], V) :-
    !,
    (occursIn(H, V); occursIn(T, V)).
occursIn(X, V) :-
    X =.. [_ | A],
    occursIn(A, V).

checkScopes([], []).
checkScopes([H | T0], S) :-
    xdelete(Q, T0, T1),
    Q = _ :: R,
    (var(R) -> V = R; R = {_, V}),
    occursIn(H, V),
    !,
    checkScopes([Q, H | T1], S).
checkScopes([H | T0], [H | T1]) :-
    checkScopes(T0, T1).

fixQuants(X0, X2) :-
    extractQQ(X0, X1, QSTACK0),
    /*
      If a quantifier *contains* another quantifier in its restrictor
      then we will get problems later on. So we have to check for this
      and re-order them as required
      */
    checkScopes(QSTACK0, QSTACK1),
    applyQuants(QSTACK1, X1, X2).

applyQuants([], X, X).
applyQuants([Q0::V | QQ], X0, XN) :-
    var(V),
    !,
    (member(Q0, [indefinite]) ->
     Q1 = exists;
     member(Q0, [universal]) ->
     Q1 = forall;
     Q0 = tense(REF) ->
     ((REF = -) -> Q1 = exists; Q1 = the);
     Q1 = Q0),
    applyQuants(QQ, X0, X1),
    XN =.. [Q1, V, X1].
applyQuants([Q0::{R, V} | QQ], X0, XN) :-
    (member(Q0, [indefinite]) ->
     Q1 = exists;
     member(Q0, [universal]) ->
     Q1 = forall;
     Q0 = tense(REF) ->
     ((REF = -) -> Q1 = exists; Q1 = the);
     Q1 = Q0),
    applyQuants(QQ, X0, X1),
    XN =.. [Q1, V::{R, V}, X1].

/**
  I'm doing two things here. I'm not at all sure that I want to do
  either of them! I'm turning things like {{A, B}, C} into {A, B,
  C}. That's the bit that's like currying. And I'm turning things like
  {A, B, C} in A(B, C). I'm not sure that I want to do the first of
  these, and I'm fairly sure that I *don't* want to do the second, but
  the way I've written it makes disentangling them awkward. A place
  for conversation and investigation.
  **/

curry(A, A) :-
    (atomic(A); var(A)),
    !.
curry({X0, V}, Xn) :-
    !,
    curryAll(X0, V, Xn).
curry(V :: {X0}, V :: {X1}) :-
    !,
    curry({X0}, X1).
curry([H0 | T0], [H1 | T1]) :-
    !,
    curry(H0, H1),
    curry(T0, T1).
curry(X0, X1) :-
    X0 =.. L0,
    curry(L0, L1),
    X1 =.. L1.

curryAll([], _V, []) :-
    !.
curryAll([H0 | T0], V, [H2 | T1]) :-
    !,
    curry(H0, H1),
    H1 =.. [F | A0],
    curry(A0, A1),
    H2 =.. [F, V | A1],
    curryAll(T0, V, T1).
curryAll(H0, V, H1) :-
    H0 =.. [F | A0],
    curry(A0, A1),
    H1 =.. [F, V | A1].

/**
  I don't know what to do with generics. I like the version that says "John is eating peaches" means
  
  claim(at(ref(A, now=A),
         (eat(#0)
           & (dobj(#0,B)=>peach(B)
               & subject(#0, ref(C, name(C, John)))))))

  i.e. that there is some eating going on where all the things being
  eaten are peaches, because it entails the existence of some peaches
  without explicitly stating it. But this one needs to be worked
  through carefully. It does seem to get a pretty reasonable
  interpretation of "all men are fools.":

  claim(at(now, man(A)=>fool(A)))

  Not entirely mad about doing it as a separate step -- if this is
  indeed the way we want to go then it should probably happen in qff.
  **/

generics(X, X) :-
    (atomic(X); var(X)),
    !.
generics(generic(E, (THETA0, R)), THETA1 => R) :-
    !,
    THETA0 =.. [T | A],
    THETA1 =.. [T, E | A].
generics([H0 | T0], [H1 | T1]) :-
    !,
    generics(H0, H1),
    generics(T0, T1).
generics(X0, X1) :-
    X0 =.. L0,
    generics(L0, L1),
    X1 =.. L1.

/**
  This is a place for applying forward inference rules. I think that
  actually these should be done as actual forward inference rules,
  because as it stands they are very dependent on the shape of the
  tree which I'm again not mad about. The only ones we actually have
  at the moment are about "John is a fool", "all men are fools", and
  we've had to go for two rules to capture this, which is yuk, and
  even with these two we will get "No man is a hero to his valet"
  wrong
  (http://www.thisdayinquotes.com/2010/08/no-man-is-hero-to-his-valet-backstory.html)
  **/

simplify(X, X) :-
    (var(X); atomic(X)),
    !.
simplify([STOP, X0], X1) :-
    STOP == '.',
    !,
    simplify(X0, X1).
simplify(be(X) & (predication(X, A, xbar(v(-), n(+))) & subject(X, B)), B=A) :-
    !.
simplify(be(X) & ((predication(X,B,xbar(v(-),n(+)))=>P0) & subject(X, A)), P1) :-
    !,
    P0 =.. [F, B],
    P1 =.. [F, A].
simplify(present(A), now=A) :-
    !.
simplify([H0 | T0], [H1 | T1]) :-
    !,
    simplify(H0, H1),
    simplify(T0, T1).
simplify((A0, B0), (A1, B1)) :-
    !,
    simplify(A0, A1),
    simplify(B0, B1).
simplify(A0:B0, S) :-
    !,
    simplify(A0, A1),
    simplify(B0, B1),
    (A1 = (X:Y) ->
     simplify(X:(Y:B1), S);
     S = A1:B1).
simplify(X0, X1) :-
    X0 =.. L0,
    simplify(L0, L1),
    X1 =.. L1.

/**
  Pretty standard quantifier free form: the only real oddity is that
  we convert (P => Q, -) to (P, -_) =>(Q, -), and that we give queries
  negative polarity.
**/

skolem(X, L) :-
    gensym('#', S),
    X =.. [S | L].

qff(X, X, _QSTACK, _POLARITY) :-
    (var(X); atomic(X)),
    !.
qff(the(X :: {P0}, Q0), Q2, QSTACK, POLARITY) :-
    !,
    qff(Q0, Q1, QSTACK, POLARITY),
    substitute(ref(X, {P0}), X, Q1, Q2).
qff(forall(X, P0), P2, QSTACK, +) :-
    var(X),
    !,
    substitute(Y, X, P0, P1),
    qff(P1, P2, [Y | QSTACK], +).
qff(forall(X, P0), P2, QSTACK, -) :-
    var(X),
    !,
    qff(exists(X, P0), P2, QSTACK, +).
qff(forall(X :: {P0}, Q0), QFF, QSTACK, POLARITY) :-
    !,
    qff(forall(X, {P0} => Q0), QFF, QSTACK, POLARITY).
qff(no(X :: {P0}, Q0), QFF, QSTACK, POLARITY) :-
    !,
    qff(forall(X :: {P0}, not(Q0)), QFF, QSTACK, POLARITY).
qff(generic(X :: {P0}, Q0), QFF, QSTACK, POLARITY) :-
    !,
    qff(forall(X, Q0 => P0), QFF, QSTACK, POLARITY).
qff(exists(X, P0), P2, QSTACK, +) :-
    var(X),
    !,
    skolem(X, QSTACK),
    qff(P0, P2, QSTACK, +).
qff(exists(X, P0), P2, QSTACK, -) :-
    var(X),
    !,
    qff(P0, P2, [X | QSTACK], -).
qff(exists(X :: {P0}, Q0), QFF, QSTACK, POLARITY) :-
    !,
    qff(exists(X, {P0} & Q0), QFF, QSTACK, POLARITY).
qff(claim(X0), claim(X1), QSTACK, POLARITY0) :-
    !,
    qff(X0, X1, QSTACK, POLARITY0).
qff(query(X0), query(X1), QSTACK, POLARITY0) :-
    !,
    revpolarity(POLARITY0, POLARITY1),
    qff(X0, X1, QSTACK, POLARITY1).
qff([H0 | T0], [H1 | T1], QSTACK, POLARITY) :-
    !,
    qff(H0, H1, QSTACK, POLARITY),
    qff(T0, T1, QSTACK, POLARITY).
qff((A0, B0), (A1, B1), QSTACK, POLARITY) :-
    !,
    qff(A0, A1, QSTACK, POLARITY),
    qff(B0, B1, QSTACK, POLARITY).
qff(X0, X1, QSTACK, POLARITY) :-
    X0 =.. L0,
    qffArgs(L0, L1, QSTACK, POLARITY),
    X1 =.. L1.

qffArgs([], [], _QSTACK, _POLARITY).
qffArgs([H0 | T0], [H1 | T1], QSTACK, POLARITY) :-
    qff(H0, H1, QSTACK, POLARITY),
    qffArgs(T0, T1, QSTACK, POLARITY).

qff(X0, X1) :-
    qff(X0, X1, [], +).

addContexts(X, X) :-
    (atomic(X); var(X)),
    !.
addContexts(at(V, X0), X1) :-
    !,
    addContexts(X0 '\IN' V, X1).
addContexts(claim(X0), claim(X1)) :-
    !,
    addContexts(X0 '\IN' minutes, X1).
addContexts(query(X0), query(X1)) :-
    !,
    addContexts(X0 '\IN' bel(hearer), X1).
addContexts([H0 | T0], [H1 | T1]) :-
    !,
    addContexts(H0, H1),
    addContexts(T0, T1).
addContexts(X0, X1) :-
    X0 =.. L0,
    addContexts(L0, L1),
    X1 =.. L1.

/**
  Our trees are trees, i.e. nested lists. We might want to turn them
  into conjunctions. Leaving them as they are leaves us closer to
  natural logic, which we might want.
  **/

list2conj(X, X) :-
    (var(X); atomic(X)),
    !.
list2conj([X], X) :-
    !.
list2conj([H0 | T0], H1 & T1) :-
    !,
    list2conj(H0, H1),
    list2conj(T0, T1).
list2conj(X0, X1) :-
    X0 =.. L0,
    list2conjAll(L0, L1),
    X1 =.. L1.

list2conjAll([], []).
list2conjAll([H0 | T0], [H1 | T1]) :-
    list2conj(H0, H1),
    list2conjAll(T0, T1).

/**
  We might at an earlier stage have done something to make a copula
  sentence like "John is a fool" mean something like

  claim(((now=#0 & fool(#2)&(ref(A,name(A,John))=#2)IN#0) IN minutes))

  i.e. there is a fool, who we will call #2, and there is someone
  called John, and these two are actually the same thing.

  That was earlier. We *might* now want to actually replace #2 by John
  -- since they're the same thing, it might make sense to have one
  term representing them, turning this into
  claim(fool(ref(A,name(A,John)))IN[minutes,now]). Might.
  **/

elimEQ(X, X, EQ, EQ, _POLARITY) :-
    (var(X); atomic(X)),
    !.
elimEQ([H0 | T0], [H1 | T1], EQ0, EQ2, POLARITY) :-
    !,
    elimEQ(H0, H1, EQ0, EQ1, POLARITY),
    elimEQ(T0, T1, EQ1, EQ2, POLARITY).
elimEQ((A=B) & C1, C2, EQ0, [A=B | EQ1], +) :-
    !,
    elimEQ(C1, C2, EQ0, EQ1, +).
elimEQ(C1 & (A=B), C2, EQ0, [A=B | EQ1], +) :-
    !,
    elimEQ(C1, C2, EQ0, EQ1, +).
elimEQ((A & B) & C, E, EQ0, EQ1, POLARITY) :-
    !,
    elimEQ(A & (B & C), E, EQ0, EQ1, POLARITY).
elimEQ(A0 => B0, A1 => B1, EQ0, EQ2, POLARITY) :-
    !,
    revpolarity(POLARITY, REVPOLARITY),
    elimEQ(A0, A1, EQ0, EQ1, REVPOLARITY),
    elimEQ(B0, B1, EQ1, EQ2, POLARITY).
elimEQ((A0, B0), (A1, B1), EQ0, EQ2, POLARITY) :-
    !,
    elimEQ(A0, A1, EQ0, EQ1, POLARITY),
    elimEQ(B0, B1, EQ1, EQ2, POLARITY).
elimEQ(X0, X1, EQ0, EQ1, POLARITY) :-
    X0 =.. L0,
    elimEQ(L0, L1, EQ0, EQ1, POLARITY),
    X1 =.. L1.

applyEQ([], X, X).
applyEQ([A=B | EQ0], X0, X2) :-
    substitute(A, B, X0, X1),
    substitute(A, B, EQ0, EQ1),
    applyEQ(EQ1, X1, X2).

elimEQ(X0, X2) :-
    elimEQ(X0, X1, [], EQ, +),
    applyEQ(EQ, X1, X2).

/**
  We use X \IN C to mean that X is true in the context C. We sometimes
  end up with things like (X \IN C0) \IN C1, which would be better as
  X \IN C0+C2.
  **/

flattenLabels(X, X) :-
    (var(X); atomic(X)),
    !.
flattenLabels((X0 '\IN' L1) '\IN' L0, X1 '\IN' [L0 | L2]) :-
    !,
    flattenLabels(X0 '\IN' L1, X1 '\IN' L2).
flattenLabels(X0 '\IN' L, X1 '\IN' [L]) :-
    !,
    flattenLabels(X0, X1).
flattenLabels([H0 | T0], [H1 | T1]) :-
    !,
    flattenLabels(H0, H1),
    flattenLabels(T0, T1).
flattenLabels(X0, X1) :-
    X0 =.. L0,
    flattenLabels(L0, L1),
    X1 =.. L1.

/**
  Do it all, print stuff out as you go. If you didn't do some step
  because the flags weren't set to make you do it, don't print out the
  intermediate result:

  Flags are [qlf, fixQuants, qff, addContexts, curry, generics,
  list2conj, simplify, elimEQ]. Some don't do much unless others have
  already been applied, but I'm not doing anything here to ensure that
  only a coherent set are asserted.

  member(F, [qlf, fixQuants, qff, addContexts, curry, generics,
  list2conj, simplify, elimEQ]), set(F), fail.
  **/

nf(X, FLATTENLABELS) :-
    (?printing -> (nl, pretty(baseTree(dtree@X))); true),
    denest(dtree@X, DN),
    justRoots(DN, JR),
    (?printing -> (nl, pretty(justRoots(JR))); true),
    nfTree(JR, NF),
    (?printing -> (pretty(nf(NF))); true),
    (qlf -> qlf(NF, QLF); QLF = NF),
    ((?printing, qlf) -> (nl, pretty(qlf(QLF))); true),
    (?fixQuants -> fixQuants(QLF, FQ); FQ = QLF),
    ((?printing, fixQuants) -> (nl, pretty(fixQuants(FQ))); true),
    (?qff -> qff(FQ, QFF); QFF = FQ),
    ((?printing, qff) -> (nl, pretty(qff(QFF))); true),
    (?addContexts -> qff(QFF, CONTEXTUALISED); QFF = CONTEXTUALISED),
    ((?printing, addContexts) -> (nl, pretty(addContexts(CONTEXTUALISED))); true),
    (?curry -> curry(CONTEXTUALISED, CURRIED); CURRIED = QFF),
    ((?printing, curry) -> (nl, pretty(curried(CURRIED))); true),
    (?generics -> generics(CURRIED, GENERICS); GENERICS = CURRIED),
    ((?printing, generics) -> (nl, pretty(generics(GENERICS))); true),
    (?list2conj -> list2conj(GENERICS, LIST2CONJ); LIST2CONJ = GENERICS),
    ((?printing, list2conj) -> (nl, pretty(list2conj(LIST2CONJ))); true),
    (?simplify -> simplify(LIST2CONJ, SIMP); SIMP = LIST2CONJ),
    ((?printing, simplify) -> (nl, pretty(simplify(LIST2CONJ, SIMP))); true),
    (?elimEQ -> elimEQ(SIMP, ELIMEQ); ELIMEQ = QFF),
    ((?printing, ?elimEQ) -> (nl, pretty(elimEQ(SIMP, ELIMEQ))); true),
    flattenLabels(ELIMEQ, FLATTENLABELS).
