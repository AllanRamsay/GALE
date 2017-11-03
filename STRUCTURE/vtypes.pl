
%%%% VTYPES.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***********************************************************************
Declarations of major verb classes. Sets the argument list, but then
positions/2 sorts out the order and case marking. The generic one
is is \texttt{valency(N, [A1, A2, A3])}, where the \texttt{Ai} are the
names of thematic roles and \texttt{N} says that the first \texttt{N}
in the list are obligatory, which turns out to be a neat way of
specifying optional arguments.

The others are just various important irregular cases. Most thoroughly
developed for English, of course, but as usual I am sure that other
languages have just as many cases which are just as irregular.
***********************************************************************/  

/***
Check that the type assigned to the argument is compatible with the
type of the filler: penalise if we're being relaxed about type
violations, reject if not.
***/
checkType(X, TYPE) :-
    SORT <-> sort@X,
    (SORT = TYPE ->
	true;
	doSoftParse(SORT ~~ TYPE, 200, typeViolation(index@X, SORT, TYPE), X)).

/***
Map a list of thematic roles to a list of arguments. Default is that
arguments are lists, first clause deals with cases where the type of
the argument has been supplied.
***/
theta_2_arg(THETA:TYPE, SIGN) :-
    !,
    theta@SIGN <-> arg(THETA),
    current_language(L),
    (somali(L) -> +clitic@SIGN; true),
    default(np(SIGN)),
    T --- TYPE,
    [sort, remarks, index]@SIGN <-> [sort, remarks, index]@DUMMY,
    trigger(index@SIGN, checkType(DUMMY, T)). 
    
theta_2_arg(H, H) :-
    current_language(L),
    (somali(L) -> +clitic@H; true),
    H <> sign,
    \+ \+ -predicative@H,
    !,
    default((\+ \+ -fixed@H, -bracketed@H, np(H))).
theta_2_arg(H, H) :-
    H <> sign,
    current_language(L),
    (somali(L) -> +clitic@H; true),
    !,
    default(-bracketed@H).
theta_2_arg(pcase(T), SIGN) :-
    current_language(L),
    (somali(L) -> +clitic@SIGN; true),
    !,
    theta@SIGN <-> arg(T),
    pcase(SIGN, T),
    default(np(SIGN)). 
theta_2_arg(T, SIGN) :-
    current_language(L),
    (somali(L) -> +clitic@SIGN; true),
    theta@SIGN <-> arg(T),
    default(np(SIGN)). 
 
thetas_2_args([], []). 
thetas_2_args([*H | THETAS], ARGS) :-
     !,
     (thetas_2_args([H | THETAS], ARGS);
      thetas_2_args(THETAS, ARGS)). 
thetas_2_args([H | THETAS], [A | ARGS]) :-
     theta_2_arg(H, A),
     !,
     thetas_2_args(THETAS, ARGS).

/* the German vtype has a slightly different format in the dictionary - 
this is the conversion */

g_vtype(V, N, THETAS) :-
    language@V <> german,
    (verb(V) -> vtype(V, valency(N, THETAS)); true).
    
noTopicalisation(SCORE, TOPIC) :-
    +moved:dir@displaced@TOPIC,
    !,
    (before:dir@displaced@TOPIC == + ->
	testWHMarked(TOPIC);
     (+moved@displaced@X, displaced@X <> xafter) ->
	incrementScore(SCORE, 200);
	true).
noTopicalisation(_, _).

vtype(N, _TYPE) :-
     N <> noun,
     ntype(N, simple).
     
vtype(V, unknown) :-
    vtype(V, valency(1, [agent:living, object])),
    ptag:tag@tag@V <> tag(verb). %% ordinary intransitive/transitive 
/*
vtype(V, unknown) :-
    vtype(V, valency(3,[agent, object, to])),
    ptag:tag@tag@V <> tag(verb). % like "give" 
*/
vtype(V, unknown) :-
    vtype(V, valency(2, [agent, S])), % I know she loves me
    S <> s,
    ptag:tag@tag@V <> tag(verb),
    theta@S <-> arg(event).

vtype(X, open) :-
    vtype(X, unknown).
  
vtype(V, valency(N0, THETAS)) :-
    V <> verb,
    default(V <> eventlike),
    thetas_2_args(THETAS, ARGS),
    valency(V, N0, ARGS),
    length(ARGS, L0),
    length(args@V, L1),
    penalise(V, L0-L1),
    ptag:tag@tag@V <> tag(verb),
    language@V <-> LANGUAGE,
    ((english(LANGUAGE), THETAS = [_, _ | _]) ->
         default(-after:dir@displaced@subject@V);
         true).  

cleftOrThere(TOPIC, OBJECT) :-
    TOPIC <> np,
    OBJECT <> [np],
    type@OBJECT <-> [headless | _],
    theta@OBJECT <-> arg(pred((predicative@OBJECT):(specifier@TOPIC):(semantics(index@TOPIC)))),
    OBJECT <> [no_case_default, notnom, saturated].

checkCleftOrThere(for, DUMMYOBJECT) :-
    DUMMYOBJECT <> s.
checkCleftOrThere(to, DUMMYOBJECT) :-
    DUMMYOBJECT <> vp.

cleftOrThere(TOPIC, OBJECT) :-
    TOPIC <> [adj],
    OBJECT <> [verbal, to_infinitive_form],
    comp@OBJECT <-> *(COMP),
    [cat, args]@OBJECT <-> [cat, args]@DUMMYOBJECT,
    trigger(index@OBJECT, checkCleftOrThere(COMP, DUMMYOBJECT)),
    theta@OBJECT <-> arg(pred(topred(specifier@TOPIC, semantics(index@TOPIC)))).

vtype(V, copula(OBJECT)) :-
    language@V <> english,
    +after:dir@displaced@subject@V,
    default(semantics@V <-> event1(state)),
    V <> mverb,
    +active@V,
    IT <> [np, word],
    text@IT <-> it,
    theta@IT <-> arg(dummySubject),
    TOPIC <> np,
    OBJECT <> [np, acc],
    type@OBJECT <-> [headless | _],
    theta@OBJECT <-> arg(pred(adjpred)),
    OBJECT <> [no_case_default, notnom, saturated],
    theta@TOPIC <-> arg(topic(ref)),
    +n:xbar@cat@TOPIC,
    TOPIC <> no_case_default,
    args@V <-> [IT, TOPIC, OBJECT].

vtype(V, copula(OBJECT)) :-
    language@V <> english,
    +after:dir@displaced@subject@V,
    +after:dir@displaced@OBJECT,
    semantics@V <-> event1(predication(PRED, TOPICTYPE)),
    V <> mverb,
    TOPIC <> np,
    +referential@TOPIC,
    +intensional@TOPIC,
    text@TOPIC <-> TOPICTEXT,
    +active@V,
    theta@TOPIC <-> arg(topic(TOPICTYPE)),
    % allow gerunds as topics
    % type@TOPIC <-> [noun | _],
    start@TOPIC <-> STARTT,
    start@OBJECT <-> STARTO,
    start@V <-> STARTV,
    trigger(ground(STARTT+STARTO), \+ (STARTT > STARTO, STARTO > STARTV)),
    MOVEDOBJ <-> moved:dir@displaced@OBJECT,
    trigger(MOVEDOBJ, noTopicalisation(score@V, OBJECT)),
    -date@TOPIC,
    predicative@OBJECT <-> *PRED,
    % The predication might be negated, in which case the whole thing is
    polarity@OBJECT <-> polarity@V,
    theta@OBJECT <-> arg(pred(PRED)),
    trigger(PRED, setTopicType(PRED, TOPICTYPE, TOPICTEXT)),
    [modified]@OBJECT <->[modified]@DUMMYOBJ,
    score@V <-> score@DUMMYV,
    trigger(TOPICTEXT,
	    (TOPICTEXT == there ->
		preferModified(DUMMYV, DUMMYOBJ);
		true)),
    OBJECT <> [no_case_default, notnom, saturated, fullySpecified],
    -referential@OBJECT,
    -intersective@OBJECT,
    args@V <-> [OBJECT, TOPIC].

setTopicType(PRED, TOPICTYPE, TOPICTEXT) :-
    (PRED = adjpred -> TOPICTYPE = ref; 
	(PRED = nppred; PRED = barePluralAsPred) ->
	trigger(TOPICTEXT,
		(TOPICTEXT = there -> TOPICTYPE = there; TOPICTYPE = ref));
	TOPICTYPE = ref).

preferModified(V, X) :-
    trigger(modified@X,
	    (modified@X > 0 -> (penalise(V, -1000)); true)).

vtype(V, copula) :-
    % -modifiable@V,
    vtype(V, copula(_)).

vtype(V, give0(_PTYPE)) :-
     V <> [verb, eventlike],
     -aux@V,
     AGENT <> np,
     particle(OBJECT),
     theta@AGENT <-> arg(agent),
     theta@OBJECT <-> arg(particle),
     args@V <-> [AGENT, OBJECT].  

vtype(V, give1(_PTYPE)) :-
     V <> [verb, eventlike],
     -aux@V,
     AGENT <> np,
     theta@AGENT <-> arg(agent),
     OBJECT <> np,
     theta@OBJECT <-> arg(object),
     particle(PTCLE),
     theta@PTCLE <-> arg(particle),
     args@V <-> [AGENT, OBJECT, PTCLE].

properVP(X) :-
    \+((X <> word, text@core@X == ',')).

% Allan's treatment of auxiliaries as scomp's

vtype(V, aux(AUX)) :-
    COMP <> [s],
    vtype(V, aux(COMP, AUX)).

vtype(V, aux(COMP, AUX)) :-
     +aux@V,
     vtype(V, aux1(COMP, AUX)).

/*
vtype(V, aux(COMP, _AUX)) :-
    V <> verb,
    +fixed@V,
    args@V <-> [SUBJ, S],
    head@V <-> head@S,
    SUBJ <> [np, reallyNom],
    dir@SUBJ <> xafter,
    -moved:dir@displaced@SUBJ,
    S <> [s, tensed_form],
    dir@S <> xbefore,
    -moved:dir@displaced@S.
*/

vtype(V, aux2(COMP, AUX)) :-
     V <> verb,
     +fixed@V,
     semantics@V <-> AUX,
     +active@V,
     -moved:dir@displaced@COMP,
     dir@COMP <> xafter,
     theta@COMP <-> arg(*auxComp(AUX)),
     args@V <-> [COMP],
     COMP <> [clause, fullyUnspecified],
     % -imperative@COMP,
     polarity@V <-> polarity@COMP,
     specified@V <-> tensed@V,
     [subject, agree]@V <-> [subject, agree]@COMP,
     agree@V <-> agree@subject@V,
     arg@V <-> identity,
     trigger(tensed@V,
	     checkZeroSubjForAux(tensed@V, value:def@zero@subject@COMP)).

checkZeroSubjForAux(TENSED, ZSUBJ) :-
    (TENSED = + -> ZSUBJ = -; true).
    
vtype(V, modal(MODE0)) :-
     +aux@V,
     -specified@COMP,
     polarity@V <-> polarity@COMP,
     V <> [tensed_form, mverb],
     subject@V <-> subject@COMP,
     args@V <-> [COMP],
     MODE1 =.. [MODE0, X, lambda(E, opaque(0, (Y:E)))],
     semantics@V <-> lambda(X, lambda(Y, MODE1)),
     theta@COMP <-> arg(modal),
     ptag:tag@tag@V <> tag(modal(_MODAL)),
     COMP <> [s, simple_infinitive].

vtype(V, aux1(COMP, AUX)) :-
     specifier@COMP <-> time(_DEFCOMP, TSPEC, ASPECT),
     specifier@V <-> time(_DEFV, [tense@V | TSPEC], ASPECT),
     vtype(V, aux2(COMP, AUX)).

vtype(V, will) :-
     +aux@V,
     vtype(V, aux2(COMP, standardAux)),
     specifier@V <-> time(existential, _TENSE, _ASPECT),
     specifier@COMP <-> time(_DEFCOMP, TSPEC, ASPECT),
     specifier@V <-> time(_DEFV, [tense@V | TSPEC], ASPECT),
     COMP <> simple_infinitive.

% Don't trust the definition of "go"  

vtype(V, go) :-
     +aux@V,
     V <> [verb, pres_part_form],
     COMP <> [to_infinitive_form],
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     specifier@COMP <-> time(_DEFCOMP, TSPEC, ASPECT),
     specifier@V <-> time(_DEFV, [tense@V, future | TSPEC], ASPECT),
     vtype(V, aux2(COMP, standardAux)).
 
vtype(V, go) :-
     vtype(V, valency(1, [agent])).

vtype(V, go) :-
     vtype(V, valency(2, [agent, ACTIVITY])),
     ACTIVITY <> [vp, pres_part_form, vpcomp, fullyUnspecified].

vtype(V, do) :-
    specifier@V <-> time(definite, _TENSE, _ASPECT),
    vtype(V, aux(COMP, standardAux)),
    COMP <> simple_infinitive,
    ptag:tag@tag@V <> tag(doAux).

vtype(V, do) :-
    V\tag <-> VX\tag,
    vtype(VX, valency(2, [agent, object])),
    ptag:tag@tag@V <> tag(doTrans).

% This is the reading of "be" as an auxiliary

vtype(V, be) :-
     V <> verb,
     ptag:tag@tag@V <> tag(beAux),
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     -infinitive@COMP,
     +participle@COMP,
     % +active@COMP,
     COMP <> present,
     vtype(V, aux(COMP, standardAux)).

vtype(V, be) :-
     V <> verb,
     ptag:tag@tag@V <> tag(beCop),
     vtype(V, copula).

vtype(X, make) :-
    vtype(X, valency(2, [agent, object])).

vtype(X, make) :-
    vtype(X, valency(3, [CAUSE, object, COMP])),
    theta@COMP <-> arg(object1),
    -referential@COMP,
    -intersective@COMP,
    COMP <> adj,
    arg@CAUSE <-> up,
    theta@CAUSE <-> arg(cause).
           
vtype(X, make) :-
    vtype(X, valency(2, [cause, COMP])),
    theta@COMP <-> arg(scomp),
    -value:def@zero@subject@COMP,
    -before:dir@displaced@COMP,
    COMP <> [s, simple_infinitive].

vtype(V, get) :-
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     -infinitive@COMP,
     +participle@COMP,
     -active@COMP,
     % +active@COMP,
     COMP <> present,
     vtype(V, aux(COMP, standardAux)).   
        
vtype(V, get) :-
     V <> verb,
     +active@V,
     vtype(V, valency(2, [agent, object])).

vtype(V, get) :-
    V <> verb,
    +active@V,
    vtype(V, valency(2, [agent, PRED])),
    PRED <> [saturated, no_case_default],
    predicative@PRED <-> *(P),
    theta@PRED <-> arg(pred(P)),
    trigger(index@PRED, (pp(PRED); adj(PRED))).

vtype(V, haveAux) :-
     V <> verb,
     COMP <> [past, participle_form],
     -to_form@COMP,
     +active@COMP,
     -value:def@zero@subject@COMP,
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     vtype(V, aux(COMP, standardAux)),
     ptag:tag@tag@V <> tag(haveAux).

vtype(V, have) :-
     vtype(V, haveAux).

vtype(V, have) :-
     V <> verb,
     VX\tag <-> V\tag,
     vtype(VX, valency(2, [agent, COMP])),
     COMP <> [to_infinitive_form, scomp],
     +value:def@zero@subject@COMP,
     -moved:dir@displaced@COMP,
     sense@V <-> haveTo,
     ptag:tag@tag@V <> tag(haveTo).

vtype(V, have) :-
     V <> verb,
     +active@V,
     subject@V <-> SUBJ,
     +after:dir@SUBJD,           
     +after:dir@displaced@OBJ,
     displaced@OBJ <-> OBJD,
     displaced@SUBJ <-> SUBJD,
     xstart@SUBJ <-> XSSUBJ,
     xstart@OBJ <-> XSOBJ,
     OBJ <> np,
     sense@V <-> have,
     theta@OBJ <-> arg(object),
     T --- ~event,
     trigger(index@OBJ, checkType(OBJ, T)),
     end@V <-> ENDV,
     start@V <-> STARTV,
     trigger(XSSUBJ,
	     (%% pretty(SUBJ),
		 %% pretty(OBJ),
		 XSSUBJ =< ENDV, (XSSUBJ = ENDV -> (XSSUBJ < XSOBJ); true))),
     trigger(ground(XSSUBJ+STARTV+XSOBJ), \+ (XSSUBJ < STARTV, XSSUBJ < XSOBJ, XSOBJ < STARTV)),
     trigger(XSOBJ, XSOBJ-ENDV < 3),
     trigger(moved:dir@SUBJD, haveInversion(SUBJD, OBJD)),
     gerund@OBJ <-> GERUND,
     trigger(GERUND, \+ GERUND = *(to)),
     vtype(V, valency(2, [agent:living, OBJ])).

vtype(V, haben) :-
     V <> verb,
     COMP <> [past, participle_form],
     -to_form@COMP,
     +active@COMP,
     -value:def@zero@subject@COMP,
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     vtype(V, aux(COMP, standardAux)).

vtype(V, sein) :-
    vtype(V, valency(2, [agent, object])).

vtype(V, haben) :-
    vtype(V, valency(2, [agent, object])).
    
vtype(V, to) :-
     V <> verb,
     COMP <> [simple_infinitive],
     +active@COMP,
     end@V <-> start@core@COMP,
     start@subject@COMP <-> STARTSUBJ,
     end@subject@COMP <-> ENDSUBJ,
     start@V <-> STARTV,
     trigger(ground(STARTSUBJ+STARTV),
	     (STARTSUBJ < STARTV; STARTSUBJ = ENDSUBJ)),
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     vtype(V, aux(COMP, to)),
     ptag:tag@tag@V <> tag(toComp),
     itag:tag@tag@V <> tag(to).  

vtype(V, for) :-
     V <> verb,
     COMP <> [to_infinitive_form],
     +active@COMP,
     end@V <-> start@subject@COMP,
     specifier@V <-> time(definite, _TENSE, _ASPECT),
     vtype(V, aux(COMP, standardAux)).  

vtype(V, persianAuxiliary) :-
     V <> verb,
     specifier@V <-> time([{definite, _TENSE, _IRREAL} | _]),
     -infinitive@COMP,
     +participle@COMP,
     % +active@COMP,
     COMP <> past,
     vtype(V, aux(COMP, standardAux)).  
  
vtype(V, ast) :-
     V <> verb,
     vtype(V, persianAuxiliary).  
  
vtype(V, ast) :-
     V <> verb,
     vtype(V, copula).  

vtype(V, dar) :-
     vtype(V, aux(COMP, dar)),
     tense@V <-> tense@COMP.

vtype(V, bvd) :-
     V <> verb,
     COMP <> [past, participle_form],
     -to_form@COMP,
     +active@COMP,
     +definite@V,
     vtype(V, aux(COMP, standardAux)).  

haveInversion(SD, OD) :-
    ((+moved:dir@SD, -before:dir@SD) ->
	(+moved:dir@OD, -before:dir@OD);
	true).

vtype(V, faire) :-
     vtype(V, valency(2, [agent, object])).  

vtype(V, faire) :-
     vtype(V, valency(2, [agent, EVENT])),
     EVENT <> [vp, simple_infinitive, vpcomp]. 

to(X) :-
    vtype(X, to).

