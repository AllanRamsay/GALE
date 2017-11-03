%%%% EQUIV.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*** 
You can often view something from a number of different angles. I
prefer to use logical equivalences, saying that if you've got an X
then you can view it as a Y, rather than having rules with one
daughter and using these to actually create an object of type Y every
time I see something of type X.

\medpara
Harold tells me that this notion was investigated extensively by
people doing \scare{tagmemics}. If I ever get round to reading them I
may change various things.

\medpara
Also very closely related to HPSG \scare{lexical rules} (sort of
\scare{post-lexical rules}). See some work by Louisa Sadler.

\medpara
Individual rules are complex, and need to have proper comments
attached. The idea is simple enough: \texttt{X <==> Y} means that if
something looks like a \texttt{Y} (its \scare{internal} syntactic
properties are those of a \texttt{Y}) then it can be \emph{used} in
situations where \texttt{X}s are required (its \scare{external}
appearance is that of an \texttt{X}.
*/

/****************************************************************/

GERUND <==> VP :-
    equivalence@GERUND <-> equiv(index@VP, vgerund(index@VP)),
    language@VP <> english,
    agree@subject@VP <-> agree@target@GERUND,
    VP <> [s, present, nonfinite],
    +compact@VP,
    -bracketed@VP,
    definite@VP <-> dummy,
    -conj@VP,
    -repaired@VP,
    +realised@GERUND,
    GERUND <-> gerund@VP,
    specifier@GERUND <-> gerundSpec,
    GERUND <> [saturated, adjunct],
    -date@GERUND,
    %% -modifiable@GERUND,
    [finite, wh]@GERUND <-> [finite, wh]@VP,
    target@GERUND <> x, 
    %% arg@GERUND <-> identity,
    %% modifier@GERUND <-> *gerund_as_mod(function@subject@VP), 
    specf@result@GERUND <-> specf@target@GERUND,
    trigger(index@target@GERUND,
	    (+value:def@zero@subject@VP,
		target_of_gerund(sort@subject@VP, target@GERUND),
		incrementScore(score@GERUND, 300))),
    %% GERUND <> fullyUnspecified,
    VP <> fullyUnspecified,
    dir@target@GERUND <-> DIR,
    (nonvar(start@VP) ->
	((start@VP = xstart@VP -> true; VP <> setWHMarked),
       	     % Force gerunds to1 obey William's rule, 24/11
	    (-fixed@target@GERUND ->
		(check_ends(VP) -> DIR <> xbefore; DIR <> xafter);
		true));
	true),
    sort@GERUND --- event,
    ZVAL <-> value:def@zero@subject@VP,
    (ZVAL == + -> (semantics@subject@VP = gerundZeroSubj); true),
    penalise(GERUND, 25).      

ELLIPSE <==> NP :-
     flag(ellipses),
     equivalence@ELLIPSE <-> equiv(index@NP, ellipse(index@NP)),
     NP <> [np, testNoWH, acc],
     start@NP <-> 0,
     ELLIPSE <> [s, tensed_form, setNoWH, saturated],
     topicalised@ELLIPSE <-> [],
     semantics@ELLIPSE <-> semantics@NP,
     arg@ELLIPSE <-> ellipse_arg,
     +ellipse@ELLIPSE,
     definite@ELLIPSE <-> dummy,
     extendWithX(elliptical(index@ELLIPSE), failures@ELLIPSE).

NP <==> NN :-
    equivalence@NP <-> equiv(index@NN, determinerless),
    -date@NP,
    NN <> [nn, saturated, fullyUnspecified],
    NP <> [np],
    [agree, case]@NP <-> [agree, case]@NN,
    penalise(NP, 10),
    setNoWH(NP),
    soft(determinerlessNPs).

% WH-less relatives  
% (as in the "the man you saw")

NP <==> RCLAUSE :-
     equivalence@NP <-> equiv(index@RCLAUSE, free_relative),
     language@RCLAUSE <> english,
     RCLAUSE <> [s, declarative1, tensed_form, testNoWH],
     -comp@RCLAUSE,
     +compact@RCLAUSE,
     topicalised@RCLAUSE <-> [TOPIC | _],
     -conj@RCLAUSE,
     type@TOPIC <-> [noun, pronoun(-) | _],
     TOPIC <> [noun, notnom],
     %% [nonfoot]@NP <-> [nonfoot]@TOPIC,
     date@NP <-> date@TOPIC,
     NP <> [np, saturated],
     semantics@NP <-> freeRelSemantics,
     modified@result@NP <-> 6.5,
     setNoWH(NP),
     penalise(NP, 70),
     predicative@NP <-> *(freeRelAsPredication).

% Headless relatives  
% WHCLAUSE is the internal description
% NP is the external view

checkSortForWHTarget(WHSORT, TARGET) :-
    (sort@TARGET ~~ WHSORT ->
	true;
	penalise(TARGET, 33)).

NP <==> WHCLAUSE :-
    equivalence@NP <-> equiv(index@WHCLAUSE, headlessEq),
    -value:def@zero@WH,
    language@WHCLAUSE <-> LANGUAGE,
    irreal@result@NP <-> irreal@NP,
    -modifiable@NP,
    %% ANY LANGUAGE !
    %%(LANGUAGE <> english; LANGUAGE <> persian; LANGUAGE <> arabic),
    %% -value@zero@subject@WHCLAUSE,
    %% What is the WHCLAUSE like?
    WHCLAUSE <> s,
    trigger(index@target@NP, checkSortForWHTarget(sort@WH, target@NP)),
    -main@WHCLAUSE,
    (LANGUAGE <> english ->
	WHCLAUSE <> declarative1;
	true),
    WHCLAUSE <> [s, simpleTensedForm], % removed because of "that sleep"
    subject@WHCLAUSE <> reallyNom,
    +compact@WHCLAUSE,
    %% -comp@WHCLAUSE,
    %% It has a proper WH-marker (no repetitions, Allan, 13/1/4)
    %% the next line makes sure that we only get one reading for Somali
    %% examples, despite the fact that all the pronouns are potentially
    %% WH-marked. This is a temporary hack (which I'll probably forget
    %% and wonder why we don't get all the readings!) Allan, 13/01/06
    (checkWHMarked(WHCLAUSE, WH) -> true; fail),
    %% do this after determining that it is indeed a WH clause
    %% because we get dummy sentences (e.g. "so") which don't actually
    %% have subjects. But they're not WH-marked, so if we do it now then
    %% it won't cause a problem
    %% (LANGUAGE <> english -> \+ start@subject@WHCLAUSE < start@core@WHCLAUSE; true),
    focus@WHCLAUSE <-> []/[],
    (LANGUAGE <> german ->
	xend@WHCLAUSE <-> end@core@WHCLAUSE;
	true),
    %% Block non-resumptive ones for non-args
    %% (this one is open to discussion: if we include it then we don't
    %% get example (11) of the ACL submission, and V & N didn't say we
    %% can't have that one. But N did say in other conversations that
    %% resumptive pronouns must be arguments: so try that as test)
    (LANGUAGE <> persian ->
	(text@WH == kh ->
	    theta@WH = arg(_);
	    true);
	true),
    [syntax]@NP <-> [syntax]@WH,
    NP <> saturated, % needed because under certain circumstances Somali doesn't set it properly: AR, 04/01/06
    %% We need to fill in a few extra details that weren't actually present
    NP <> [setNoWH, third, fullySpecified],
    -date@NP,
    modified@result@NP <-> 4,
    %% modified@result@NP <-> 12,
    trigger(index@target@NP, target_of_rclause(target@NP, result@NP)),
    %% The semantics of these things is REALLY difficult and is what follows
    %% specifier@NP <-> specifier@WHCLAUSE,
    specifier@NP <-> headlessSpec,
    type@NP <-> [headless, positions@WH | _],
    /******** THIS IS IT *************************************************/
    semantics@NP <-> headless,
    arg@NP <-> headlessAsArg,
    modifier@NP <-> *headlessMod.

% Temporal expressions (every day, last week, ...)  
MOD <==> NP :-
     language@NP <> english,
     equivalence@MOD <-> equiv(index@NP, date),
     +specified@MOD,
     NP <> [np, acc],
     +date@NP,
     cat@MOD <-> date,
     MOD <> [saturated, adjunct],
     target@MOD <> vp,
     -aux@target@MOD,
     +fixed@target@MOD,
     modified@result@MOD <-> 2.1,
     [semantics, specf, wh]@MOD <-> [semantics, specf, wh]@NP,
     modifier@MOD <-> *date. 
    
% Appositive NPs

MOD <==> ANP :-
    equivalence@MOD <-> equiv(index@ANP, app),
    ANP <> np,
    bracketed@ANP <-> ",",
    -pform@ANP,
    cat@MOD <-> appositiveNP,
    MOD <> saturated,
    target@MOD <> np,
    theta@MOD <-> modifier(_),
    -predicative@MOD,
    modified@result@MOD <-> 2.

/*
X <==> NCC :-
    nonfoot@X <-> nonfoot@NCC,
    ncc@NCC <-> dummy(I, Y),
    wh@X <-> wh@NCC,
    nonvar(I),
    meaning@X <-> meaning@NCC,
    args@X <-> [Y | args@NCC].
*/

/* PERSIAN POSSESSIVES */

DET <==> NP :-
    equivalence@DET <-> equiv(index@NP, possessive),
    language@NP <> persian,
    NP <> [np, gen, testNoWH],
    DET <> [det, saturated],
    target@DET <> [noun],
    +unspecified@target@DET,
    dir@target@DET <> xafter,
    -moved:dir@displaced@target@DET.

/* MALAY */
  
DET <==> NP :-
     $$(equivalence(DET, NP, possessive)),
     language@NP <> malay,
     NP <> [np, gen, testNoWH],
     DET <> [det, not_case_marked],
     +definite@DET,
     args@DET <-> [N],
     N <> [noun, saturated],
     dir@N <-> before,
     N <> genuine,
     dtype(DET, simple, _).

TOPIC <==> NP :-
    language@NP <> somali,
    equivalence@TOPIC <-> equiv(index@NP, somaliTopic),
    cat@TOPIC <-> topic,
    TOPIC <> [saturated, adjunct],
    [case, wh]@TOPIC <-> [case, wh]@NP,
    target@TOPIC <-> TARGET,
    +fixed@TARGET,
    % +compact@TARGET,
    +topic@result@TOPIC,
    TARGET <> [s],
    modifier@TOPIC <-> *mod(topic),
    NP <> [np],
    -caseDefault@NP,
    % block headless relatives from forming topics
    % (might be over the top, but it'll do for now: Allan, 12-01-06)
    % -equivalence@NP,
    comp@TARGET <-> comp@result@TOPIC,
    modified@result@TOPIC <-> 10,
    clitic@result@TOPIC <-> clitic@target@TOPIC,
    specifier@TOPIC <-> identity,
    semantics@TOPIC <-> topicMeaning,
    theta@NP <-> lambda(_A, lambda(B, B)),
    focus@TOPIC <-> focus@NP,
    % to stop pronouns (and headless relatives based on pronouns) being topics
    -clitic@NP,
    -moved:dir@displaced@TOPIC,
    dir@TARGET <> xbefore,
    % end@TOPIC <-> start@TARGET,
    dtrs@TARGET <-> DTRS,
    +clitic@COINDEX,
    [discourse]@COINDEX <-> [index]@NP,
    -clitic@TOPIC,
    trigger(index@TARGET, (cmember(COINDEX, DTRS), !)).

/*  ARABIC */

X <==> NP :-
     language@NP <> arabic,
     semantics@X <-> nomsent,
     equivalence@X <-> equiv(index@NP, nomsent),
     +realised@NP,
     -clitic@NP,
     NP <> [np, nomOrAcc],
     X <> [verb, setNoWH, pres_tense, fullySpecified, mverb],
     agree@NP <-> agree@PRED,
     [syntax, semantics]@subject@X <-> [syntax, semantics]@NP,
     theta@PRED <-> arg(pred(nomsent(predicative@PRED))),
     args@X <-> [PRED],
     dir@PRED <> xafter,
     +fixed@X,
     PRED <> [nomOrPrep, saturated],
     predicative@PRED <-> *(_),
     %% -indefinite@NP,
     -after:dir@displaced@PRED,
     %% +indefinite@PRED,
     %% +definite@PRED,
     PRED <> nomOrPrep,
     %% test is currently -indefinite (AR, 16/2/05)
     %% the conditions here are very complicated: current description
     %% says that if the subject is definite then the predicate must be
     %% either a PP or a nominative NP, and that it may not be
     %% displace; and that if it's indefinite then
     %% the predicate must be a left-shifted PP. Who knows what the
     %% right answer is?
     (-indefinite@NP ->
         (% PRED <> reallyNom, % commented out for Hanady's 15/02/05 examples
	     -moved:dir@displaced@PRED);
         (PRED <> [pcase, np], +moved:dir@displaced@PRED)).

/*
% IT describes the construction phrase in Arabic (EDafaa) 

% Now dealt with as a constraint
            
DET <==> NP :-
     language@NP <> arabic,
     equivalence@DET <-> equiv(index@NP, construction),
     NP <> [np, gen],
     DET <> [det, saturated],
     % +definite@target@DET,
     -indefinite@target@DET,
     -definite@target@DET,
     dir@target@DET <> xafter,
     -moved:dir@displaced@DET,
     result@DET <> [not_case_marked],
     +definite@NP,
     [core, head]@result@DET <-> [core, head]@target@DET,
     % This one works if we demand that the right-subjects of nominal
     % sentences must be indefinite
     def@target@DET <-> def@result@DET,
     % This is what Hanady wants
     % result@DET <> construct,
     dtype(DET, simple, _).
*/

/*
VP <==> SUBJ :-
    equivalence@VP <-> equiv(index@SUBJ, nomsent),
    SUBJ <> [np, reallyNom],
    language@SUBJ <> somali,
    VP <> verbal,
    wh@VP <-> wh@SUBJ,
    args@VP <-> [NP],
    theta@NP <-> arg(pred),
    +realised@NP,
    dir@NP <> xafter,
    -moved:dir@displaced@NP,
    NP <> [np, acc].
*/
