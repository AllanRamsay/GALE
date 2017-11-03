 %%%% POSITIONS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***********************************************************************
The stuff in here allocates semantic roles to syntactic
\scare{positions}. The general idea is that there are a number of
syntactic roles, such as subject, object, indirect object, which are
marked by case-marking and word-order. Different languages impose
(slightly) different case-markers and (radically) different word
orders, but all the languages I know anything about have three
distinguished roles: subject, usually marked by nominative case in
tensed sentences and usually agreeing with the verb in number \&
gender; object, marked by accusative case; and 2nd object, which is
marked by a combination of case-markers and constraints on word
order. The most interesting constraints on the second object are found
in German, where there are several forms of inflectional case-marker,
but a common-ish pattern is that the second object can be marked
either by a morphological case marker or a preposition, but that these
markers correspond to particular word orders: this is what accounts
for the dative shift in English -- \q{I gave my mother a book}/\q{I
gave a book to my mother}.

\medpara
This is done by providing a set \scare{positions}, for which semantic
roles compete. The competition is pretty simple minded--we have a list
of roles, and the earlier you appear in the list the better you do in
the competition. If there is a list of roles at some point in this
list then they are equally likely to win the competition. This does
Fillmore's \q{I opened the door with the key}/\q{I opened the
door}/\q{The key opened the door}/\q{the door opened} set. The
positions are marked in a rather weird feature which has three
\texttt{+/-} constituents, with the item that wins the first
competition getting the value \texttt{\{+,-,-\}} and so on the
others. Don't ask me why I did it that way: looked clever at the time,
works OK, I may even have had a good reason for it.

\medpara
The competition is run using a set of language specific orders. It
would be better if it used a set of thematic roles that cover lots of
languages, but I've let people use their own sets of thematic roles,
so we're currently stuck with a single set. It would be nice to tidy
this up! Lots of theoretical brownie points!

\medpara
positions/3 is quite complex. It does, in fact, embody quite a
lot of the linguistic theory. In particular it includes the
language-specific constraints on subjects, which can be quite complex
(see Arabic) and passives. It also invokes a set of lanuage specific
constraints for assigning case-markers to the other two nominated
positions. I'm currently assuming that in any situation where a verb
has four arguments then the fourth is marked by a preposition: I'm not
actually aware of any four-argument verbs, but that's what would
happen if we did have any.
***********************************************************/   

/***	
better defines a partial order over theta markers. This has to be
done on a per language basis: what we have here is for english
***/  

better(X, Y) :-
     X == Y,
     !,
     fail.  

better(X, Y) :-
     language@X <> english,
     theta@X <-> arg(TX),
     theta@Y <-> arg(TY),
     !,
     better(TX, TY, [dummySubject, agent, cause, cause(_), with, topic, topic(_), actor, protagonist, 
                     extra, particle,
		     [dummy, dummyObject, object, to, for],
		     cleft,
		     [pred, pred(_)],
		     object1,
		     [loc(_)],
		     [state, event, presupp_event(_)]]).

better(X, Y) :-
     language@X <> somali,
     theta@X <-> arg(TX),
     theta@Y <-> arg(TY),
     !,
     better(TX, TY, [cause, agent, [patient, object, object1, u, ka, ku, la]]).

better(X, Y) :-
     language@X <> greek,
     theta@X <-> arg(TX),
     theta@Y <-> arg(TY),
     !,
     % You need to replace to and for by the relevant Greek prepositions
     better(TX, TY, [agent, [object, to, for], [state, event, presupp_event(_)]]).      

better(X, Y) :-
     language@X <> french,
     theta@X <-> arg(TX),
     theta@Y <-> arg(TY),
     !,
     better(TX, TY, [agent, cause, topic, actor, protagonist,
                    [object, av], particle, predication, [state, event, presupp_event(_)]]).  

better(X, Y) :-
    language@X <> german,  
    theta@X <-> arg(lab(TX, _, M_CASE1)), 
    theta@Y <-> arg(lab(TY, _, M_CASE2)), 
    \+ M_CASE1 <-> -,
    \+ M_CASE2 <-> -,
    !,
    better(TX, TY, [es, poss, von, noprep, [gen, an, f_ur], mit, vor, event]).

better(X, Y) :-
     language@X <> spanish,
     !,
     better(theta@X, theta@Y,
             [agent, topic, particle, [event, presupp_event(_)], loc(_LOC), object]).  

/*
    The preference order for Arabic verb arguments.
    
*/

better(X, Y) :-
    language@X <> arabic,
    theta@X <-> arg(TX),
    theta@Y <-> arg(TY),
    !,
    better(TX, TY, [agent, object, object1, l, event]).
    
/*
    The preference order for Persian verb arguments.
    arab*/

better(X, Y) :-
    language@X <> persian,
    theta@X <-> arg(TX),
    theta@Y <-> arg(TY),
    !,
    better(TX, TY, [topic, agent, event, ra, light(_, _)]).
    
better(X, Y, [H | _]) :-
     (X <-> H; (cmember(X, H), \+ cmember(Y, H))),
     !. 

better(X, Y, [H | _]) :-
     (Y <-> H; (cmember(Y, H), \+ cmember(X, H))),
     !, fail. 

better(X, Y, [_ | T]) :-
     better(X, Y, T).  

/***
Use better to get the argument to fill the next "position": may
return alternative answers, since what we actually want is something
which is AT LEAST AS GOOD AS anything else, not something which is
BETTER THAN anything else. 
*/  

best(X, ARGS, REST) :-
     position@X <-> position@Y,
     delete(X, ARGS, REST),
      \+ ((cmember(Y, REST), better(Y, X))).

/***

This is the bit that uses the \scare{positions} to define the
canonical order: we've marked items for "importance", and we now
decide what that does to the surface order (very like "obliqueness" in
HPSG). Language specific.

Given a list of arguments, what should be next, and should it be left or right?

***/

earlier({+, -, -}, _ANY, L) :-
    L <> arabic,
    !.
earlier({-, +, -}, {-, -, +}, L) :-
    L <> arabic,
    !.

earlier(_ANY, {+, -, -}, L) :-
    L <> persian,
    !.
earlier({-, -, +}, {-, +, -}, L) :-
    L <> persian,
    !.
    
earlier(_ANY, {+, -, -}, L) :-
    L <> somali,
    !.
earlier({-, -, +}, {-, +, -}, L) :-
    L <> somali,
    !.

earlier({-,+,-}, _ANY, L) :-
    \+ cmember(L, [spanish, greek, arabic, persian, somali]),
    !.
earlier(_ANY, {+, -, -}, L) :-
    \+ cmember(L, [spanish, greek, arabic, persian, somali]),
    !. 

% If we specified +fixed for the head, we assume that the list
% we were given was in the right order already (so override any
% principles we may think we've got).

next_arg(X, [A | REST], A, REST) :-
    \+ -fixed@X,
    !.

% The next argument should be chosen on the basis of its 'position marker'
    
next_arg(X, ARGS0, N, ARGS1) :-
    \+ cmember(language@X, [spanish, greek]),
    !,
    delete(N, ARGS0, ARGS1),
    \+ (cmember(Y, ARGS1), earlier(position@Y, position@N, language@X)).

next_arg(X, ARGS, N, REST) :-
    language@X <> spanish,
    subject@X <-> SUBJECT,
    !,
    delete(N, ARGS, REST),
    (N = SUBJECT ->
	true;
	default(-value:def@zero@SUBJECT)).

next_arg(_X, ARGS, N, REST) :-
    delete(N, ARGS, REST).

/***
This is for encoding syntactic theta roles as semantic thematic roles
*/

sort_args(_X, [], []).
sort_args(X, [A | ARGS], [A | ARGS]) :-
    fixed@X <-> FIXED,
    \+ FIXED = -,
    !.
sort_args(X, ARGS0, [NEXT1 | ARGS1]) :-
    language@X <-> language@NEXT1,
    next_arg(X, ARGS0, NEXT1, ARGS1).

/***        
Position markers are used for two things:
(i) to decide on how the argument should be case marked:
    +-- denotes subject
    -+- is morphologically case marked in English
    --+ is prepositionally case marked in English except for object
(ii) to decide on the order in which the arguments are to be found
-+- precedes --+ precedes +-- in English
This is used by next_arg for English and French
This looks odd. The left->right order is +-- -+- --+, which looks
sensible, but they're not collected from left to right.
 */   

positions(ARGS, P, OTHERS) :-
     position@A1 <-> {+, -, -},
     position@A2 <-> {-, +, -},
     position@A3 <-> {-, -, +},
     (best(A1, ARGS, REST1) -> true; REST1 <-> ARGS),
     ((best(A2, REST1, REST2), (best(A3, REST2, OTHERS) -> true; OTHERS <-> REST2));
      (\+ best(A2, REST1, _REST), OTHERS <-> REST1)),
     noDummies([A1, A2, A3], P).  
     
noDummies([], []).
noDummies([H | T0], T1) :-
    var(theta@H),
    !,
    noDummies(T0, T1).
noDummies([H | T0], [H | T1]) :-
    noDummies(T0, T1).

:- dynamic positions/2.

positions(V, L2) :-
    (V <> verb;
     (cat@V <-> compound(K, _), verb(K))),
     language@V <-> LANGUAGE,
     zero@S <-> ZERO,
     % S is the subject
     % Some languages allow zero subjects
     (notFGGM(LANGUAGE) ->
          -usedef:def@ZERO;
          true),
     % For now, force all Somali subjects to be nominative
     (LANGUAGE <> somali ->
	 reallyNom(S);
	 true),
     S <> not_case_marked,
     tensed@V <-> VTENSED,
     (LANGUAGE <> english ->
	 trigger(VZS, ((VZS = +) -> VTENSED = -; true));
	 true),
     /*
     [positions, index, cat, case, zero, theta, def, forms, displaced, type, wh, sort, semantics]@subject@V
  	<-> [positions, index, cat, case, zero, theta, def, forms, displaced, type, wh, sort, semantics]@S,
	*/
     [positions, index, cat, case, zero, def, forms, displaced, type, wh, meaning\sort]@subject@V
  	<-> [positions, index, cat, case, zero, def, forms, displaced, type, wh, meaning\sort]@S,
     start@S <-> SSTART,
     start@core@V <-> VSTART,
     value:def@zero@S <-> VZS,
     (LANGUAGE <> german -> nom(S); true),
     [agree, case, specf, marked]@S <-> [agree, case, specf, marked]@DUMMYS,
     [agree, main, marked]@V <-> [agree, main, marked]@DUMMYV,
     (arabic(LANGUAGE) ->
         (softParse(SSTART, gender@S = gender@V, subjGender(index@V, index@S), V),
	     S <> notgen,
	     %% -cliticised@S,
	     softParse(SSTART, checkArabicSubjAgree(SSTART, VSTART, VZS, DUMMYS, DUMMYV), subjAgree(index@V, index@subject@V), V),
	     softParse(SSTART, checkArabicSubjPosition(DUMMYS, DUMMYV), markedSubject(index@V, index@subject@V), V),
	     true);
         agree@S <-> agree@V),
     active@V <-> ACTIVE,
     !,
     % Remove any dummies from L0: note that any arg may actually be missing.
     select(V, ARGS1), % if there are optional arguments, try deleting them
     ((ACTIVE = +, ARGS2 = ARGS1);
      (LANGUAGE <> somali, ACTIVE = middle, middleVoice(ARGS1, ARGS2));
      (ACTIVE = -, penalise(V, 7), passiveArgs(ARGS1, ARGS2, LANGUAGE))),
     positions(ARGS2, L1, OTHERS),
     (LANGUAGE <> english ->
	 plantConstraintsOnArgs(start@S, start@V, L1);
	 true),
     nth(1, S, L1), %% Allocate S, O1, O2 to variables for inspection
     (nth(2, O1, L1) ->
         (case_mark(V, O1) -> true; true);
         true),
     (nth(3, O2, L1) ->
         (case_mark(V, O2) -> true; true);
         true),
     markothers(V, OTHERS),
     append(L1, OTHERS, L2), %% add any non-positioned args on the end
     theta@S <-> THETAS, %% Insist that the subject has a proper theta role
     \+ var(THETAS),
     aux@V <-> AUXV, %% Subject of English/French main verb appears to the left
     ((englishOrFrench(LANGUAGE), AUXV = -) ->
  	subjleft(V, S);
  	true).

positions(V, ARGS) :-
     ARGS <-> args@V.

plantConstraintsOnArgs(_SSTART, _VSTART, T) :-
    \+ \+ T = [],
    !.
plantConstraintsOnArgs(SSTART, VSTART, [H | T]) :-
    trigger(SSTART, notBetweenSubjectAndVerb(start@H, SSTART, VSTART)),
    plantConstraintsOnArgs(SSTART, VSTART, T).

notBetweenSubjectAndVerb(HSTART, SSTART, VSTART) :-
    trigger(HSTART, \+ (SSTART < HSTART, HSTART < VSTART)).

checkArabicSubjAgree(SSTART, VSTART, VZS, S, V) :-
    (number(SSTART) ->
	((SSTART < VSTART; VZS == +) ->
	    (+marked@V, agree@S <-> agree@V,
		(+definite@S; -main@V)); 
	    (third_sing_only(V)));
	true).

checkArabicSubjPosition(S, V) :-
    indefinite@S <-> I,
    main@V <-> M,
    marked@V <-> B,
    ((I = +, M = +) ->
	B = -;
	true).

middleVoice(L, L).

passiveArgs(L0, L1, LANGUAGE) :-
    L0 = [_A, B | L05],
    (LANGUAGE <> somali ->
	(theta@CAUSE <-> arg(cause),
	    cmember(CAUSE, L0));
	true),
    np(B),
    theta@B <-> THETAB,
    \+ var(THETAB),
    (not_case_marked(B) ->
	L1 = [B | L05];
	(prep(P),
            P <> word,
            pform@P <-> THETAB,
            theta@P <-> arg(particle),
            args@P <-> [_],
            C <> np,
            position@C <-> {+, -, -},
            position@P <-> {-, -, +},
            theta@C <-> arg(THETAB),
            L1 = [C, P | L05])).

% This is for "he is expected to die", where "he to die" is the SUBJECT
% of "expected". Only tricky bit is that the core of "he to die" follows
% expected
passiveArgs(L0, L1) :-
    L0 = [_A0, B],
    -value:def@zero@subject@B,
    B <> s,
    dir@B <> xafter,
    L1 = [B]. 

subjleft(_V, S) :-
     default(dir@S <> xbefore),
     default(-after:dir@displaced@S).

/***
	CASE MARKING
	
 	Language specific: shouldn't be here (but if not here, then where?) 
*/   

% Don't try to case mark VPs, Ss or objects whose type is unknown  

case_mark(_V, X) :-
     position@X <-> {-, _, _},
     language@X <> persian,
     theta@X <-> arg(ra),
     !,
     % X <> acc_or_prep,
     X <> pcase,
     pform@X <-> ra,
     X <> no_case_default.

case_mark(_V, X) :-
     \+ X <> np,
     caseDefault@X <-> DC,
     !,
     (DC = -). 

% This one's for soft args that shouldn't even be there. So how can I
% imagine what their case should be?

case_mark(_V, X) :-
     theta@X <-> arg(extra),
!,
     -caseDefault@X.

%% For the object, say nothing (the default will then fix it to nom if it
%% manages to turn up as the subject of a tensed verb (i.e. in a passive
%% or an ergative) or failing all else to acc)

case_mark(_V, X) :-
     cmember(theta@X, [arg(object), arg(object1), arg(dummy), arg(pred(_))]),
     notGM(language@X),
     !.  
    
case_mark(_V, X) :-
    theta@X <-> arg(object1),
    language@X <> arabic,
    !.
    
case_mark(V, X) :-
    theta@X <-> arg(T),
    language@V <> arabic,
    !,
    default(pcase(X, T)).

% Anything given 2nd object position must be marked by a preposition 
% (for cases like the "with" in "He opened it with the key")  

case_mark(_V, X) :-
     position@X <-> {-, -, +},
     notSGMP(language@X),
     theta@X <-> arg(THETAX),
     !,
     pcase(X, THETAX).      

% From here on, we're only looking at first object position items  

case_mark(_V, X) :-
     theta@X <-> arg(T),
     cmember(T, [to, for]),
     language@X <> english,
     !,
     default(X <> dat).      
     
case_mark(_V, X) :-
     theta@X <-> arg(T),
     cmember(T, [loc(_)]),
     language@X <> english,
     !,
     default(X <> pcase),
     target@X <> vp.          

case_mark(_V, X) :-
     theta@X <-> arg(T),
     cmember(T, [to, for]),		% Fix the right prepositions here
     language@X <> greek,
     !,
     X <> gen.    

% This will only take effect for clitics  

case_mark(_V, X) :-
     theta@X <-> arg(av),
     language@X <> french,
     !,
     X <> dat.  

case_mark(_V, X) :-
     position@X <-> {-, +, -},
     language@X <> spanish,
     theta@X <-> arg(THETAX),
     !,
     pcase(X, THETAX).  

/**** PERSIAN **********************************************************/

case_mark(V, X) :-
     language@X <> persian,
     theta@X <-> arg(light(ROOT, OPTIONS)),
     text@core@X <-> {{ROOT,''},''},
     X <> word,
     !,
     X <> [acc, word],
     +realised@X,
     modified@X <-> 0,
     +active@V,
     % -moved:dir@displaced@X,
     moved:dir@displaced@X <-> M,
     score@V <-> score@DUMMYV,
     trigger(index@X, (cmember(ROOT, OPTIONS), ((M == +) -> penalise(DUMMYV, 30); true))).

case_mark(_V, X) :-
     theta@X <-> arg(event),
     language@X <> persian,
     !,
     X <> acc_or_prep,
     X <> pcase,
     pform@X <-> ra,
     X <> no_case_default.  

case_mark(_V, X) :-
     position@X <-> {-, +, -},
     language@X <> persian,
     theta@X <-> arg(THETAX),
     !,
     pcase(X, THETAX).
     
/* GERMAN */  

case_mark(_V, X) :-
    theta@X <-> arg(lab(comp, -, +)),
    language@X <> german,
    !.

% does not have a cut because we have to try to case-mark it morphologically
case_mark(V, X) :-
    language@X <> german,
    theta@X <-> arg(lab(PREP, CASE, M_CASE)),
    \+ (M_CASE = +),  
    \+ position@X <-> {+, -, -},
    X <> no_case_default,
    casemark_constr(X, V, (PREP,CASE),  _CX, 'prepositional object').

case_mark(V, X) :-
    language@X <> german,
    theta@X <-> arg(lab(_PREP, _P_CASE, +)),
    position@X <-> {-, +, -},
    !,
    X <> no_case_default,
    casemark_constr(Y, V, acc,  _CX, 'direct object'),
    trigger(index@X, (X=Y -> true; (soft(xxx), extendWithX('We have a problem', failures@V)))).

% for verbs with two acc-marked arguments
case_mark(V, X) :-
    language@X <> german,
    theta@X <-> arg(lab(_PREP, _P_CASE, acc)),
    position@X <-> {-, -, +},
    !,
    X <> no_case_default,
    casemark_constr(X, V, acc,  _CX, 'accusative object').

case_mark(V, X) :-
    language@X <> german,
    theta@X <-> arg(lab(_PREP, _P_CASE, dat)),
    !,
    X <> no_case_default,
    casemark_constr(X, V, dat,  _CX, 'indirect object').

case_mark(V, X) :-
    language@X <> german,
    theta@X <-> arg(lab(_PREP, _P_CASE, gen)),
    !,
    X <> no_case_default,
    casemark_constr(X,  V, gen,  _CX, 'genitive object').

% This will leave it to be fixed by the default rule if it can (this will demand acc)
case_mark(_V, X) :-
    \+ language@X <> german,
    !.
    
% This will leave it to be fixed by the default rule if it can 
% (this will demand acc) 
case_mark(_V, _X) :-
     !.

markothers(_V, []) :-
    !. 
markothers(V, [H | T]) :-
     verb(H),
     -position@H,
     !,
     markothers(V, T). 
markothers(V, [H | T]) :-
    -position@H,
    theta@H <-> arg(THETA),
    language@V <> german,
    !,
    functor(THETA, PREP, 2), 
    TYPE =.. [PREP, _]
,
    pcase(H, TYPE),
    markothers(T).
markothers(V, [H | T]) :-
     -position@H,
     theta@H <-> arg(THETA),
     pcase(H, THETA),
     markothers(V, T).  
     
casemark_constr(X, _V, CASE, _CASE_ACT, SYN_FUNCT) :-
	(
	 SYN_FUNCT <-> 'prepositional object' -> 
		CASE <-> (PREP, P_CASE);
	 true
	),
	trigger(index@X,
	    (
(CASE = dat; CASE = gen; CASE = acc) -> 
	        (case_marked(X, CASE), \+ + case_marked(X, prep, _));
 	        (
case_marked(X, prep, of_type(PREP, _)),
 	         case_marked(X, P_CASE)
)
)).
