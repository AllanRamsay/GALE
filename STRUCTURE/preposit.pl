 
%%%% PREPOSITIONS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***
Prepositions in general make case-marked NPs which are capable
of functioning as modifiers. The point of making them into case-marked
NPs is to make it easy to give them much the same treatment as other
NPs when they are used as verb-complements.

\medpara
There is a general transitive form for prepositions, which
says that they take some complement and can modify anything which is
modifiable.  Ones that take NPs as their complements are so common
that we specify this as a subtype. 
***/

/**********************************************************************/

prep_german(X, CASE, TYPE, DIR) :-
      X <> prep,
      language@X <> german,
      args@X <-> [COMP],
      (DIR <-> both_pos
        -> (+fixed@X,
            +before:dir@dir@COMP,
            +after:dir@dir@COMP,
            prep_german(X, CASE, TYPE)
            );
       DIR <-> post_pos
        -> (dir@COMP <> xbefore,
            prep_german(X, CASE, TYPE)
            );
       DIR <-> diff_pos
         -> (((dir@COMP <> xafter, case_marked(COMP, dat));
              (dir@COMP <> xbefore, case_marked(COMP, acc))
             ),
             prep_german(X, CASE, TYPE)
            )
       ).

prep_german(X, CASE, TYPE) :-
      X <> prep, 
      language@X <> german,
      args@X <-> [COMP],

/* the following line makes sure X remembers the morph case of the NP, 
but not the fact that the NP was not a PP 
gprepcase defined in agree.pl */

      %%%%% THERE IS A PROBLEM HERE ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      % case\gprepcase@X <-> case\gprepcase@COMP,
      COMP <> np,
      (
       (CASE <-> acc_dat -> (no_case_default(COMP), acc_or_dat(COMP)));
       (CASE <-> dat_gen -> dat_or_gen(COMP));
       ((CASE <-> acc; CASE <-> dat; CASE <-> gen) 
         -> case_marked(COMP, CASE)
       )
      ), 
	preposition(X, comp(COMP),TYPE).

preposition(X, mcomp(COMP), TYPE) :-
    sort@X <-> sort@COMP,
    %% Somali prepositions can take zero object pronouns
    (language@X <> somali ->
	true;
	+realised@COMP),
    %% -modifiable@X,
    -pitchMark:phoneme@phon@X,
    X <> adjunct,
    target@X <> x,
    pcase(X, TYPE),
    args@X <-> [COMP],
    date@X <-> date@COMP,
    +intensional@COMP,
    %% Commented out for Somali, Allan 05/01/06 (but it seems to me that this
    %% should never actually happen, so I'm wondering which language allows
    %% non-accusative arguments for prepositions)
    %% no_case_default(COMP),
    theta@COMP <-> arg(*preparg),
    %% modifier@X <-> *prep1(lambda(F, lambda(G, PREL))), PREL =.. [TYPE, F, G],
    modifier@X <-> *prep1(TYPE),
    semantics@X <-> identity,
    X <> fullySpecified,
    definite@X <-> dummy,
    %% Check commented out for messing with interruptions
    %% -after@displaced@COMP,
    %% Removed because otherwise the requirement that the subject and
    %% complement agree (which is a generally sensible rule) means that
    %% you can't have a PP as a complement unless it has the same head
    %% features as the subject (which is silly: it would rule out
    %% "they are in the park")
    %% agree@X <-> agree@COMP,
    target@X <-> TARGET,
    specf@TARGET <-> specf@result@X,
    language@X <-> LANGUAGE,
    [modified, syntax]@result@result@X <-> [modified, syntax]@result@TARGET,
    (LANGUAGE <> arabic -> -moved:dir@displaced@COMP; true),
    (LANGUAGE <> arabic -> sort@COMP<-> sort@X; true),
    (LANGUAGE <> arabic ->
	trigger(index@TARGET, (TARGET <> noun; (TARGET <> s, -comp@TARGET)));	
      LANGUAGE <> notSGGMP ->
	trigger(index@TARGET, noun_or_vp(TARGET));
     LANGUAGE <> persian ->
	trigger(index@TARGET, (TARGET <> noun; (TARGET <> s, -comp@TARGET)));
     LANGUAGE <> somali ->
	trigger(index@TARGET, (TARGET <> noun; (TARGET <> [verb])));
	trigger(index@TARGET, (TARGET <> noun; (TARGET <> s, -comp@TARGET)))),
    pconstraints@COMP <-> PCONSTRAINTS,
    sort@TARGET <-> SORTTARGET,
    score@TARGET <-> SCORE,
    trigger(sort@TARGET, setPConstraint(PCONSTRAINTS, TYPE, SORTTARGET, SCORE)),
    ptag:tag@tag@X <> tag(prep),
    itag:tag@tag@X <> tag(prep1).

/***
If the ground puts constraints on the figure for this preposition then
check that they are satisfied.
*/
checkPConstraint(CONSTRAINTS, PREP, SORT2, N) :-
    (cmember(PREP=SORT1, CONSTRAINTS) ->
	(var(SORT2) -> N = 0;
	SORT1 ~~ SORT2 -> N = -5;
	N = 5);
	N = 0).

/***
Used for putting restrictions on figure-prep-ground triples. It's a bit
odd, because I do it by attaching the constraint to the ground, and then
triggering it when the figure is found. 
*/
setPConstraint(PCONSTRAINTS, TYPE, SORTTARGET, SCORE) :-
    (PCONSTRAINTS = [] -> true; true),
    checkPConstraint(PCONSTRAINTS, TYPE, SORTTARGET, N),
    incrementScore(SCORE, N).

preposition(X, comp(COMP), TYPE) :-
     default(modified@result@X <-> 2.1),
     predicative@X <-> *pppred(TYPE),
     preposition(X, mcomp(COMP), TYPE),
     MVCOMP <-> moved:dir@displaced@COMP,
     (language@X <> spanish ->
 	xafter(X);
      language@X <> persian ->
          (+fixed@X, 
           default(dir@COMP <> xafter),
           MVCOMP = -);
 	true),
     target@X <-> TARGET,
     start@X <-> XSTART,
     end@TARGET <-> ENDTARGET,
     end@COMP <-> ENDCOMP,
     trigger(index@COMP, (ENDCOMP =< XSTART -> ENDCOMP < XSTART; true)),
     trigger(index@target@X, (nn(TARGET) -> XSTART = ENDTARGET; true)). 

preposition(X, ncomp, TYPE) :-
     %% fullySpecified removed for Persian. Will it do damage elsewhere?
     COMP <> [nominal, no_case_default, notnom],
     (language@X <> arabic -> COMP <> gen; true),
     %% Helen, 31/07/01
     irreal@X <-> irreal@COMP,
     %% leaving this to filter
     %%irreal@result@X <-> irreal@X,
     %% can't rely on agreement anymore? - real unmarked
     irreal@result@X <-> irreal@target@X,
     preposition(X, comp(COMP), TYPE).

preposition(X, npcomp, TYPE) :-
     %% fullySpecified removed for Persian. Will it do damage elsewhere?
     COMP <> [np, no_case_default, notnom, saturated],
     (language@X <> arabic -> COMP <> gen; true),
     (language@X <> english -> COMP <> acc; true),
     %% Helen, 31/07/01
     irreal@X <-> irreal@COMP,
     %% leaving this to filter
     %%irreal@result@X <-> irreal@X,
     %% can't rely on agreement anymore? - real unmarked
     irreal@result@X <-> irreal@target@X,
     preposition(X, comp(COMP), TYPE).     
	 
preposition(X, nncomp, TYPE, WORDS) :-
     COMP <> nn,
     preposition(X, comp(COMP), TYPE),
     trigger(index@COMP, cmember(text@core@COMP, WORDS)).    
      
preposition(X, npcomp(loc), TYPE) :-
     COMP <> [np, acc_or_prep, no_case_default],
     preposition(X, comp(COMP), TYPE).  

preposition(X, npvpcomp, TYPE) :-
     (COMP <> np;
      COMP <> [vp, pres_part_form]),
     preposition(X, comp(COMP), TYPE).  

preposition(X, intransitive1, TYPE) :-
    modified@X <-> -1,
    modified@result@X <-> 2.1,
    X <> [np, adjunct, intransitive],
    predicative@X <-> *lambda(A, lambda(B, lambda(C, (((modifier@X):B):A):C))),
    semantics@X <-> identity,
    default(modifier@X <-> *prep0(TYPE)),
    pcase(X, TYPE),
    dir@target@X <> xafter,
    ptag:tag@tag@X <> tag(prep),
    itag:tag@tag@X <> tag(prep0).  

preposition(X, intransitive, TYPE) :- 
     modified@result@X <-> 2.1,
     target@X <-> TARGET,
     TARGET <> x,
     trigger(index@TARGET, noun_or_vp(TARGET)),
     preposition(X, intransitive1, TYPE).  

preposition(X, persianIntransitive, TYPE) :- 
     modified@result@X <-> 2.1,
     target@X <-> TARGET,
     TARGET <> x,
     trigger(index@TARGET, (noun(TARGET); s(TARGET))),
     preposition(X, intransitive1, TYPE).  

preposition(X, sentential, TYPE) :-
     target@X <-> TARGET,
     TARGET <> s,
     preposition(X, intransitive1, TYPE).  

preposition(X, literal(text@COMP), TYPE) :-
     modified@result@X <-> 2.1,
     COMP <> [noun, intransitive],
     preposition(X, comp(COMP), TYPE).  

/*** Somali adpositions */
adposition(X, TYPE) :-
    preposition(X, npcomp, TYPE),
    args@X <-> [A],
    +clitic@X,
    +clitic@A,
    +fixed@A,
    target@X <-> TARGET,
    %% -moved:dir@displaced@TARGET,
    -comp@TARGET,
    TARGET <> verb,
    dir@TARGET <> xafter,
    theta@A <-> arg(_),
    +compact@result@X.
