%%%% CLASSES.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/***

Definitions of types (see Appendix \ref{Sorts})
***/

/**********************************************************/

unmoved(X) :-
    -moved:dir@dir@X.

equivalence(X, Y, E) :-
     index@X <-> (index@Y=E).  

application(lambda(A, lambda(B, A:B))).  

identity(lambda(I, I)).  

eventlike(V) :-
    text@V <-> TEXT,
    translation@V <-> TRANS,
    sense@V <-> SENSE,
    trigger(TEXT,
	    (nonvar(TRANS) ->
		with_output_to_atom(format('~w (~w)', [TEXT, TRANS]), EVENT);
		EVENT = TEXT)),
    semantics@V <-> event1(EVENT),
    SORT0 <-> sort@V,
    SORT1 <-> (text@V & event),
    default(SORT0 = SORT1),
    mverb(V).  

/***
Wrap up all the stuff you're going to need for sorting out tense and 
aspect in one big package for unpack to deal with  
*/

mverb(V) :-
     specifier@V 
         <-> time(existential, [tense@V], aspect(aspect@V)).

known(X) :-
     \+ var(cat@X).  

/*** reintroduction of the X from XBAR theory */
x(X) :-
    v:xbar@cat@X <-> _.

n(X) :-
	+n:xbar@X.

nominal(X) :-
    -v:xbar@cat@X,
    +n:xbar@cat@X.

nounlike(X) :-
    X <> nominal,
    type@X <-> [noun | _].  

noun(X) :-
    X <> nominal,
    type@X <-> [noun, pronoun(-) | _],
    card@X <-> agree@X.

nn(X) :-
    nominal(X),
    +unspecified@X.

basic_pronoun(X) :-
    fullnp(X),
    X <> saturated,
    type@X <-> [noun, pronoun(+) | _],
    -heavy@X,
    -cat@target@X.		% Change from -target

pronoun1(X) :-
     basic_pronoun(X),
     predicative@X <-> *nppred.  

pronoun(X) :-
     % Helen' stuff, 31/7/01
     -irreal@X,
     pronoun1(X),
     modified@X <-> -1,
     X <> not_case_marked.  

name(X) :-
    X <> nominal,
    type@X <-> [noun, name | _],
    predicative@X <-> *nppred,
     % Helen' stuff, 31/7/01
    -irreal@X.  

bareAdj(X) :-
     +v:xbar@cat@X,
     +n:xbar@cat@X.
    
adj1(X) :-
     bareAdj(X),
     adjunct(X),
     [card, agree, def]@X <-> [card, agree, def]@target@X.    

adj(X) :-
     %% X <> fullyUnspecified,	% 2-part treatment of specified
     adj1(X).  

verbal(X) :-
    +v:xbar@cat@X,
    -n:xbar@cat@X.     

verb(X) :-
     verbal(X),
     subject@X <-> SUBJ,
     SUBJ <> no_case_default,
     -comp@X.

vp(X) :-
     verbal(X),
     args@X <-> [SUBJECT],
     index@SUBJECT <-> index@subject@X.  

proper_vp(X) :-
     vp(X).  

clause(X) :-
     X <> [saturated,verbal].     

s(X) :-
     clause(X),
     -cat@target@X,	% Change from -target
     -predicative@X.  

prep(X) :-
    X <> nominal,
    type@X <-> [prep | _],
    X <> fullySpecified,	% 2-part treatment of specified
    adjunct(X),
    pcase(X),
    card@target@X <-> card@result@X.  
     
iprep(X) :-
    prep(X),
    inflected(X).

pp(X) :-
     prep(X),
     X <> saturated.  
     

particle(X, type@X) :-
     cat@X <-> particle,
     semantics@X <-> type@X,
     X <> [saturated, fullySpecified],
     definite@X <-> dummy.  

particle(X) :-
     particle(X, _).  

dresult(X, result@X) :-
    flag(detAsMod),
	!.
dresult(X, X).

dtarget(X, target@X) :-
    flag(detAsMod),
	!.
dtarget(X, N) :-
	args@X <-> [N].

det0(X) :-
     dtarget(X, TARGET),
     dresult(X, RESULT),
     -moved:dir@displaced@TARGET,
     modified@RESULT <-> 10,
     cat@X <-> specifier,
     -predicative@X,
     +compact@X.
	
det1(X) :-
     flag(detAsMod),
     !,
     X <> [saturated, det0].  

det(X) :-
     flag(detAsMod),
     !,
     det1(X),
     dresult(X, RESULT),
     np(RESULT),
     X <> not_case_marked.

det2(X) :-
     flag(detAsMod),
     !,
     det1(X),
     agree@X <-> agree@target@X.

det1(X) :-
    X <> [nominal, fullySpecified].  

det(X) :-
     det1(X),
     X <> not_case_marked.

det2(X) :-
    det1(X),
    args@X <-> [N],
    agree@X <-> agree@N.

np(X) :-
    X <> nominal,
    +specified@X,
    X <> saturated.  

proper_np(X) :-
    np(X),
    -v:xbar@cat@X,
    +n:xbar@cat@X,
    type@X <-> [noun | _].  

fullnp(X) :-
    -unspecified@X,
    modified@X <-> -1,
    np(X).  

simpleZero(X) :-
    +value:def@zero@X,
    dummy(X),
    start@X <-> end@X,
    [xstart, xend]@X <-> [start, end]@X,
    +compact@X, 
    -moved:dir@displaced@X,
    span@X <-> 0,
    dtrs@X <-> [],
    positions@core@X <->positions@X.  

xzero(X) :-
    text@X <-> '0',
    simpleZero(X).

punct(X) :-
    cat@X <-> punct.   

inflected1(X) :-
    affixes@X <-> [].
    
inflected(X) :-
    lextype@X <-> "",
    affixes@X <-> [].

terminal(X) :-
    postfix:fst@fstIn@X <-> [].

suffix(X) :-
    dir@X <> xafter.  

suffix1(X) :-
    dir@X <> xafter,
    affixes@X <-> [],
    affix@X <-> *1.  

prefix(X) :-
    dir@X <> xbefore.  

derprefix(X, K) :-
    -affix@X,
    affixes@X <-> [K],
    -affix@K,
    dir@K <> xafter,
    syntax@X <-> syntax@K,
    genuine(X),
    affixes@K <-> [].  

word(X) :-
    dtrs@X <-> [].
    
lexitem(X) :-
    affixes@X <-> [],
    -affix@X.

derprefix(X) :-
    derprefix(X, _K).  

dummy(X) :-
    -realised@X.  

genuine(X) :-
    +realised@X.  

adjunct(X) :-
     [specf, uses, head, sort]@target@X
      	<-> [specf, uses, head, sort]@result@X.

spec(X) :-
    X <> fullySpecified,
    dtarget(X, A),
    +unspecified@A.

% GERMAN AFFIXES  
/* details the consequences of roots subcategorising for certain affixes */

subcat_aff(X, AFF) :-
 	-affix@X,
      affix@A <-> AFF,
      dir@A <> xafter,
      [syntax, lextype, branch, umlauted]@X
              <-> [syntax, lextype, branch, umlauted]@A,
 	affixes@X <-> [A].  

/* details the consequences of affixes subcategorising for certain other affixes */
  
subcat_inf(X, affix@X, affix@A) :-
 	dir@X <> xafter,
 	affixes@X <-> [A],
 	[syntax, lextype, branch, umlauted]@X 
             <-> [syntax, lextype, branch, umlauted]@A,
 	dir@A <> xafter.  

aff_lex(X) :-
 	subcat_inf(X, *lex, *inf1).  

aff_inf1(X) :-
 	subcat_inf(X, *inf1, *inf2).  

aff_inf2(X) :-
 	dir@X <> xafter,
 	affixes@X <-> [],
 	affix@X <-> *inf2.  

aff_inf2(X, [LEFT]) :-
 	dir@X <> xafter,
 	affix@X <-> *inf2,
 	affixes@X <-> [LEFT],
 	[syntax, lextype, branch]@X <-> [syntax, lextype, branch]@LEFT,
 	dir@LEFT <> xbefore,
 	affixes@LEFT <-> []. 
