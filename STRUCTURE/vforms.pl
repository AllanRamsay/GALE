
%%%% VFORMS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/********************************************************************
Stuff for setting up the semantic consequences of stating that
something has a particular tense or aspect. It depends quite heavily
on the use of unresolved quantifiers, since there are complicated
interactions between these things and the quantifiers introduced by
NPs, but o.w. it shouldn't be too unmanageable.
**********************************************************************/

/***
Set the aspect for V. For non-event type verbs (i.e. auxiliaries,
modal copulas), use local information to help work out what to do.
*/

set_aspect(V, ASPECT) :-
    aspect@V <-> ASPECT.

% The goal here is just to say that the relation simple holds between
% two items (namely an instant and an event type). But sorting out which
% arguments are going to be supplied when, and who has what quant, makes
% it LOOK complicated.

% Do it properly: aspect is a relation between instants and event types 

aspect_simple(V) :-
    set_aspect(V, simple(sort@V, irreal@V)).

set_tense(V, _TENSE) :-
    !,
    aspect_simple(V).

set_tense(V, TENSE) :-
    tense@V <-> TENSE,
    aspect_simple(V).

pres_tense(V) :-
    V <> [present, simpleTensedForm],
    -cat@target@V,
    itag:tag@tag@V <> tag(presTense),
    set_tense(V, tense(present)).

past_tense(V) :-
    V <> [past, simpleTensedForm],
    -cat@target@V,
    +active@V,
    itag:tag@tag@V <> tag(pastTense),
    set_tense(V, tense(past)).

perfect(V) :-
    set_aspect(V, perfect).

future_tense(V) :-
    V <> [future, simpleTensedForm],
    -cat@target@V,
    set_tense(V, tense(future)).

pres_part(V) :-
    pres_part_form(V),
    nonfinite(V),
    -cat@target@V,
    set_aspect(V, prog),
    gerund@V <-> G,
    itag:tag@tag@V <> tag(presPart),
    trigger(realised@V, trigger(realised@G, presPartGerund(G))).

presPartGerund(G) :-
    G <> [nominal, setNoWH, saturated, not_case_marked],
    modifier@G <-> *gerund_as_mod(theta@subject@_V, v:xbar@cat@target@G),
    type@G <-> [gerund | _],
    modified@result@G <-> 5.5.

past_part(V) :-
    past_part_form(V),
    -cat@target@V,
    -gerund@V,
    set_aspect(V, perfect).

ed2(V) :-
    V <> [past],
    -infinitive@V,
    +active@V,
    -cat@target@V,
    -gerund@V,
    subject@V <> nom,
    set_aspect(V, ASPECT),
    tensed@V <-> TENSED,
    itag:tag@tag@V <-> ITAG,
    trigger(TENSED,
	    (TENSED = + ->
		(ASPECT = simplePast, tag(ITAG, pastTense));
		(ASPECT = perfect, tag(ITAG, pastPart)))).
          
ed2(V) :-	
    V <> [present, participle_form],
    -infinitive@V,
    -active@V,
    -cat@target@V,
    set_aspect(V, simplePast),
    gerund@V <-> G,
    itag:tag@tag@V <> tag(passivePart),
    trigger(realised@V, trigger(realised@G, passivePartGerund(G))).
    
passivePartGerund(G) :-
    -modifiable@G,
    cat@G <-> passiveGerund,
    G <> [setNoWH, saturated],
    type@G <-> [gerund | _],
    modified@result@G <-> 5.5,
    -predicative@G,
    % this may be overly restrictive: Allan, 07/06/06
    target@G <> nn,
    G <> notcasemarked.

% present or _untensed_ infinitive
rootForm(V) :-	
    -cat@target@V,
    -participle@V,
    -to_form@V,
    +active@V,
    -cat@target@V,
    -gerund@V,
    tensed@V <-> T,
    subject@V <-> S,
    agree@V <-> agree@S,
    zero@S <-> zero@subject@DUMMYV,
    [agree, vfeatures, aspect, tag]@V <-> [agree, vfeatures, aspect, tag]@DUMMYV,
    when(nonvar(T), setRootAgr(DUMMYV)).    
    
setRootAgr(V) :-
    itag:tag@tag@V <> tag(presTense),
    -value:def@zero@subject@V,
    set_aspect(V, simple),
    V <> [tensed_form, not_third_sing].
setRootAgr(V) :-
    itag:tag@tag@V <> tag(infinitive),
    V <> simple_infinitive.
 
    

