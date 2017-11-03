
%%%% PUREVFORMS.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subjunctive(X) :-
    irreal@X <-> *(subjunctive).

jussive(X) :-
    irreal@X <-> *(jussive).

conditional(X) :-
    irreal@X <-> *(conditional).

indicative(X) :-
    -irreal@X.

irreal1(X) :-
    irreal@X <-> *(_IRREAL).

declarative1(X) :-
    +declarative@X,
    -imperative@X,
    -interrogative@X,
    -wh_interrogative@X.

imperative1(X) :-
    -declarative@X,
    +imperative@X,
    -interrogative@X,
    -wh_interrogative@X,
    irreal@X <-> *(imperative).

interrogative1(X) :-
    -declarative@X,
    -imperative@X,
    +interrogative@X,
    -wh_interrogative@X,
    % made interrogatives real - HG 02/08/01
    irreal@X <-> *(_).

wh_interrogative1(X) :-
    -declarative@X,
    -imperative@X,
    +interrogative@X,
    +wh_interrogative@X,
    irreal@X <-> *(_).

simpleTensedForm(V) :-
    -gerund@V,
    % -value@zero@subject@V,
    +tensed@V,
    -participle@V,
    -infinitive@V.
    
tensed_form(V, tensed@V, active@V) :-
    simpleTensedForm(V).

tensed_form(V, TENSE) :-
    tensed_form(V, TENSE, +).

tensed_form(V) :-
    tensed_form(V, _TENSE).
    
proper_tensed_form(V) :-
    tensed_form(V),
    % agree@V <-> agree@subject@V,
    subject@V <> nom.
       
passive_tensed_form(V) :-
    tensed_form(V, _, -).
    
any_tensed_form(V) :-
    tensed_form(V, _, _).
full_tensed_form(V) :-
    tensed_form(V).

sentence(V) :-
    s(V),
    tensed_form(V).

nonfinite(V) :-
    +unspecified@V,
    -tensed@V.

propernonfinite(V) :-
    nonfinite(V),
    subject@V <> acc.
    
participle_form(V) :-
    nonfinite(V),
    -infinitive@V,
    +participle@V.

not_participle_form(V) :-
    -participle@V.

notense(V) :-
    -present@V,
    -past@V,
    -future@V,
    -preterite@V,
    -free@V.

present(V) :-
    +present@V,
    -past@V,
    -future@V,
    -preterite@V,
    -free@V.	

past(V) :-
    -present@V,
    +past@V,
    -future@V,
    -preterite@V,
    -free@V,
    -infinitive@V.

future(V) :-
    -present@V,
    -past@V,
    +future@V,
    -preterite@V,
    -free@V.

futureTense(tense@V) :-
    V <> future.
    %+irreal@V. /* Copied COMMENTED OUT from Helen, 31/07/01 */

preterite(V) :-	
    -present@V,
    -past@V,
    -future@V,
    +preterite@V,
    -free@V.

pres_or_past_or_future(V) :-
    -preterite@V,
    -free@V.	
    
pres_or_past(V) :- 
    -future@V,
    -preterite@V,
    -free@V.

pres_or_future(V) :-
    -past@V,
    -preterite@V,
    -free@V.	

pres_part_form(V) :-
    +active@V,
    +participle@V,
    V <> present.

pres_part_or_to(V) :-
    V <> present,
    -tensed@V,
    to_form@V <-> *(to).

past_part_form(V) :-
    -gerund@V,
    V <> [past, participle_form].

infinitive_form(V, to_form@V) :-
    nonfinite(V),
    -participle@V,
    +infinitive@V,
    +active@V.

to_infinitive_form(V) :-
    infinitive_form(V, *to).

not_present_form(V) :-
    -present@V.

not_infinitive_form(V) :-
    -infinitive@V.
               
simple_infinitive(V) :-
    -gerund@V,
    V <> infinitive_form(-).

somaliInfinitive(V) :-
    V <> nonfinite,
    -participle@V,
    +infinitive@V,
    -to_form@V,
    -gerund@V.

to_infinitive(V) :-
    infinitive_form(V, *(to)),
    gerund@V <-> G,
    trigger(realised@V, trigger(realised@G, toGerund(G))).

toGerund(G) :-
    G <> [np, adjunct],
    modifier@G <-> *to_gerund_as_mod(theta@subject@V),
    dir@target@G <> xafter,
    +fixed@target@G,
    predicative@G <-> *toAsPred,
    modified@result@G <-> 6.5,
    -modifiable@G,
    gerund@G <-> *(to).

not_to_form(V) :-
    -to_form@V.

not_simple_infinitive(V) :-
    to_form@V <-> *(_).
