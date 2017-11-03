 /*
 	PRONOUNS.PL
 */  

proTag(X) :-
    ptag:tag@tag@X <> tag(pronoun),
    itag:tag@tag@X <-> ITAG,
    trigger(nom@X,
	    (nom(X) -> tag(ITAG, nom);
		acc(X) -> tag(ITAG, acc);
		tag(ITAG, other))).

whproTag(X) :-
    itag:tag@tag@X <-> ITAG,
    trigger(nom@X,
	    (nom(X) -> tag(ITAG, nom);
		acc(X) -> tag(ITAG, acc);
		tag(ITAG, other))).

c_pro_ref(X, P) :-
    proTag(X),
    semantics@X <-> P,
    -date@X,
    X <> definite1,
    specifier@X <-> pro_ref(start@X),
    card@X <-> agree@X,
    arg@X <-> identity.  

c_anph_ref(X, P) :-
    proTag(X),
    semantics@X <-> P,
    X <> definite1,
    -date@X,
    specifier@X <-> anph_ref(start@X),
    card@X <-> agree@X,
    arg@X <-> identity.  

c_or_a_pro_ref(X, P) :-
    proTag(X),
    semantics@X <-> P,
    X <> definite1,
    -date@X,
    specifier@X <-> anph_or_pro_ref(start@X),
    card@X <-> agree@X,
    arg@X <-> identity.  

n_pro_ref(X, P) :-
    proTag(X),
    semantics@X <-> P,
    X <> definite1,
    -date@X,
    specifier@X <-> pro(start@X),
    card@X <-> agree@X,
    arg@X <-> identity.  

properName(X, P) :-
    n_pro_ref(X, P),
    noun(X),
    X <> not_case_marked.

wh_pronoun(X) :-
    -date@X,
    -indefinite@X,
    semantics@X <-> whpronoun,
    specifier@X <-> whspec,
    whproTag(X).  

rel_pronoun(X, REL) :-
     -date@X,
     wh_pronoun(X),
     rel(X => REL).      
     
that(X) :-
    c_pro_ref(X, lambda(Z, thing(Z) & distal(Z))),
    X <> setNoWH.
  

