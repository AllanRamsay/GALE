
%%%% ZERO.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**********************************************************************
Definitions of zero pronouns for various languages
 **********************************************************************/

checkZero(ZERO) :-
    language@ZERO <> arabic,
    !,
    specifier@ZERO <-> pro_ref(start@X),
    [agree, gender]@ZERO <-> [agree, gender]@DUMMY,
    semantics@ZERO <-> lambda(X, human(X)),
    +clitic@ZERO,
    ZERO <> [setNoWH, pronoun, fullySpecified, definite1],
    text@ZERO <-> '0',
    underlying@ZERO <-> [O],
    character('0', O),
    penalise(ZERO, 50).

checkZero(ZERO) :-
    language@ZERO <> somali,
    !,
    somaliWHPro(ZERO),
    +clitic@ZERO,
    dir@ZERO <> xbefore,
    -caseDefault@ZERO,
    specifier@ZERO <-> pro_ref,
    semantics@ZERO <-> person(agree@ZERO),
    nom(ZERO, NOM),
    when(nonvar(NOM), (acc(ZERO) -> third_sing_only(ZERO); true)),
    penalise(ZERO, 5).

checkZero(ZERO) :-
    %% specifier@ZERO <-> pro_ref,
    %% semantics@ZERO <-> person(agree@ZERO),
    semantics@X <-> whpronoun,
    specifier@X <-> whspec,
    +clitic@ZERO,
    ZERO <> [setNoWH, pronoun, fullySpecified],
    penalise(ZERO, 20).
    
