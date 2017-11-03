%%%% LEXTYPES.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**********************************************************************

Rules that spell out the significance of assigning an item to
a lexical class. Typically you get a frame with various values filled
in for comparison with the properties of a range of affixes.
	
This is, in particular, the place where the fine details of Arabic
lexical classes are specified: see the comment attached to
arabicRegular/4 for more detail. It also includes
underlyingForms/1, which is mainly intended for putting all the
elements of an Arabic word together, though it is also useful for
words in some other languages. But Arabic is the worst.

********************************************************************/

immediate(X) :-
    +immediate@X.

% GERMAN


/*       All German frames have the following general structure: 
      frame(CLASS, SHAPE, LEFT). 
*/
      %  GERMAN NOUNS

      % takes s for plural, no_dat_suff
foreign_noun(X, LEX) :-
      X <> noun,
      subcat_aff(X, *lex),
      lextype@X <-> frame(strong(foreign), ["", LEX, "s", -], "").

       % takes ns for genitive
mixed_noun(X, LEX, BR) :-
      X <> noun,
      masculine(X),
      subcat_aff(X, *lex),
      lextype@X <-> frame(n_decl(mixed), [BR, LEX, _PL, -], ""). 

strong_noun(X, LEX) :-
      strong_noun(X, LEX, "", -).

strong_noun(X, LEX, BR) :-
      strong_noun(X, LEX, BR, -).

strong_noun(X, LEX, BR, UML) :-
      strong_noun(X, LEX, BR, UML, "").

strong_noun(X, LEX, BR, UML, LEFT) :- 
      X <> noun,
      subcat_aff(X, *lex),
      lextype@X <-> frame(strong(_GROUP), [BR, LEX, _PL, UML], LEFT).

strongNoun(X, GROUP, PL, UML) :-
      X <> noun,
      subcat_aff(X, *lex),
      lextype@X <-> frame(strong(GROUP), ["", "", PL, UML], []).

weakNoun(X) :-
    weak_noun(X).

subst_noun(X, LEX_SING, LEX_PL) :-
      X <> noun,
      subcat_aff(X, *lex),
      (
       (lextype@X <-> frame(strong(subst), [_BR, LEX_SING, _PL, -], ""),
        X <> third_sing
       );
       (
        lextype@X <-> frame(strong(subst), [_BR, LEX_PL, _PL, -], ""),
        X <> third_plural
       )
	).

weak_noun(X) :-
      weak_noun(X, "").

weak_noun(X, LEX) :-
      weak_noun(X, LEX, "").

weak_noun(X, LEX, BR) :-
      X <> noun,
      subcat_aff(X, *lex),
      lextype@X <-> frame(n_decl(common), [BR, LEX, _PL, -], "").

      % for irregular plurals
n_plural(X, PL) :-
      X <> noun,
      lextype@X <-> frame(_GROUP, [_BR, _LEX, PL, _UML], _LEFT).
    
%     GERMAN ADJECTIVES

inflAdj(X) :-
    infl_adj(X, "").

infl_adj(X, LEX) :-
      infl_adj(X, LEX, "", -).

infl_adj(X, LEX, BR) :-
      infl_adj(X, LEX, BR, -).

infl_adj(X, LEX, BR, UML) :-
      infl_adj(X, LEX, BR, UML, "").

infl_adj(X, LEX, BR, UML, LEFT) :-
      X <> adj,
      language@X <> german,
      subcat_aff(X, AFF),
      (LEX <-> "" -> AFF <-> *inf1; AFF <-> *lex),
      lextype@X <-> frame(inflecting, [BR, LEX, UML], LEFT),
      adjtype(X, simple).	%%%%  SHOULD BE IN THE LEXICON %%%%%%%%
      
% GERMAN VERBS

/* The general verb frame gives the vowels of the following stems (indicative):
[INFINITIVE, PRES_2ND_PERS, PAST_3RD_PERS, PAST_PART]. These four forms are 
sufficient information to form the whole verb paradigm. 

four verb categories: 
non-t: strong_verb (4 stem vowels, part-en)
t-type: weak_verb (common) -> past-t, part-t)
                  (praet) -> (V1=inf, prespart, pres_plural; 
                                V2=pres_sing; 1st+3rd_pres_sing zerosuff)
                  (mixed) -> Rueckumlaut: ["e", "a"])  
*/
mixed_verb(X) :-
      X <> verb,
      subcat_aff(X, *inf1),
      lextype@X <-> frame(weak(mixed), [_BR, _LEX, "e", "e", "a", "a"], _LEFT). 

mixed_verb(X, [INF, PAST]) :- 
      X <> verb,
      subcat_aff(X, *inf1),
      lextype@X <-> frame(weak(mixed), 
                           [_BR, _LEX, INF, INF, PAST, PAST], _LEFT).

past_verb(X, [INF, PRES_SG, PAST]) :-
      X <> verb,
      subcat_aff(X, *inf1),
      lextype@X <-> frame(weak(praet), 
                           [_BR, _LEX, INF, PRES_SG, PAST, PAST], _LEFT).

strong_verb(X, [INF, PAST]) :-
      strong_verb(X, [INF, PAST, PAST], _LEFT).

strong_verb(X, [INF, PAST, PART]) :-
      strong_verb(X, [INF, PAST, PART], _LEFT).

strong_verb(X, [INF, PRES, PAST, PART]) :-
      strong_verb(X, [INF, PRES, PAST, PART], _LEFT).

strong_verb(X, [INF, PAST, PART], LEFT) :-
      strong_verb(X, [INF, INF, PAST, PART], LEFT).

strong_verb(X, [INF, PRES, PAST, PART], LEFT) :-
      X <> verb,
      subcat_aff(X, *inf1),
      lextype@X <-> frame(ablaut, [_BR, _LEX, INF, PRES, PAST, PART], LEFT).

splitAblaut(STRING, ABLAUT) :-
    atom_codes(STRING, CHARS0),
    append(_PREFIX, [123 | CHARS1], CHARS0),
    append(CORE, [125 | _SUFFIX], CHARS1),
    splitAblaut(CORE, [], ABLAUT),
    !.

splitAblaut(CHARS0, AB0, AB2) :-
    (append(AB, [47 | CHARS1], CHARS0) ->
	(splitAblaut(CHARS1, AB0, AB1), AB2 = [AB | AB1]);
	AB2 = [CHARS0]).

plantAblaut(X, PREFIX0) :-
    splitAblaut(text@X, ABLAUT),
    affix@GE <-> *ge,
    dir@GE <> xbefore,
    (PREFIX0 = + ->
	PREFIX1 = [GE];
	PREFIX1 = []),
    lextype@X <-> frame(ablaut, [_BR, [] | ABLAUT], PREFIX1).

strongVerb(X, PREFIX) :-
    X <> verb,
    subcat_aff(X, *inf1),
    when(nonvar(text@X), plantAblaut(X, PREFIX)).

weak_verb(X) :-
      weak_verb(X, "").

weakVerb(X) :-
    weak_verb(X).

weak_verb(X, LEX) :-
      weak_verb(X, LEX, "").

weak_verb(X, LEX, BR) :-
      weak_verb(X, LEX, BR, _LEFT).

weak_verb(X, LEX, BR, _LEFT) :-
      X <> verb,
      subcat_aff(X, AFF),
      ((LEX <-> "" -> AFF <-> *inf1); AFF <-> *lex),
      LEFT = [GE],
      GE <> prefix,
      affix@GE <-> *ge,
      lextype@X <-> frame(weak(common), [BR, LEX, BR, BR, BR, BR], LEFT).

% GERMAN DETERMINERS

gen_det(X, _DEF) :-
    [agree, nform]@X <-> [agree, nform]@target@X,
    [agree, nform]@X <-> [agree, nform]@result@X,
    subcat_aff(X, *inf2),
    det(X),
    target@X <> nn,
    result@X <> np,
    setNoWH(X).

plantLextype([strong_noun(G), PL, UML], X) :-
    X <> noun,
    subcat_aff(X, *lex),
    lextype@X <-> frame(strong(G), ["", _LEX, PL, UML], []).
plantLextype([weak_noun], X) :-
    X <> weak_noun.

/*
	Spanish verbs

	Two bits; first is "ablaut" (!), second is key vowels for forming 
	affixes.

*/

% V1 is initial vowel of infinitive (i.e. the "a" from "-ar")
% V2 is affix for present tense indicative not 1&2 plural
% V3 is affix for present tense subjunctive

spanishv(simple(INF), X) :-
    lextype@X <-> frame({branch@X, "", "", ""}, VOWELS),
    spanish_vowels(INF, VOWELS).
spanishv(second(INF, {B1, B2}), X) :-
    lextype@X <-> frame({branch@X, B1, B2, B2}, VOWELS),
    spanish_vowels(INF, VOWELS).

spanish_vowels(V1, [V1, V2, SUBJ]) :-
    (V1 = "i" -> V2 = "e"; V2 = V1),
    (V1 = "a" -> SUBJ = "e"; SUBJ = "a").

frenchv(INF, X) :-
    frenchv(INF, [_ | _], X, _FUT, _PPART).

frenchv(INF, X, FUT, PPART) :-
    frenchv(INF, [_ | _], X, FUT, PPART).

frenchv(INF, SUBCLASS, X, FUT, PPART) :- 
    % Include information about regular future and past part
    (INF = "e" ->
    	lextype@X <-> frame(e(SUBCLASS), i(-), ''(-), FUT, PPART, COND);
    INF = "i" ->
    	lextype@X <-> frame(e(-), i(SUBCLASS), ''(-), FUT, PPART, COND);
    INF = "" ->
    	lextype@X <-> frame(e(-), i(-), ''(SUBCLASS), FUT, PPART, COND);
	(format('No such class~w~n', [INF]), fail)).
% branch selects ablaut (!)

branch1({B1, B1, _B2, _B3, _DUMMY}).

branch2(X, {B, B1, B2, _B3, Y}) :-
    (X = Y -> B = B2; B = B1).

needs_number(X) :-  
    affixes@X <-> [NUM],
    [lextype, syntax]@X <-> [lextype, syntax]@NUM,
    dir@NUM <> xafter,
    affix@NUM <-> *number.

% ****************************  ARABIC **********************

/***
The inflectional paradigm of an Arabic root is determined by four
interrelated things:
    * the initial affix (which may be a dummy)
    * the allowable diacritics (which may include consonants!)
    * whether there are any reduplicated consonants
    * the form of the present tense affix
To set these we just have to assign the verb to an inflection class, specify
the form of the present prefix, and specify the initial affix.
In cases where the initial affix is derivational (prefix *deriv) then the
inflectional class (SUBCLASS) sets the diacritics, otherwise the initial
affix does this.
The inflectional class also sets the consonant pattern: but this is done very
simply by saying that some classes, e.g. class ii, require a require
reduplication of some particular radical(s) but the default is that there is 
no reduplication.
And that's it.
*/

bwVerb(X, PREFCLASS, [actvPres@X, actvPast@X, psvPres@X, psvPast@X], _PRESPREFIX) :-
    affixes@X <-> [PREFIX],
    affix@PREFIX <-> *deriv(PREFCLASS),
    affix@current:fst@fstOut@X <-> *deriv(_),
    actualConsonants@X <-> targetConsonants@X,
    [diacritics, syntax, lextype, theta, sort, pconstraints, targetConsonants]@X 
    <-> [diacritics, syntax, lextype, theta, sort, pconstraints, targetConsonants]@PREFIX.
    
arabicRegular(X, [actvPres@X, actvPast@X, psvPres@X, psvPast@X], _PRESPREFIX, PREFCLASS) :-
    !,
    affixes@X <-> [PREFIX],
    affix@PREFIX <-> *deriv(PREFCLASS),
    affix@current:fst@fstOut@X <-> *deriv(_),
    actualConsonants@X <-> targetConsonants@X,
    [diacritics, syntax, lextype, theta, sort, pconstraints, targetConsonants]@X 
    <-> [diacritics, syntax, lextype, theta, sort, pconstraints, targetConsonants]@PREFIX.

arabicRegular(X, SUBCLASS, _PRESFORM, PREFCLASS) :-
    setDiacritics(X, SUBCLASS),
    affixes@X <-> [PREFIX],
    affix@PREFIX <-> *deriv(PREFCLASS),
    affix@current:fst@fstOut@X <-> *deriv(_),
    actualConsonants@X <-> targetConsonants@X,
    [diacritics, syntax, lextype, theta, sort, pconstraints, targetConsonants]@X 
    <-> [diacritics, syntax, lextype, theta, sort, pconstraints, targetConsonants]@PREFIX.

/*
actvPres = *0*L*
actvPast = *A*K*
passvpast = *u*i*
passvpres = *0*a"                                 
*/

setDiacritics(X, class1(A, K, L)) :-
    !,
    actvPres@X <-> ["o", L],
    actvPast@X <-> [A, K],
    psvPres@X <-> ["o", "a"],
    psvPast@X <-> ["u", "i"].
 

/*
actvPres = *0*L*
actvPast = *a*K*
passvpast = *u*i*
passvpres = *0*a"
*/
    
setDiacritics(X, class1(K, L)) :-
    !,
    setDiacritics(X, class1("a", K, L)).
 
/*
actvPres = *0*L*
actvPast = *a*a*
passvpast = *u*i*
passvpres = *0*a"                                 
*/

setDiacritics(X, i(1, L)) :-
     !,
     setDiacritics(X, class1("a", L)).

/*
actvPres = *0*L*
actvPast = *a*i*
passvpast = *u*i*
passvpres = *0*a"                                 
*/

setDiacritics(X, i(4, L)) :-
     !,
     setDiacritics(X, class1("i", L)).

/*
actvPres = *0*L*
actvPast = *a*u*
passvpast = *u*i*
passvpres = *0*a"                                 
*/

setDiacritics(X, i(5, L)) :-
     !,
     setDiacritics(X, class1("u", L)).

setDiacritics(X, ii) :-
    !,    
    actvPres@X <-> ["a", "i"],
    actvPast@X <-> ["a", "a"],
    psvPres@X <-> ["a", "a"],
    psvPast@X <-> ["u", "i"],
    targetConsonants@X <-> [_, C, _],
    +shadda:character@C.

setDiacritics(X, iii) :-
    !,    
    actvPres@X <-> ["A","a"],
    actvPast@X <-> ["A", "a"],
    psvPres@X <-> ["A", "a"],
    psvPast@X <-> ["w", "i"].

/*
yaEozum --active present
EAzam
yuEozum
Euzim
*/


/** trying some classes ----  by nassim***/


/** mad~a**/
setDiacritics(X, c1) :-
    !,    
    actvPres@X <-> ["u"],
    actvPast@X <-> ["a"],
    psvPres@X <-> ["a"],
    psvPast@X <-> ["u"],
    targetConsonants@X <-> [_, C],
    +shadda:character@C.


/** qAla**/
/*
setDiacritics(X, c2(ACTVPRES)) :-
    !,    
    actvPres@X <-> [ACTVPRES],
    actvPast@X <-> ["A"],
    psvPres@X <-> ["A"],
    psvPast@X <-> ["y"],
    targetConsonants@X <-> [[_], [_]].
*/

setDiacritics(X, c2(ACTVPRES1)) :-
    when(nonvar(ACTVPRES1),
	 cmember(ACTVPRES1, ["A","y", "w"])),
    !,    
    actvPres@X <-> [ACTVPRES1],
    actvPast@X <-> ["A"],
    psvPres@X <-> ["A"],
    psvPast@X <-> ["y"].

setDiacritics(X, c3) :-
    !,    
    actvPres@X <-> ["o", "w"],
    actvPast@X <-> ["a", "a"],
    psvPres@X <-> ["o", "a"],
    psvPast@X <-> ["u", "i"].

setDiacritics(X, iv) :-
    !,
    actvPres@X <-> ["o", "u"],    
    actvPast@X <-> ["A", "a"],
    psvPres@X <-> ["o", "u"],
    psvPast@X <-> ["u", "i"].

setDiacritics(X, v) :-
    !,    
    actvPres@X <-> ["u", "u"],
    actvPast@X <-> ["a", "o"],
    psvPast@X <-> ["u", "i"],
    psvPres@X <-> ["o", "u"].

setDiacritics(X, hollow) :-
    !,    
    setDiacritics(X, hollow("w", "A", "A", "y")).

setDiacritics(X, hollow(APRES, APAST, PPRES, PPAST)) :-
    !,    
    actvPres@X <-> [APRES],
    actvPast@X <-> [APAST],
    psvPast@X <-> [PPAST],
    psvPres@X <-> [PPRES].

setDiacritics(X, weak(APRES, APAST, PPRES, PPAST)) :-
    !,    
    actvPres@X <-> APRES,
    actvPast@X <-> APAST,
    psvPast@X <-> PPAST,
    psvPres@X <-> PPRES.

setDiacritics(X, irreg(APRES, APAST, PPRES, PPAST)) :-
    !,    
    actvPres@X <-> APRES,
    actvPast@X <-> APAST,
    psvPast@X <-> PPAST,
    psvPres@X <-> PPRES.

setDiacritics(X, [APRES, APAST, PPRES, PPAST]) :-
    !,    
    actvPres@X <-> APRES,
    actvPast@X <-> APAST,
    psvPast@X <-> PPAST,
    psvPres@X <-> PPRES.

setDiacritics(X, methal(APRES, APAST, PPRES, PPAST)) :-
    !,    
    actvPres@X <-> APRES,
    actvPast@X <-> APAST,
    psvPast@X <-> PPAST,
    psvPres@X <-> PPRES.

%%%% The next set are for verbs with explicit derivational affixes.
%%% These all have the same passives, so we don't need to specify the
%%% passives individually
setDiacritics(X, iq) :-
    !,
    actvPast@X <-> ["a", "o", "a"], 
    actvPres@X <-> ["a", "o", "i"],
    psvPast@X <-> ["u", "o", "i"],
    psvPres@X <-> ["a", "o", "a"].

setDiacritics(_X, _ANY).

arabicBroken(X) :-
    affixes@X <-> [AGR],
    [syntax, lextype, diacritics, consonants]@X 
         <-> [syntax, lextype, diacritics, consonants]@AGR,
    affix@AGR <-> *det.

arabicSolid(X) :-
    affixes@X <-> [AGR],
    [syntax, lextype, diacritics, consonants]@X 
         <-> [syntax, lextype, diacritics, consonants]@AGR,
    affix@AGR <-> *agr.

safe_atom_chars(A, C) :-
    (nonvar(C) ->
	fillInQueries(C);
	true),
    atom_chars(A, C).

underlyingForm(X, UNDERLYINGN) :-
    !,
    fillInUnderlyingForm(underlying@X, UNDERLYING0),
    reverse(UNDERLYING0, UNDERLYING1),
    ground(UNDERLYING1),
    atom_codes(UNDERLYINGN, UNDERLYING1).

underlyingForm(X, UNDERLYINGN) :-
    text@X <-> UNDERLYING0,
    text2underlying(UNDERLYING0, UNDERLYING1, actualVowels@X),
    removeMultipleBreaks(UNDERLYING1, UNDERLYING2),
    safe_atom_chars(UNDERLYINGN, UNDERLYING2).

flattenCharList([], []).
flattenCharList([H0 | T0], Tn) :-
    U <-> underlying:character@H0,
    (U = "?" -> true; true),
    H1 <-> char:character@H0,
    (atom_codes(H1, U) -> true; true),
    flattenCharList(T0, T1),
    append(U, T1, T2),
    (-shadda:character@H0 ->
	Tn = T2;
	Tn = [0'~ | T2]).

fillInQueries([]).
fillInQueries([H | T]) :-
    ([H] = "?" -> true; true),
    fillInQueries(T).

text2underlying(?, "?", _D) :-
    !.
text2underlying(o(_), "", _D) :-
    !.
text2underlying({A, B}, TN, D) :-
    (nonvar(A); nonvar(B)),
    !,
    text2underlying(A, TA, D),
    text2underlying(B, TB, D),
    append(TA, [0'+ | TB], TN).
text2underlying({X}, T, D) :-
    !,
    text2underlying(X, T, D).
text2underlying(C, T, D) :-
    C <> character,
    !,
    text2underlying(underlying:character@C, T, D).
text2underlying(T, S1, D) :-
    atom(T),
    !,
    safe_atom_chars(T, S0),
    fillInDiacritics(S0, S1, D).
text2underlying(_, "???", _D).

fillInDiacritics([], [], _D).
fillInDiacritics([0'? | T0], TN, D0) :-
    underlying:character@C0 <-> C1,
    !,
    (D0 = [C0 | D1] ->
	((C1 = "?" -> true; true), append(C1, T1, TN));
	TN = [0'? | T1]),
    fillInDiacritics(T0, T1, D1).
fillInDiacritics([H0 | T0], [H0 | T1], D) :-
    fillInDiacritics(T0, T1, D).

removeMultipleBreaks([], []).
removeMultipleBreaks([0'+, 0'+ | T], L) :-
    !,
    removeMultipleBreaks([0'+ | T], L).
removeMultipleBreaks([H | T0], [H | T1]) :-
    removeMultipleBreaks(T0, T1).

latexForm(X, LFORM) :-
    underlyingForm(X, SFORM),
    bw2ADD(SFORM, LFORM).

fillInUnderlyingForm(L0, L1) :-
    fillInUnderlyingForm(L0, L1, fail).

fillInUnderlyingForm([], [], _) :- !.
fillInUnderlyingForm([H0 | T0], TN, NOTLAST) :-
    underlying:character@H0 <-> H1,
    shadda:character@H0 <-> S,
    (H1 = "?" ->
	H2 = H1;
	(nonvar(H1),
	    (S == + ->
		append(H1, H1, H2);
		reverse(H1, H2)))),
    fillInUnderlyingForm(T0, T1, true),
    ((final:character@H0 == +, NOTLAST) ->
	append("+", H2, HN);
	HN = H2),
    append(HN, T1, TN).

underlyingForms(SFORMS) :-
    setof(SFORM, underlyingForm(SFORM), SFORMS).

underlyingForm(SFORM) :-
    X <> word,
    -value:def@zero@X,
    retrieve(_I, X), 
    underlyingForm(X, SFORM).

showFailures([]) :-
    !.
showFailures([F | FAILURES]) :-
    functor(F, X, _),
    format('Something wrong: ~w~n', X),
    showFailures(FAILURES).

underlyingForms :-
    X <> word,
    -value:def@zero@X,
    retrieve(I, X),
    underlyingForm(X, SIMPLE),
    translation@X <-> TRANSLATION,
    default(TRANSLATION = unknown),
    (length(args@X, A) -> true; true),
    active@X <-> ACTIVE,
    (nonvar(ACTIVE) ->
	format('~w -> ~w (~w, ~w: no of args=~w, ~wactive)~n',
	       [I, text@X, SIMPLE, TRANSLATION, A, ACTIVE]);
	format('~w -> ~w (~w, ~w: no of args=~w)~n',
	       [I, text@X, SIMPLE, TRANSLATION, A])),
    (flag(showAgrAndTense) ->
	(pretty(agree@X), pretty(tense@X), pretty(irreal@X), nl);
	true),
    showFailures(failures@X),
    fail.
underlyingForms.  

tableForms(W) :-
    atom(W),
    timeTaken(T0),
    T1 is T0/1000,
    !,
    format('\\AR{~w} & \\uforms{', [W]),
    underlyingForms(SFORMS),
    tableForms(SFORMS),
    format('& ~3d \\\\ ~n\\hline~n', [T1]),
    fail.

tableForms([SFORM | SFORMS]) :-
    (SFORMS = [] ->
        format('~w} ', [SFORM]);
        (format('~w\\vsq ', [SFORM]), 
         tableForms(SFORMS))).
        
chooseBranch([], _BRANCH, []).
chooseBranch([0'{ | T0], BRANCH, L) :-
    nonvar(BRANCH),
    !,
    toClosingBracket(T0, T1),
    chooseBranch(T1, BRANCH, T2),
    append(BRANCH, T2, L).
chooseBranch([H | T0], BRANCH, [H | T1]) :-
    chooseBranch(T0, BRANCH, T1).
    
toClosingBracket([0'} | L], L) :-
    !.
toClosingBracket([_H | T0], T1) :-
    toClosingBracket(T0, T1).
  
checkSuffixIsNeeded(X) :-
    items:fst@fstIn@X <-> [Y | _],
    affixes@Y <-> [D | _],
    [syntax, affix]@D <-> [syntax, affix]@X.

checkPrefix(X) :-
    C <> character,
    postfix:fst@fstIn@X <-> [C | _].

%% setArabicCaseMarker(_, _, _) :- !.
setArabicCaseMarker(C0, C1, DUMMY) :-
    (DUMMY <> singular ->
	(DUMMY <> indefinite1 ->
	    ((reallyNom(DUMMY), C0 = uN); 
		(acc(DUMMY), C0 = aN); 
		(gen(DUMMY), C0 = iN));
	    ((acc(DUMMY), C0 = a);
		(reallyNom(DUMMY), C0 = u);
		(gen(DUMMY), C0 = i)));
	DUMMY <> dual ->
	C0 = i;
	DUMMY <> plural ->
	    (DUMMY <> masculine -> 
		C0 = a;
		(DUMMY <> indefinite1 ->
		    ((reallyNom(DUMMY), C0 = 'N');
			((acc(DUMMY); gen(DUMMY)), C0 = 'F'));
		    ((reallyNom(DUMMY), C0 = u);
			((acc(DUMMY); gen(DUMMY)), C0 = a))))),
    trigger(C0, character(C0, C1)).

whichT(WHICHT, morphology@X) :-
    actvPast@X <-> PAST,
    actvPres@X <-> PRESENT,
    psvPast@X <-> PSVPAST,
    psvPres@X <-> PSVPRES,
    targetConsonants@X <-> CONSONANTS,
    +shadda:character@C,
    ((WHICHT == t,
      PAST = ["A", "a"],
      PRESENT = ["A", "a"],
      PSVPRES = ["A", "a"],
      PSVPAST = ["w", "i"]);            
	(WHICHT == t1, 
	    PAST = ["a", "a"], 
	    PRESENT = ["a", "a"]);
	(WHICHT = t2,
	    CONSONANTS = [[_], [C], [_]],
	    PRESENT= ["a", "o"],
	    PAST = ["a", "o"], 
	    PSVPRES = ["a", "a"],
	    PSVPAST = ["u", "i"])).

/**** CONSTRAINTS ON TENSE MARKERS *****************************************/

notBlocked(X0) :-
    [person, remarks]@X0
    <-> [person, remarks]@X1,
    person@X0 <-> P,
    softParse(ground(P),
	      \+ blockedAgreement(P),
	      blocked(index@X0),
	      X1).

constraintsOnYA(_AGRTEXT, X) :-
    X <> [pres_or_future, third].

constraintsOnNA(_AGRTEXT, X) :-
    X <> [pres_or_future, first, not_singular].

constraintsOnTA(_AGRTEXT, X) :-
    X <> [pres_or_future, not_first].

constraintsOnO(_AGRTEXT, X) :-
    X <> [pres_or_future, first_sing_only].

constraintsOnEmptyTense(AGRTEXT, subject@X, agree@X) :-
    (AGRTEXT == '' ->
	X <> [third_sing_only, masculine];
	AGRTEXT == t ->
	trigger(index@subject@X,
		(X <> first_sing_only; X <> second_sing_only; X <> [third_sing_only, feminine]));
	(fail, AGRTEXT == n) ->
	X <> [not_first, not_plural];
	true).

/**** CONSTRAINTS ON AGREEMENT MARKERS ***********************/

constraintsOnEmptyAgr(_N, Y, _TEXT, _UNDERLYING, [DET], _IRREAL, colour) :-
    affix@DET <-> *det,
    syntax@Y <-> syntax@DET,
    Y <> adj1.
constraintsOnEmptyAgr(N, Y, TEXT0, TEXT1, AFFIXES, _IRREAL, LEXTYPE) :-
    affix@DET <-> *det,
    syntax@Y <-> syntax@DET,
    Y <> suffix,
    ((N = +) ->
	doSoftParse((singular(Y),
		     (LEXTYPE = pronoun ->
			 (AFFIXES = [], masculine(Y));
			 (LEXTYPE = noun(pname) ->
			     AFFIXES = [];
			     (LEXTYPE = noun(_),
				 AFFIXES = [DET],
				 LEXTYPE = lextype@DET,
				 third_sing_only(Y))))),
		    agr(index@X, text@X),
		    X);
	(verb(Y),
	 AFFIXES = [],
	 (past(Y) ->
	  TEXT0 = a;
	  trigger(irreal@Y,
		   ((indicative(Y) ->
		    TEXT0 = u;
		    subjunctive(Y) ->
		    TEXT0 = a;
		    jussive(Y) ->
		    TEXT0 = o),
		  trigger(TEXT0, (character(TEXT0, TEXT1)))))))),
    affix@current:fst@fstOut@Y <-> *(_).

constraintsOnEmptyGender(X0, ANYNOUN) :-
    agree@X0 <-> agree@X1,
    affixes@X0 <-> [NUM],
    [lextype, syntax]@X0 <-> [lextype, syntax]@NUM,
    affix@NUM <-> *NEXT,
    trigger(ANYNOUN,
	    ((ANYNOUN = broken(BROKEN),
	      doSoftParse(BROKEN = '', lextype(index@X0, broken(''), ANYNOUN), X0), 
	      feminine(X1), third_plural_only(X1), NEXT=det);
		(ANYNOUN = solid, NEXT = det);
		(doSoftParse(ANYNOUN = regular(_), lextype(index@X0, regular(''), ANYNOUN), X0),
		    NEXT = agr,
		    softParse(ANYNOUN, (masculine(X1) -> true; third_plural_only(X1)), gender(index@X0, text@X0, masculine), X0)))).


constraintsOnNA(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    softParse(lextype@X0,
	      (first(X1), not_singular(X1)),
	      agr(index@X0, text@X0),
	      X1),
    notBlocked(X0),
    softParse(lextype@X0,
	      past(X1),
	      tense(index@X0, text@X0),
	      X1).

actualConstraintsOnT(X) :-
    X <> [masculine, second_sing_only],
    characters("ta", underlying@X).
actualConstraintsOnT(X) :-
    X <> [feminine, second_sing_only],
    characters("ti", underlying@X).
actualConstraintsOnT(X) :-
    X <> first_sing_only,
    characters("tu", underlying@X).

constraintsOnT(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    doSoftParse(actualConstraintsOnT(X1), subjAgree(index@X0, index@subject@X0), X1).

constraintsOnTA1(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    softParse(lextype@X0, third_dual_only(X1),  agr(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, feminine(X1), gender(index@X0, text@X0, G), X1).

constraintsOnTN(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    softParse(lextype@X0, second_plural(X1), agr(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, feminine(X1), gender(index@X0, text@X0, G), X1),
    softParse(lextype@X0, past(X1), tense(index@X0, text@X0), X1).

constraintsOnTMA(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    softParse(lextype@X0, second_dual_only(X1), agr(index@X0, text@X0), X1),
    softParse(lextype@X0, past(X1), tense(index@X0, text@X0), X1).

constraintsOnTM(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    softParse(lextype@X0, second_plural_only(X1), agr(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, masculine(X1), gender(index@X0, text@X0, G), X1),
    softParse(lextype@X0, past(X1), tense(index@X0, text@X0), X1).

constraintsOnUWA(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    softParse(lextype@X0, third_plural_only(X1), agr(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, masculine(X1), gender(index@X0, text@X0, G), X1),
    softParse(lextype@X0, past(X1), tense(index@X0, text@X0), X1).

constraintsOnAA(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    notBlocked(X0),
    shortGender(X1, G),
    trigger(lextype@X0,
	    (pres_or_future(X1) ->
		doSoftParse((dual(X1), not_first(X1)), agr(index@X0, text@X0), X1);
		(doSoftParse(masculine(X1), gender(index@X0, text@X0, G), X1),
		    doSoftParse(third_dual_only(X1), agr(index@X0, text@X0), X1)))).

constraintsOnAAN(X0) :-
    X0 <> [verb, indicative],
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    affixes@X0 <-> [],
    notBlocked(X0),
    softParse(lextype@X0, pres_or_future(X1), tense(index@X0, text@X0), X1),
    softParse(index@subject@X0, (third_dual_only(X1); (second_dual_only(X1))), agr(index@X0, text@X0), X1).

constraintsOnAAN(X0) :-
    +n:xbar@cat@X0,
    [agree, index, remarks]@X0
    <-> [agree, index, remarks]@X1,
    affixes@X0 <-> [DET],
    affix@DET <-> *det,
    [syntax, lextype]@X0 <-> [syntax, lextype]@DET,
    notBlocked(X0),
    softParse(lextype@X0, third_dual_only(X1), agr(index@X0, text@X0), X1).

constraintsOnN(X0) :-
    X0 <> [verb, suffix, checkSuffixIsNeeded, immediate],
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    affixes@X0 <-> [],
    notBlocked(X0),
    shortGender(X1, G),
    softParse(lextype@X0, feminine(X0), gender(index@X0, text@X0, G), X1),
    softParse(lextype@X0, ((past(X0) -> plural(X0); not_singular(X0)), not_first(X0)), agr(index@X0, text@X0), X1).

constraintsOnWA(X0) :-
    X0 <> [verb, suffix, checkSuffixIsNeeded, immediate],
	[agree, vfeatures, index, remarks]@X0
	<-> [agree, vfeatures, index, remarks]@X1,
    affixes@X0 <-> [],
    notBlocked(X0),
    softParse(lextype@X0, pres_or_future(X1), tense(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, masculine(X), gender(index@X0, text@X0, G), X1),
    softParse(lextype@X0, (plural(X), not_first(X)), agr(index@X0, text@X0), X1).

constraintsOnAT(X0) :-
    notBlocked(X0),
    shortGender(X1, G),
    lextype@X0 <-> LEXTYPE,
    softParse(LEXTYPE,
	      (LEXTYPE = noun(regular('N/Ap')); feminine(X0)),
	      gender(index@X0, text@X0, G),
	      X0),
    softParse(LEXTYPE, third_plural_only(X0), agr(index@X0, text@X0), X0),
    localConstraintsOnAT(X0).

localConstraintsOnAT(X0) :-
    +n:xbar@cat@X0,
    [syntax, lextype]@X0 <-> [syntax, lextype]@DET,
    lextype@X0 <-> noun(LEXTYPE),
    softParse(LEXTYPE, LEXTYPE=regular(_), lextype(index@X0, LEXTYPE, regular(_)), X0),
    affix@X0 <-> *gender,
    affixes@X0 <-> [DET],
    affix@DET <-> *det.

/**
%% I used to have 'At' as a verbal ending: not sure where that came from, because it isn't there
%% in weakverbs.pl (which is my reference)

localConstraintsOnAT(X0) :-
    X0 <> verbal,
    affix@X0 <-> *agr,
    affixes@X <-> [].
**/
    
constraintsOnYN(X0) :-
    +n:xbar@cat@X0,
    [agree, index, remarks]@X0
    <-> [agree, index, remarks]@X1,
    lextype@X0 <-> noun(LEXTYPE),
    affix@X0 <-> *agr,
    affixes@X0 <-> [DET],
    affix@DET <-> *det,
    [syntax, lextype]@X0 <-> [syntax, lextype]@DET,
    notBlocked(X0),
    softParse(LEXTYPE, LEXTYPE=regular(''), lextype(index@X0, LEXTYPE, regular('')), X1),
    shortGender(X1, G),
    softParse(lextype@X0, masculine(X1), gender(index@X0, text@X0, G), X1),
    %% Doing this earlier breaks it on indefinite nouns (Allan, 19/01/11)
    softParse(LEXTYPE, third_plural_only(X1), agr(index@X0, text@X), X1),
    X0 <> [suffix, checkSuffixIsNeeded, immediate, acc].

constraintsOnYN(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    affixes@X0 <-> [],
    X0 <> [verb, suffix, checkSuffixIsNeeded, immediate, indicative],
    notBlocked(X0),
    softParse(lextype@X0, pres_or_future(X1), tense(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, feminine(X1), gender(index@X0, text@X0, G), X1),
    softParse(lextype@X0, second_sing_only(X1), agr(index@X0, text@X0), X1).

constraintsOnWN(X0) :-
    +n:xbar@cat@X0,
    [agree, index, remarks]@X0
    <-> [agree, index, remarks]@X1,
    lextype@X0 <-> noun(LEXTYPE),
    affix@X0 <-> *agr,
    affixes@X0 <-> [DET],
    affix@DET <-> *det,
    [syntax, lextype]@X0 <-> [syntax, lextype]@DET,
    notBlocked(X0),
    softParse(LEXTYPE, LEXTYPE=regular(''), lextype(index@X0, LEXTYPE, regular('')), X1),
    shortGender(X1, G),
    softParse(lextype@X0, masculine(X1), gender(index@X0, text@X0, G), X1),
    %% Doing this earlier breaks it on indefinite nouns (Allan, 19/01/11)
    softParse(LEXTYPE, third_plural_only(X1), agr(index@X0, text@X0), X1),
    X0 <> [suffix, checkSuffixIsNeeded, immediate, nom].

constraintsOnWN(X0) :-
    [agree, vfeatures, index, remarks]@X0
    <-> [agree, vfeatures, index, remarks]@X1,
    affix@X0 <-> *agr,
    affixes@X0 <-> [],
    X0 <> [verb, suffix, checkSuffixIsNeeded, immediate, indicative],
    notBlocked(X0),
    softParse(lextype@X0, pres_or_future(X1), tense(index@X0, text@X0), X1),
    softParse(lextype@X0, (not_first(X0), plural(X1)), agr(index@X0, text@X0), X1),
    shortGender(X1, G),
    softParse(lextype@X0, masculine(X1), gender(index@X0, text@X0, G), X1).

bwNXap(X) :-
    trigger(ground(gender@X),
	    ((third_sing_only(X), feminine(X));
		(third_plural_only(X), masculine(X)))).

softDiacritic(X, C1, C2) :-
    softParse(C1, character(C2, C1), 31, wrongDiacritics(index@X, C1, C2), X).

checkConsonants(X, X).

checkDiacritics(X) :-
    active@X <-> ACTIVE,
    trigger(ACTIVE, checkActual(X)).

checkActual(X) :-
    active@X <-> ACTIVE,
    actualVowels@X <-> ACTUAL,
    actvPres@X <-> ACTVPRES,
    psvPres@X <-> PSVPRES,
    actvPast@X <-> ACTVPAST,
    psvPast@X <-> PSVPAST,
    (present(X) ->
	(ACTIVE = + ->
	    checkActual(ACTUAL, ACTVPRES);
	    checkActual(ACTUAL, PSVPRES));
	(ACTIVE = + ->
	    checkActual(ACTUAL, ACTVPAST);
	    checkActual(ACTUAL, PSVPAST))).

checkActual(X, Y) :-
    mapTarget(Y, X).

%%% checkActual(X, Y) :-
%%%     checkActual1(X, Y).

mapTarget([], []).
mapTarget([H0 | T0], [H1 | T1]) :-
    ((H0 = [U], var(U)) ->
	H0 = underlying:character@H1;
	(atom_codes(A0, H0),
	    character(A0, H1))),
    mapTarget(T0, T1).

checkActual1([], []) :-
    !.
checkActual1([H0 | T0], [[H1] | T1]) :-
    !,
    (nonvar(H1) ->
	(atom_codes(H2, [H1]),
	    character(H2, H0));
	[H1] = underlying:character@H0),
    checkActual1(T0, T1).
checkActual1([H0 | T0], [H1 | T1]) :-
    underlying:character@H0 = U,
    nonvar(U),
    U = H1,
    checkActual1(T0, T1).

checkPresentPrefix(V0, X, TXT0, TXT1) :-
    active@X <-> ACTIVE,
    actualVowels@X <-> [ACTUAL],
    (ACTIVE = + ->
	character(V0, ACTUAL);
	character(u, ACTUAL)),
    actualVowels@X <-> [A],
    atom_concat(TXT0, char:character@A, TXT1).

checkDiacritics(X, X, _S) :-
    !.
checkDiacritics([H | T0], [H | T1], SUFFIX) :-
    !,
    checkDiacritics(T0, T1, SUFFIX).
checkDiacritics([[H0] | T0], [[L, 0'?, S] | T1], SUFFIX) :-
    when(ground(SUFFIX),
	 checkWeakDiacritics(SUFFIX, H0, L, S)),
    checkDiacritics(T0, T1, SUFFIX).

checkWeakDiacritics(SUFFIX, H0, L, S) :-
    (cmember(SUFFIX, "auiAwy") ->
	(H0 = L);
	(H0 = S)).

genderMarker(X) :-
    X <> [suffix, checkSuffixIsNeeded, immediate],
    +n:xbar@cat@X,
    affix@X <-> *gender,
    affixes@X <-> [NUM],
    affix@current:fst@fstOut@X <-> *(_),
    affix@NUM <-> *agr,
    [lextype, syntax]@X <-> [lextype, syntax]@NUM.

applyTests([], _X).
applyTests([T | TT], X) :-
    T =.. [F | A],
    G =.. [F, X | A],
    G,
    applyTests(TT, X).

applyTests(TESTS, X, SORT) :-
    applyTests(TESTS, X),
    setSort(X, SORT).

/*
    Persian
*/

persianFullPresent(X) :-
    X <> [verb, present, tensed_form],
    affixes@X <-> [AGR],
    affix@AGR <-> *1,
    [syntax, lextype]@AGR <-> [syntax, lextype]@X.

persianPresent(X) :-
    X <> [verb, present, tensed_form],
    affixes@X <-> [AGR],
    affix@AGR <-> *agr,
    [syntax, lextype]@AGR <-> [syntax, lextype]@X.

persianPast(X) :-
    X <> [verb, past],
    affixes@X <-> [AGR],
    % dir@AGR <> xafter,
    affix@AGR <-> *aspect,
    [syntax, lextype]@AGR <-> [syntax, lextype]@X.

/************************ SOMALI **************************/

decl0(declension( -, -, -, -, -, -, -)).

decl1(declension( +, -, -, -, -, -, -)).

decl2(declension( -, +, -, -, -, -, -)).

decl3(declension( -, -, +, -, -, -, -)).

decl4(declension( -, -, -, +, -, -, -)).

decl5(declension( -, -, -, -, +, -, -)).

decl6(declension( -, -, -, -, -, +, -)).

decl7(declension( -, -, -, -, -, -, +)).

decl2_3_4_5_7(declension( -, _, _, _, _, -, _)).

decl1_6(declension( _, -, -, -, -, _, -)).

decl1_3(declension(_, -, _, -, -, -, -)).

somaliBigPron(X) :-
    specifier@X <-> pro_ref,
    X <> saturated,
    -clitic@X.

