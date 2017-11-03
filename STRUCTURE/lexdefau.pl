/*
	LEXDEFAULTS.PL

	Things you can work out about a word {\em before} you put it in the
	dictionary, e.g. that Spanish nouns ending in "-o" are generally
	masculine.
	
	Merged with ~/VERSIONS/HANADY, 23/4/01
	
	Substantially rearranged to separate the treatment of different 
	languages: not all languages are known to be secure (31/07/01)

*/

prefix([], L, L).
prefix([H|T0], [H|T1], L) :-
    prefix(T0, T1, L).

tail(L0, L1) :-
    reverse(L0, R0),
    prefix(R0, L1, _).

needsFirstAffix(X) :-
    affixes@X <-> [A1],
    affix@A1 <-> *1,
    [lextype, branch, syntax, uses, sort]@X 
        <-> [lextype, branch, syntax, uses, sort]@A1.

somaliFirstAffix(X) :-
    affixes@X <-> [A1],
    affix@A1 <-> *1,
    [branch, syntax, uses]@X 
        <-> [branch, syntax, uses]@A1.

lexdefaults(X, ROOT) :-
    lexdefaults(X, ROOT, language@X),
    default(-affix@X),
    true. %%default(set_root(X, ROOT)).
    
lexdefaults(X, _ROOT, LANGUAGE) :-
    affixes@X <-> AFFIXES,
    LANGUAGE <> somali,
    -affix@X,
    !,
    (var(AFFIXES) ->
    	needsFirstAffix(X);
    	true).
    
lexdefaults(X, _ROOT, LANGUAGE) :-
    lextype@X <-> LEXTYPE,
    affixes@X <-> AFFIXES,
    LANGUAGE <> persian,
    -affix@X,
    !,
    (\+ \+ X <> noun -> 	% double negation tests but does not change
        default(LEXTYPE <-> ha);
        true),
    (var(AFFIXES) ->
    	needsFirstAffix(X);
    	true).

lexdefaults(X, _ROOT, LANGUAGE) :-
    lextype@X <-> LEXTYPE,
    LANGUAGE <> arabic, 
    -affix@X,
    !,
    (LEXTYPE = regular(SUBCLASS, PRESFORM, PREFCLASS) ->
	(affix@A <-> *deriv(_),
	    affixes@X <-> [A],
	    [branch]@X <-> [branch]@A,
	    trigger(SUBCLASS, arabicRegular(X, SUBCLASS, PRESFORM, PREFCLASS)));
	LEXTYPE = bwVerb(PREFIX, DIACRITICS, PREFCLASS) ->
	bwVerb(X, PREFIX, DIACRITICS, PREFCLASS);
	default(inflected1(X))).   

lexdefaults(X, _ROOT, LANGUAGE) :-
    lextype@X <-> LEXTYPE,
    affixes@X <-> AFFIXES,
    LANGUAGE <> english, 
    -affix@X,
    X <> x,
    var(AFFIXES),
    !,
    (\+ \+ X <> noun -> 
        default(LEXTYPE <-> frame(noun(singular(""), plural("s")), 
                                  _ADJ, 
                                  _VERB, 
                                  _BRANCHES));
     \+ \+ X <> verb ->
        default(LEXTYPE <-> frame(_NOUN, _ADJ, [ing, s, ed2], ["", ""]));
     \+ \+ X <> adj ->
        default(LEXTYPE <-> frame(_NOUN, ["", ly], _VERB, _BRANCHES));
	true),
    (var(AFFIXES) ->
	    X <> needsFirstAffix;
	    true).
	    
lexdefaults(X, ROOT, LANGUAGE) :-
    lextype@X <-> LEXTYPE,
    affixes@X <-> AFFIXES,
    LANGUAGE <> spanish, 
    -affix@X,
    known(X),
    dir@A1 <> xafter,
    affix@A1 <-> *number,
    [lextype, branch, syntax]@X
        <-> [lextype, branch, syntax]@A1,
    reverse(ROOT, REVROOT),
    !,
    (\+ \+ (X <> noun; X <> adj; X <> pronoun) ->
	    default(LEXTYPE <-> ["", s]);
	    true),
	
    (REVROOT = [V | _] ->
	(\+ \+ X <> noun ->
	    ([V] = "a" ->
	    	default(feminine(X));
             tail("io/n", REVROOT) ->
	    	default(feminine(X));
	        default(masculine(X)));
	    true);
	true),

    (var(AFFIXES) ->
	     (\+ \+ X <> noun ->
	    	(AFFIXES <-> [A1],
		 affix@A1 <-> *number);
	     \+ \+ (X <> adj; X <> det) ->
	    	(AFFIXES <-> [A0, A1],
		 dir@A0 <> xafter,
		 affix@A0 <-> *gender,
    	     	 [lextype, branch, syntax]@X 
			<-> [lextype, branch, syntax]@A0,
		 affix@A1 <-> *number);
	     \+ \+ X <> verb ->
		(AFFIXES = [A1],
		 affix@A1 <-> *theme);
		true);
	true).

lexdefaults(X, ROOT, LANGUAGE) :-
    lextype@X <-> LEXTYPE,
    affixes@X <-> AFFIXES,
    LANGUAGE <> french, 
    -affix@X, 
    known(X), 
    reverse(ROOT, REVROOT),
    dir@A1 <> xafter,
    affix@A1 <-> *number,
    [lextype, branch, syntax]@X
        <-> [lextype, branch, syntax]@A1,
    !,
    (\+ \+ (X <> noun; X <> adj; X <> pronoun) ->
	    default(LEXTYPE <-> ["", s]);
	    true),
    (REVROOT = [V | _] ->
	(\+ \+ X <> noun ->
	    ([V] = "e" ->
	    	default(feminine(X));
	        default(masculine(X)));
	    true);
	true),
    (var(AFFIXES) ->
	     (\+ \+ X <> noun ->
	    	(AFFIXES <-> [A1],
		 affix@A1 <-> *number);
	     \+ \+ (X <> adj; X <> det) ->
	    	(AFFIXES <-> [A0, A1],
		 dir@A0 <> xafter,
		 affix@A0 <-> *gender,
    	     	 [lextype, branch, syntax]@X 
			<-> [lextype, branch, syntax]@A0,
		 affix@A1 <-> *number);
	     \+ \+ X <> verb ->
		(AFFIXES = [A1],
		 affix@A1 <-> *theme);
		true);
	true).

lexdefaults(X, _ROOT, LANGUAGE) :-
    lextype@X <-> LEXTYPE,
    LANGUAGE <> german, 
    !,
    ((known(X), noun(X), -affix@X, 
      LEXTYPE = frame(GROUP,[_BR, LEX, PL, _UML], _GE),
      nonvar(LEX)) ->
	(GROUP = strong(_) ->
	    (((LEX <-> ""; LEX <-> "el"; LEX <-> "tum") 
	     -> default(masculine(X));
	      (LEX <-> "i"; LEX <-> "icht") 
	      -> default(neuter(X));
	      true),
		(LEX = "" ->
		    (neuter(X) -> default(PL <-> "er"); default(PL <-> "e"));
		    true));
	    default(masculine(X)));
	true).
 
lexdefaults(X, _ROOT, LANGUAGE) :-
    affixes@X <-> AFFIXES,
    LANGUAGE <> greek,	
    dir@A1 <> xafter,	
    [lextype, branch, syntax]@X 
        <-> [lextype, branch, syntax]@A1,
    -affix@X, 
    !,
    ((known(X), var(AFFIXES)) ->	
	    (\+ \+ X <> noun ->	
	    	(AFFIXES <-> [A1],	
		 affix@A1 <-> *number);
	     \+ \+ X <> adj ->
	    	(AFFIXES <-> [A1],
		 affix@A1 <-> *adjmarker);
	     \+ \+ X <> det ->	
	    	(AFFIXES <-> [A1],	
		 affix@A1 <-> *number);	    
	     \+ \+ X <> pronoun ->	
	    	(AFFIXES <-> [A1],	
		 affix@A1 <-> *number);
		true);
	true).
  
lexdefaults(X, _ROOT, _LANGUAGE) :-
    default(inflected(X)).

set_root(X, ROOT) :-
    var(text@X),
    splice(ROOT, _FIRST, SPLICED),
    atom_codes(text@X, SPLICED).

splice([], _SPLICE, []).
/*
%%%% This one removed following decision to use * for diacritics: 13/11/07
splice([0'* | REST], SPLICE, SPLICED) :-
    !,
    append(SPLICE, REST, SPLICED).
*/
splice([H | T], SPLICE, [H | SPLICED]) :-
    splice(T, SPLICE, SPLICED).

nominalPrefix(X) :-
    X <> [noun, prefix],
    lextype@X <-> regular(nominal, AFFIXES, _),
    affix@X <-> *deriv(1),
    actualVowels@X <-> D,
    affixes@X <-> [NUM],
    affix@NUM <-> *agr,
    dir@NUM <> xafter,
    lextype@NUM <-> noun(NTYPE),
    trigger(AFFIXES,
	    (cmember(_:L, AFFIXES),
		cmember(D:_C2:NTYPE:SORT:TESTS, L),
		applyTests(TESTS, X),
		sort@X --- SORT)).
