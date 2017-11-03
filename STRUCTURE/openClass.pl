/*
     OPENCLASS.PL
*/


openClass(WORD, SIGN, START, language@SIGN, END) :-
    current_language(language@SIGN),
    underlying@SIGN <-> WORD,
    +realised@SIGN,
    start@SIGN <-> START,
    [xstart, xend]@SIGN <-> [start, end]@SIGN,
    end@SIGN is END,
    span@SIGN is 1 << START,
    +compact@SIGN,
    SIGN <> word,
    underlying@X <-> underlying@SIGN,
    (unknownNoun(X); unknownVerb(X)),
    language@X <-> language@SIGN,
    fix_args(X, SIGN),
    [start, end, index, morphology, text, underlying, cat]@core@SIGN
      	<-> [start, end, index, morphology, text, underlying, cat]@SIGN,
     (-conj@SIGN -> true; true),
     -date@SIGN,
     setNoWH(SIGN).

unknownNoun(X) :-
    % checkConstraint(unknownWords, X),
    atom_codes(underlying@X, UNDERLYING),
    ROOT <-> text@X,
    ITAG <-> itag:tag@tag@X,
    (makeRoot(ROOT, "s", UNDERLYING) ->
	(X <> third_plural_only, tag(ITAG, pluralNoun));
	(makeRoot(ROOT, "", UNDERLYING),
	    tag(ITAG, singNoun),
	    X <> [third_sing_only, gnoun])),
    X <> [noun, saturated, adjunct],
    ptag:tag@tag@X <> tag(noun),
    target@X <> noun.

unknownVerb(X) :-
    X <> verb,
    ptag:tag@tag@X <> tag(verb),
    % checkConstraint(unknownWords, X),
    atom_codes(underlying@X, UNDERLYING),
    ITAG <-> itag:tag@tag@X,
    text@X <-> TEXT,
    (makeRoot(TEXT, "ing", UNDERLYING) ->
	(tag(ITAG, presPart), pres_part(X));
     makeRoot(TEXT, "s", UNDERLYING) ->
	(X <> [not_participle_form, present],
	 +active@X);
     makeRoot(TEXT, "ed", UNDERLYING) ->
	ed2(X);
	(makeRoot(TEXT, "", UNDERLYING), rootForm(X))),
    vtype(X, unknown).

makeRoot({ROOTn, AFFIX1}, AFFIX0, UNDERLYING) :-
    append(ROOT0, AFFIX0, UNDERLYING),
    atom_codes(AFFIX1, AFFIX0),
    appendAll(["?", ROOT0, "?"], ROOT1),
    atom_codes(ROOTn, ROOT1).