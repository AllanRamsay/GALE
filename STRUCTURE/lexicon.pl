
%%%% LEXICON.PL %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/********************************************************************
Various list processing procedures for taking words apart into sequences
of vowels or consonants or putting them back together. Most of the actual
work is by language specific rules in  STRUCTURE/spelling.pl.

\medpara
There are several things in here. There's a pile of stuff for
manipulating the input string before we even start looking in the
dictionary, there's a horrible set of linked predicates for getting
into the dictionary, and there's some stuff for managing compound
words.

\paragraph{Preprocessing:} this is a combination of
things. insertBreaks/2 deals with elisions (\q{can't}, \q{isn't},
\ldots), numbers (spotting that the commas in \q{1,350,600} and the
dot in \q{1.63} are part of the number and other really banal
issues). Most of the things in here are for English. It is tempting to
put a lot of stuff in here, \eg the definite article in Arabic, but it
should only be used for \emph{really} boring things. substitutions/2
is there for other similarly boring things like abbreviations
(\q{Mr.}) and for the rather weird fact that money amounts in English
are written right$\to$left (try reading \q{I paid £30 for it}
aloud--the \q{pounds} follows the \q{thirty}!). removeNumbers/2 uses a
grammar of numbers to replace things like \q{three hundred and sixty
four} by the figure \q{364}. There may well be other things that
should be dealt with at this stage: removeNumbers/2 and
substitutions/2 are both defined in STRUCTURE/preprocessing.pl.

\paragraph{Accessing the dictionary:} the thing that actually takes a
string of characters and traces them through the lexical trie is
lookup_word/3, which is defined in STRUCTURE/lexnet.pl. All the
things with names like just_find_word/3, really_find_word/5,
findWords1/5 are wrappers for lookup_word/3. So to find out how we
walk through the trie you have to go to
STRUCTURE/lexnet.pl. What these other predicates add to
lookup_word/3 is what to do if you didn't find actually find the word,
so they will do things like suggest trying again if the word had an
initial capital letter, making guesses about unknown open class words,
and so on.

\paragraph{Compounds:} the one thing that findWords1/5 does
that is interesting is that it deals with compound words (basically
clitics). Clitics are put in the dictionary as items whose
\texttt{cat} is something like

\begin{verbatim}
{syntax(nonfoot(head(cat(compound({syntax(nonfoot(head(cat(xbar(-v,
                                                                +n)),
                                                       _A),
                                                  mcopy(type([noun,
                                                              -pronoun | _B])),
                                                  minor(specf(|_A|))))},
                                  {syntax(nonfoot(head(cat(xbar(-v,
                                                                +n)),
                                                       _C),
                                                  mcopy(type([noun,
                                                              -pronoun | _D])),
                                                  minor(specf(|_C|))))})))))}
\end{verbatim}

\medpara
would describe something made out of two nouns (\eg a compound noun
like \q{teapot}). Breaking this into two pieces is pretty
straightforward. The real problem lies in working out where they begin
and end -- if we had had the sentence \q{I broke the duke's teapot},
where do \scare{tea} and \q{pot} begin and end. It's easy enough here
-- \q{tea} starts at 5 and ends at 6 (note that \q{'s} is a separate
lexical item, which is picked up by insertBreaks/3) and \q{pot}
starts at 6 and ends at 7. But you find sometimes that a given string
might be one word, or it might be a compound made out of two
words. You also sometimes find that a single string might be two
compounds, where the items that make up the different compounds are
different (yuk) -- you might have a string with is either a conjunction
and a verb or a preposition and a noun, and you wouldn't want to
combine the conjunction and the noun, because they weren't parts of
the same compound. This is all dealt with by cliticGroups/3.

************************************************************************/

maxLength(L, N) :-
    maxLength(L, 0, N).

maxLength([], N, N).
maxLength([H | T], I, N) :-
    length(H, J),
    max(I, J, K),
    maxLength(T, K, N).

makeSpan(0, S, S) :-
    !.
makeSpan(I, S0, S2) :-
    J is I-1,
    S1 is 2*S0+1,
    makeSpan(J, S1, S2).

makeSpan(I, S) :-
    makeSpan(I, 0, S).

cliticGroup([], []).
cliticGroup([X | T], G) :-
    cat@X <-> CAT,
    nonvar(CAT),
    language@X <-> language@Y,
    language@X <-> language@Z,
    CAT = compound(_, [Y, Z]),
    UX <-> underlying@X,
    UY <-> underlying@Y,
    UZ <-> underlying@Z,
    ((nonvar(UZ), var(UY)) ->
	append(UZ, UY, UX);
	true),
    ((nonvar(UY), var(UZ)) ->
	append(UZ, UY, UX);
	true),
    !,
    cliticGroup([Y, Z | T], G).
cliticGroup([Z | T0], [Z | T1]) :-
    cliticGroup(T0, T1).

assignPositions([S], I, N) :-
    [start, end, index, morphology, forms, tag, cat, translation, sense, case, tensed]@core@S
    <-> [start, end, index, morphology, forms, tag, cat, translation, sense, case, tensed]@S,
    S <> word,
    [start, end]@S <-> [xstart, xend]@S,
    !,
    start@S <-> I,
    end@S is I+N,
    makeSpan(N, SPAN),
    span@S is SPAN << I.
assignPositions([S | T], I, M) :-
    [start, end, index, morphology, forms, tag, cat, translation, sense, case, tensed]@core@S
    <-> [start, end, index, morphology, forms, tag, cat, translation, sense, case, tensed]@S,
    S <> word,
    [start, end]@S <-> [xstart, xend]@S,
    J is I+1,
    start@S <-> I,
    end@S <-> J,
    makeSpan(1, SPAN),
    span@S is SPAN << I,
    assignPositions(T, J, M-1).

assignPositionsToGroups([], _I, _N).
assignPositionsToGroups([H | T], I, N) :-
    assignPositions(H, I, N),
    assignPositionsToGroups(T, I, N).

split3(LIST,[LIST]) :-
	\+(member1(43,LIST)).		

split3(LIST,ANS) :-
	append(X,[43|Y],LIST),
	split3(Y,Ys),
	ANS=[X|Ys],
	!.

/*
split3([1,2,3,4,43,5,6,7,43,8,9], ANS).
ANS = [[1,2,3,4],[5,6,7],[8,9]]
*/	
	
atom_codess([],[]).		
atom_codess([X|Xs],[Y|Ys] ) :-
	atom_codes(Y,X),
	atom_codess(Xs,Ys).
/*
atom_codess([[66,66],[77,77]],ANS).
ANS = ['BB','MM']
*/	

	
	
splitTag(TAG,TAGS) :-
	atom_codes(TAG,LIST),
	split3(LIST,LISTS),
	atom_codess(LISTS,TAGS).

/*
splitTag('AA+BB',ANS).
ANS = ['AA','BB']
*/
	
		
splitTags([],[]).		
splitTags([X|Xs],[Y|Ys]) :-
	splitTag(X,Y),
	splitTags(Xs,Ys).
	
/*
splitTags(['AA+BB','LL+MM', 'SS'],ANS).
ANS = [['AA','BB'],['LL','MM'],['SS']]

*/

	
splitTagN(T0:N0,TAGS:N0) :-
	atom_codes(T0,LIST),
	split3(LIST,LISTS),
	atom_codess(LISTS,TAGS).

	
	
splitTagsN([],[]).		
splitTagsN([X0:N0|Xs],[Y0:N0|Ys]) :-
	splitTag(X0,Y0),
	splitTagsN(Xs,Ys).
/*
splitTagsN(['NN+VV':1.0, 'KK+LL':0.50],K).                                                                    
K = [['NN','VV']:1.0,['KK','LL']:0.5] ? 
*/

	
%% ['AA','BB']:1.0 => ['AA':1.0,'BB':1.0]

normaliseSplit([],[]).	
normaliseSplit([X]:N,[X:N]).
normaliseSplit([X|Xs]:N, [X:N|Ys]) :- 
	normaliseSplit(Xs:N,Ys).
	
normaliseSplits([],[]).

normaliseSplits([X:N|Xs],[Y|Ys]) :-
	normaliseSplit(X:N,Y),
	normaliseSplits(Xs,Ys).
	
/*
splitTagsN(['NN+VV':1.0, 'KK+LL':0.50],K), normaliseSplits(K,L).
K = [['NN','VV']:1.0,['KK','LL']:0.5],
L = [['NN':1.0,'VV':1.0],['KK':0.5,'LL':0.5]]
*/


scoreTags(WORD) :-
    %% pretty(tag@WORD),
    scoreTags(ptag:tag@tag@WORD, ntag:tag@tag@WORD, WORD).

scoreTags(PTAG, NTAGS, WORD) :-
    (var(PTAG); var(NTAGS)),
    !,
    penalise(WORD, -5).
scoreTags(PTAG, NTAGS, WORD) :-
    (flag(usingTags) ->
	(cmember(PTAG:N, NTAGS) ->
	    (J is -5*N, penalise(WORD, J));
	    allowMismatchedTags);
	true).

assignTags(I, [WORD | WORDS], TAGS1) :-
    findall(T, (cmember(X, TAGS1), nth(I, T, X)), L),
    L <-> ntag:tag@tag@WORD,
    J is I+1,
    scoreTags(WORD),
    assignTags(J, WORDS, TAGS1).
assignTags(_I, [], _TAGS).

checkTags(WORDS, TAGS0) :-
    length(WORDS, N),
    findall(TAG, (cvmember(TAG, TAGS0), length(TAG, N)), TAGS1),
    assignTags(1, WORDS, TAGS1).

cliticGroups([], [], _TAGS).
cliticGroups([H0 | T0], [H1 | T1], TAGS) :-
    %% pretty(TAGS),
    cliticGroup([H0], H1),
    checkTags(H1, TAGS),
    cliticGroups(T0, T1, TAGS).

expandCompounds(I, SIGNS0, SIGNS3, TAGS) :-
    cliticGroups(SIGNS0, SIGNS1, TAGS),
    maxLength(SIGNS1, N),
    assignPositionsToGroups(SIGNS1, I, N),
    findallWithWhen(SIGN, contains(SIGNS1, SIGN), SIGNS2),
    noCopies(SIGNS2, SIGNS3).

contains(L, Z1) :-
    cmember(Y, L),
    getMembersOfGroup(Y, S),
    cmember(Z1, S).

getMembersOfGroup(Y, S) :-
    findall(Z0, subcontains(Y, Z0), S),
    allRepresented(Y, S).

allRepresented(L0, L1) :-
    positions@X <-> positions@Y,
    \+ (cmember(X, L0), \+ cmember(Y, L1)).

subcontains(Y, Z1) :-
    cmember(Z0, Y),
    positions@Z0 <-> positions@Z1,
    [structure\dir, morphology, syntax\subcat, phon, meaning, remarks]@Z0
    <-> [structure\dir, morphology, syntax\subcat, phon,  meaning, remarks]@Z1,
    ((flag(forceMood), verb(Z0)) ->
     (indicative(Z0); jussive(Z0));
     true),
    fix_args(Z0, Z1),
    %% indicative(Z1),
    %% It's only at this point that we have enough information about
    %% the diacritics to know whether we like them, because it's the
    %% active/passive distinction that fixes them
    (language@Y <> arabic ->
     (findSyllableBoundaries(Z0, L0, L1),
      splitSyllables(L1, SYLL),
      doSoftParse(assignStress(SYLL), stress(index@Z1), Z1),
      doSoftParse(\+ checkBanned(L0, Z1), weak(index@Z1), Z1));
     true),
    wh@Z1 <-> WH,
    (var(WH) -> setNoWH(Z1); true),
    -value:def@zero@Z1.

noCopies([], []).
noCopies([H | T], L) :-
    cmember(X, T),
    msubsume(syntax@H, syntax@X),
    H = X,
    !,
    noCopies(T, L).
noCopies([H | T0], [H | T1]) :-
    noCopies(T0, T1).

findWords(SIGN0, START, LANGUAGE, END, ALLSIGNS1) :-
    findWords1(SIGN0, START, LANGUAGE, END, ALLSIGNS1).

findWords1(SIGN0, START, LANGUAGE, _END, ALLSIGNS1) :-
    %% retractall(typos),
    ALLSIGNS0 <-> [_ | _],
    ALLSIGNS1 <-> [_ | _],
    tag@SIGN0 <-> tag@SIGN,
    +compact@SIGN,
    findallWithWhen(SIGN,
		    really_find_word(underlying@SIGN0, SIGN, START, LANGUAGE, _J),
		    ALLSIGNS0),
    %% I've just looked in the dictionary and found all the ways there are
    %% of thinking about this word: the written string is underlying@SIGN0
    %% So we can call tag on this to get a list of all possible tags & their
    %% scores
    ((flag(usingTags), tag(underlying@SIGN0, 2, 2, TAGS0, _BESTTAG)) -> true; true),
    (var(TAGS0) ->
	true;
	(splitTagsN(TAGS0, TAGS1),
	    normaliseSplits(TAGS1, TAGS2))),
    expandCompounds(START, ALLSIGNS0, ALLSIGNS1, TAGS2),
    !.
    
findWords1(SIGN0, 0, LANGUAGE, 1, ALLSIGNS1) :- 
    %% First word in the sentence
    \+ (LANGUAGE <> arabic; LANGUAGE <> persian),
    atom_codes(underlying@SIGN0, [I0 | REST]),
    uc([I0]),
    I1 is I0+32,
    atom_codes(WORD1, [I1 | REST]),
    ALLSIGNS0 <-> [_ | _],
    findallWithWhen(SIGN,
	    really_find_word(WORD1, SIGN, 0, LANGUAGE, 1),
	    ALLSIGNS0),
    expandCompounds(0, ALLSIGNS0, ALLSIGNS1, _TAGS),
    !.  

findWords1(SIGN, START, LANGUAGE, END, [SIGN]) :-
    %% Not in the dictionary: name?
     underlying@SIGN <-> WORD,   
     ((LANGUAGE <> arabic; LANGUAGE <> persian) ->
        fail; 
        (atom_codes(WORD, [I0 | _REST]), uc([I0]))),  
     \+ temp(START, END, _),
     language@SIGN <-> LANGUAGE,
     start@SIGN <-> START,
     end@SIGN is END,
     span@SIGN is 1 << START,
     +compact@SIGN,
     [xstart, xend]@SIGN <-> [start, end]@SIGN,
     SIGN <> word,
     new_name(WORD, SIGN),
     [start, end, index, morphology, forms, tag, cat, case, tensed]@core@SIGN
      	<-> [start, end, index, morphology, forms, tag, cat, case, tensed]@SIGN,
     SIGN <> [intransitive, saturated, setNoWH, name],
     topicalised@SIGN <-> [],
     text@SIGN <-> WORD,
     !.            

findWords1(SIGN0, START, LANGUAGE, J, ALLSIGNS) :-
    addFailure(SIGN, 50, unknownWord(index@SIGN, text@SIGN)),
    (flag(unknownWords) ->
	(format('%%%% ~w unknown, so allowing it to be any open class~n',
		[underlying@SIGN0]),
	    findallWithWhen(SIGN,
			    openClass(underlying@SIGN0, SIGN, START, LANGUAGE, J),
			    ALLSIGNS));
	(\+ temp(START, J, _),
	    format('%"~w" ::: ~n', underlying@SIGN0),
	    ALLSIGNS=[],
	    assert1(unknownWord(underlying@SIGN0)))).

really_find_word(WORD, SIGN, _START, LANGUAGE, _END) :-
     +realised@SIGN,
     just_find_word(WORD, LANGUAGE, SIGN),
     -value:def@zero@SIGN,
     SIGN <> word,
     %% Filled in here because post_defaults can miss it
     (-conj@SIGN -> true; true).

just_find_word(WORD, LANGUAGE, SIGN1) :-
     language@SIGN0 <-> LANGUAGE, 
     startTimer(lookup_word),
     [syntax, meaning, remarks, structure, (morphology\fstIn)\fstOut]@SIGN0
     <-> [syntax, meaning, remarks, structure, (morphology\fstIn)\fstOut]@SIGN1,
     lookup_word(WORD, SIGN0, LANGUAGE),
     stopTimer(lookup_word).

fix_args(SIGN0, SIGN2) :-
     fixed@SIGN0 <-> fixed@SIGN1,
     set_args(SIGN0, SUBCAT, SIGN1),
     sort_args(SIGN2, SUBCAT, args@SIGN2),
     [start, end, index, morphology, forms, tag, cat, case, tensed]@core@SIGN1
      	<-> [start, end, index, morphology, forms, tag, cat, case, tensed]@SIGN1,
     [remarks, nonfoot, meaning]@SIGN0 <-> [remarks, nonfoot, meaning]@SIGN2,
     [structure\dir, morphology, forms, tag, nonfoot, foot, remarks, fixed]@SIGN1
     <-> [structure\dir, morphology, forms, tag, nonfoot, foot, remarks, fixed]@SIGN2.  

new_name(WORD, SIGN) :-
     SIGN <> [np, third_sing, adjunct],
     % -modifiable@SIGN,
     +specified@SIGN,
     surface@SIGN <-> WORD,
     underlying@SIGN <-> WORD,
     -date@SIGN,
     target@SIGN <> [np, not_case_marked],
     type@target@SIGN <-> [noun, name | _],
     modified@result@SIGN <-> 0,
     modified@SIGN <-> 0,
     -value:def@zero@SIGN,
     n_pro_ref(SIGN, named(WORD)).  

new_var(SIGN) :-
     SIGN <> [np, fullySpecified],
     surface@SIGN <-> WORD,
     -date@SIGN,
     atom_concat('VAR', WORD, V),
     semantics@SIGN <-> var(V, _V).

new_var_noun(SIGN) :-
     SIGN <> [noun, saturated],
     specifier@SIGN <-> generic(intensional@SIGN),
     surface@SIGN <-> WORD,
     -date@SIGN,
     atom_concat('VAR', WORD, V),
     semantics@SIGN <-> var(V, _V).

nlVar(X) :-
    var(X).
nlVar(X) :-
    atom(X),
    atom_codes(X, C),
    append("VAR", _, C).

read_chars(STRING) :-
     get0(X),
     ((X = -1; X = 13; X = 10) ->
         STRING = [];
         (read_chars(SUBSTRING),
          STRING = [X | SUBSTRING])).

readAtom(A) :-
    read_chars(STRING),
    atom_codes(A, STRING).

readString(STRING) :-
    read_chars(CHARS),
    itemise(CHARS, STRING, 0, 0).  
 
numChar(I) :-
    integer(I),
    I >= 48,
    I =< 57.

insertBreaks(S0, Sn) :-
    insertBreaks(S0, Sn, true).

%% A pile for dealing (mainly English) elided forms
insertBreaks([], [], _INITIAL) :-
    !.
insertBreaks(S0, Sn, INITIAL) :-
    current_language(LANGUAGE),
    ((S0 = [D1, C, D2 | T], [C] = ",", numChar(D1), numChar(D2)) ->
	insertBreaks([D1, D2 | T], Sn, INITIAL);
	(LANGUAGE <> english, prefix("can't", S0, S1)) ->
	(append("cannot", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("won't", S0, S1)) ->
	(append("will not", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("n't", S0, S1)) ->
	(append(" not", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("'ve", S0, S1)) ->
        (append(" have", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("'ll", S0, S1)) ->
        (append(" will", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("'m", S0, S1)) ->
        (append(" am", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("cannot", S0, S1)) ->
        (append("can not", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("o'clock", S0, S1)) ->
	(append(" o_clock", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("'s", S0, S1)) ->
	(append(" SS", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(LANGUAGE <> english, prefix("s' ", S0, S1)) ->
	(append("s S ", S1, S2),
	    insertBreaks(S2, Sn, INITIAL));
	(INITIAL, LANGUAGE <> english,
	    cmember(XXX, ["any", "every", "some", "no"]),
	    prefix(XXX, S0, S1),
	    cmember(YYY, ["thing", "one", "body", "where"]),
	    prefix(YYY, S1, S2)) ->
	(append(YYY, [32 | S2], S3),
	    append(XXX, [32 | S3], S4),
	    insertBreaks(S4, Sn, INITIAL));
	(\+ arabic(LANGUAGE), prefix("$", S0, S1)) ->
	(append(" ", S1, S2),
	    insertBreaks(S2, S3, INITIAL),
	    append("$", S3, Sn));
	(S0 = [X, N | S1], [X] = ".", numChar(N)) ->
	(insertBreaks(S1, S2, INITIAL),
	    Sn = [X, N | S2]);
	(S0 = [0'-, X | S1], \+ X = 0'-) ->
	(insertBreaks([X | S1], S2, INITIAL),
	    Sn = [0'-, 32 | S2]);
	(S0 = [X, 0'- | S1], \+ X = 0'-) ->
	(insertBreaks([0'- | S1], S2, INITIAL),
	    Sn = [X, 32 | S2]);
	(S0 = [X | S1], cmember(X, "£,:?.%!{[]()")) ->
	(insertBreaks([32 | S1], S2, INITIAL),
	    Sn = [32, X, 32 | S2]);
	(S0 = [H | S1],
	    (cmember(H, [13, 10, 32]) ->
		(insertBreaks(S1, S2, true),
		    Sn = [32 | S2]);
		(insertBreaks(S1, S2, fail),
		    Sn = [H | S2])))).

string2atoms(STRING, ATOMS) :-
    string2atoms(STRING, [], true, ATOMS).

%% skip initial spaces
string2atoms([32 | S], L, true, ATOMS) :-
    !,
    string2atoms(S, L, fail, ATOMS).
%% catch cases where there's a final space
string2atoms([], [], _INITIAL, []) :-
    !. 
string2atoms([], L, _INITIAL, [A]) :-
    !,
    string2atom(L, A).
string2atoms([32, 32 | S0], L, _INITIAL, ATOMS) :-
    !, %% skip double and initial spaces
    string2atoms([32 | S0], L, fail, ATOMS).
string2atoms([32 | S0], L, _INITIAL, [A | ATOMS]) :-
    !,
    string2atom(L, A),
    string2atoms(S0, [], fail, ATOMS).
string2atoms([H | T], L0, _INITIAL, A) :-
    append(L0, [H], L1),
    string2atoms(T, L1, fail, A).

string2atom([H | T], A) :-
    (numChar(H) ->
	number_codes(A, [H | T]);
	atom_codes(A, [H | T])).

uc([H | _]) :-
     H > 64,
     H < 91.

readText(SIGNS) :-
     readString(STRING),
     string2signs(STRING, SIGNS).

atoms2signs([], []).
atoms2signs([underlying@H | T0], [H | T1]) :-
    atoms2signs(T0, T1).

string2signs(STRING0, SIGNS) :-
     retractall(savedString(_)),
     atom_codes(SAVEDSTRING, STRING0),
     assert(savedString(SAVEDSTRING)),
     insertBreaks(STRING0, STRING1),
     string2atoms(STRING1, ATOMS0),
     removeNumbers(ATOMS0, ATOMS1),
     money(ATOMS1, ATOMS2),
     substitutions(ATOMS2, ATOMSn),
     atoms2signs(ATOMSn, SIGNS).
     
