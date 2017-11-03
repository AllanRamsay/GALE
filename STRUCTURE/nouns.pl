/*
 	NOUNS.PL
  	Various kinds of noun.
*/

%% This needs work: see nounPatternsForParasite.txt in buckwalter
%% (but then nouns as adjectives in Arabic needs work in general)
elative(X0) :-
    [person, def]@X0 <-> [person, def]@X1,
    X0 <> [not_plural, masculine],
    theta@X0 <-> THETA,
    trigger(THETA,
	    (THETA = arg(_) ->
		definite1(X1);
		third_sing_only(X1))).

setTargetAsConstruct(N) :-
     target@N <-> TARGET,
     result@N <-> RESULT,
     TARGET <> [noun, saturated],
     % -indefinite@TARGET,
     +definite@TARGET,
     +indefinite@TARGET,
     % TARGET <> reallyNom,
     case@TARGET <-> case@result@RESULT,
     dir@TARGET <> xafter,
     RESULT <> [not_case_marked, fullySpecified],
     RESULT <> definite1,
     [core, head, predicative, target, result]@RESULT
         <-> [core, head, predicative, target, result]@TARGET,
     modified@RESULT <-> 10,
     modifier@N <-> *constructNP,
     [case, displaced]@N <-> [case, displaced]@DUMMYN,
     trigger(index@TARGET,
	     (penalise(TARGET, 4),
		 constructSatellite(DUMMYN),
		 setTargetAsConstruct(RESULT))).

constructSatellite(N) :-
    -moved:dir@displaced@N,
    % +definite@N,
    N <> gen.

nmod(N) :-
     % A noun can modify another noun, to form a noun-noun compound
     target@N <> noun,
     +unspecified@target@N,
     modified@result@N <-> 2.5,
     modifier@result@N <-> *noun_as_mod,
     N <> adjunct,
     modifier@N <-> *noun_as_mod,
     -moved:dir@displaced@target@N,
     +compact@target@N,
     (language@N <> persian ->
	 dir@target@N <> xafter;
	 true),
     trigger(index@target@N, incrementScore(score@target@N, 100)).

ntype(N, simple, semantics@N) :-
    ptag:tag@tag@N <-> 'NN',
    ntype(N, basic),
    LANGUAGE <-> language@N,
    N <> setNoWH,
    ((LANGUAGE <> english; LANGUAGE <> persian) ->
	default(nmod(N));
	LANGUAGE <> arabic ->
	setTargetAsConstruct(N); %% Was done by an inflectional affix, but now back here
	-cat@target@N).

ntype(N, basic) :-
    N <> saturated,
    ntype(N, basic1).

ntype(N, basic1) :-
    modified@N <-> 0,
    LANGUAGE <-> language@N,
    unspecified@N <-> U,
    (LANGUAGE <> arabic ->
	true;
	U = +),
    (LANGUAGE <> english ->
	specifier@N <-> generic(intensional@N);
	language@N <> persian ->
	definite1(N);
	true),
    N <> [noun, third].

ntype(N, comp(X)) :-
    args@N <-> [X],
    semantics@N <-> noun2(text@N),
    ntype(N, basic1).

ntype(N, simple1) :-
    S0 <-> text@N,
    default(sort@N <-> text@N),
    translation@N <-> TRANS,
    (nonvar(TRANS) ->
	with_output_to_atom(format('~w (~w)', [text@N, TRANS]), MEANING);
	MEANING = text@N),
    SEMANTICS = noun(MEANING),
    ntype(N, simple, SEMANTICS),
    N <> noun,
    -date@N,
    type@N <-> [noun, pronoun(-) | _].

ntype(N, simple) :-
    % -predicative@N,
    ntype(N, simple1).

ntype(N, sort(S0)) :-
    sort@N --- S0,
    ntype(N, simple, lambda(X, sort(S0, X, _, _))).

ntype(N, simpleArabic) :-
    % +indefinite@N,
    predicative@N <-> *(arabicNoun),
    ntype(N, simple1).

ntype(N, simpleSomali) :-
    specifier@N <-> quant(existential, 0, start@N),
    -clitic@N,
    ntype(N, simple).

ntype(N, somaliBigPron) :-
    N <> [noun, saturated],
    +unspecified@N,
    indefinite1(N),
    -date@N,
    semantics@N <-> noun(text@N).

ntype(N, date) :-
     ntype(N, simple, sort(text@N)),
     N <> noun,
     +date@N,
     type@N <-> [noun, pronoun(-) | _]. 

ntype(N, predicative) :-
     predicative@N <-> *nppred,
     ntype(N, simple1).  

ntype(N, measure(_M)) :-
     N <> [saturated, noun, third],
     type@N <-> [_, pronoun(_PRO), measure | _],
     semantics@N <-> noun(text@N).  

ntype(N, month) :-
     N <> [noun, third],
     +specified@N,
     -date@N,
     ((args@N <-> [NUM],
       NUM <> det,
       +fixed@N,
       type@NUM <-> number);
      args@N <-> []). 

ntype(N, of) :-     
     N <> [noun, third],
     NP <> pp,
     args@N <-> [NP],
     pcase(NP, of).  

ntype(N, scomp) :-
     N <> [noun, third],
     args@N <-> [S],
     S <> s,
     S <> declarative1,
     +comp@S.  

/* 

A number of words like "some", "few", "one" hover in a grey area between
nouns and determiners. The crucial test for me seems to be whether they combine
with an NN or an NP: determiners modify with NNs, nouns subcategorise for
NPs. Some words, notably cardinal numbers, do both, and as such they
deserve a multiple entry in the dictionary.

For ones that combine with NPs, there are a number of variations:

- does it take an OF-marked NP? If so, is this optional (zeroable)?
- does it take an accusative NP?
- is the result known to be +specified or -specified, or is it open to either?

*/

% Takes a possibly zero of-marked NP, produces either an NN or an NP  

ntype(N, number(NP)) :-
     modified@N <-> -1,
     N <> [noun],
     args@N <-> [NP],
     NP <> np,
     pcase(NP, of),
     -usedef:def@zero@NP,
     -usedef:def@generic@N,
     type@N <-> [noun, pronoun(_), number | _].  

% Takes an accusative NP, produces either an NN or an NP  

ntype(N, accnp) :-
     N <> [noun],
     args@N <-> [NP],
     NP <> np,
     type@N <-> [noun, pronoun(_), number | _].  

% Basic number: the argument is zeroable  

ntype(N, number) :-
     ntype(N, number(_NP)).  

% "half" behaves like an ordinary number  

ntype(N, half) :-
     ntype(N, number).  
     
% It also accepts a straight accusative NP and produces an NP  

ntype(N, half) :-
     ntype(N, accnp),
     +value:def@generic@N.  

% "all" behaves like a number, but the argument is not zeroable  

ntype(N, all) :-
     ntype(N, number(NP)),
     -value:def@zero@NP.

% And it also accepts an accusative NP and produces an NP  

ntype(N, all) :-
     ntype(N, accnp),
     +value:def@generic@N.  

% "few" behaves like a number   

ntype(N, few) :-
     ntype(N, number).  

% It can take an accusative NP: result can be an NN or an NP  

ntype(N, all) :-
     ntype(N, accnp).
     
oclock(X, NUM) :-
    dtype(NUM, number, _),
    NUM <> [det, setNoWH],
    +definite@X,
    specifier@X <-> pro,
    semantics@X <-> lambda(_D, lambda(H, hour(H) & (((arg@NUM):identity):H))).
     
size(X, NUM) :-
    dtype(NUM, number, _),
    NUM <> [det, setNoWH],
    +definite@X,
    specifier@X <-> pro,
    semantics@X <-> (lambda(H, hour(H) & (((arg@NUM):identity):H))).
     

