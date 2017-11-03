/*
	LANGUAGES.PL
*/

english(english).
french(french).
spanish(spanish).
german(german).
greek(greek).
malay(malay).
arabic(arabic).
somali(somali).
persian(persian).

notFGGM(X) :-
    trigger(X, \+ cmember(X, [french, german, greek, malay])).
englishOrFrench(X) :-
    trigger(X, (X = english; X = french)).
notGM(X) :-
    trigger(X, \+ cmember(X, [german, malay])).
notSGMP(X) :-
    trigger(X, \+ cmember(X, [spanish, german, malay, persian])).
notSGGMP(X) :-
    trigger(X, \+ cmember(X, [spanish, german, greel, malay, persian])).

/*
english(X) :-
    +english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

french(X) :-
    -english:lang@X,
    +french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

spanish(X) :-
    -english:lang@X,
    -french:lang@X,
    +spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

german(X) :-
    -english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    +german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

greek(X) :-
    -english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    +greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

malay(X) :-
    -english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    +malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

arabic(X) :-
    -english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    +arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

persian(X) :-
    -english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    +persian:lang@X,
    -somali:lang@X.

somali(X) :-
    -english:lang@X,
    -french:lang@X,
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    +somali:lang@X.

notFGGM(X) :-
    -french:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X.

englishOrFrench(X) :-
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -arabic:lang@X,
    -persian:lang@X,
    -somali:lang@X.

notGM(X) :-
    -german:lang@X,
    -malay:lang@X.

notSGMP(X) :-
    -spanish:lang@X,
    -german:lang@X,
    -malay:lang@X,
    -persian:lang@X.

notSGGMP(X) :-
    -spanish:lang@X,
    -german:lang@X,
    -greek:lang@X,
    -malay:lang@X,
    -persian:lang@X.
*/