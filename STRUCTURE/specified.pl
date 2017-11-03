/*
	SPECIFIED.PL
	
	Standard configurations of +/-specified, +/-unspecified
*/

fullySpecified(X) :-
    +specified@X,
    -unspecified@X.
    
fullyUnspecified(X) :-
    -specified@X,
    +unspecified@X.
