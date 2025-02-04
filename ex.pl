%author(AuthorID, Name, YearOfBirth, CountryOfBirth).
author(1,	'John Grisham',		1955,	'USA').
author(2,	'Wilbur Smith',		1933,	'Zambia').
author(3,	'Stephen King',		1947,	'USA').
author(4,	'Michael Crichton',	1942,	'USA').

%book(Title, AuthorID, YearOfRelease, Pages, Genres).
book('The Firm', 		1, 	1991, 	432,	['Legal thriller']).
book('The Client',		1,	1993,	422,	['Legal thriller']).
book('The Runaway Jury',	1,	1996,	414, 	['Legal thriller']).
book('The Exchange',		1,	2023,	338,	['Legal thriller']).

book('Carrie',			3,	1974,	199,	['Horror']).
book('The Shining',		3,	1977,	447,	['Gothic novel', 'Horror', 'Psychological horror']).
book('Under the Dome',		3,	2009,	1074,	['Science fiction', 'Political']).
book('Doctor Sleep',		3,	2013,	531,	['Horror', 'Gothic', 'Dark fantasy']).

book('Jurassic Park',		4,	1990,	399,	['Science fiction']).
book('Prey',			4,	2002, 	502, 	['Science fiction', 'Techno-thriller', 'Horror', 'Nanopunk']).
book('Next', 			4,	2006, 	528, 	['Science fiction', 'Techno-thriller', 'Satire']).


book_author(Title, Author) :- book(Title, Id, _, _, _), author(Id, Author, _, _).

multi(Title) :- book(Title, _, _, _, Genres), length(Genres, Num), Num > 1.

shared(Title1, Title2, CommonGenres) :- book(Title1, _, _, _, Genres1),
                                        book(Title2, _, _, _, Genres2),
                                        intersection(Genres1, Genres2, CommonGenres).

similarity(Title1, Title2, Similarity):-book(Title1, _, _, _, Genres1),
                                        book(Title2, _, _, _, Genres2),
                                        intersection(Genres1, Genres2, CommonGenres),
                                        union(Genres1, Genres2, AllGenres),
                                        length(CommonGenres, NumI),
                                        length(AllGenres, NumU),
                                        Similarity is NumI/NumU.


intersection([], _L, []).
intersection([H|T], L, [H|R]) :- member(H, L), !,
                                 intersection(T, L, R).
intersection([_|T], L, R):-
	intersection(T, L, R).                    

union(S1, S2, US) :- append(S1, S2, All),
                     sort(All, US).


gives_gift_to(bernardete, 'The Exchange', celestina).
gives_gift_to(celestina, 'The Brethren', eleuterio).
gives_gift_to(eleuterio, 'The Summons', felismina).
gives_gift_to(felismina, 'River God', juvenaldo).
gives_gift_to(juvenaldo, 'Seventh Scroll', leonilde).
gives_gift_to(leonilde, 'Sunbird', bernardete).
gives_gift_to(marciliano, 'Those in Peril', nivaldo).
gives_gift_to(nivaldo, 'Vicious Circle', sandrino).
gives_gift_to(sandrino, 'Predator', marciliano).


% gives_gift_to(Person, _T, Receiver)

% Main predicate: Starts the circle size computation
circle_size(Person, Size) :-
    circle_size_helper(Person, [], 0, Size).  % Initialize with empty visited list & accumulator 0

% Base case: If Person is already in the visited list, return the accumulated count
circle_size_helper(Person, Visited, Acc, Size) :-
    member(Person, Visited),  % Check if Person was already visited (cycle detected)
    Size = Acc.               % Stop recursion, return accumulated size

% Recursive case: Continue traversing the gift chain
circle_size_helper(Person, Visited, Acc, Size) :-
    gives_gift_to(Person, _, Receiver),  % Find who Person gives a gift to
    \+ member(Person, Visited),          % Ensure we don't revisit the same person
    Acc1 is Acc + 1,                      % Increment the counter
    circle_size_helper(Receiver, [Person | Visited], Acc1, Size).  % Recurse with updated visited list & count