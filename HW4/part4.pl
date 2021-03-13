%part1
element(E,S) :-
    member_of(E,S).

member_of(E,[E|_]).

member_of(E,[_|List_]):-
    member_of(E,List_).

%part4

equivalent(S1,S2) :-
    size_equal(S1,S2),
    is_included(S1,S2).

size_equal([],[]).

size_equal([_|Rest1],[_|Rest2]) :-
    size_equal(Rest1,Rest2).

is_included([],_).

is_included([E|Rest],S2):-
	member_of(E,S2),
	is_included(Rest,S2).


%part2
union(S1,S2,S3):-
    create_union(S1,S2,U),
    equivalent(S3,U).

create_union([E|S1Rest],S2,U) :-
    member_of(E,S2),
    create_union(S1Rest,S2,U).

create_union([E|S1Rest],S2,[E|URest]) :-
    not(member_of(E,S2)),
    create_union(S1Rest,S2,URest).

create_union([],S2,S2).



%part3
%In this part I could also check every element of S3
%for member_of s1 and s2 but this is not the
%definition of the intersection.
intersect(S1, S2, S3):-
	create_intersection(S1, S2, S4),
	equivalent(S3,S4).

create_intersection([E|Rest1],S2,[E|Rest3]) :-
    member_of(E,S2),
    create_intersection(Rest1,S2,Rest3).

create_intersection([E|Rest1],S2,Rest3) :-
    not(member_of(E,S2)),
    create_intersection(Rest1,S2,Rest3).

create_intersection([],_,[]).
