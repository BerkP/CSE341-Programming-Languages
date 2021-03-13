%facts

when_(102, 10).
when_(108, 12).
when_(341, 14).
when_(455, 16).
when_(452, 17).

where(102, z23).
where(108, z11).
where(341, z06).
where(455, 207).
where(452, 207).

enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

%part1
schedule(S, P, T) :-
    enroll(S, C),
    where(C, P),
    when_(C,T).

%part2
usage(P, T):-
	where(C, P),
	when_(C, T).

%part3
conflict(X,Y):-
    when_(X,Tx),
    when_(Y,Ty),
    Tx==Ty.

conflict(X,Y):-
    where(X,Px),
    where(Y,Py),
    Px==Py.

%part4
meet(S1,S2):-
	schedule(S1, P1, T1),
	schedule(S2, P2, T2),
	P1 == P2,
	T1 == T2.
