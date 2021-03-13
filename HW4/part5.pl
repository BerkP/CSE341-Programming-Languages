
find_operators([E1|Rest],Res,["+"|Oprest]) :-
    Newres is Res - E1,
    find_operators(Rest,Newres,Oprest).

find_operators([E1,E2|Rest],Res,["-"|Oprest]) :-
    Newres is Res - E1,
    Et is E2 * (-1),
    find_operators([Et|Rest],Newres,Oprest).

find_operators([E1,E2|Rest],Res,["*"|Oprest]) :-
    Newelem is E1 * E2,
    find_operators([Newelem|Rest],Res,Oprest).

find_operators([E1,E2|Rest],Res,["/"|Oprest]) :-
    Newelem is E1 / E2,
    find_operators([Newelem|Rest],Res,Oprest).

find_operators([Res],Res,[]).


find_equation() :-
    open("input.txt",read,Stream),
    read_stream_to_codes(Stream, Codes),
    read_from_chars(Codes, Intlist),
    last_elem(Intlist,Res),
    remove_last(Intlist,Newlist),
    find_operators(Newlist,Res,Ops),
    open("output.txt",write,OStream),
    print_merged(Newlist,Ops,Res,OStream),
    close(OStream).


print_merged([I|Ints],[O|Ops],Res,OStream):-
    write(OStream, I),
    write(OStream, O),
    print_merged(Ints,Ops,Res, OStream).

print_merged([I],[],Res,OStream):-
    write(OStream, I),
    write(OStream, "="),
    write(OStream, Res).


last_elem([L],L).

last_elem([_|Rest], L):-
    last_elem(Rest,L).


remove_last([E|Rest1],[E|Rest2]):-
    remove_last(Rest1,Rest2).

remove_last([_],[]).
