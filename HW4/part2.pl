%facts
flight(ankara,istanbul).
flight(ankara,konya).
flight(ankara,van).

flight(antalya,istanbul).
flight(antalya,gaziantep).
flight(antalya,konya).

flight(burdur,ýsparta).

flight(edirne,edremit).

flight(edremit,edirne).
flight(edremit,erzincan).

flight(erzincan,edremit).

flight(ýsparta,burdur).
flight(ýsparta,izmir).

flight(istanbul,ankara).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,izmir).
flight(istanbul,rize).
flight(istanbul,van).

flight(izmir,ýsparta).
flight(izmir,istanbul).

flight(gaziantep,antalya).
flight(gaziantep,istanbul).

flight(konya,antalya).
flight(konya,ankara).

flight(rize,istanbul).
flight(rize,van).

flight(van,ankara).
flight(van,istanbul).
flight(van,rize).

%-----------

distance(ankara,istanbul,352).
distance(ankara,konya,227).
distance(ankara,van,920).

distance(antalya,istanbul,483).
distance(antalya,gaziantep,592).
distance(antalya,konya,192).

distance(burdur,ýsparta,25).

distance(edirne,edremit,235).

distance(edremit,edirne,235).
distance(edremit,erzincan,1066).

distance(erzincan,edremit,1066).

distance(ýsparta,burdur,25).
distance(ýsparta,izmir,309).

distance(istanbul,ankara,352).
distance(istanbul,antalya,483).
distance(istanbul,gaziantep,847).
distance(istanbul,izmir,329).
distance(istanbul,rize,968).
distance(istanbul,van,1262).

distance(izmir,ýsparta,309).
distance(izmir,istanbul,329).

distance(gaziantep,antalya,592).
distance(gaziantep,istanbul,847).

distance(konya,antalya,192).
distance(konya,ankara,227).

distance(rize,istanbul,968).
distance(rize,van,373).

distance(van,ankara,920).
distance(van,istanbul,1262).
distance(van,rize,373).


%part 2
sroute(A,B,Dis) :-
    findall(D,route_distance(A,B,D),Dislist),
    find_min(Dislist,Dis).

route_distance(A,B,Distance):-
    r_d_helper(A,B,[],Distance).

r_d_helper(A,B,Cities,Distance) :-
    flight(A,C),
    not(member(C,Cities)),
    distance(A,C,D2),
    r_d_helper(C,B,[A|Cities],D),
    Distance is D+D2.

r_d_helper(A,B,Cities,Distance) :-
    flight(A,C),
    not(member(C,Cities)),
    distance(A,B,Distance),
    B = C.

find_min(List,Min):-
    f_m_helper(List,Min).

f_m_helper([F,S|Rest],Min) :-
    F =< S,
    f_m_helper([F|Rest],Min).

f_m_helper([F,S|Rest],Min) :-
    F > S,  f_m_helper([S|Rest],Min).

f_m_helper([Min],Min).
