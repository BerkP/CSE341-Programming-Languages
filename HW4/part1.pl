%facts
flight(ankara,istanbul).
flight(ankara,konya).
flight(ankara,van).

flight(antalya,istanbul).
flight(antalya,gaziantep).
flight(antalya,konya).

flight(burdur,ısparta).

flight(edirne,edremit).

flight(edremit,edirne).
flight(edremit,erzincan).

flight(erzincan,edremit).

flight(ısparta,burdur).
flight(ısparta,izmir).

flight(istanbul,ankara).
flight(istanbul,antalya).
flight(istanbul,gaziantep).
flight(istanbul,izmir).
flight(istanbul,rize).
flight(istanbul,van).

flight(izmir,ısparta).
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


%part 1
route(A,B) :-
    r_helper(A,B,[]).

r_helper(A,B,Cities) :-
    flight(A,C),
    not(member(C,Cities)),
    r_helper(C,B,[A|Cities]).

r_helper(A,B,Cities) :-
    flight(A,C),
    not(member(C,Cities)),
    B = C.
