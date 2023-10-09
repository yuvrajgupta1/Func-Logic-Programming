
point(p1).
point(p2).
point(p3).
point(p4).
point(p5).
point(p6).
point(p7).

line(l1).
line(l2).
line(l3).
line(l4).
line(l5).
line(l6).
line(l7).

linear(p1, p2, p3, l1).
linear(p1, p4, p6, l5).
linear(p1, p5, p7, l2).
linear(p2, p4, p7, l6).
linear(p2, p5, p6, l7).
linear(p3, p4, p5, l4).
linear(p3, p6, p7, l3).

trisect(l1, l2, l5, p1).
trisect(l1, l6, l7, p2).
trisect(l1, l3, l4, p3).
trisect(l2, l3, l6, p7).
trisect(l2, l4, l7, p5).
trisect(l4, l5, l6, p4).
trisect(l3, l5, l7, p6).

intersect(L1,L2,P) :- 
    trisect(L1,L2,_,P);
    trisect(L1,_,L2,P);
    trisect(_,L1,L2,P);
    trisect(L2,L1,_,P);
    trisect(L2,_,L1,P);
    trisect(_,L2,L1,P).

connect(P1,P2,L) :-
    linear(P1,P2,_,L);
    linear(P1,_,P2,L);
    linear(_,P1,P2,L);
    linear(_,P2,P1,L);
    linear(P2,P1,_,L);
    linear(P2,_,P1,L).
