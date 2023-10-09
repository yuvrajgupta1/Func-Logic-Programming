% directed graph
arc(0,1).
arc(0,2).
arc(2,3).
arc(1,3).

% checks if 2 vertices are connected in a directed graph
connected(X,X) :- !.
connected(X,Y) :- arc(X,Z), connected(Z,Y).

% simple graph
sarc(0,1).
sarc(0,2).
sarc(1,0).
sarc(1,3).
sarc(2,0).
sarc(2,3).
sarc(3,1).
sarc(3,2).

% checks if 2 vertices are connected in a simple graph.
sconnected(X,Y) :- walk(X,Y, []).
walk(X,Y,List):- sarc(X,Z),
    not(member(Z,List)),
    (Y = Z; walk(Z,Y,[X|List])).
