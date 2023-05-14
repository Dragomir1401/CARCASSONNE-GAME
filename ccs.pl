:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').



% tile/2
% tile(Index, Tile)
%
% Fiecare soluție a predicatului tile este o corespondență între index
% (numărul piesei în lista din enunț) și reprezentarea internă a piesei
% respective.
%
% Puteți alege orice reprezentare doriți, în așa fel încât să puteți
% reprezenta toate piesele din enunț.
%
% Orice muchie a unei piese este o cetate, un drum, sau o pajiște.
% Pot exista cel mult 1 drum și cel mult 2 castele pe aceeași piesă.
%
% Reprezentarea trebuie să poată fi rotită (vezi predicatul ccw/3 mai
% jos) pentru a produce reprezentarea piesei rotite cu 90 de grade.
%
% Trebuie să definiți reprezentări pentru fiecare dintre cele 16 piese
% din enunțul temei.
%
% Exemplu: apelul tile(1, T1). trebuie să lege T1 la reprezentarea pe
% care o faceți pentru piesa 1. Această reprezentare poate fi transmisă
% celorlalte predicate din temă, pentru a întreba, de exemplu, ce se
% află pe muchia de nord a piesei 1, sau dacă piesa 1 se potrivește cu o
% altă piesă.

% make representation for each tile based on index 
% 1 - 16 N-E-S-W
% tile/2
% tile(Index, Tile)
tile(1, T1) :- T1 = tile(1, [c, c, p, c]). % tile 1 has 2 castles, 1 road, 1 grass
tile(2, T2) :- T2 = tile(2, [c, c, d, c]). % tile 2 has 2 castles, 1 road, 1 grass
tile(3, T3) :- T3 = tile(3, [c, c, p, p]). % tile 3 has 2 castles, 2 grass
tile(4, T4) :- T4 = tile(4, [c1, c2, p, p]). % tile 4 has 2 castles, 2 grass
tile(5, T5) :- T5 = tile(5, [c1, p, c2, p]). % tile 5 has 1 castle, 2 roads, 1 grass
tile(6, T6) :- T6 = tile(6, [c, p, c, p]). % tile 6 has 1 castle, 2 roads, 1 grass
tile(7, T7) :- T7 = tile(7, [c, p, p, p]). % tile 7 has 1 castle, 3 grass
tile(8, T8) :- T8 = tile(8, [c, c, d, d]). % tile 8 has 2 castles, 2 roads
tile(9, T9) :- T9 = tile(9, [c, p, d, d]). % tile 9 has 1 castle, 1 road, 2 grass
tile(10, T10) :- T10 = tile(10, [c, d, d, p]). % tile 10 has 1 castle, 1 road, 2 grass
tile(11, T11) :- T11 = tile(11, [c, d, p, d]). % tile 11 has 1 castle, 1 road, 2 grass
tile(12, T12) :- T12 = tile(12, [c, d, d, d]). % tile 12 has 1 castle, 3 roads
tile(13, T13) :- T13 = tile(13, [p, p, d, d]). % tile 13 has 2 roads, 2 grass
tile(14, T14) :- T14 = tile(14, [p, d, p, d]). % tile 14 has 1 road, 2 grass, 1 castle
tile(15, T15) :- T15 = tile(15, [p, d, d, d]). % tile 15 has 3 roads, 1 grass
tile(16, T16) :- T16 = tile(16, [d, d, d, d]). % tile 16 has 4 roads



% at/3
% at(+Tile, +Direction, ?What)
%
% Predicatul este adevărat dacă pe piesa Tile are pe muchia de pe
% direcția Direction o entitate de tipul What.
%
% Directions este una dintre n, e, s, w (vezi predicatul directions/1
% din utils.pl).
%
% Entitatea (What) este una dintre c, d, sau p. reprezentând cetate,
% drum, sau pajiște.
%
% De exemplu, piesa 4 are cetate în nord și în este, și pajiște în sud
% și vest. Iar piesa 10 are cetate în nord, drum în este și sud, și
% pajiște în vest.
%
% Dacă What nu este legat, trebuie legat la entitatea care se află pe
% muchia din direcția Dir.

% at/3
% at(+Tile, +Direction, ?What)
at(tile(_, [Var, _, _, _]), n, c) :- member(Var, [c1, c2]), !.
at(tile(_, [_, Var, _, _]), e, c) :- member(Var, [c1, c2]), !.
at(tile(_, [_, _, Var, _]), s, c) :- member(Var, [c1, c2]), !.
at(tile(_, [_, _, _, Var]), w, c) :- member(Var, [c1, c2]), !.

at(tile(_, [Var, _, _, _]), n, Var). % check to see value at north
at(tile(_, [_, Var, _, _]), e, Var). % check to see value at east
at(tile(_, [_, _, Var, _]), s, Var). % check to see value at south
at(tile(_, [_, _, _, Var]), w, Var). % check to see value at west


% atL/3
% atL(+Tile, +Directions, +What)
%
% Predicatul este adevărat dacă piesa Tile are entitatea what pe toate
% direcțiile din lista Directions, cu aceleași valori pentru entități și
% direcții ca și la predicatul at/3.
%
% De exemplu, predicatul este adevărat pentru reprezentarea piesei 1,
% pentru lista [w,n,e], și pentru entitatea c. Este adevărat de asemenea
% pentru reprezentarea piesei 14, pentru lista [e,w], și pentru
% entitatea d.
%
% Atenție! Pentru ca predicatul să fie adevărat, nu este nevoie ca în
% Directions să fie *toate* direcțiile pe care se află entitatea
% respectivă, pot fi doar o submulțime a acestora.
% De exemplu, la piesa 14, predicatul este adevărat pentru entitatea d
% și pentru oricare dintre listele [w], [e] sau [e,w].
% atL/3
% atL(+Tile, +Directions, +What)
atL(_, [], _).
atL(Tile, [CurrDirection | Rest], What) :-
    at(Tile, CurrDirection, What), % check to see if What is at CurrDirection
    atL(Tile, Rest, What). % check for the rest of the directions


% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
% Predicatul întoarce adevărat dacă pe piesă există două cetăți diferite
% (ca în piesele 4 și 5).
hasTwoCitadels(tile(_, Entities)) :-
    member(c1, Entities), % check to see if c1 is in Tile Entities
    member(c2, Entities), % check to see if c2 is in Tile Entities
    c1 \= c2. % check to see if c1 is different than c2


% ccw/3
% ccw(+Tile, +Rotation, -RotatedTile)
% Predicatul este adevărat dacă RotatedTile este reprezentarea piesei cu
% reprezentarea Tile, dar rotită de Rotation ori, în sens trigonometric.
%
% De exemplu, dacă T4 este reprezentarea piesei 4, atunci ccw(4, 1, R)
% va lega R la reprezentarea unei piese care are pajiște la nord și
% vest, și cetate la est și sud.
%
% Pentru piesele cu simetrie, reprezentarea unora dintre rotații este
% identică cu piesa inițială.
% De exemplu, la piesele 5, 6 și 14, rotirea cu Rotation=2 va duce la o
% reprezentare identică cu piesa inițială, și la fel rezultatele pentru
% Rotation=1 și Rotation=3 vor fi identice.
% La piesa 16, orice rotire trebuie să aibă aceeași reprezentare cu
% reprezentarea inițială.

% helper function that rotates a list once to the left

% append/3 
% append(+List1, +List2, -List1AndList2)
% append([1,2,3], [4,5,6], R). -> R = [1,2,3,4,5,6]

% rotate_once/2
% rotate_once(+List, -RotatedList)
rotate_once([], []). % base case
rotate_once([Head | Tail], Acc) :- append(Tail, [Head], Acc). % rotate_once([1,2,3], Acc). -> Acc = [2,3,1]

% ccw/3
% ccw(+Tile, +NumberOfRotations, -RotatedTile)
ccw(Tile, 0, Tile). % base case
ccw(tile(Index, List), NumberOfRotations, tile(Index, RotatedList)) :-
    NumberOfRotations > 0, % check for recursion
    rotate_once(List, TempList), % rotate the list once
    NewNumberOfRotations is NumberOfRotations - 1, % decrease the number of rotations
    ccw(tile(Index, TempList), NewNumberOfRotations, tile(Index, RotatedList)). % call ccw recursively



% rotations/2
% rotations(+Tile, -RotationPairs)
%
% Predicatul leagă RotationPairs la o listă de perechi
% (Rotation, RotatedTile)
% în care Rotation este un număr de rotații între 0 și 3 inclusiv și
% RotatedTile este reprezentarea piesei Tile rotită cu numărul respectiv
% de rotații.
%
% Rezultatul trebuie întotdeauna să conțină perechea (0, Tile).
%
% IMPORTANT:
% Rezultatul nu trebuie să conțină rotații duplicate. De exemplu, pentru
% piesele 5,6 și 14 rezultatul va conține doar 2 perechi, iar pentru
% piesa 16 rezultatul va conține o singură pereche.
%
% Folosiți recursivitate (nu meta-predicate).

% normalize_tile/2
% normalize_tile(+Tile, -NormalizedTile)
% maplist/3
% maplist(:Goal, ?List1, ?List2)
normalize_tile(tile(Number, Types), tile(Number, NormalizedTypes)) :-
    maplist(normalize_type, Types, NormalizedTypes). % apply normalize_type to each element of Types and store the result in NormalizedTypes

% normalize_type/2
% normalize_type(+Type, -NormalizedType)
normalize_type(Type, c) :- 
    member(Type, [c1, c2]), !. % if Type is c1 or c2, then NormalizedType is c
normalize_type(Type, Type). % if Type is not c1 or c2, then NormalizedType is Type

% add_to_list/4
% add_to_list(+NumberOfRotations, +RotatedTile, +OldList, -NewList)
add_to_list(_, RotatedTile, OldList, NewList) :- 
    normalize_tile(RotatedTile, NormalizedTile), % normalize the tile
    member((_, NormalizedTile), OldList), !, % check if the tile is already in the list
    NewList = OldList. % if it is, then the new list is the same as the old list
add_to_list(NumberOfRotations, RotatedTile, OldList, [(NumberOfRotations, NormalizedTile) | OldList]) :- % if the tile is not in the list, then add it to the list
    normalize_tile(RotatedTile, NormalizedTile). % normalize the tile

% find_rotations/4
% find_rotations(+Tile, +NumberOfRotations, +TempRotationPairs, -RotationPairs)
find_rotations(_, 4, RotationPairs, RotationPairs) :- !. % if the number of rotations is 4, then stop the recursion
find_rotations(Tile, NumberOfRotations, TempRotationPairs, RotationPairs) :-
    ccw(Tile, NumberOfRotations, RotatedTile), % rotate the tile
    normalize_tile(RotatedTile, NormalizedTile), % normalize the tile
    add_to_list(NumberOfRotations, NormalizedTile, TempRotationPairs, NewTempRotationPairs), % add the tile to the list
    NewNumberOfRotations is NumberOfRotations + 1, % increase the number of rotations
    find_rotations(Tile, NewNumberOfRotations, NewTempRotationPairs, RotationPairs). % call find_rotations recursively

% rotations/2
% rotations(+Tile, -RotationPairs)
rotations(Tile, RotationPairs) :-
    find_rotations(Tile, 0, [], RotationPairs). % call the helper function with the initial number of rotations 0, the initial list [], and the result list RotationPairs


% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
%
% Predicatul întoarce adevărat dacă NeighborTile poate fi pusă în
% direcția NeighborDirection față de Tile și se potrivește, adică muchia
% comună este de același fel.
%
% De exemplu, dacă T2 este reprezentarea piesei 2, iar T16 este
% reprezentarea piesei 16, atunci match(T2, T16, s) este adevărat.
%
% Similar, pentru piesele 8 și 10, este adevărat
% ccw(T8, 3, T8R), match(T8R, T10, w).
%
% Puteți folosi predicatul opposite/2 din utils.pl.

% match/3
% match(+Tile, +NeighborTile, +NeighborDirection)
match(Tile, NeighborTile, NeighborDirection) :- 
    at(Tile, NeighborDirection, What), % get the type of the edge of the tile
    opposite(NeighborDirection, OppositeDirection), % get the opposite direction
    at(NeighborTile, OppositeDirection, What). % check if the opposite edge of the neighbor tile is the same as the edge of the tile



% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
%
% Predicatul leagă Rotation la rotația (între 0 și 3 inclusiv) pentru
% piesa cu reprezentarea Tile, astfel încât piesa să se potrivească cu
% vecinii din Neighbors.
%
% Neighbors este o listă de perechi (NeighborTile, NeighborDirection) și
% specifică că pe direcția NeighborDirection se află piesa cu
% reprezentarea NeighborTile. Este posibil ca Neighbors să conțină mai
% puțin de 4 elemente.
%
% Se vor da toate soluțiile care duc la potrivire.
%
% De exemplu, pentru piesa 11, dacă la nord se află piesa 14 rotită o
% dată (drumul este vertical), iar la sud se află piesa 2 rotită de 2
% ori (drumul este spre nord), atunci posibilele rotații pentru piesa 11
% sunt 1 sau 3, deci findRotation trebuie să aibă 2 soluții, în care
% leagă R la 1, și la 3.
% În același exemplu, dacă am avea și piesa 1 ca vecin spre est, atunci
% soluția de mai sus s-ar reduce doar la rotația 3.
%
% Hint: Prolog face backtracking automat. Folosiți match/3.

% matches_all_neighbors/3
% matches_all_neighbors(+RotatedTile, +Neighbors, +Rotation)
matches_all_neighbors(_, [], _). % base case
matches_all_neighbors(RotatedTile, [(NeighborTile, NeighborDirection) | RestOfNeighbors], Rotation) :- 
    match(RotatedTile, NeighborTile, NeighborDirection), % check if the rotated tile matches the neighbor tile
    matches_all_neighbors(RotatedTile, RestOfNeighbors, Rotation). % call the function again with the rest of the neighbors

% findRotation/3
% findRotation(+Tile, +Neighbors, -Rotation)
findRotation(Tile, Neighbors, NumberOfRotations) :-
    rotations(Tile, RotationPairs), % get all the rotations of the tile
    member((NumberOfRotations, RotatedTile), RotationPairs), % check if the pair (NumberOfRotations, RotatedTile) is in the list of rotations
    matches_all_neighbors(RotatedTile, Neighbors, NumberOfRotations). % check if the rotated tile matches all the neighbors




