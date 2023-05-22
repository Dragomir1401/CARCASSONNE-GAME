:- dynamic detailed_mode_disabled/0.
:- ensure_loaded('files.pl').


%% TODO
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



%% TODO
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



%% TODO
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



%% TODO
% hasTwoCitadels/1
% hasTwoCitadels(+Tile)
%
% Predicatul întoarce adevărat dacă pe piesă există două cetăți diferite
% (ca în piesele 4 și 5).
hasTwoCitadels(tile(_, Entities)) :-
    member(c1, Entities), % check to see if c1 is in Tile Entities
    member(c2, Entities), % check to see if c2 is in Tile Entities
    c1 \= c2. % check to see if c1 is different than c2



%% TODO
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



%% TODO
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




%% TODO
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




%% TODO
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



%%%%%%%%%%%%%%%%%%%%%%%%%% Etapa 2


%% TODO
% emptyBoard/1
% emptyBoard(-Board)
% emptyBoard(Board) :-
%     emptyTile(E),
%     repeat(E, 4, Row), % create an empty row
%     repeat(Row, 4, Board). % create the board with 8 rows  
emptyBoard([]).




%% TODO
% boardSet/4
% boardSet(+BoardIn, +Pos, +Tile, -BoardOut)
%
% Predicatul întoarce false dacă se încearcă plasarea unei piese pe o
% poziție pe care este deja o piesă, pe o poziție fără muchie comună
% cu o piesă existentă, sau într-un loc unde piesa nu se potrivește cu
% vecinii săi.
%
% Pentru o tablă goală, predicatul reușește întotdeauna, și poziția Pos
% devine singura de pe tablă.
%
% Poziția este dată ca un tuplu (X, Y).

% boardSet/4
% boardSet(+BoardIn, +Pos, +Tile, -BoardOut)
boardSet(emptyBoard, Pos, Tile, [(Pos, Tile)]).
boardSet(BoardIn, Pos, Tile, BoardOut) :-
    \+ member((Pos, _), BoardIn), % check if the position is not already occupied
    fitsWithNeighbors(BoardIn, Pos, Tile), % check if the tile fits with the neighbors
    placeTile(BoardIn, Pos, Tile, BoardOut). % place the tile on the board

% placeTile/4
% placeTile(+BoardIn, +Pos, +Tile, -BoardOut)
placeTile(BoardIn, Pos, Tile, [(Pos, Tile) | BoardIn]).

% findall/3
% findall(+Template, +Goal, -Bag)
% findall/3 is true when Bag is the list of all instances of Template that make Goal provable.

%% TODO
% boardGet/3
% boardGet(+Board, +Pos, -Tile)
%
% Predicatul leagă Tile la reprezentarea piesei de pe tabla Board, de la
% poziția Pos. Poziția este dată ca un tuplu (X, Y).
%
% Dacă la poziția Pos nu este nicio piesă, predicatul eșuează.
% boardGet/3
% boardGet(+Board, +Pos, -Tile)
boardGet([(Pos, Tile) | _], Pos, Tile). % base case
boardGet([_ | Rest], Pos, Tile) :- 
    boardGet(Rest, Pos, Tile). % call the function again with the rest of the board


%% TODO
% boardGetLimits/5
% boardGetLimits(+Board, -XMin, -Ymin, -XMax, -YMax)
%
% Predicatul leagă cele 4 argumente la coordonatele x-y extreme la
% care există piese pe tablă.
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: max_list/2 și min_list/2
boardGetLimits(Board, XMin, YMin, XMax, YMax) :-
    findall(X, (boardGet(Board, (X, _), _)), Xs), % get all the Xs
    findall(Y, (boardGet(Board, (_, Y), _)), Ys), % get all the Ys
    max_list(Xs, XMax), % get the max X
    min_list(Xs, XMin), % get the min X
    max_list(Ys, YMax), % get the max Y
    min_list(Ys, YMin). % get the min Y


%% TODO
% canPlaceTile/3
% canPlaceTile(+Board, +Pos, +Tile)
%
% Întoarce adevărat dacă este o mișcare validă plasarea piese Tile la
% poziția Pos pe tabla Board. Poziția este dată ca un tuplu (X, Y).
%
% O mișcare este validă dacă tabla este goală sau dacă:
% - poziția este liberă;
% - poziția este adiacentă (are o muchie comună) cu o piesă deja
% existentă pe tablă;
% - piesa se potrivește cu toți vecinii deja existenți pe tablă.
%
% Hint: neighbor/3 și directions/1 , ambele din utils.pl

canPlaceTile(emptyBoard, _, _).
canPlaceTile(Board, Pos, Tile) :-
    \+ member((Pos, _), Board), % check if the position is not already occupied
    getAvailablePositions(Board, AvailablePositions), % get all the available positions
    member(Pos, AvailablePositions), % check if the position is available
    fitsWithNeighbors(Board, Pos, Tile). % check if the tile fits with the neighbors


% fitsWithNeighbors/3
% fitsWithNeighbors(+Board, +Pos, +Tile)
fitsWithNeighbors(Board, Pos, Tile) :-
    boardGetNeighbors(Board, Pos, Neighbors), % get all the neighbors of the position
    forall(member((NeighborTile, Direction), Neighbors), match(Tile, NeighborTile, Direction)). % check if the tile matches with all the neighbors


% Define the directions in which we can find neighbors. We'll use this in boardGetNeighbors.
% directions/3
% directions(+Pos, -Direction, -NeighborPos)
direction((X, Y), n, (X, Y1)) :- Y1 is Y + 1. % north
direction((X, Y), e, (X1, Y)) :- X1 is X + 1. % east
direction((X, Y), s, (X, Y1)) :- Y1 is Y - 1. % south
direction((X, Y), w, (X1, Y)) :- X1 is X - 1. % west

% boardGetNeighbors/3
% boardGetNeighbors(+Board, +Pos, -Neighbors)
boardGetNeighbors(Board, Pos, Neighbors) :-
    findall((NeighborTile, Direction), % pairs of (neighbor tile, direction)
        (   
            direction(Pos, Direction, NeighborPos), % get the position of the neighbor
            boardGet(Board, NeighborPos, NeighborTile) % get the tile at the neighbor position
        ),
        Neighbors % store it in Neighbors
    ).

%% TODO
% getAvailablePositions/2
% getAvailablePositions(+Board, -Positions)
%
% Predicatul leagă Positions la o listă de perechi (X, Y)
% a tuturor pozițiilor de pe tabla Board unde se pot pune piese (poziții
% libere vecine pe o muchie cu piese existente pe tablă).
%
% Pentru o tablă goală, predicatul eșuează.
%
% Hint: between/3 (predefinit) și neighbor/3 din utils.pl
%
% Atenție! Și în afara limitelor curente există poziții disponibile.

% Helper predicate to get all adjacent positions to a given one.
adjacentPositions((X, Y), [(X1, Y), (X, Y1), (X2, Y), (X, Y2)]) :-
    X1 is X + 1, % +1 to east
    Y1 is Y + 1, % +1 to north
    X2 is X - 1, % -1 to west
    Y2 is Y - 1. % -1 to south

% Helper predicate to check if a position is already occupied on the board.
% isOccupied/2
% isOccupied(+Board, +Pos)
isOccupied(Board, Pos) :-
    member((Pos, _), Board).

% Helper predicate to get all occupied positions on the board.
occupiedPositions(Board, Occupied) :-
    findall(Pos, member((Pos, _), Board), Occupied). % get all the occupied positions

% Helper predicate to check if a position is adjacent to an occupied position and is free.
% adjacentAndFree/3
% adjacentAndFree(+Board, +Pos, -Adjacent)
adjacentAndFree(Board, Pos, Adjacent) :-
    adjacentPositions(Pos, AdjacentPos), % get all the adjacent positions
    member(Adjacent, AdjacentPos), % get an adjacent position
    \+ isOccupied(Board, Adjacent). % check if the position is not occupied

% Refactored main predicate.
getAvailablePositions(emptyBoard, _) :- false.
getAvailablePositions(Board, Available) :-
    \+ emptyBoard(Board), % check if the board is not empty
    occupiedPositions(Board, Occupied), % get all the occupied positions
    findall(Adjacent, (member(Pos, Occupied), adjacentAndFree(Board, Pos, Adjacent)), AvailablePositionsWithDuplicates), % get all the adjacent and free positions
    list_to_set(AvailablePositionsWithDuplicates, Available). % remove duplicates



%% TODO
% findPositionForTile/4
% findPositionForTile(+Board, +Tile, -Position, -Rotation)
%
% Predicatul are ca soluții toate posibilele plasări pe tabla Board ale
% piesei Tile, legând Position la o pereche (X, Y) care reprezintă
% poziția și Rotation la un număr între 0 și 3 inclusiv, reprezentând de
% câte ori trebuie rotită piesa ca să se potrivească.
%
% Unele piese se pot potrivi cu mai multe rotații pe aceeași poziție și
% acestea reprezintă soluții diferite ale predicatului, dar numai dacă
% rotațiile duc la rezultate diferite.
%
% Dacă tabla este goală, predicatul leagă Position la (0, 0) și Rotation
% la 0.
%
% De exemplu, dacă pe tablă se află doar piesa 11, la vest de ea piesa 9
% se potrivește cu rotația 1 sau 2 - două soluții diferite. Pentru
% plasarea la vest de piesa 11 a piesei 16 însă există o singură soluție
% - rotație 0.
%
% În ieșirea de la teste, rezultatele vor fi asamblate ca
% (X,Y):Rotation.

% findPositionForTile(emptyBoard, _, (0,0), 0).
findPositionForTile(Board, Tile, Position, Rotation) :-
    (emptyBoard(Board) -> 
        Position = (0,0), Rotation = 0 % if the board is empty, the position is (0,0) and the rotation is 0
    ;
        getAvailablePositions(Board, AvailablePositions), % get all the available positions
        member(Position, AvailablePositions), % check if the position is available
        rotations(Tile, Rotations), % get all the rotations of the tile
        member((Rotation, RotatedTile), Rotations), % get a rotation of the tile
        fitsWithNeighbors(Board, Position, RotatedTile) % check if the tile fits with the neighbors
    ).








