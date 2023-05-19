player(b).
player(r).
opositePlayer(r, b):-!.
opositePlayer(b, r).
minMax(Board, Colomn, Player):-
    minMaxHelper(Board, Player, 0, null, null, _, Colomn).
minMaxHelper(_, _, Index, Score, Col, Score, Col):-
    Index > 6, !.
minMaxHelper(Board, Player, Index, PScore, PCol, Score, Colomn):-
    addPiece(Board, Index, Added, Player, _),
    Index2 is Index + 1,
    Added = Board -> (minMaxHelper(Board, Player, Index2, PScore, PCol, Score, Colomn)),!.
minMaxHelper(Board, Player, Index, PScore, PCol, Score, Colomn):-
    addPiece(Board, Index, Added, Player, _),
    measureState(Added, Score1, Player),
    Score1 = unKnown,
    opositePlayer(Player, Player2),
    minMaxHelper(Added, Player2, 0, null, null, ScoreOp, _),
    Score2 is (ScoreOp * -1),
    best(PScore, PCol, Score2, Index, Score3, Col3),
    Index2 is Index + 1,
    minMaxHelper(Board, Player, Index2, Score3, Col3, Score, Colomn),!.
minMaxHelper(Board, Player, Index, PScore, PCol, Score, Colomn):-
    addPiece(Board, Index, Added, Player, _),
    measureState(Added, Score1, Player),
    Score1 \= unKnown,
    best(PScore, PCol, Score1, Index, Score3, Col3),
    Index2 is Index + 1,
    (Score1 \= 1 -> (minMaxHelper(Board, Player, Index2, Score3, Col3, Score, Colomn), !);(Score = 1, Colomn = Index)).
best(Score1, _, Score2, Col2, Score, Col):-
    Score1 = null, Score = Score2, Col = Col2, !.
best(Score1, Col1, Score2, _, Score, Col):-
    Score2 = null, Score = Score1, Col = Col1, !.
best(Score1, _, Score2, Col2, Score, Col):-
    Score2 > Score1, Score = Score2, Col = Col2, !.
best(Score1, Col1, Score2, _, Score, Col):-
    Score1 > Score2, Score = Score1, Col = Col1, !.
best(Score, Col, Score, _, Score, Col).
addPiece(Board, Row, Added, Color, Last):-
   addPieceHelper(Board, Row, 0, Added, Color, Last).
addPieceHelper([H|T], Row, Index, [H|T2], Color, Last):-
    Index < Row,
    NewIndex is Index + 1,
    addPieceHelper(T, Row, NewIndex, T2, Color, Last).
addPieceHelper([H|T], Row, Index, [H2|T], Color, Last):-
    Index = Row,
    addToRow(H, Color, Last, H2).
addToRow([w|T], Color, w,[Updated|T2]):-
    addToRow(T, Color, Updated, T2),
    !.
addToRow([H|T], Color, Color, [H|T]).
addToRow([], Color, Color, []).

measureState(Board, Score, Player):-
    win(Board, Player1),
    player(Player1),
    ((Player1 = Player) -> (Score = 1, !);
    Score = -1, !).
measureState(Board, Score, _):-
    fill(Board)->
    Score = 0, !.
measureState(_, unKnown, _).

win(Board, Player):-
    (winD(Board, Player1), player(Player1),Player = Player1, !);
    (winH(Board, Player1), player(Player1),Player = Player1, !);
    (winV(Board, Player1), player(Player1),Player = Player1).
winH([H|T], Player):-
    (winHRow(H, Player1), player(Player1),Player = Player1, !);
    winH(T, Player).
winH([], f).
winHRow([Player, Player, Player, Player|_], Player1):-
    player(Player), Player1 = Player,!.
winHRow([_|T], Player):-
    winHRow(T, Player).
winHRow([], f).
winV(Board, Player):-
    transpose(Board, Trans),
    winH(Trans, Player).
winD([H|T], Player):-
    winDRow(H, T,  Player1, 0), player(Player1) -> (Player = Player1);
    winD(T, Player).
winD([], f).
winDRow([H|_], Remain, Player, Index):-
    player(H),
    winDHelper(H, Remain, Player1, 1, Index, 1),
    player(Player1),
    Player = Player1,!.
winDRow([H|_], Remain, Player, Index):-
    player(H),
    winDHelper(H, Remain, Player1, 1, Index, -1),
    player(Player1),
    Player = Player1, !.
winDRow([w|T], Remain, Player, Index):-
    Index2 is Index + 1,
    winDRow(T, Remain, Player, Index2), !.
winDRow(_, _, f, _).
winDHelper(Color, _, Color, 4, _, _):-!.
winDHelper(Color, [H|T], Player, Count, Index, Change):-
    (Tar is Index + Change,
    (Tar > -1 , Tar < 6) ->
    (getInRow(H, 0, Tar, Color2),
    Color2 = Color,
    Count2 is Count + 1,
    winDHelper(Color, T, Player, Count2, Tar, Change), !);
    Player = f),!.
winDHelper(_, [], f, _, _, _).
getInRow([Color|_], I, Tar, Color):-
    I = Tar, !.
getInRow([_|T], I, Tar, Color):-
    I2 is I + 1,
    getInRow(T, I2, Tar, Color).
% all winner in this row
fill([H|T]):-
    fillRow(H),
    fill(T).
fill([]).
fillRow([H|T]):-
    H \= w,
    fillRow(T).
fillRow([]).
% Helper function to transpose a matrix
transpose([], []):-!.
transpose([[]|_], []):-!.
transpose(Matrix, [Row|Rows]) :-
    first_column(Matrix, Row, RestMatrix),
    transpose(RestMatrix, Rows).
first_column([], [], []):-!.
first_column([[H|T]|Rows], [H|Hs], [T|Ts]) :-
    first_column(Rows, Hs, Ts).

















