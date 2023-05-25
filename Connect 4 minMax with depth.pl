player(b).
player(r).
opositePlayer(r, b):-!.
opositePlayer(b, r).
minMax(Board, Player, Column):-
    minMaxHelper(Board, Player, 0, [[0, 0], [0, 0]], 0, _, Column, 0, 50, _).
minMaxHelper(_, _, Index, Score, Col, Score, Col, _, Steps, Steps):-
    Index > 4, !.
minMaxHelper(Board, Player, Index, PScore, PCol, Score, Colomn,InitialSteps, Steps, StepsO):-
    addPiece(Board, Index, Added, Player, _),
    Index2 is Index + 1,
    Added = Board -> (minMaxHelper(Board, Player, Index2, PScore, PCol, Score, Colomn,InitialSteps, Steps, StepsO)),!.
minMaxHelper(Board, Player, Index, [PScoreMe, PScoreOp], PCol, Score, Colomn, InitialSteps, StepsBro, StepsO):-
    addPiece(Board, Index, Added, Player, _),
    count(Added, Player, Score1),
    (status(Score1, Status), Status = unKnown, \+fill(Added)),
    opositePlayer(Player, Player2),
    Index2 is Index + 1,
    InitialSteps2 is InitialSteps + 1,
    ((InitialSteps2 < 5) ->
    (minMaxHelper(Added, Player2, 0, [[0, 0], [0, 0]], 0, [ScoreOp, ScoreMe], _, InitialSteps2, 50, Steps2),
     (Score2 = [ScoreMe, ScoreOp]), !
     ); (Score2 = Score1, Steps2 = InitialSteps2)),
    best([PScoreMe, PScoreOp], PCol, Score2, Index, Score3, Col3, StepsBro, Steps2, Steps),
    ((status(Score3, Status), Status \= win) ->
    (minMaxHelper(Board, Player, Index2, Score3, Col3, Score, Colomn, InitialSteps, Steps, StepsO), !);
    (Score = Score3, Colomn = Col3, StepsO = Steps)),!.
minMaxHelper(Board, Player, Index, PScore, PCol, Score, Colomn, InitialSteps, StepsBro, StepsO):-
    addPiece(Board, Index, Added, Player, _),
    count(Added, Player, Score1),
    best(PScore, PCol, Score1, Index, Score3, Col3, StepsBro, InitialSteps, Steps),
    Index2 is Index + 1,
    ((status(Score3, Status), Status \= win) -> (
         minMaxHelper(Board, Player, Index2, Score3, Col3, Score, Colomn, InitialSteps, Steps, StepsO), !);
    (Score = Score3, Colomn = Col3, StepsO = Steps)).

status([[S, _],[_, _]], win):- S >= 4, !.
status([[_, _], [S, _]], lose):- S >= 4, !.
status(_,  unKnown).

best([[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col1, [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col2, Score, Col, Steps1, Steps2, Steps ):-
    ((ScoreMe1 >= 4, ScoreMe2 < 4) -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1, !));
    ((ScoreMe2 >= 4, ScoreMe1 < 4) -> (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2, !));
    ((ScoreOp2 >= 4, ScoreOp1 < 4) -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1, !));
    ((ScoreOp1 >= 4, ScoreOp2 < 4) -> (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2, !));
    ((ScoreMe1 >= 4, ScoreMe2 >= 4) -> (Steps2 < Steps1 ->
                                     (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2, !);
                                     (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1)
                                     ), !
    );
    ((ScoreOp1 >= 4, ScoreOp2 >= 4) -> (Steps2 >= Steps1 ->
                                     (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2, !);
                                     (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1)
                                     ), !
    ).
best([[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col1, [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col2, Score, Col, Steps1, Steps2, Steps ):-
    Diff1 is (ScoreMe1 - ScoreOp1), Diff2 is (ScoreMe2 - ScoreOp2),
    (
    (Diff1 > Diff2 -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1), !);
    (Diff1 = Diff2 -> (Diff3 is (N_me1 - N_op1), Diff4 is (N_me2 - N_op2),
                      (
                      (Diff3 > Diff4 -> Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1, !);
                      (Diff3 < Diff4 -> Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2, !);
                      (Steps1 < Steps2 ->
                      (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col = Col1, Steps = Steps1, !));
                      (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2, !)
                      )
                      )
    )
    );
    Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col = Col2, Steps = Steps2.
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

%Score, Max, Score openent, Max

count(Board, Player, [[Max, N], [Op_Max, Op_N]]):-
    countHelper(Board, Player, -100, Max1, 0, N1),
    transpose(Board, Trans),
    countHelper(Trans, Player, Max1, Max, N1, N),
    opositePlayer(Player, Op),
    countHelper(Board, Op, -100, Max2, 0, N2),
    transpose(Board, Trans),
    countHelper(Trans, Op, Max2, Op_Max, N2, Op_N).
countHelper([R|Remain], Player, Max1, Max, N1, N):-
    countRow(R, Remain, Player, 0, Max1, Max2, N1, N2),
    countHelper(Remain, Player, Max2, Max, N2, N).
countHelper([], _, M, M, N, N).
countRow([H|T], Remain, H, Index, Max1, Max, N1, N):-
    countH(T, H, 1, C1),
    countD(H, Index, Remain, 1, 1, C2),
    countD(H, Index, Remain, -1, 1, C3),
    Index2 is Index + 1,
    getMax([C1, C2, C3], Max1, Max2, N1, N2),
    countRow(T, Remain, H, Index2, Max2, Max, N2, N), !.
countRow([_|T], Remain, Player, Index, Max1, Max, N1, N):-
    Index2 is Index + 1,
    countRow(T, Remain, Player, Index2, Max1, Max, N1, N), !.
countRow([], _, _, _, Max, Max, N, N).
getMax([H|T], Max1, Max, N1, N):-
    (
    (H > Max1 -> (Max2 = H, N2 = 1), !);
    (Max1 > H -> (Max2 = Max1, N2 = 1), !);
    (Max2 = H, N2 is N1 + 1)
    ),
    getMax(T, Max2, Max, N2, N).
getMax([], Max, Max, N, N).
countH([H|T], H, C, Res):-
    player(H), C2 is C + 1, countH(T, H, C2, Res), !.
countH(_, _, Res, Res).

countD(Player, Index, [Row|T], Change, Count, Res):-
    Tar is Index + Change,
    getInRow(Row, 0, Tar, Player),
    Count2 is Count + 1,
    countD(Player, Tar, T, Change, Count2, Res), !.
countD(_, _, _, _, Res, Res).






