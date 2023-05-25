player(b).
player(r).

opositePlayer(r, b):-!.
opositePlayer(b, r).

minMax(Board, Player, D, Column):-
    minMaxHelper(Board, Player, 0, [n, n], n, _, Column, D, 0, 50, _).
minMaxHelper(_, _, Index, Score, Col, Score, Col, _, _, Steps, Steps):-
    Index > 6, !.
minMaxHelper(Board, Player, Index, PScore, PCol, Score, Colomn, D,InitialSteps, Steps, StepsO):-
    addPiece(Board, Index, Added, Player, _),
    Index2 is Index + 1,
    Added = Board, minMaxHelper(Board, Player, Index2, PScore, PCol, Score, Colomn, D,InitialSteps, Steps, StepsO), !.
minMaxHelper(Board, Player, Index, [PScoreMe, PScoreOp], PCol, Score, Colomn, D, InitialSteps, StepsBro, StepsO):-
    addPiece(Board, Index, Added, Player, _),
    count(Added, Player, Score1),
    (status(Score1, Status), Status = unKnown, \+fill(Added)),
    opositePlayer(Player, Player2),
    Index2 is Index + 1,
    InitialSteps2 is InitialSteps + 1,
    ((InitialSteps2 < D) ->
    (minMaxHelper(Added, Player2, 0, [n, n], n, [ScoreOp, ScoreMe], _, D, InitialSteps2, 50, Steps2),
     ([ScoreMe2, ScoreOp2] = [ScoreMe, ScoreOp]), !
     ); ([ScoreMe2, ScoreOp2] = Score1, Steps2 = InitialSteps)),
    best([PScoreMe, PScoreOp, StepsBro], PCol, [ScoreMe2, ScoreOp2, Steps2], Index, [ScoreMe3, ScoreOp3, Steps3], Col3),
    ((status([ScoreMe3, ScoreOp3], Status), Status \= win) ->
    (minMaxHelper(Board, Player, Index2, [ScoreMe3, ScoreOp3], Col3, Score, Colomn, D, InitialSteps, Steps3, StepsO), !);
    (Score = [ScoreMe3, ScoreOp3], Colomn = Col3, StepsO = Steps3)),!.

minMaxHelper(Board, Player, Index, [PScoreMe, PScoreOp], PCol, Score, Colomn, D, InitialSteps, StepsBro, StepsO):-
    addPiece(Board, Index, Added, Player, _),
    count(Added, Player, [ScoreMe1, ScoreOp1]),
    best([PScoreMe, PScoreOp, StepsBro], PCol, [ScoreMe1, ScoreOp1, InitialSteps], Index,[ScoreMe3, ScoreOp3, Steps], Col3),
    Index2 is Index + 1,
    ((status([ScoreMe3, ScoreOp3], Status), Status \= win) -> (
          minMaxHelper(Board, Player, Index2, [ScoreMe3, ScoreOp3], Col3, Score, Colomn, D, InitialSteps, Steps, StepsO), !);
    (Score = [ScoreMe3, ScoreOp3], Colomn = Col3, StepsO = Steps)).

next_state(Board, Player, Added, Index):-
    (Index is 0, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 1, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 2, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 3, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 4, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 5, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 6, addPiece(Board, Index, Added, Player, _) , (Board\=Added)).

alpha_beta(Board, Player, Column):-
    Alpha = [n, n, n],
    Beta = [n, n, n],
    findall([V,Column] , (next_state(Board, Player, Next, Column), min_value(Next, Player, Player, Alpha, Beta, 1, V)) , Choices),
    column_of_choice(Choices,[_,Column]).
column_of_choice([], [_, null, _]).
column_of_choice([X], X):-!.
column_of_choice([[V,Col]|T], [V_Res, Col_Res]):-
    column_of_choice(T, [Rest_V, Rest_Col]),
    best(V, Col, Rest_V,Rest_Col, V_Res, Col_Res).


max_value(
    Board,  %in
    Player, %in
    _,
    _,      %in
    _,      %in
    Steps,
    [M, O, Steps]       %out
):-
    count(Board, Player, [M, O]), status([M, O], Status), ((Status \= unKnown, !);fill(Board)), !.

max_value(
    Board,  %in
    Player, %in
    Player2,
    Alpha,  %in
    Beta,   %in
    Steps,
    V       %out
):-
    count(Board, Player, [M, O]),
    opositePlayer(Player2, Player3),
    findall(Next,next_state(Board, Player3, Next,_),Children),
    Steps2 is Steps + 1,
    ((Steps < 6,
    loop_over_children_min(Children, Player, Player3, Alpha, Beta, [n, n, n], Steps2, V), !); V = [M, O, Steps2]), !.

loop_over_children_min([], _, _,  _,_, V, _, V) :- !.
loop_over_children_min([H|T], Player, Player2, Alpha, Beta, V, Steps, V_Result) :-
    min_value(H, Player, Player2, Alpha, Beta, Steps, V_dash),
    best(V_dash, _, V, _, Max, _),
    (
        (best(V_dash, _, Beta, _, V_dash, _), Beta \= [n, n, n]) ->
        (   V_Result = Max, !)
        ;
        (
            best(Alpha, _, V_dash, _, New_alpha, _),
            loop_over_children_min(T, Player, Player2, New_alpha, Beta, Max, Steps, V_Result)
        )
    ).

min_value(
    Board,  %in
    Player, %in
    _,
    _,      %in
    _,      %in
    Steps,
    [M, O, Steps]       %out
):-
    count(Board, Player, [M, O]), status([M, O], Status), ((Status \= unKnown, !);fill(Board)), !.

min_value(
    Board,  %in
    Player, %in
    Player2,
    Alpha,  %in
    Beta,   %in
    Steps,
    V       %out
):-
    count(Board, Player, [M, O]),
    opositePlayer(Player2, Player3),
    findall(Next,next_state(Board, Player3, Next,_),Children),
    Steps2 is Steps + 1,
    ((Steps2 < 6,
    loop_over_children_max(Children, Player, Player3, Alpha, Beta, [n, n, n], Steps2, V), !); V = [M, O, Steps2]), !.
loop_over_children_max([], _, _,  _,_, V, _, V) :- !.
loop_over_children_max([H|T], Player, Player2, Alpha, Beta, V, Steps, V_Result) :-
    max_value(H, Player, Player2, Alpha, Beta, Steps, V_dash),
    worst(V_dash, V, Min),
    (
        best(Alpha, _, V_dash, _, V_dash, _) ->
        (
            worst(V_dash, Beta, New_beta),
            loop_over_children_max(T, Player, Player2, Alpha, New_beta, Min, Steps, V_Result), !
        )
        ;
        (V_Result = V)
    ).
best([n,n, _], _, V, Col, V, Col):-!.
best(V, Col, [n, n, _], _, V, Col):-!.
best([[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col1,[[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col2, Score, Col):-
    ((ScoreMe1 >= 4, ScoreMe2 < 4) -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1, !));
    ((ScoreMe2 >= 4, ScoreMe1 < 4) -> (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2, !));
    ((ScoreOp2 >= 4, ScoreOp1 < 4) -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1, !));
    ((ScoreOp1 >= 4, ScoreOp2 < 4) -> (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2, !));
    ((ScoreMe1 >= 4, ScoreMe2 >= 4) -> (Steps2 < Steps1 ->
                                     (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2,  !);
                                     (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1)
                                     ), !
    );
    ((ScoreOp1 >= 4, ScoreOp2 >= 4) -> (Steps2 >= Steps1 ->
                                     (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2, !);
                                     (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1)
                                     ), !
    ).
best([[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col1, [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col2, Score, Col):-
    Diff1 is (ScoreMe1 - ScoreOp1), Diff2 is (ScoreMe2 - ScoreOp2),
    (
    ((N_me1 = 100, N_me2 \= 100) -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1), !);
    ((N_me2 = 100, N_me1 \= 100) -> (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2), !);
    ((N_op2 = 100, N_op1 \= 100) -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1), !);
    ((N_op1 = 100, N_op2 \= 100) -> (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2), !);
    ((N_me1 = 100, N_me2 = 100) -> (Steps2 < Steps1 ->
                                     (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2,  !);
                                     (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1)
                                     ), !
    );
    ((N_op1 = 100, N_op2 = 100) -> (Steps2 >= Steps1 ->
                                     (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2, !);
                                     (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1)
                                     ), !
    );
    (Diff1 > Diff2 -> (Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1), !);
    (Diff1 = Diff2 -> (Diff3 is (N_me1 - N_op1), Diff4 is (N_me2 - N_op2),
                      (
                      (Diff3 > Diff4 -> Score = [[ScoreMe1, N_me1], [ScoreOp1, N_op1], Steps1], Col = Col1, !);
                      (Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2, !)
                      )
                      )
    )
    );
    Score = [[ScoreMe2, N_me2], [ScoreOp2, N_op2], Steps2], Col = Col2.
worst([Me1, OP1, Steps1], [Me2, OP2, Steps2], [Me, OP, Steps]):-
    best([OP1, Me1, Steps1], _, [OP2, Me2, Steps2], _, [OP, Me, Steps], _).

count(Board, Player, [[Max, N], [Op_Max, Op_N]]):-
    countHelper(Board, Board, Player, 0, Max1, 0, N1, 0),
    transpose(Board, Trans),
    countHelper(Trans, Trans, Player, Max1, Max, N1, N, 0),
    opositePlayer(Player, Op),
    countHelper(Board, Board, Op, 0, Max2, 0, N2, 0),
    transpose(Board, Trans),
    countHelper(Trans, Trans, Op, Max2, Op_Max, N2, Op_N, 0).
countHelper(Board, [R|Remain], Player, Max1, Max, N1, N, I):-
    countRow(Board, R, Remain, Player, Max1, Max2, N1, N2, I, 0),
    I2 is I + 1,
    countHelper(Board, Remain, Player, Max2, Max, N2, N, I2).
countHelper(_, [], _, M, M, N, N, _).
countRow(Board, [H|T], Remain, H, Max1, Max, N1, N, I, J):-
    (Max1 >= 4 -> (Max = Max1, N = N1, !));
    (countH(T, H, 1, C1, Next1),
    PJH is J - 1, getIn(Board, 0, I, PJH, PCH),
    ((PCH \= w, Next1 \= w, C1 < 4) -> (C12 is 0, !); C12 = C1, !),
    countD(H, J, Remain, 1, 1, C2, Next2),
    PID is I - 1, PJD is J - 1, getIn(Board, 0, PID, PJD, PCD),
    ((PCD \= w, Next2 \= w, C2 < 4) -> (C22 is 0, !); C22 = C2, !),
    countD(H, J, Remain, -1, 1, C3, Next3),
    PID2 is I + 1, PJD2 is J + 1, getIn(Board, 0, PID2, PJD2, PCD2),
    ((PCD2 \= w, Next3 \= w, C3 < 4) -> (C32 is 0, !); C32 = C3, !),
    ((
       ((C1 >= 4, !); (C2 >= 4, !); (C3 >= 4, !)) -> (Max = 4, N = 1, !);
       (Max1 < 4, C1 = 3, PCH = w, Next1 = w) -> (Max = 3, N = 100, !),
       !);
    (getMax([C12, C22, C32], Max1, Max2, N1, N2),
    J2 is J + 1,
    countRow(Board, T, Remain, H, Max2, Max, N2, N, I, J2), !))), !.
countRow(Board, [_|T], Remain, Player, Max1, Max, N1, N, I, J):-
    J2 is J + 1,
    countRow(Board, T, Remain, Player, Max1, Max, N1, N, I, J2), !.
countRow(_, [], _, _, Max, Max, N, N, _, _).
getMax([H|T], Max1, Max, N1, N):-
    (
    (H > Max1 -> (Max2 = H, N2 = 1), !);
    (Max1 > H -> (Max2 = Max1, N2 = 1), !);
    (Max2 = H, N2 is N1 + 1)
    ),
    getMax(T, Max2, Max, N2, N).
getMax([], Max, Max, N, N).
countH([H|T], H, C, Res, N):-
    player(H), C2 is C + 1, countH(T, H, C2, Res, N), !.
countH([H|_], _, Res, Res, H):-!.
countH(_, _, Res, Res, n).
countD(Player, Index, [Row|T], Change, Count, Res, N):-
    Tar is Index + Change,
    getInRow(Row, 0, Tar, Player),
    Count2 is Count + 1,
    countD(Player, Tar, T, Change, Count2, Res, N), !.
countD(_, Tar, [R|_], Change, R, R, H):-
    Tar2 is Tar + Change, getInRow(R, 0, Tar2, H), !.
countD(_, _, _, _, Res, Res, n).
status([[S, _],[_, _]], win):- S >= 4, !.
status([[_, _], [S, _]], lose):- S >= 4, !.
status(_,  unKnown).


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
getIn(_, _, I, J, n):-
    ((I < 0, !); J < 0), !.
getIn([H|_], I, I, J, C):-
    getInRow(H, 0, J, C), !.
getIn([_|T], I1, I, J, C):-
    I2 is I1 + 1,
    getIn(T, I2, I, J, C), !.
getIn([], _,_,_,n).
getInRow([Color|_], Tar, Tar, Color):-!.
getInRow([_|T], J, Tar, Color):-
    J2 is J + 1,
    getInRow(T, J2, Tar, Color), !.
getInRow([], _, _, n).
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














