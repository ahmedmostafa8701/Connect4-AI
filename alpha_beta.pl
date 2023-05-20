player(b).
player(r).

opositePlayer(r, b):-!.
opositePlayer(b, r).

next_state(Board, Player, Added, Index):-
    (Index is 0, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 1, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 2, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 3, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 4, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 5, addPiece(Board, Index, Added, Player, _) , (Board\=Added));
    (Index is 6, addPiece(Board, Index, Added, Player, _) , (Board\=Added)).

alpha_beta(Board, Player, Column):-
    Alpha = -10,
    Beta = 10,
    findall([V,Column, Steps] , (next_state(Board, Player, Next, Column), min_value(Next, Player, Player, Alpha, Beta, 1, V,Steps)) , Choices),
    column_of_choice(Choices,[_,Column, _]).
column_of_choice([], [_, null, _]).
column_of_choice([[V,Col, Steps]], [V,Col, Steps]):-!.
column_of_choice([[V,Col, Steps]|T], [V_Res, Col_Res, Steps_Res]):-
    column_of_choice(T, [Rest_V, Rest_Col, Rest_Steps]),
    best(V, Col, Steps, Rest_V,Rest_Col, Rest_Steps, V_Res, Col_Res, Steps_Res).


max_value(
    Board,  %in
    Player, %in
    _,
    _,      %in
    _,      %in
    Steps,
    V       %out
    , Steps):-
    count(Board, Player, V), status(V, Status), ((Status \= unKnown, !);fill(Board)), !.

max_value(
    Board,  %in
    Player, %in
    Player2,
    Alpha,  %in
    Beta,   %in
    Steps,
    V,       %out
    StepsO):-
    opositePlayer(Player2, Player3),
    findall(Next,next_state(Board, Player3, Next,_),Children),
    Steps2 is Steps + 1,
    loop_over_children_min(Children, Player, Player2, Alpha, Beta, [n, n], Steps2, n, StepsO, V), !.
max_value(Board, Player, _, _, _, S, V, S):-
    count(Board, Player, V).

loop_over_children_min([], _, _,  _,_, V, _, Steps, Steps, V) :- !.
loop_over_children_min([H|T], Player, Player2, Alpha, Beta, [Me, Op], InitialSteps, BroSteps, Steps_Result, V_Result) :-
    min_value(H, Player, Player2, Alpha, Beta, InitialSteps, [Me2, Op2], Steps_dash),
    best([Me, Op], _, BroSteps, [Me2, Op2], _, Steps_dash, [Me, Op], _, Shortest_Steps),
    getV([Me, Op], V_dash),
    (
        V_dash >= Beta ->
        (   V_Result = [Me, Op] , Steps_Result = Shortest_Steps,!)
        ;
        (
            max(Alpha, V_dash, New_alpha),
            loop_over_children_min(T, Player, Player2, New_alpha, Beta, [Me, Op], InitialSteps, Shortest_Steps, Steps_Result, V_Result)
        )
    ).

getV([[ScoreMe, N_me], [ScoreOp, N_op]], V):-
    V1 = ScoreMe - ScoreOp,
    (status([[ScoreMe, N_me], [ScoreOp, N_op]], Status),
    (Status = win -> V is (V1 + 4), !);(Status = lose -> V is (V1 - 4), !); V = V1).
max(N1,N2,Max):-
    (N1 >= N2 ->
        (Max = N1, !)
    ;
        (Max = N2)
    ).

min_value(
    Board,  %in
    Player, %in
    _,
    _,      %in
    _,      %in
    Steps,
    V       %out
    , Steps):-
    count(Board, Player, V), status(V, Status), ((Status \= unKnown, !);fill(Board)), !.

min_value(
    Board,  %in
    Player, %in
    Player2,
    Alpha,  %in
    Beta,   %in
    Steps,
    V,       %out
    StepsO):-
    opositePlayer(Player2, Player3),
    findall(Next,next_state(Board, Player3, Next,_),Children),
    Steps2 is Steps + 1,
    loop_over_children_max(Children, Player, Player2, Alpha, Beta, [n, n], Steps2, n, StepsO, V), !.
min_value(Board, Player, _, _, _, S, V, S):-
    count(Board, Player, V).
loop_over_children_max([], _, _,_, _, V, _, Steps, Steps, V) :- !.
loop_over_children_max([H|T], Player, Player2, Alpha, Beta, [Me1, Op1], InitialSteps, BroSteps, Steps_Result, V_Result) :-
    max_value(H, Player, Player2, Alpha, Beta, InitialSteps, [Me2, Op2], Steps_dash),
    best([Op1, Me1], _, BroSteps, [Op2, Me2], _, Steps_dash, [Op, Me], _, Shortest_Steps),
    getV([Me, Op], V2)
    ,getV([Me2, Op2], V),
    (
        Alpha < V ->
        (
            min(V2, Beta, New_beta),
            loop_over_children_max(T, Player, Player2, Alpha, New_beta, [Me, Op],InitialSteps, Shortest_Steps, Steps_Result, V_Result), !
        )
        ;
        (V_Result = [Me, Op], Steps_Result = BroSteps)
    ).
best([n,n], _, _, Score, Col, Steps, Score, Col, Steps).
best([[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col1, Steps1,[[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col2, Steps2, Score, Col, Steps ):-
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
best([[ScoreMe1, N_me1], [ScoreOp1, N_op1]], Col1, Steps1, [[ScoreMe2, N_me2], [ScoreOp2, N_op2]], Col2, Steps2, Score, Col, Steps ):-
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

min(N1,N2,Min):-
    ((N2 >= N1) ->
        (Min = N1, !)
    ;
        Min = N2
    ).


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

getInRow([Color|_], I, Tar, Color):-
    I = Tar, !.
getInRow([_|T], I, Tar, Color):-
    I2 is I + 1,
    getInRow(T, I2, Tar, Color).
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














