% Display the current game state with two separate boards
display_game(game(Board, CurrentPlayer, (P1X, P1Y), (P2X, P2Y))) :-
    write('Current Board (Stone Heights):'), nl,
    display_board(Board),
    nl,
    write('Current Board (Player Positions):'), nl,
    display_player_positions(P1X, P1Y, P2X, P2Y),
    format('Next turn: ~w~n', [CurrentPlayer]).

% Display the board with the stone heights
display_board([]).
display_board([Row|Rest]) :-
    write(Row), nl,
    display_board(Rest).

% Display the positions of the players on the board
display_player_positions(P1X, P1Y, P2X, P2Y) :-
    format('P1(~w,~w)  P2(~w,~w)~n', [P1X, P1Y, P2X, P2Y]).

% Switch between players
next_player(player1, player2).
next_player(player2, player1).

% Move a pawn and update the game state
move(game(Board, CurrentPlayer, (X, Y), (P2X, P2Y)), (NewX, NewY), game(NewBoard, NextPlayer, (NewX, NewY), (P2X, P2Y))) :-
    valid_move(Board, X, Y, NewX, NewY),
    replace_in_list(Board, Y, X, empty, TempBoard),
    replace_in_list(TempBoard, NewY, NewX, CurrentPlayer, NewBoard),
    next_player(CurrentPlayer, NextPlayer).

% Valid move for a pawn
valid_move(Board, X, Y, NewX, NewY) :-
    adjacent(X, Y, NewX, NewY),
    within_bounds(NewX, NewY),
    valid_stack_move(Board, X, Y, NewX, NewY).

% Check if the target position is adjacent
adjacent(X, Y, NewX, NewY) :-
    (NewX is X + 1; NewX is X - 1; NewX = X),
    (NewY is Y + 1; NewY is Y - 1; NewY = Y).

% Check if the position is within bounds
within_bounds(X, Y) :-
    X >= 1, X =< 5,
    Y >= 1, Y =< 5.

% Validate the stack move by checking height difference
valid_stack_move(Board, X, Y, NewX, NewY) :-
    get_element(Board, Y, X, CurrentHeight),
    get_element(Board, NewY, NewX, NewHeight),
    (NewHeight =:= CurrentHeight; NewHeight =:= CurrentHeight + 1; NewHeight =:= CurrentHeight - 1).

% Get element from board (manually accessing the board data)
get_element(Board, Row, Col, Value) :-
    nth_row(Board, Row, R),
    nth_element(R, Col, Value).

% Get a specific row from the board
nth_row([Row|_], 1, Row).
nth_row([_|Rest], N, Row) :-
    N > 1,
    N1 is N - 1,
    nth_row(Rest, N1, Row).

% Get an element from the row
nth_element([Elem|_], 1, Elem).
nth_element([_|Rest], N, Elem) :-
    N > 1,
    N1 is N - 1,
    nth_element(Rest, N1, Elem).

% Replace an element in the list
replace_in_list([Row|Rest], 1, X, Elem, [NewRow|Rest]) :-
    replace_in_row(Row, X, Elem, NewRow).
replace_in_list([Row|Rest], Pos, X, Elem, [Row|NewRest]) :-
    Pos > 1,
    NewPos is Pos - 1,
    replace_in_list(Rest, NewPos, X, Elem, NewRest).

% Replace an element in a row
replace_in_row([_|T], 1, Elem, [Elem|T]).
replace_in_row([H|T], Pos, Elem, [H|NewT]) :-
    Pos > 1,
    NewPos is Pos - 1,
    replace_in_row(T, NewPos, Elem, NewT).

% Pickup and place a stone
pickup_and_place_stone(Board, (X, Y), NewBoard) :-
    % Find the smallest unoccupied stack
    find_smallest_stack(Board, SmallestX, SmallestY),
    replace_in_list(Board, SmallestY, SmallestX, empty, TempBoard),
    NewHeight is 1, % New stone height for the placed stone
    replace_in_list(TempBoard, Y, X, NewHeight, NewBoard).

find_smallest_stack(Board, X, Y) :-
    % Find the smallest stack on the board (excluding current position)
    findall((X1, Y1, Height), (nth_row(Board, Y1, Row), nth_element(Row, X1, Height), (X1 \= 1 ; Y1 \= 1)), Heights),
    min_member(Height, Heights).

% Check if the game is over
game_over(game(Board, _CurrentPlayer, _Pawn1Position, _Pawn2Position)) :-
    \+ valid_moves(game(Board, _CurrentPlayer, _Pawn1Position, _Pawn2Position), _Moves).

% Core game loop
game_loop(GameState) :-
    display_game(GameState),
    (   game_over(GameState, Winner) 
    ->  (Winner = draw -> write('Game over! It\'s a draw.'), nl 
                        ; format('Game over! Winner: ~w~n', [Winner]))
    ;   GameState = game(_, CurrentPlayer, PawnPosition, _),
        format('~w\'s turn. Make your move (format: X,Y): ', [CurrentPlayer]),
        read((X, Y)),
        (   move(GameState, (X, Y), NewGameState)
        ->  game_loop(NewGameState)
        ;   write('Invalid move, try again.'), nl, 
            game_loop(GameState))).
