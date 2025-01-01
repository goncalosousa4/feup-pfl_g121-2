% Display the game state
display_game(game_state(Board, CurrentPlayer, _Players, Pawns, PrevMove)) :-
    nl, write('Current Player: '), write(CurrentPlayer), nl,
    display_board(Board),
    nl, write('Pawns: '), write(Pawns), nl,
    write('Previous Move: '), write(PrevMove), nl.

display_board(Board) :-
    write('Board: '), nl,
    display_rows(Board, 1).

display_rows([], _).
display_rows([Row | Rest], RowNumber) :-
    write(RowNumber), write(' | '),
    display_row(Row), nl,
    NextRow is RowNumber + 1,
    display_rows(Rest, NextRow).

display_row([]).
display_row([Stack | Rest]) :-
    display_stack(Stack),
    write('  '), % Space between stacks
    display_row(Rest).

display_stack([]) :- write('.').  % Empty cell
display_stack([pawn(Player, Number) | _]) :-
    write('P'), write(Player), write('_'), write(Number).  % Show the pawn details


% Game initialization
play :-
    initial_state([player1, player2, 5], GameState),
    game_cycle(GameState).

initial_state([Player1, Player2, Size], game_state(Board, Player1, [Player1, Player2], Pawns, no_prev_move)) :-
    create_initial_board(Size, Board),
    initialize_pawns(Player1, Player2, Pawns, Board).

create_initial_board(Size, Board) :-
    length(Board, Size),
    maplist(create_row(Size), Board).

create_row(Size, Row) :-
    length(Row, Size),
    maplist(=([]), Row).

initialize_pawns(Player1, Player2, 
                 [pawns(Player1, [[1, 5], [5, 5]]), 
                  pawns(Player2, [[1, 1], [5, 1]])], 
                 Board) :-
    place_pawn(Board, 1, 5, pawn(Player1, 1), Board1),
    place_pawn(Board1, 5, 5, pawn(Player1, 2), Board2),
    place_pawn(Board2, 1, 1, pawn(Player2, 1), Board3),
    place_pawn(Board3, 5, 1, pawn(Player2, 2), _).

place_pawn(Board, Row, Col, Pawn, NewBoard) :-
    nth1(Row, Board, OldRow, RestRows),
    nth1(Col, OldRow, OldStack, RestCells),
    append([Pawn], OldStack, NewStack),
    nth1(Col, NewRow, NewStack, RestCells),
    nth1(Row, NewBoard, NewRow, RestRows).

% Game cycle
game_cycle(GameState) :-
    display_game(GameState),
    (   check_winner(GameState) ->
        nl, write('Game Over!'), nl
    ;   make_move(GameState, NewGameState),
        game_cycle(NewGameState)
    ).

% Check winner (stub)
check_winner(_) :- fail.

% Make a move
make_move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), NewGameState) :-
    write('Choose your pawn index (1 or 2): '),
    read(PawnIndex),
    write('Enter your move (NewRow, NewCol): '),
    read((NewRow, NewCol)),
    (   move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), 
             (PawnIndex, NewRow, NewCol), NewGameState) ->
        true
    ;   write('Invalid move, try again.'), nl,
        make_move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), NewGameState)
    ).

move(game_state(Board, CurrentPlayer, Players, Pawns, _PrevMove), 
     (PawnIndex, NewRow, NewCol), 
     game_state(NewBoard, NextPlayer, Players, NewPawns, (NewRow, NewCol))) :-
    select_pawn(CurrentPlayer, Pawns, PawnIndex, [CurrRow, CurrCol]), % Get current position
    validate_move(Board, [CurrRow, CurrCol], [NewRow, NewCol]),       % Validate the move
    update_board(Board, CurrRow, CurrCol, [], TempBoard),             % Remove pawn from old position
    update_board(TempBoard, NewRow, NewCol, [pawn(CurrentPlayer, PawnIndex)], NewBoard), % Place at new position
    update_pawns(Pawns, CurrentPlayer, PawnIndex, [NewRow, NewCol], NewPawns), % Update pawns list
    switch_player(CurrentPlayer, Players, NextPlayer).               % Switch turn to the next player


% Select the N-th pawn for the current player
select_pawn(Player, [pawns(Player, PawnList) | _], Index, Pos) :-
    (nth1(Index, PawnList, Pos) -> true ; fail).
select_pawn(Player, [_ | Rest], Index, Pos) :-
    select_pawn(Player, Rest, Index, Pos).

validate_move(Board, [CurrRow, CurrCol], [NewRow, NewCol]) :-
    length(Board, Size),
    NewRow > 0, NewRow =< Size,
    NewCol > 0, NewCol =< Size,
    abs(NewRow - CurrRow) =< 1, abs(NewCol - CurrCol) =< 1.

% Update the board with the move
update_board(Board, Row, Col, Value, NewBoard) :-
    nth1(Row, Board, OldRow, RestRows),    % Extract the target row
    nth1(Col, OldRow, _, RestCells),       % Extract the target column
    nth1(Col, NewRow, Value, RestCells),  % Replace the column with the new value
    nth1(Row, NewBoard, NewRow, RestRows). % Replace the row in the board


update_pawns([pawns(Player, PawnList) | Rest], Player, Index, NewPos, 
             [pawns(Player, NewPawnList) | Rest]) :-
    nth1(Index, PawnList, _, RestPawns),  % Remove the old position
    nth1(Index, NewPawnList, NewPos, RestPawns).  % Insert the new position

update_pawns([Other | Rest], Player, Index, NewPos, [Other | NewRest]) :-
    update_pawns(Rest, Player, Index, NewPos, NewRest).  % Recurse for other players

% Switch players
switch_player(CurrentPlayer, [CurrentPlayer, OtherPlayer], OtherPlayer).
switch_player(CurrentPlayer, [OtherPlayer, CurrentPlayer], OtherPlayer).

% Get the N-th element from a list (1-based indexing)
nth1(1, [Head | _], Head).
nth1(N, [_ | Tail], Elem) :-
    N > 1,
    N1 is N - 1,
    nth1(N1, Tail, Elem).
