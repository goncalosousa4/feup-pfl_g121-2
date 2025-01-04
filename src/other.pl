:- use_module(library(lists)).
:- use_module(library(between)).

% Display the game state
display_game(game_state(Board, CurrentPlayer, _Players, Pawns, PrevMove)) :-
    nl, write('Current Player: '), write(CurrentPlayer), nl,
    display_board(Board),
    nl, write('Pawns: '), write(Pawns), nl,
    write('Previous Move: '), write(PrevMove), nl.

% Display cell based on content
display_cell([]) :- write('[ ]').
display_cell('o') :- write('o').
display_cell([pawn(Player, Number) | Rest]) :- 
    sub_atom(Player, 6, 1, _, PlayerLetter),
    write(PlayerLetter), write(Number),
    (Rest = ['o'] -> write('o') ; true).
display_cell(Stack) :-
    is_list(Stack),
    count_stones(Stack, N),
    N > 0,
    write(N), write('o').

% Count stones in a stack
count_stones([], 0).
count_stones(['o'|Rest], N) :-
    count_stones(Rest, N1),
    N is N1 + 1.
count_stones([pawn(_,_)|Rest], N) :-
    count_stones(Rest, N).

display_board(Board) :-
    write('Board:'), nl,
    display_rows(Board, 1).

display_rows([], _).
display_rows([Row|Rest], RowNum) :-
    write(RowNum), write(' | '),
    display_row(Row),
    nl,
    NextNum is RowNum + 1,
    display_rows(Rest, NextNum).

display_row([]).
display_row([Cell|Rest]) :-
    display_cell(Cell),
    write('    '),
    display_row(Rest).

% Game initialization
play :-
    initial_state([playerA, playerB, 5], GameState),
    game_cycle(GameState).

initial_state([PlayerA, PlayerB, Size], game_state(Board, PlayerA, [PlayerA, PlayerB], Pawns, no_prev_move)) :-
    create_initial_board(Size, EmptyBoard),
    initialize_pawns(PlayerA, PlayerB, Pawns, EmptyBoard, Board).

create_initial_board(Size, Board) :-
    length(Board, Size),
    maplist(create_row(Size), Board).

create_row(Size, Row) :-
    length(Row, Size),
    maplist(=('o'), Row).

initialize_pawns(PlayerA, PlayerB, 
                [pawns(PlayerA, [[1, 1], [5, 5]]), 
                 pawns(PlayerB, [[1, 5], [5, 1]])], 
                EmptyBoard, Board) :-
    place_pawn(EmptyBoard, 1, 1, pawn(PlayerA, 1), TempBoard1),
    place_pawn(TempBoard1, 5, 5, pawn(PlayerA, 2), TempBoard2),
    place_pawn(TempBoard2, 1, 5, pawn(PlayerB, 1), TempBoard3),
    place_pawn(TempBoard3, 5, 1, pawn(PlayerB, 2), Board).

place_pawn(Board, Row, Col, Pawn, NewBoard) :-
    nth1(Row, Board, OldRow, RestRows),
    nth1(Col, OldRow, OldStack, RestCells),
    (OldStack = 'o' ->
        NewStack = [Pawn, 'o']
    ;   append([Pawn], OldStack, NewStack)),
    nth1(Col, NewRow, NewStack, RestCells),
    nth1(Row, NewBoard, NewRow, RestRows).

% Game cycle
game_cycle(GameState) :-
    display_game(GameState),
    (game_over(GameState) ->
        nl, write('Game Over!'), nl
    ;   make_move(GameState, NewGameState),
        game_cycle(NewGameState)
    ).

% Make a move
make_move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), NewGameState) :-
    write('Choose your pawn index (1 or 2): '),
    read(PawnIndex),
    write('Enter your move (NewRow, NewCol): '),
    read((NewRow, NewCol)),
    write('Enter stone pickup position (Row, Col): '),
    read((PickupRow, PickupCol)),
    write('Enter stone placement position (Row, Col): '),
    read((PlaceRow, PlaceCol)),
    (move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), 
          (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol), 
          NewGameState) ->
        true
    ;   write('Invalid move, try again.'), nl,
        make_move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), NewGameState)
    ).

% Move validation and execution
move(game_state(Board, CurrentPlayer, Players, Pawns, _PrevMove),
     (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol),
     game_state(FinalBoard, NextPlayer, Players, NewPawns, (NewRow, NewCol))) :-
    % 1. Validate and move pawn
    select_pawn(CurrentPlayer, Pawns, PawnIndex, [CurrRow, CurrCol]),
    valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol]),
    
    % Keep stone at current position when moving pawn
    update_board(Board, CurrRow, CurrCol, ['o'], TempBoard1),
    
    % Move pawn to new position, preserving stones
    get_stack(TempBoard1, NewRow, NewCol, TargetStack),
    (TargetStack = 'o' -> 
        NewTargetStack = [pawn(CurrentPlayer, PawnIndex), 'o']
    ; is_list(TargetStack) ->
        append([pawn(CurrentPlayer, PawnIndex)], TargetStack, NewTargetStack)
    ; NewTargetStack = [pawn(CurrentPlayer, PawnIndex)]),
    update_board(TempBoard1, NewRow, NewCol, NewTargetStack, TempBoard2),
    
    % 2. Pick up stone
    valid_stone_pickup(TempBoard2, PickupRow, PickupCol, [NewRow, NewCol]),
    get_stack(TempBoard2, PickupRow, PickupCol, PickupStack),
    (PickupStack = 'o' ->
        update_board(TempBoard2, PickupRow, PickupCol, [], TempBoard3)
    ; is_list(PickupStack) ->
        remove_one_stone(PickupStack, NewPickupStack),
        update_board(TempBoard2, PickupRow, PickupCol, NewPickupStack, TempBoard3)
    ),
    
    % 3. Place stone
    valid_stone_placement(TempBoard3, PlaceRow, PlaceCol, [NewRow, NewCol], [PickupRow, PickupCol]),
    get_stack(TempBoard3, PlaceRow, PlaceCol, PlaceStack),
    add_stone_to_stack(PlaceStack, NewPlaceStack),
    update_board(TempBoard3, PlaceRow, PlaceCol, NewPlaceStack, FinalBoard),
    
    % Update pawns and switch player
    update_pawns(Pawns, CurrentPlayer, PawnIndex, [NewRow, NewCol], NewPawns),
    switch_player(CurrentPlayer, Players, NextPlayer).

% Valid moves predicate
valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol]) :-
    length(Board, Size),
    NewRow > 0, NewRow =< Size,
    NewCol > 0, NewCol =< Size,
    % Allow movement to any adjacent cell (including diagonals)
    RowDiff is NewRow - CurrRow,
    ColDiff is NewCol - CurrCol,
    abs(RowDiff) =< 1,
    abs(ColDiff) =< 1,
    % Don't allow staying in the same position
    \+ (RowDiff = 0, ColDiff = 0),
    % Get actual stone heights (excluding pawns)
    get_stone_only_height(Board, CurrRow, CurrCol, CurrHeight),
    get_stone_only_height(Board, NewRow, NewCol, NewHeight),
    HeightDiff is NewHeight - CurrHeight,
    between(-1, 1, HeightDiff).

% Get stack at position
get_stack(Board, Row, Col, Stack) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Stack).

% Get height counting only stones
get_stone_only_height(Board, Row, Col, Height) :-
    get_stack(Board, Row, Col, Stack),
    count_only_stones(Stack, Height).

% Count only stones in a stack
count_only_stones('o', 1) :- !.
count_only_stones([], 0) :- !.
count_only_stones(Stack, Height) :-
    is_list(Stack),
    include(=(o), Stack, Stones),
    length(Stones, Height).

% Validate stone pickup
valid_stone_pickup(Board, Row, Col, [PawnRow, PawnCol]) :-
    get_stack(Board, Row, Col, Stack),
    % Must contain a stone and not be empty
    (Stack = 'o' ; (is_list(Stack), member('o', Stack))),
    % Cannot be the stack we just moved from
    (Row \= PawnRow ; Col \= PawnCol),
    % Must be one of the smallest stacks (counting only stones)
    get_stone_only_height(Board, Row, Col, Height),
    \+ (    between(1, 5, R),
            between(1, 5, C),
            (R \= Row ; C \= Col),
            get_stack(Board, R, C, OtherStack),
            (OtherStack = 'o' ; (is_list(OtherStack), member('o', OtherStack))),
            (R \= PawnRow ; C \= PawnCol),
            get_stone_only_height(Board, R, C, OtherHeight),
            OtherHeight < Height
        ).

% Validate stone placement
valid_stone_placement(Board, Row, Col, [PawnRow, PawnCol], [PickupRow, PickupCol]) :-
    % Must be a different position than pickup and pawn
    (Row \= PawnRow ; Col \= PawnCol),
    (Row \= PickupRow ; Col \= PickupCol),
    % Position must exist on board
    length(Board, Size),
    Row > 0, Row =< Size,
    Col > 0, Col =< Size,
    % Get current stack at target
    get_stack(Board, Row, Col, Stack),
    % Allow placement on any valid stack with stones
    (Stack = 'o' ; (is_list(Stack), member('o', Stack)) ; Stack = []).

% Helper predicates for stone manipulation
remove_one_stone(Stack, NewStack) :-
    delete(Stack, 'o', NewStack).

add_stone_to_stack([], ['o']) :- !.
add_stone_to_stack('o', ['o', 'o']) :- !.
add_stone_to_stack(Stack, ['o'|Stack]) :-
    is_list(Stack).

% Select pawn for current player
select_pawn(Player, [pawns(Player, PawnList) | _], Index, Pos) :-
    nth1(Index, PawnList, Pos).
select_pawn(Player, [_ | Rest], Index, Pos) :-
    select_pawn(Player, Rest, Index, Pos).

update_board(Board, Row, Col, Value, NewBoard) :-
    nth1(Row, Board, OldRow, RestRows),
    nth1(Col, OldRow, _, RestCols),
    nth1(Col, NewRow, Value, RestCols),
    nth1(Row, NewBoard, NewRow, RestRows).

% Update pawns after move
update_pawns([pawns(Player, PawnList) | Rest], Player, Index, NewPos, 
             [pawns(Player, NewPawnList) | Rest]) :-
    nth1(Index, PawnList, _, RestPawns),
    nth1(Index, NewPawnList, NewPos, RestPawns).
update_pawns([Other | Rest], Player, Index, NewPos, [Other | NewRest]) :-
    update_pawns(Rest, Player, Index, NewPos, NewRest).

% Switch players
switch_player(CurrentPlayer, [CurrentPlayer, OtherPlayer], OtherPlayer).
switch_player(CurrentPlayer, [OtherPlayer, CurrentPlayer], OtherPlayer).

% Game over check (to be implemented)
game_over(_) :- fail.