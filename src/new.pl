:- use_module(library(lists)).     % might be necessary
:- use_module(library(between)).

% Display the game state
display_game(game_state(Board, CurrentPlayer, _Players, Pawns, PrevMove)) :-
    nl, write('Current Player: '), write(CurrentPlayer), nl,
    display_board(Board),
    nl, write('Pawns: '), write(Pawns), nl,
    write('Previous Move: '), write(PrevMove), nl.

% Updated display cell logic to show stacks as No
display_cell([]) :- write('[ ]'). % Empty space displays as []
display_cell('o') :- write('o').  % Single stone
display_cell([pawn(Player, Number) | _]) :- 
    sub_atom(Player, 6, 1, _, PlayerLetter),  % Extract player letter (e.g., A or B)
    write(PlayerLetter), write(Number).       % Display as "A1" or "B1"

% For stacked stones, count them and display as No (e.g., 2o for two stones)
display_cell(Stack) :-
    is_list(Stack),
    count_stones(Stack, N),
    N > 0,
    write(N), write('o').

% Count stones in a stack
count_stones([], 1).
count_stones(['o'|Rest], N) :-
    count_stones(Rest, N1),
    N is N1 + 1.
count_stones([_|Rest], N) :-  % Skip non-stone elements (like pawns)
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
    append([Pawn], OldStack, NewStack),
    nth1(Col, NewRow, NewStack, RestCells),
    nth1(Row, NewBoard, NewRow, RestRows).

% Game cycle
game_cycle(GameState) :-
    display_game(GameState),
    (   game_over(GameState) ->
        nl, write('Game Over!'), nl
    ;   make_move(GameState, NewGameState),
        game_cycle(NewGameState)
    ).

% Check winner (stub)
game_over(_) :- fail.

% Make a move with stone movement
make_move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), NewGameState) :-
    write('Choose your pawn index (1 or 2): '),
    read(PawnIndex),
    write('Enter your move (NewRow, NewCol): '),
    read((NewRow, NewCol)),
    write('Enter stone pickup position (Row, Col): '),
    read((PickupRow, PickupCol)),
    write('Enter stone placement position (Row, Col): '),
    read((PlaceRow, PlaceCol)),
    (   move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), 
             (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol), 
             NewGameState) ->
        true
    ;   write('Invalid move, try again.'), nl,
        make_move(game_state(Board, CurrentPlayer, Players, Pawns, PrevMove), NewGameState)
    ).

% Updated move predicate with stone movement
move(game_state(Board, CurrentPlayer, Players, Pawns, _PrevMove), 
     (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol), 
     game_state(FinalBoard, NextPlayer, Players, NewPawns, (NewRow, NewCol))) :-
    % 1. Move pawn
    select_pawn(CurrentPlayer, Pawns, PawnIndex, [CurrRow, CurrCol]),
    valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol]),
    update_board(Board, CurrRow, CurrCol, [], TempBoard1),
    update_board(TempBoard1, NewRow, NewCol, [pawn(CurrentPlayer, PawnIndex)], TempBoard2),
    
    % 2. Pick up stone
    valid_stone_pickup(TempBoard2, PickupRow, PickupCol, [NewRow, NewCol]),
    get_stack(TempBoard2, PickupRow, PickupCol, Stack),
    update_board(TempBoard2, PickupRow, PickupCol, [], TempBoard3),
    
    % 3. Place stone
    valid_stone_placement(TempBoard3, PlaceRow, PlaceCol, [NewRow, NewCol], [PickupRow, PickupCol]),
    update_board(TempBoard3, PlaceRow, PlaceCol, ['o'], FinalBoard),
    
    % Update pawns and switch player
    update_pawns(Pawns, CurrentPlayer, PawnIndex, [NewRow, NewCol], NewPawns),
    switch_player(CurrentPlayer, Players, NextPlayer).

% Get stack at position
get_stack(Board, Row, Col, Stack) :-
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Stack).

% Get stack height
get_stack_height(Board, Row, Col, Height) :-
    get_stack(Board, Row, Col, Stack),
    stack_height(Stack, Height).

% Calculate height of a stack
stack_height('o', 1).  % Single stone
stack_height([], 0).   % Empty space
stack_height(Stack, Height) :-    % List of pieces
    is_list(Stack),
    length(Stack, Height).

% Validate stone pickup
valid_stone_pickup(Board, Row, Col, [PawnRow, PawnCol]) :-
    get_stack(Board, Row, Col, Stack),
    % Stack must be unoccupied (no pawns) and contain a stone
    Stack = 'o',
    % Cannot be the stack we just moved from
    (Row \= PawnRow ; Col \= PawnCol),
    % Must be one of the smallest stacks
    is_smallest_stack(Board, Row, Col).

% Check for pawn in stack
has_pawn([pawn(_, _)|_]).
has_pawn('o') :- false.
has_pawn([]) :- false.

% Validate smallest stack
is_smallest_stack(Board, Row, Col) :-
    get_stack_height(Board, Row, Col, Height),
    \+ (nth1(R, Board, RowList),
        nth1(C, RowList, OtherStack),
        (R \= Row ; C \= Col),
        OtherStack = 'o',  % Only compare with stacks that have stones
        get_stack_height(Board, R, C, OtherHeight),
        OtherHeight < Height).

% Validate stone placement
valid_stone_placement(Board, Row, Col, [PawnRow, PawnCol], [PickupRow, PickupCol]) :-
    get_stack(Board, Row, Col, Stack),
    % Must be a different position than pickup and pawn
    (Row \= PawnRow ; Col \= PawnCol),
    (Row \= PickupRow ; Col \= PickupCol),
    % Stack must exist and be unoccupied
    Stack = 'o'.

% Select pawn for current player
select_pawn(Player, [pawns(Player, PawnList) | _], Index, Pos) :-
    nth1(Index, PawnList, Pos).
select_pawn(Player, [_ | Rest], Index, Pos) :-
    select_pawn(Player, Rest, Index, Pos).

% Validate moves
valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol]) :-
    length(Board, Size),
    NewRow > 0, NewRow =< Size,
    NewCol > 0, NewCol =< Size,
    abs(NewRow - CurrRow) =< 1, abs(NewCol - CurrCol) =< 1.

% Update board with move
update_board(Board, Row, Col, Value, NewBoard) :-
    nth1(Row, Board, OldRow, RestRows),
    nth1(Col, OldRow, OldStack, RestCells),
    update_stack(OldStack, Value, NewStack),
    nth1(Col, NewRow, NewStack, RestCells),
    nth1(Row, NewBoard, NewRow, RestRows).

% Update stack handling
update_stack(OldStack, [], []).  % Clear the stack
update_stack(OldStack, [pawn(Player, Number)], [pawn(Player, Number)|Stones]) :-
    % When adding a pawn, keep any stones below it
    (OldStack = [] -> Stones = [] ;
     (OldStack = 'o' -> Stones = ['o'] ;
      (is_list(OldStack) -> filter_stones(OldStack, Stones) ; Stones = []))).
update_stack(_, ['o'], ['o']).  % Single stone
update_stack(OldStack, ['o'], ['o'|Stones]) :-  % Add stone to existing stack
    (OldStack = [] -> Stones = [] ;
     (OldStack = 'o' -> Stones = ['o'] ;
      (is_list(OldStack) -> filter_stones(OldStack, Stones) ; Stones = []))).

% Helper to filter stones
filter_stones([], []).
filter_stones(['o'|Rest], ['o'|Filtered]) :- 
    filter_stones(Rest, Filtered).
filter_stones([_|Rest], Filtered) :-  % Skip non-stone elements
    filter_stones(Rest, Filtered).

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