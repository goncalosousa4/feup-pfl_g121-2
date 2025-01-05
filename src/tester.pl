:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).

% Display the game state
display_game(GameState) :-
    GameState = game_state(Board, CurrentPlayer, Players, Pawns, PrevMove),
    nl, write('Current Player: '), write(CurrentPlayer), nl,
    display_board(Board),
    nl, write('Pawns: '), write(Pawns), nl,
    write('Previous Move: '), write(PrevMove), nl,
    % Add value display for both players
    display_values(GameState, Players).

% Helper predicate to display values for all players
display_values(GameState, []).
display_values(GameState, [Player|Rest]) :-
    value(GameState, Player, Value),
    format('~w: ~2f~n', [Player, Value]),
    display_values(GameState, Rest).

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
    write_stones(N).  % Write repeated 'o' for stones

% Helper to write 'o' N times
write_stones(0).  % No stones to write
write_stones(N) :- 
    N > 0, 
    write('o'), 
    N1 is N - 1, 
    write_stones(N1).

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
    select_game_mode(Mode),
    initialize_game(Mode, GameState),
    game_cycle(GameState, Mode).

select_game_mode(Mode) :-
    nl, write('Select Game Mode:'), nl,
    write('1 - Player vs Player'), nl,
    write('2 - Player vs Easy Bot'), nl,
    read(Choice),
    (   Choice = 1 -> Mode = pvp
    ;   Choice = 2 -> Mode = pve_easy
    ;   write('Invalid choice, try again.'), nl, select_game_mode(Mode)
    ).

initialize_game(_, GameState) :-
    initial_state([playerA, playerB, 5], GameState).

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

% Player vs Player
game_cycle(GameState, pvp) :-
    display_game(GameState),
    (   game_over(GameState) ->
            nl, write('Game Over!'), nl
    ;   make_move(GameState, NewGameState),
        game_cycle(NewGameState, pvp)
    ).

% Player vs Easy Bot
game_cycle(GameState, pve_easy) :-
    display_game(GameState),
    (   game_over(GameState) ->
            nl, write('Game Over!'), nl
    ;   GameState = game_state(_, CurrentPlayer, _, _, _),
        (   CurrentPlayer = playerA -> 
                make_move(GameState, NewGameState)
        ;   easy_bot_move(GameState, NewGameState)
        ),
        game_cycle(NewGameState, pve_easy)  % Continue to the next cycle
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

valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol]) :-
    length(Board, Size),
    
    % Check bounds
    (NewRow > 0, NewRow =< Size, NewCol > 0, NewCol =< Size),
    
    RowDiff is NewRow - CurrRow,
    ColDiff is NewCol - CurrCol,
    
    % Ensure we are moving, not staying in the same position
    \+ (RowDiff = 0, ColDiff = 0),
    
    % Allow diagonal or adjacent moves
    abs(RowDiff) =< 1,
    abs(ColDiff) =< 1,
    
    % Check stone height difference
    get_stone_only_height(Board, CurrRow, CurrCol, CurrHeight),
    get_stone_only_height(Board, NewRow, NewCol, NewHeight),
    HeightDiff is NewHeight - CurrHeight,
    
    % Validate the height difference
    between(-1, 1, HeightDiff),
    
    % Ensure the target position has something (either a pawn or a stone)
    get_stack(Board, NewRow, NewCol, Stack),
    Stack \= [ ], % Disallow moving to an empty space
    (Stack = 'o' ; is_list(Stack)), % Target must have stones or a stack
    
    % Ensure the target position does not have another pawn
    \+ (is_list(Stack), member(pawn(_, _), Stack)).

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
    
    % Cannot be the stack where the current pawn is placed
    (Row \= PawnRow ; Col \= PawnCol),
    
    % Ensure the position doesnt contain a pawn (we can't take stones from a pawn's position)
    \+ (is_list(Stack), member(pawn(_, _), Stack)),

    % Must be one of the smallest stacks (counting only stones)
    get_stone_only_height(Board, Row, Col, Height),
    \+ (between(1, 5, R),
        between(1, 5, C),
        (R \= Row ; C \= Col),
        get_stack(Board, R, C, OtherStack),
        (OtherStack = 'o' ; (is_list(OtherStack), member('o', OtherStack))),
        (R \= PawnRow ; C \= PawnCol),
        get_stone_only_height(Board, R, C, OtherHeight),
        OtherHeight < Height
    ).


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

    % Ensure target position is not empty
    Stack \= [ ],  % Disallow placement in empty positions
    
    % Allow placement only on stacks with stones or valid stacks
    (Stack = 'o' ; is_list(Stack)),
    
    % Ensure target position does not already contain a pawn
    \+ (is_list(Stack), member(pawn(_, _), Stack)).


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

% The game is over if the current player has no valid moves left
game_over(game_state(Board, CurrentPlayer, Players, Pawns, _PrevMove)) :-
    % Find all pawns of the current player that have no valid moves
    findall([PawnIndex, CurrRow, CurrCol],
            (select_pawn(CurrentPlayer, Pawns, PawnIndex, [CurrRow, CurrCol]),
             \+ has_valid_move(Board, [CurrRow, CurrCol])  % Check if the pawn has no valid moves
            ),
            NoMoves),
    
    % If there are no valid moves for at least one pawn, the game is over
    NoMoves \= [].

has_valid_move(Board, [CurrRow, CurrCol]) :-
    % Generate all possible adjacent cells
    findall([NewRow, NewCol],
            (adjacent_cell(CurrRow, CurrCol, NewRow, NewCol),
             valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol])
            ),
            ValidMoves2),
    
    % Debugging output to see the possible valid moves
    sort(ValidMoves2, ValidMoves),
    write('Valid moves from ['), write(CurrRow), write(','), write(CurrCol), write(']: '), write(ValidMoves), nl,
    
    % If there are valid moves, return true
    ValidMoves \= [].


adjacent_cell(CurrRow, CurrCol, NewRow, NewCol) :-
    % Generate all possible row and column changes: -1, 0, +1 for each direction
    member(RowDiff, [-1, 0, 1]),      % Possible row changes
    member(ColDiff, [-1, 0, 1]),      % Possible column changes

    % Ensure we are not staying at the same cell
    (RowDiff \= 0 ; ColDiff \= 0),    % We cant stay in the same position

    % Calculate the new position based on the row and column changes
    NewRow is CurrRow + RowDiff,
    NewCol is CurrCol + ColDiff,

    % Ensure NewRow and NewCol are within valid bounds (1 to 5)
    NewRow > 0, NewRow =< 5,
    NewCol > 0, NewCol =< 5.


% Main value predicate
value(game_state(Board, _, Players, Pawns, _), Player, Value) :-
    % Calculate mobility score
    mobility_score(Board, Pawns, Player, MobilityScore),
    
    % Calculate height advantage score
    height_score(Board, Pawns, Player, HeightScore),
    
    % Calculate position score
    position_score(Board, Pawns, Player, PositionScore),

    Value is (MobilityScore * 0.5) + (HeightScore * 0.3) + (PositionScore * 0.2) + 50.

% Mobility score - counts number of valid moves available
mobility_score(Board, Pawns, Player, Score) :-
    findall(1, 
            (select_pawn(Player, Pawns, PawnIndex, [Row, Col]),
             adjacent_cell(Row, Col, NewRow, NewCol),
             valid_moves(Board, [Row, Col], [NewRow, NewCol])),
            Moves),
    length(Moves, NumMoves),
    Score is NumMoves * 10.  % Multiply by 10 to give more weight to mobility

% Height score - evaluates the height advantage of player's pawns
height_score(Board, Pawns, Player, Score) :-
    findall(Height,
            (select_pawn(Player, Pawns, _, [Row, Col]),
             get_stone_only_height(Board, Row, Col, Height)),
            Heights),
    sum_list(Heights, TotalHeight),
    Score is TotalHeight * 10.  % Multiply by 15 to give significant weight to height advantage

% Position score - evaluates strategic positioning
position_score(Board, Pawns, Player, Score) :-
    findall(PositionValue,
            (select_pawn(Player, Pawns, _, [Row, Col]),
             calculate_position_value(Row, Col, PositionValue)),
            PositionValues),
    sum_list(PositionValues, Score).

% Helper predicate to calculate position value based on board position
calculate_position_value(Row, Col, Value) :-
    % Center positions are worth more
    (Row = 3, Col = 3 -> 
        Value = 30
    ; (Row = 3; Col = 3) ->
        Value = 20
    ; % Corner positions are worth less
    ((Row = 1; Row = 5), (Col = 1; Col = 5)) ->
        Value = 10
    ; % Other positions have moderate value
        Value = 15
    ).

% Helper predicate to sum a list
sum_list([], 0).
sum_list([H|T], Sum) :-
    sum_list(T, RestSum),
    Sum is H + RestSum.

% Easy bot: Random valid move
easy_bot_move(GameState, NewGameState) :-
    findall(Move, valid_bot_move(GameState, Move), Moves),
    random_member(BotMove, Moves),
    execute_move(GameState, BotMove, NewGameState).

% Find valid moves for a bot
valid_bot_move(game_state(Board, CurrentPlayer, Players, Pawns, _), 
               (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol)) :-
    select_pawn(CurrentPlayer, Pawns, PawnIndex, [CurrRow, CurrCol]),
    adjacent_cell(CurrRow, CurrCol, NewRow, NewCol),
    valid_moves(Board, [CurrRow, CurrCol], [NewRow, NewCol]),
    valid_stone_pickup(Board, PickupRow, PickupCol, [NewRow, NewCol]),
    valid_stone_placement(Board, PlaceRow, PlaceCol, [NewRow, NewCol], [PickupRow, PickupCol]).

% Execute a move for the bot
execute_move(GameState, (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol), NewGameState) :-
    move(GameState, (PawnIndex, NewRow, NewCol, PickupRow, PickupCol, PlaceRow, PlaceCol), NewGameState).