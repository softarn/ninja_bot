-module(ninja_move).

-compile(export_all).
%-export([ninja_move/3, convert_position/1]).

-include("../include/reversi.hrl").

%Spawn the first available moves
minmax(Game, Who, Depth) ->
    Avail = board:check_avail(Game, Who),
    Amount = length(Avail),
    min_spawner(Game, Who, Avail, Depth, 1338),
    min_receiver(Amount, {v,v,v,-1338}).

min_spawner(_, _, [], _, _) -> [];
min_spawner(Game, Who, [Avail|Tail], Depth, Beta) ->
    spawn(?MODULE, min_wrap, [self(), Game, Avail, Who, Depth, -1338]),
    min_spawner(Game, Who, Tail, Depth, Beta).

min_receiver(0,BestMove) -> BestMove;
min_receiver(Amount,BestMove) ->
    {_,_,_,BestVal} = BestMove,
    receive
	{X,Y,W,Val} ->
	    case Val > BestVal of
		true -> min_receiver(Amount-1, {X,Y,W,Val});
		false -> min_receiver(Amount-1, BestMove)
	    end
    end.

min_wrap(Pid, Game, {X,Y} , Who, Depth, BestVal) ->
    {ok, NewGame} = reversi:move(Game, X,Y,Who),
    Value = min_response(NewGame, Who, Depth-1, BestVal),
    Pid ! {X, Y, Who, Value}.

%Max functions
max(Game, Who, Depth, Beta) ->
    Avail = board:check_avail(Game, Who),
    case length(Avail) of
	0 -> win_val(Game,Who);
	_ -> loop_max_moves(Game, Avail, Who, Depth, Beta, -1338)
    end.

%Loops through all the max moves and request the min response
loop_max_moves(_,[],_,_,_,BestVal) -> BestVal;
loop_max_moves(_,_,_,_,Beta,BestVal) when BestVal > Beta -> BestVal;
loop_max_moves(Game, [{X,Y}|Moves], Who, Depth, Beta, BestVal) ->
    {ok, NewGame} = reversi:move(Game, X,Y,Who),

    ReVal = min_response(NewGame, Who, Depth-1, BestVal),

    case ReVal > BestVal of
	true -> loop_max_moves(Game, Moves, Who, Depth, Beta, ReVal);
        false -> loop_max_moves(Game, Moves, Who, Depth, Beta, BestVal)
    end.

%If the depth is 0, stop and evaluate
min_response(Game, Move, 0, _) -> 
    evaluate_board(Game, Move);
min_response(Game, Who, Depth, Alpha) -> 
    min(Game, swap(Who), Depth, Alpha).


%Min functions
min(Game, Who, Depth, Alpha) ->
    Avail = board:check_avail(Game, Who),
    case length(Avail) of
	0 -> win_val(Game,Who);
	_ -> loop_min_moves(Game, Avail, Who, Depth, Alpha, 1338)
    end.

loop_min_moves(_,[],_,_,_,BestVal) -> BestVal;
loop_min_moves(_,_,_,_,Alpha,BestVal) when BestVal < Alpha -> BestVal;
loop_min_moves(Game, [{X,Y}|Moves], Who, Depth, Alpha, BestVal) ->
    {ok, NewGame} = reversi:move(Game, X,Y,Who),

    ReVal = max_response(NewGame, Who, Depth-1, BestVal),

    case ReVal < BestVal of
	true -> loop_min_moves(Game, Moves, Who, Depth, Alpha, ReVal);
	false -> loop_min_moves(Game, Moves, Who, Depth, Alpha, BestVal)
    end.

max_response(Game, Who, 0, _) -> 
    evaluate_board(Game, swap(Who));
max_response(Game, Who, Depth, Beta) -> 
    max(Game, swap(Who), Depth, Beta).

%Evaluations functions
evaluate_board(Game, Who) -> 
    case board:check_avail_length(Game, Who) of
	0 -> win_val(Game, Who);
	_ -> 
	    case board:game_state(Game) of
		early_game -> early_eval(Game, Who);
		early_mid_game -> early_mid_eval(Game, Who);
		mid_game -> mid_eval(Game, Who);
		late_mid_game -> late_mid_eval(Game, Who);
		end_game -> end_eval(Game, Who)
	    end
    end.

early_eval(Game, Who) ->
    avail_val(Game, Who) - 
    frontier_val(Game, Who).

early_mid_eval(Game, Who) ->
    avail_val(Game, Who) - 
    frontier_val(Game, Who) +
    pos_val(Game, Who)/2.

mid_eval(Game, Who) ->
    avail_val(Game, Who) - 
    frontier_val(Game, Who)/2 +
    pos_val(Game, Who) +
    board_score(Game, Who)/2.

late_mid_eval(Game, Who) ->
    avail_val(Game, Who)/2 -
    frontier_val(Game, Who)/5 +
    pos_val(Game, Who) +
    board_score(Game, Who).

end_eval(Game, Who) ->
    avail_val(Game, Who)/3 +
    board_score(Game, Who).

win_val(Game, Who) ->
    case winner(Game) of
	Who -> 1337;
	-1 -> 0;
	_ -> -1337
    end.

frontier_val(Game, Who) ->
    board:front_val(Game, Who).

pos_val(Game, Who) ->
    {Black, White} = board:pos_list(Game),
    case Who of
	0 -> evaluate_pos(Black) - evaluate_pos(White);
	1 -> evaluate_pos(White) - evaluate_pos(Black)
    end.

avail_val(Game, Who) -> 
    board:check_avail_length(Game, Who) - board:check_avail_length(Game, swap(Who)).

winner(Game) ->
    case reversi:winner(Game) of
	"Black" -> 0;
	"White" -> 1;
	_ -> -1
    end.

board_score(#game{points = {_,Points}}, 1) -> Points;
board_score(#game{points = {Points, _}}, 0) -> Points.

evaluate_pos([]) -> 0;
evaluate_pos([{X,Y}|Moves]) ->
    position_rating({X,Y}) + evaluate_pos(Moves).

position_rating({X,Y}) ->
    {Cx, Cy} = convert_position({X,Y}),
    case Cx of
	0 -> case Cy of
		0 -> 99 
	    end;
	1 -> case Cy of
		0 -> -24;
		1 -> -29
	    end;
	2 -> case Cy of
		0 -> 5;
		1 -> -15;
		2 -> 7
	    end;
	3 -> case Cy of
		0 -> 3;
		1 -> -7;
		2 -> 2;
		3 -> 1
	    end
    end.

convert_position({X,Y}) ->
    Nx = 
    case X > 3 of
	true -> -(X-7);
	false -> X
    end,
    Ny =
    case Y > 3 of
	true -> -(Y-7);
	false -> Y 
    end,
    case Ny > Nx of
	true -> {Ny,Nx};
	false -> {Nx,Ny}
    end.

swap(0) -> 1;
swap(1) -> 0.

