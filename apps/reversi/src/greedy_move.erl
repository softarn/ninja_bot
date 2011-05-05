-module(greedy_move).

-compile(export_all).
%-export([ninja_move/3, convert_position/1]).

-include("../include/reversi.hrl").

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

loop_max_moves(_,[],_,_,_,BestVal) -> BestVal;
loop_max_moves(_,_,_,_,Beta,BestVal) when BestVal > Beta -> BestVal;
loop_max_moves(Game, [{X,Y}|Moves], Who, Depth, Beta, BestVal) ->
    {ok, NewGame} = reversi:move(Game, X,Y,Who),

    ReVal = min_response(NewGame, Who, Depth-1, BestVal),

    case ReVal > BestVal of
	true -> loop_max_moves(Game, Moves, Who, Depth, Beta, ReVal);
        false -> loop_max_moves(Game, Moves, Who, Depth, Beta, BestVal)
    end.

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
	    board_score(Game, Who)
    end.

win_val(Game, Who) ->
    case winner(Game) of
	Who -> 1337;
	-1 -> 0;
	_ -> -1337
    end.

winner(Game) ->
    case reversi:winner(Game) of
	"Black" -> 0;
	"White" -> 1;
	_ -> -1
    end.

board_score(#game{points = {_,Points}}, 1) -> Points;
board_score(#game{points = {Points, _}}, 0) -> Points.

swap(0) -> 1;
swap(1) -> 0.

