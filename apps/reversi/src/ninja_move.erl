-module(ninja_move).

-compile(export_all).
%-export([ninja_move/3, convert_position/1]).

-include("../include/reversi.hrl").

%Max functions
max(Game, Who, Depth) ->
    Avail = reversi:check_avail(Game, Who),
    loop_max_moves(Game, Avail, Who, Depth, {void,void,void,-1338}).

loop_max_moves(_,[],_,_,BestMove) -> BestMove;
loop_max_moves(Game, [{X,Y,_}|Moves], Who, Depth, BestMove) ->
    {ok, NewGame} = reversi:move(Game, X,Y,Who),
    {_,_,_,Bestval} = BestMove,
    
    ReValue = get_min_response(NewGame, {X, Y, Who}, BestMove, Depth-1),

    case ReValue > Bestval of
	true -> loop_max_moves(Game, Moves, Who, Depth,{X,Y,Who,ReValue});
        false -> loop_max_moves(Game, Moves, Who, Depth, BestMove)
    end.

get_min_response(Game, Move, _, 0) -> evaluate_board(Game, Move);
get_min_response(Game, {_X, _Y, Who}, Alpha, Depth) -> min(Game, swap(Who), Depth, Alpha).

%Min functions
min(Game, Who, Depth, Alpha) ->
    Avail = reversi:check_avail(Game, Who),
    loop_min_moves(Game, Avail, Who, Depth, Alpha, {void,void,void,1338}).

loop_min_moves(_,[],_,_,_,BestMove) -> BestMove;
loop_min_moves(_,_,_,_,Alpha,BestMove) when BestMove < Alpha -> BestMove;
loop_min_moves(Game, [{X,Y,_}|Moves], Who, Depth, Alpha, BestMove) ->
    {ok, NewGame} = reversi:move(Game, X,Y,Who),
    {_,_,_,Bestval} = BestMove,

    ReValue = get_max_response(NewGame, {X,Y,Who}, Depth-1),

    case ReValue < Bestval of
	true -> loop_min_moves(Game, Moves, Who, Depth, Alpha, {X,Y,Who,ReValue});
	false -> loop_min_moves(Game, Moves, Who, Depth, Alpha, BestMove)
    end.

get_max_response(Game, {X, Y, Who}, 0) -> evaluate_board(Game, {X, Y, swap(Who)});
get_max_response(Game, {_X, _Y, Who}, Depth) -> max(Game, swap(Who), Depth).


%Evaluations functions
evaluate_board(Game, {_X, _Y, Who} ) ->
    Avail = reversi:check_avail(Game, Who),
    case length(Avail) of
	0 -> 
	    get_win_val(Game, Who);
	_ ->
	    get_board_score(Game, Who)
    end.

get_win_val(Game, Who) ->
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

get_board_score(#game{points = {_,Points}}, 1) -> Points;
get_board_score(#game{points = {Points, _}}, 0) -> Points.





%Not used yet!
evaluate_moves([Head]) ->
    Value = get_position_rating(Head),
    erlang:append_element(Head, Value);
evaluate_moves([Head|Moves]) ->
    Value = get_position_rating(Head),
    NextMove = evaluate_moves(Moves),
    {_,_,_,NextValue} = NextMove,
    case Value < NextValue of
	true -> NextMove;
	false -> erlang:append_element(Head, Value)
    end.

get_position_rating({X,Y,_}) ->
    {Cx, Cy} = convert_position({X,Y}),
    case Cx of
	0 -> case Cy of
		0 -> 99
	    end;
	1 -> case Cy of
		0 -> -8;
		1 -> -24
	    end;
	2 -> case Cy of
		0 -> 8;
		1 -> -4;
		2 -> 7
	    end;
	3 -> case Cy of
		0 -> 6;
		1 -> -3;
		2 -> 4
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
