-module(board).

-compile(export_all).

-include("../include/reversi.hrl").

game_state(#game{board = {Black, White}}) ->
    case count_ones(Black bor White) of
	X when X < 20 -> early_game;
	X when X < 40 -> mid_game;
	_ -> end_game
    end.

avail_moves(#game{board = {Black, White}}, Who) ->
    case Who of
	0 -> avail_search(Black, White);
	1 -> avail_search(White, Black)
    end.

avail_search(Player, Other) ->
    io:format("players:~w~n", [(Player bor Other)]),
    Empty = (pow(2,64) - 1) bxor (Player bor Other),
    io:format("Empty:~w~n", [Empty]),
    NewPla = Player bsr 8,
    Potential = (NewPla band Other),
    io:format("Pot:~w~n", [Potential]),
    avail_search(Other, Potential, 0, Empty).

avail_search(_, 0, Possible, _) -> Possible;
avail_search(Other, Potential, Possible, Empty) ->
    io:format("Pot:~w~n", [Potential]),
    NewPot = (Potential bsr 8) band Other,
    NewPos = Possible bor ((Potential bsr 8) band Empty),
    io:format("Pos:~w~n", [Possible]),
    avail_search(Other, NewPot, NewPos, Empty).

%From klarnas reversi.erl
count_ones(I) ->
    count_ones(I, 0).

count_ones(0, Ones) -> Ones;
count_ones(I, Ones) ->
    case I band 1 of
        1 -> count_ones(I bsr 1, Ones+1);
        0 -> count_ones(I bsr 1, Ones)
    end.

%From http://www.erlang.org/pipermail/erlang-questions/2009-April/043013.html 2011-03-20
pow(X, N) when is_integer(N), N >= 0 -> pow(X, N, 1);
pow(X, N) when is_integer(N) -> 1 / pow(X, -N, 1);
pow(X, N) when is_float(N) -> math:pow(X, N).

pow(_, 0, P) -> P;
pow(X, N, A) when N rem 2 =:= 0 ->
    pow(X * X, N div 2, A);
pow(X, N, A) -> pow(X, N - 1, A * X).
