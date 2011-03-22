-module(board).

-compile(export_all).

-include("../include/reversi.hrl").

game_state(#game{board = {Black, White}}) ->
    case count_ones(Black bor White) of
	X when X < 20 -> early_game;
	X when X < 40 -> mid_game;
	_ -> end_game
    end.

check_avail(Game, Who) ->
    move_parse(avail_moves(Game,Who)).

avail_moves(#game{board = {Black, White}}, Who) ->
    case Who of
	0 -> avail_init(Black, White);
	1 -> avail_init(White, Black)
    end.

%Parses move integer to list of {X,Y,ok}
move_parse(Moves) ->
    move_parse(Moves, 0, []).

move_parse(0,_,List) -> List;
move_parse(Moves, Count, List) ->
    case 1 band Moves of
	1 -> move_parse(Moves bsr 1, Count+1, [{Count rem 8, Count div 8, ok}|List]);
	0 -> move_parse(Moves bsr 1, Count+1, List)
    end.


avail_init(Player, Other) ->
    Full = pow(2,64) - 1,
    Empty = Full bxor (Player bor Other),
    MaskT = Full bxor 255,			%Masks for deleting wrapping bits
    MaskB = Full bxor 18374686479671623680,
    MaskR = Full bxor 9259542123273814144,
    MaskL = Full bxor 72340172838076673,    
    MaskRT = MaskR band MaskT,
    MaskLT = MaskL band MaskT,
    MaskLB = MaskL band MaskB,
    MaskRB = MaskR band MaskB,
    avail_start(Player, Other, Empty, MaskB, fun(X) -> X bsr 8 end) bor	    %North search
    avail_start(Player, Other, Empty, MaskLB, fun(X) -> X bsr 7 end) bor    %Norteast search
    avail_start(Player, Other, Empty, MaskL, fun(X) -> X bsl 1 end) bor	    %East search
    avail_start(Player, Other, Empty, MaskLT, fun(X) -> X bsl 9 end) bor    %Southeast search
    avail_start(Player, Other, Empty, MaskT, fun(X) -> X bsl 8 end) bor	    %South search
    avail_start(Player, Other, Empty, MaskRT, fun(X) -> X bsl 7 end) bor    %Southwest search
    avail_start(Player, Other, Empty, MaskR, fun(X) -> X bsr 1 end) bor	    %West search
    avail_start(Player, Other, Empty, MaskRB, fun(X) -> X bsr 9 end).	    %Northwest search

avail_start(Player, Other, Empty, Mask, Bitfun ) ->
    NewPla = Bitfun(Player),
    Potential = (NewPla band Other) band Mask,
    avail_search(Other, Potential, 0, Empty, Mask, Bitfun).

avail_search(_, 0, Possible, _, _,_) -> Possible;
avail_search(Other, Potential, Possible, Empty, Mask, Bitfun) ->
    NewPot = (Bitfun(Potential) band Other) band Mask,
    NewPos = Possible bor ((Bitfun(Potential) band Mask) band Empty),
    avail_search(Other, NewPot, NewPos, Empty, Mask, Bitfun).

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
