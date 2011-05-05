-module(board).

-compile(export_all).

-include("../include/reversi.hrl").

game_state(#game{board = {Black, White}}) ->
    case count_ones(Black bor White) of
	X when X < 10 -> early_game;
	X when X < 20 -> early_mid_game;
	X when X < 30 -> mid_game;
	X when X < 50 -> late_mid_game;
	_ -> end_game
    end.

pos_list(#game{board = {Black, White}}) ->
    {pos_parse(Black), pos_parse(White)}.

check_avail_length(Game, Who) ->
    count_ones(avail_moves(Game, Who)).

check_avail(Game, Who) ->
    pos_parse(avail_moves(Game,Who)).

%Parses move integer to list of {X,Y}
pos_parse(Moves) ->
    pos_parse(Moves, 0, []).

pos_parse(0,_,List) -> List;
pos_parse(Moves, Count, List) ->
    case 1 band Moves of
	1 -> pos_parse(Moves bsr 1, Count+1, [{Count rem 8, Count div 8}|List]);
	0 -> pos_parse(Moves bsr 1, Count+1, List)
    end.

%Search for frontier positions (An empty spot next to a disk)
front_val(#game{board = {Black, White}}, Who) ->
    Front_search = fun front_search/5,
    case Who of
	0 -> count_ones(bitsearch_init(Black, White, Front_search));
	1 -> count_ones(bitsearch_init(White, Black, Front_search))
    end.

front_search(Player, _Other, Empty, Mask, Bitfun) ->
    (Bitfun(Player) band Mask) band Empty.


%Search for available moves
avail_moves(#game{board = {Black, White}}, Who) ->
    Avail_search = fun avail_start/5,
    case Who of
	0 -> bitsearch_init(Black, White, Avail_search);
	1 -> bitsearch_init(White, Black, Avail_search)
    end.

avail_start(Player, Other, Empty, Mask, Bitfun ) ->
    NewPla = Bitfun(Player),
    Potential = (NewPla band Other) band Mask,
    avail_search(Other, Potential, 0, Empty, Mask, Bitfun).

avail_search(_, 0, Possible, _, _,_) -> Possible;
avail_search(Other, Potential, Possible, Empty, Mask, Bitfun) ->
    NewPot = (Bitfun(Potential) band Other) band Mask,
    NewPos = Possible bor ((Bitfun(Potential) band Mask) band Empty),
    avail_search(Other, NewPot, NewPos, Empty, Mask, Bitfun).


%Creates masks and starts the search in each direction and adds up the result
bitsearch_init(Player, Other, Search) ->
    Full = pow(2,64) - 1,
    Empty = Full bxor (Player bor Other),
    MaskT = Full bxor 255,				%Masks for deleting wrapping bits
    MaskB = Full bxor 18374686479671623680,
    MaskR = Full bxor 9259542123273814144,
    MaskL = Full bxor 72340172838076673,    
    MaskRT = MaskR band MaskT,
    MaskLT = MaskL band MaskT,
    MaskLB = MaskL band MaskB,
    MaskRB = MaskR band MaskB,
    Search(Player, Other, Empty, MaskB, fun(X) -> X bsr 8 end) bor  %North search
    Search(Player, Other, Empty, MaskL, fun(X) -> X bsl 1 end) bor  %East 
    Search(Player, Other, Empty, MaskT, fun(X) -> X bsl 8 end) bor  %South 
    Search(Player, Other, Empty, MaskR, fun(X) -> X bsr 1 end) bor  %West 
    Search(Player, Other, Empty, MaskLB, fun(X) -> X bsr 7 end) bor %Norteast 
    Search(Player, Other, Empty, MaskLT, fun(X) -> X bsl 9 end) bor %Southeast 
    Search(Player, Other, Empty, MaskRT, fun(X) -> X bsl 7 end) bor %Southwest 
    Search(Player, Other, Empty, MaskRB, fun(X) -> X bsr 9 end).    %Northwest 


%From https://github.com/cjkjellander/KPC2011/blob/master/apps/reversi/src/reversi.erl 2011-03-20
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
