-module(ninja_bot).

-export([
         start/4,
	 start/0
        ]).

-include("../include/reversi.hrl").

start() ->
    %start(kpc2011.klarna.com,7676,"Ninja1","fsMyc6jEV1").
    start(localhost,7676,"Ninja1","15fbRmGCIQ").

start(Host, Port, Name, Passwd) ->
    case gen_tcp:connect(Host, Port, [binary,{packet, 0}]) of
	{ok, Sock} ->
            login(Sock, Name, Passwd),
	    gen_tcp:close(Sock);
	_ ->
	    error
    end.

do_move(Sock, Name, Passwd, Who, Game, Cookie) ->
    reversi:draw_board(Game),
    {Time, {X,Y,_,_}} = timer:tc(ninja_move, minmax, [Game,Who,3]), 
    io:format("Time ~w~n", [Time]),
    Ready = mk_move(Cookie, Who, X, Y),
    Reply = send_cmd(Sock, Ready),
    %io:format("~s~n", [binary_to_list(Reply)]),
    case parse_data(Reply) of
        {ok, {your_move, NewGame}} ->
            do_move(Sock, Name, Passwd, Who, NewGame, Cookie);
        {ok, {please_wait, NewGame}} ->
            do_wait(Sock, Name, Passwd, Who, NewGame, Cookie);
        {ok, {game_over, #game{player_b = Black, player_w = White}, Win}} ->
	    {ok, WriteDescr} = file:open("/home/softarn/stats.txt", [raw, append]),
	    case Win of 
		0 -> file:write(WriteDescr,multi_concat([Black, "* ", White]));
		1 -> file:write(WriteDescr,multi_concat([Black, " ", White, "*"]));
		-1 -> file:write(WriteDescr,multi_concat([Black, " ", White]))

	    end,
	    file:write(WriteDescr,"\n"),
	    file:close(WriteDescr),
            ready(Sock, Name, Passwd);
        Error -> io:format("do_move ~p~n-~n~p~n", [Error, Reply])
    end.


login(Sock, Name, Passwd) ->
    catch flush(),
    Login = mk_login(Name, Passwd),
    Reply = send_cmd(Sock, Login),
    case parse_data(Reply) of
        {ok, welcome} -> ready(Sock, Name, Passwd);
        Error -> io:format("login ~p~n", [Error]),
                 timer:sleep(5000),
                 login(Sock, Name, Passwd)
    end.

ready(Sock, Name, Passwd) ->
    Ready = mk_ready(),
    Reply = send_cmd(Sock, Ready),
    case parse_data(Reply) of
        {ok, waiting_for_challenge} ->
            wait_for_chal(Sock, Name, Passwd);
        {ok, {lets_play, Who, Game, Cookie}} ->
                start_game(Sock, Name, Passwd, Who, Game, Cookie);
        Error -> io:format("ready ~p~n", [Error])
    end.

wait_for_chal(Sock, Name, Passwd) ->
    {value, {_,_,Reply}} = wait_long_reply(Sock),
    %io:format("~s~n", [binary_to_list(Reply)]),
    case parse_data(Reply) of
        {ok, {lets_play, Who, Game, Cookie}} ->
            start_game(Sock, Name, Passwd, Who, Game, Cookie);
        Error -> io:format("wait_for_chal ~p~n-~n~p~n", [Error, Reply])
    end.

start_game(Sock, Name, Passwd, Who, Game, Cookie) ->
    Ready = mk_start(Cookie, Who),
    Reply = send_cmd(Sock, Ready),
    case parse_data(Reply) of
        {ok, wait_for_other_guy} ->
            do_wait(Sock, Name, Passwd, Who, Game, Cookie);
        {ok, {your_move, NewGame}} ->
            do_move(Sock, Name, Passwd, Who, NewGame, Cookie);
        {ok, {please_wait, NewGame}} ->
            do_wait(Sock, Name, Passwd, Who, NewGame, Cookie);
        Error -> io:format("start_game ~p~n-~n~p~n", [Error, Reply])
    end.

do_wait(Sock, Name, Passwd, Who, _Game, Cookie) ->
    {value, {_,_,Reply}} = wait_long_reply(Sock),
    case parse_data(Reply) of
        {ok, {your_move, NewGame}} ->
            do_move(Sock, Name, Passwd, Who, NewGame, Cookie);
        {ok, {please_wait, NewGame}} ->
            do_wait(Sock, Name, Passwd, Who, NewGame, Cookie);
        {ok, {game_over, #game{player_b = Black, player_w = White}, Win}} ->
	    {ok, WriteDescr} = file:open("/home/softarn/stats.txt", [raw, append]),
	    case Win of 
		0 -> file:write(WriteDescr,multi_concat([Black, "* ", White]));
		1 -> file:write(WriteDescr,multi_concat([Black, " ", White, "*"]));
		-1 -> file:write(WriteDescr,multi_concat([Black, " ", White]))
	    end,
	    file:write(WriteDescr,"\n"),
	    file:close(WriteDescr),
            ready(Sock, Name, Passwd);
        Error -> io:format("do_wait ~p~n-~n~p~n", [Error, Reply])
    end.

send_cmd(Sock, Cmd) ->
    io:format("~s~n", [Cmd]),
    gen_tcp:send(Sock, Cmd),
    {value, {_,_,Reply}} = wait_reply(Sock),
    io:format("~s~n", [binary_to_list(Reply)]),
    Reply.

flush() ->
    receive
    after 0 ->
	    ok
    end.

wait_reply(_Timeout) ->
    receive
	Reply ->
	    {value, Reply}
    after 10000 ->
	    timeout
    end.

wait_long_reply(_Timeout) ->
    receive
	Reply ->
	    {value, Reply}
    after 1000000 ->
	    timeout
    end.

mk_login(Name, Passwd) ->
    io_lib:format("{login,\"~s\",\"~s\"}.", [Name, Passwd]).

mk_ready() ->
    "{i_want_to_play}.".

mk_start(Cookie, Who) ->
    io_lib:format("{login,~p,~p}.", [Cookie, Who]).

mk_move(Cookie, Who, X, Y) ->
    io_lib:format("{move,~p,~p,~p,~p}.", [Cookie, Who, X, Y]).

multi_concat([Head]) -> Head;
multi_concat([Head|Tail]) ->
    string:concat(Head, multi_concat(Tail)).

parse_data("\n") ->
    [];
parse_data(RawData) ->
    parse_data2(binary_to_list(RawData) ++ ".").

parse_data2(RawData) ->
    try
        {ok, Tokens, _} = erl_scan:string(RawData),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _:_ ->
            {error, could_not_parse_command}
    end.