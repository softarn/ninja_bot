%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the reversi_rest application.

-module(reversi_rest_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for reversi_rest.
start(_Type, _StartArgs) ->
    reversi_rest_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for reversi_rest.
stop(_State) ->
    ok.
