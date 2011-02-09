-module(erlgeo).
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the erlgeo server.
start() ->
    application:start(erlgeo).


%% @spec stop() -> ok
%% @doc Stop the erlgeo server.
stop() ->
    application:stop(erlgeo).

