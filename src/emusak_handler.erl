-module(emusak_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
         }).

init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Json} = emusak_playlist:playlist(),
    {ok, Req2} = cowboy_req:reply(
                   200,
                   [
                    {<<"content-type">>, <<"application/json">>}
                   ],
                   Json,
                   Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
