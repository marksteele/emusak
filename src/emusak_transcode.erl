-module(emusak_transcode).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(state, {
         }).

init(_, Req, _Opts) ->
  {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
  {ok, Json} = emusak_playlist:getlist(10),
  {ok, Req2} = cowboy_req:reply(
                 200,
                 [{<<"content-type">>, <<"application/json">>}],
                 Json,
                 Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
  ok.

%% Loop handler in cowboy
%% init(_Type, Req, _Opts) ->
%%   {ok, Req2} = cowboy_req:chunked_reply(200, [], Req),
%%       {loop, Req2, undefined_state}.
%% info(eof, Req, State) ->
%%   {ok, Req, State};
%% info(info({chunk, Chunk}, Req, State) ->
%%     ok = cowboy_req:chunk(Chunk, Req),
%%     {loop, Req, State};
%% info(_Msg, Req, State) ->
%%     {loop, Req, State}.

%% Encoding:
%% flac -c -d /share/Music/foo.flac 2>/dev/null | opusenc --quiet --raw - -

%% %% run port command streaming input
%% run() ->
%%   P5 = erlang:open_port({spawn_executable, "sh test.sh"},
%%                         [stderr_to_stdout, in, exit_status,binary]),

%%   loop(P5).

%% loop(P) ->
%%   receive{P, Data} ->

%%       io:format("Data ~p~n",[Data]),
%%       loop(P)
%%   end.
