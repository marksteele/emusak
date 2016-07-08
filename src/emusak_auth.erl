-module(emusak_auth).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-record(
   state,{
     key,
     password
    }
  ).

init(_, Req, _Opts) ->
  {ok, Key} = application:get_env(emusak,key),
  {ok, Pass} = application:get_env(emusak,password),
  {ok, Req, #state{key=Key,password=Pass}}.

handle(Req, State=#state{key=Key,password=Pass}) ->
  {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
  io:format("~p~n",[proplists:get_value(<<"secret">>,PostVals)]),
  case proplists:get_value(<<"secret">>,PostVals) of
    Pass ->
      Payload = [{<<"auth">>,true}],
      Token = ejwt:encode(Payload, Key),
      {ok, Req3} = cowboy_req:reply(
                     204,
                     [
                      {<<"x-auth-token">>, Token}
                     ],
                     <<"">>,
                     Req2),
      {ok, Req3, State};
    _ ->
      {ok, Req3} = cowboy_req:reply(403,[],<<"Sorry buddy!">>,Req2),
      {ok, Req3, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.
