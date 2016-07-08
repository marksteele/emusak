%%%-------------------------------------------------------------------
%%% @author Mark Steele <mark@control-alt-del.org>
%%% @copyright (C) 2016, Mark Steele
%%% @doc
%%%
%%% @end
%%% Created :  4 Jul 2016 by  <mark@control-alt-del.org>
%%%-------------------------------------------------------------------
-module(emusak_auth_middleware).
-behaviour(cowboy_middleware).

%% API
-export([execute/2]).

%%%===================================================================
%%% API
%%%===================================================================

execute(Req, Env) ->
  {Path, Req2} = cowboy_req:path(Req),
  {ok, Key} = application:get_env(emusak,key),
  case Path of
    <<"/transcode",_/binary>> ->
      check_auth_qs(Req2,Env,Key);
    <<"/playlist", _/binary>> ->
      check_auth_header(Req2,Env,Key);
    _ ->
      {ok, Req2, Env}
  end.

check_auth_header(Req,Env,Key) ->
  {AuthHeader,Req2} = cowboy_req:header(<<"authorization">>,Req),
  case AuthHeader of
    <<"Bearer ", Token/binary>> ->
      check_token(Token,Key,Env,Req2);
    _ ->
      {error, 403, Req2}
  end.

check_auth_qs(Req,Env,Key) ->
  {Token,Req2} = cowboy_req:qs_val(<<"auth">>, Req),
  check_token(Token,Key,Env,Req2).

check_token(Token,Key, Env, Req) ->
  case ejwt:decode(Token,Key) of
    error ->
      {error,403,Req};
    _ ->
      {ok, Req, Env}
  end.
