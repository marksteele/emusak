-module(emusak_transcode).
-behaviour(cowboy_http_handler).
-include("emusak.hrl").

-export([init/3]).
-export([handle/2]).
-export([info/3]).
-export([terminate/3]).

init(_, Req0,_) ->
  {Type, Req1} = cowboy_req:binding(type,Req0),
  {Id, Req2} = cowboy_req:binding(id,Req1),
  case Type == undefined of
    true ->
      throw(error);
    _ ->
      ok
  end,
  case Id == undefined of
    true ->
      throw(error);
    _ ->
      ok
  end,
  Item = emusak_playlist:get_entry(binary_to_integer(Id)),
  io:format("Item: ~p~n",[Item]),

  case Type of
    <<"mp3">> ->
      {ok, Req2, Item};
    _ ->
      Cmd = "/usr/bin/ffmpeg -loglevel quiet -i '" ++
        re:replace(Item#song.file,"'","'\\\\''",[{return,list},global]) ++
        "' -acodec pcm_s16le -f wav - 2>/dev/null | /usr/bin/lame -r - - 2>/dev/null",
      io:format("Launching: /bin/sh -c ~p~n",[Cmd]),
      _Port = erlang:open_port(
                {spawn_executable,"/bin/sh"},
                [
                 binary,
                 stream,
                 exit_status,
                 use_stdio,
                 in,
                 {args,["-c",Cmd]}
                ]),
      {ok, Req3} = cowboy_req:chunked_reply(
                     200, [
                           {<<"content-type">>,<<"audio/mp3">>},
                           {<<"connection">>,<<"close">>}
                          ],
                     Req2
                    ),
      {loop, Req3, undefined_state}
  end.

handle(Req,Item) ->
  F = fun(Socket,Transport) ->
          Transport:sendfile(Socket,Item#song.file)
      end,
  Req2 = cowboy_req:set_resp_body_fun(Item#song.size, F, Req),
  {ok, cowboy_req:reply(200,[{<<"content-type">>,<<"audio/mp3">>}],Req2),{}}.

info({_Port,{data,Data}},Req,State) ->
  ok = cowboy_req:chunk(Data,Req),
  {loop, Req, State};
info({Port,{exit_status,_}},Req,State) ->
  catch(port_close(Port)),
  {ok, Req,State};
info(_,Req,_) ->
  {halt,Req,{}}.

terminate(_Reason, _Req, _State) ->
  ok.
