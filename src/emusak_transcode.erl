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

  Cmd = case Item#song.type of
          "mp3" ->
            "/usr/bin/mpg123 -s -q --no-control -o wav '" ++
              re:replace(Item#song.file,"'","'\\\\''",[{return,list},global]) ++
              "' | /usr/bin/opusenc - - 2>/dev/null";
          "flac" ->
            "/usr/bin/flac -c -d --totally-silent '" ++
               re:replace(Item#song.file,"'","'\\\\''",[{return,list},global]) ++
              "' | /usr/bin/opusenc - - 2>/dev/null"
        end,
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
                       {<<"content-type">>,<<"audio/ogg, codec=opus">>},
                       {<<"connection">>,<<"close">>}
                      ],
                 Req2
                ),
  {loop, Req3, undefined_state}.

handle(_,_) ->
  ok.

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
