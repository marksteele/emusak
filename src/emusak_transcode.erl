-module(emusak_transcode).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([info/3]).
-export([terminate/3]).

init(_, Req0, _) ->
  {Path0,Req1} = cowboy_req:path(Req0),
  Path1 = cow_qs:urldecode(Path0),
  FSPath = "/share/Music" ++ string:substr(binary_to_list(Path1),11),

  _Port = erlang:open_port(
           {spawn_executable,"/root/emusak/ftm.sh"},
           [
            binary,
            stream,
            exit_status,
            use_stdio,
            in,
            {args,[filename:rootname(FSPath) ++ ".flac"]}
           ]),
  {ok, Req2} = cowboy_req:chunked_reply(
                 200, [
                       {<<"content-type">>,<<"audio/ogg, codec=opus">>},
                       {<<"connection">>,<<"close">>}
                      ],
                 Req1
                ),
  {loop, Req2, undefined_state}.

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
