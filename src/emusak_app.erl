-module(emusak_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/playlist",emusak_handler,[]},
                                           {"/transcode/[:type]/[:id]",emusak_transcode,[]},
                                           {"/",cowboy_static,{file,"/home/mark/emusak/web/app/index.html"}},
                                           {"/[...]",cowboy_static, {dir,"/home/mark/emusak/web/app"}}
                                          ]
                                    }
                                   ]),
  {ok, _} = cowboy:start_http(
              http,
              100,
              [{port, 80}],
              [
               {env, [{dispatch, Dispatch}]}
              ]),
  {ok,_} = emusak_playlist_sup:start_link(),
  emusak_sup:start_link().

stop(_State) ->
  ok.
