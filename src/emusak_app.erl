-module(emusak_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/random_song",emusak_handler,[]},
                                           {"/transcode",emusak_transcode,[]},
                                           {"/", cowboy_static, {priv_file, emusak, "index.html"}},
                                           {"/js/[...]",cowboy_static, {priv_dir,emusak,"js/"}},
                                           {"/swf/[...]",cowboy_static, {priv_dir,emusak,"swf/"}},
                                           {"/[...]",cowboy_static, {dir,"/share/Music"}}
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
