-module(emusak_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  {ok, Dir} = application:get_env(emusak,web_dir),
  Dispatch = cowboy_router:compile([
                                    {'_', [
                                           {"/playlist",emusak_handler,[]},
                                           {"/auth",emusak_auth,[]},
                                           {"/transcode/[:type]/[:id]",emusak_transcode,[]},
                                           {"/",cowboy_static,{file,Dir ++ "/index.html"}},
                                           {"/[...]",cowboy_static, {dir,Dir}}
                                          ]
                                    }
                                   ]),
  {ok, _} = cowboy:start_http(
              http,
              100,
              [{port, 80}],
              [
                {env,[{dispatch,Dispatch}]},
                {middlewares,[cowboy_router, emusak_auth_middleware,cowboy_handler]}
              ]
              ),
  {ok,_} = emusak_playlist_sup:start_link(),
  emusak_sup:start_link().

stop(_State) ->
  ok.
