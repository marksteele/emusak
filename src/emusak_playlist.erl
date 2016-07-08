%%%-------------------------------------------------------------------
%%% @author root <>
%%% @copyright (C) 2016, root
%%% @doc
%%%
%%% @end
%%% Created : 27 Feb 2016 by root <>
%%%-------------------------------------------------------------------
-module(emusak_playlist).

-behaviour(gen_server).
-include("emusak.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% API
-export([
         start_link/0,
         playlist/0,
         get_entry/1,
         list_artists/0,
         songs_by_artist/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{count,artists}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

playlist() ->
  gen_server:call(?SERVER,playlist).

get_entry(Id) ->
  gen_server:call(?SERVER,{get_entry,Id}).

list_artists() ->
  gen_server:call(?SERVER,list_artists).

songs_by_artist(Artist) ->
  gen_server:call(?SERVER,{songs_by_artist,Artist}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
  {ok, Dir} = application:get_env(emusak,music_dir),
  ets:new(
    playlist,
    [
     set,
     private,
     named_table,
     {keypos,#song.id},
     {read_concurrency,false},
     {write_concurrency,false},
     compressed
    ]
   ),
  {Songs,Artists} = filelib:fold_files(
            Dir,
            "(mp3|flac|m4a|MP3|FLAC|M4A|mp2|MP2|ogg|OGG|wma|WMA|ape|APE)\$",
            true,
            fun(F,{CountAcc,ArtistsAcc}) ->
                FileSize = filelib:file_size(F),
                FileParts = filename:split(F),
                FileName = filename:basename(lists:last(FileParts)),
                SongTitle = filename:rootname(FileName),
                Artist = lists:nth(4,FileParts),
                FileType = string:substr(filename:extension(F),2),
                ets:insert(playlist,#song{id=CountAcc,artist=list_to_binary(Artist),type=FileType,name=list_to_binary(SongTitle),file=F,size=FileSize}),
                {CountAcc + 1,sets:add_element(Artist,ArtistsAcc)}
            end,
            {1,sets:new()}
            ),
  random:seed(erlang:timestamp()),
  {ok, #state{count=Songs,artists=sets:to_list(Artists)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call(playlist, _, State) ->
  Playlist = [
              begin
                #{<<"url">> => list_to_binary(
                                  "/transcode/" ++
                                    Item#song.type ++
                                    "/" ++ integer_to_list(Item#song.id)),
                  <<"title">> => Item#song.name,
                  <<"artist">> => Item#song.artist}
              end
              || Item <- ets:tab2list(playlist)],
  {reply, {ok,jiffy:encode(Playlist)}, State};

handle_call({get_entry,Id}, _, State) ->
  [Item] = ets:lookup(playlist,Id),
  {reply, Item , State};

handle_call(list_artists,_,State=#state{artists=A}) ->
  {reply, {ok, jiffy:encode(A)},State};

handle_call({songs_by_artist,Artist},_,State) ->
  Playlist = [
              begin
                 #{<<"url">> => list_to_binary(
                                  "/transcode/" ++
                                     Item#song.type ++
                                     "/" ++ integer_to_list(Item#song.id)),
                    <<"title">> => Item#song.name,
                   <<"artist">> => Item#song.artist}
              end || Item <- ets:match_object(
                               playlist,
                               {song,'$1',Artist,'$2','$3','_'}
                              )
             ],
  {reply,{ok,jiffy:encode(Playlist)},State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
