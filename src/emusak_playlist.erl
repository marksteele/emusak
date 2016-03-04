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

%% API
-export([
         start_link/0,
         getlist/1
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {playlist=[],current=1,length}).

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

getlist(N) ->
  gen_server:call(?SERVER,{getlist,N}).

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
  {Author,Type,Name,Path}
  PL = ets:new(
         playlist,
         [
          bag,
          private,
          named_table,
          {keypos,3},
          {read_concurrency,false},
          {write_concurrency,false},
          compressed
         ]
        ),
  Playlist = filelib:fold_files(
               "/share/Music",
               "(mp3|flac)\$",
               true,
               fun(F,Acc) ->
                   FileParts = filename:split(F),
                   FileName = filename:basename(lists:last(FileParts)),
                   SongTitle = filename:rootname(FileName),
                   Artist = lists:nth(4,FileParts),
                   FileType0 = string:substr(filename:extension(F),2),
                   {FileType1,URLPath} = case FileType0 of
                                           "flac" ->
                                             {<<"oga">>,"/transcode/" ++ Artist ++ "/" ++ SongTitle ++ ".ogg"};
                                           _ ->
                                             {list_to_binary(FileType0),re:replace(F,"^/share/Music","",[{return,list}])}
                                         end,
                   Acc ++ [#{FileType1 => list_to_binary(URLPath),
                            <<"title">> => list_to_binary(SongTitle),
                            <<"artist">> => list_to_binary(Artist)}]
               end,
               []),
  Shuffled = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- Playlist])],
  {ok, #state{playlist=Shuffled,length=length(Playlist)}}.

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

handle_call({getlist,N}, _, State=#state{playlist=Playlist,current=Current,length=L}) ->
  List = lists:sublist(Playlist,Current,N),
  NewPosition = case (Current + N) > L of
                  true ->
                    1;
                  false ->
                    Current + N
                end,
  {reply, {ok,jiffy:encode(List)}, State#state{current=NewPosition}};

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
