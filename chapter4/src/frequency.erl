-module(frequency).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, allocate/0, deallocate/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

allocate() ->
    gen_server:call(?SERVER, {allocate, self()}).

deallocate(Frequency) ->
    gen_server:cast(?SERVER, {deallocate, Frequency}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    Frequencies = {get_frequencies(), []},
    {ok, Frequencies}.



handle_call({allocate, Pid}, _From, Frequencies) ->
    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
    {reply, Reply, NewFrequencies}.

handle_cast({deallocate, Freq}, Frequencies) ->
    NewFrequencies = deallocate(Frequencies, Freq),
    {noreply, NewFrequencies}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


get_frequencies() ->
    [10,11,12,13,14,15].

allocate({[],Allocated}, _Pid) ->
    {{[],Allocated},{error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free,[{Freq,Pid}|Allocated]},{ok, Freq}}.


deallocate({Free, Allocated}, Freq) ->
    NewAllocated=lists:keydelete(Freq,1,Allocated),
    {[Freq|Free], NewAllocated}.
 
