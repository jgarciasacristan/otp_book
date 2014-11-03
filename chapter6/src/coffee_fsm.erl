-module(coffee_fsm).
-behaviour(gen_fsm).
-version('1.0').
-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).
-export([tea/0, espresso/0, capuccino/0, americano/0, cup_removed/0, pay/1, cancel/0]).


%% ------------------------------------------------------------------
%% gen_fsm Function Exports
%% ------------------------------------------------------------------

-export([init/1, selection/2, payment/2, remove/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3,
         code_change/4]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).


%% Client Functions for Drink Selection

tea()       -> gen_fsm:send_event(?MODULE, {selection, tea,       100}).
espresso()  -> gen_fsm:send_event(?MODULE, {selection, espresso,  150}).
americano() -> gen_fsm:send_event(?MODULE, {selection, americano, 100}).
capuccino() -> gen_fsm:send_event(?MODULE, {selection, capuccino, 150}).


%% Client Functions for Actions

cup_removed() -> gen_fsm:send_event(?MODULE, cup_removed).
pay(Coin)     -> gen_fsm:send_event(?MODULE, {pay, Coin}).
cancel()      -> gen_fsm:send_event(?MODULE, cancel).






%% ------------------------------------------------------------------
%% gen_fsm Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    hw:reboot(),
    hw:display("Make your selection", []),
    process_flag(trap_exit, true),
    {ok, selection, []}.

selection({selection, Type, Price}, _State) ->
    hw:display("Please pay: ~w", [Price]),
    {next_state, payment, {Type, Price, 0}, ?TIMEOUT};
selection({pay, Coin}, State) ->
    hw:return_change(Coin),
    {next_state, selection, State};
selection(_Other, State) -> 
    {next_state, selection, State}.



payment({pay, Coin}, {Type,Price,Paid}) when Coin+Paid < Price ->
    NewPaid = Coin + Paid,
    hw:display("Please pay:~w",[Price - NewPaid]),
    {next_state, payment, {Type, Price, NewPaid}, ?TIMEOUT};
payment({pay, Coin}, {Type,Price,Paid}) when Coin+Paid >= Price ->
    NewPaid = Coin + Paid,
    hw:display("Preparing Drink.",[]),
    hw:return_change(NewPaid - Price),
    hw:drop_cup(), hw:prepare(Type),
    hw:display("Remove Drink.", []),
    {next_state, remove, null};
payment(cancel, {_Type, _Price, Paid}) ->
    hw:display("Make Your Selection", []),
    hw:return_change(Paid),
    {next_state, selection, null};

payment(timeout, {_Type, _Price, Paid}) ->
    hw:display("Make Your Selection", []),
    hw:return_change(Paid),
    {next_state, selection, null};

payment(_Other, State) ->
    {next_state, payment, State}.

remove(cup_removed, State) ->
    hw:display("Make Your Selection", []),
    {next_state, selection, State};
remove({pay, Coin}, State) ->
    hw:return_change(Coin),
    {next_state, remove, State};
remove(_Other, State) ->
    {next_state, remove, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

