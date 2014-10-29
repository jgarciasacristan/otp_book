-module(coffee).
-export([tea/0, espresso/0, capuccino/0, americano/0, cup_removed/0, pay/1, cancel/0]).
-export([start_link/0, init/0]).

start_link() ->
    {ok, spawn_link(?MODULE, init, [])}.

init() ->
    register(?MODULE, self()),
    hw:reboot(),
    hw:display("Make your selection", []),
    selection().

%% Client Functions for Drink Selection

tea()       -> ?MODULE ! {selection, tea,       100}.
espresso()  -> ?MODULE ! {selection, espresso,  150}.
americano() -> ?MODULE ! {selection, americano, 100}.
capuccino() -> ?MODULE ! {selection, capuccino, 150}.


%% Client Functions for Actions

cup_removed() -> ?MODULE ! cup_removed.
pay(Coin)     -> ?MODULE ! {pay, Coin}.
cancel()      -> ?MODULE ! cancel.

%% State: drink selection

selection() ->
    receive
	{selection, Type, Price} ->
	    hw:display("Please pay:~w", [Price]),
	    payment(Type, Price, 0)	
   end.

%% State: payment

payment(Type, Price, Paid) ->
    receive
	{pay, Coin} ->
	    if Coin + Paid >= Price ->
		    hw:display("Preparing Drink.",[]),
		    hw:return_change(Coin + Paid - Price),
		    hw:drop_cup(),
		    hw:prepare(Type),
		    hw:display("Remove Drink.",[]),
		    remove();
	       true  ->
		    ToPay = Price - (Coin + Paid),
		    hw:display("Please pay:~w", [ToPay]),
		    payment(Type, Price, Coin + Paid)
	    end;
	cancel ->
	    hw:display("Make your selection", []),
	    selection();
	_Other ->
	    payment(Type, Price, Paid)
     end.

%% State: remove_cup

remove() ->
    receive
	cup_removed ->
	    hw:display("Make your selection", []),
	    selection()
    end.
