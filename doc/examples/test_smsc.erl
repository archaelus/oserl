%%% Copyright (C) 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%% @doc Test SMSC
%%%
%%% <p>A very simple test SMSC.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://]
%%% @version 1.0, {18 Jun 2004} {@time}.
%%% @end
-module(test_smsc).

-behaviour(gen_smsc).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
%%-compile(export_all).
-export([start_link/0, deliver_sm/2, stop/0]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([init/1,
		 handle_bind/3, 
		 handle_operation/3, 
		 handle_unbind/3, 
		 handle_listen_error/1,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SYSTEM_ID, atom_to_list(?MODULE)).
-define(SMPP_PORT, 10002).
-define(DESTINATION_ADDR, "*").

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state}
%%
%% %@doc Representation of the server's state
%%
%% <dl>
%%   <dt>: </dt><dd>
%%   </dd>
%% </dl>
%% %@end
-record(state, {message_id = 0, rx = [], tx = []}).

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start_link() -> Result
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server.
%%
%% @see gen_server
%% @see start/0
%% @end
start_link() ->
	gen_smsc:start_link({local,?SERVER}, ?MODULE, ?DEFAULT_TIMERS, [],[]),
    gen_smsc:listen_start(?SERVER, ?SMPP_PORT, infinity).


deliver_sm(SourceAddress, ShortMessage) ->
    ParamList = [{source_addr, SourceAddress}, 
				 {destination_addr, ?DESTINATION_ADDR}, 
				 {short_message, ShortMessage}],
	gen_smsc:cast(?SERVER, {deliver_sm, ParamList}).


%% @spec stop() -> ok
%%
%% @doc Stops the server.
%%
%% @see handle_call/3
%%
%% @equiv gen_server:call(?SERVER, die, 10000).
%% @end
stop() ->
	gen_smsc:call(?SERVER, die, 10000).

%%%===================================================================
%%% Server functions
%%%===================================================================
%% @spec init(Args) -> Result
%%    Args    = term()
%%    Result  = {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%%    State   = term()
%%    Timeout = int() | infinity
%%    Reason  = term()
%%
%% @doc Initiates the server
init(Args) ->
	{ok, #state{}}.


%% @spec handle_call(Request, From, State) -> Result
%%    Request   = term()
%%    From      = {pid(), Tag}
%%    State     = term()
%%    Result    = {reply, Reply, NewState}          |
%%                {reply, Reply, NewState, Timeout} |
%%                {noreply, NewState}               |
%%                {noreply, NewState, Timeout}      |
%%                {stop, Reason, Reply, NewState}   |
%%                {stop, Reason, NewState}
%%    Reply     = term()
%%    NewState  = term()
%%    Timeout   = int() | infinity
%%    Reason    = term()
%%
%% @doc Handling call messages.
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, Reply, NewState}</tt>
%%   terminate/2 is called</li>
%%   <li>On <tt>{stop, Reason, NewState}</tt>
%%   terminate/2 is called</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_call(die, _From, State) ->
	{stop, normal, ok, State}.

%% @spec handle_cast(Request, State) -> Result
%%    Request  = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc Handling cast messages.
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_cast({deliver_sm, ParamList}, S) ->
	deliver_sm_iter(ParamList, S#state.rx),
	{noreply, S}.

%% @spec handle_info(Info, State) -> Result
%%    Info     = timeout | term()
%%    State    = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc Handling all non call/cast messages
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info(Info, State) ->
	{noreply, State}.

%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc Shutdown the server.
%%
%% <p>Return value is ignored by <tt>gen_server</tt>.</p>
%% @end
terminate(Reason, State) ->
	ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc Convert process state when code is changed
%% @end
code_change(OldVsn, State, Extra) ->
	{ok, State}.


%%%===================================================================
%%% SMSC functions
%%%===================================================================
%% @spec handle_bind(Bind, From, State) -> Result
%%    Bind = {bind_receiver, Pdu}    |
%%           {bind_transmitter, Pdu} |
%%           {bind_transceiver, Pdu}
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}</tt></li>
%%    Reply = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc.html#handle_bind-3">gen_smsc - 
%% handle_bind/3</a> callback implementation.
%% @end
handle_bind({bind_receiver, Rx, Pdu}, From, S) ->
	io:format("bound_rx ~n"),
	{reply, {ok, [{system_id, ?SYSTEM_ID}]}, S#state{rx = [Rx|S#state.rx]}};
handle_bind({bind_transmitter, Tx, Pdu}, From, S) ->
	io:format("bound_tx ~n"),
	{reply, {ok, [{system_id, ?SYSTEM_ID}]}, S#state{tx = [Tx|S#state.tx]}};
handle_bind({bind_transceiver, Trx, Pdu}, From, S) ->
	io:format("bound_trx ~n"),
	{reply, {ok, [{system_id, ?SYSTEM_ID}]}, S#state{rx = [Trx|S#state.rx],
													 tx = [Trx|S#state.tx]}}.


%% @spec handle_operation(Operation, From, State) -> Result</tt>
%%    Operation = {SystemId, {broadcast_sm, Pdu}} |
%%                {SystemId, {cancel_broadcast_sm, Pdu} |
%%                {SystemId, {cancel_sm, Pdu} |
%%                {SystemId, {query_broadcast_sm, Pdu} |
%%                {SystemId, {query_sm, Pdu} |
%%                {SystemId, {replace_sm, Pdu} |
%%                {SystemId, {submit_multi, Pdu} |
%%                {SystemId, {submit_sm, Pdu} |
%%                {SystemId, {data_sm, Pdu}
%%    SystemId = string(),
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc.html#handle_operation-3">gen_smsc - 
%% handle_operation/3</a> callback implementation.
%% @end
handle_operation({Submit, Session, Pdu}, From, S) when Submit == submit_sm;
                                                       Submit == data_sm ->
    N = operation:get_param(destination_addr, Pdu),
    T = element(2, sm:message_user_data(Pdu)),
    I = S#state.message_id,
	io:format("New  SM: ~p - ~p~n", [N, T]),
    {reply, {ok, [{message_id, integer_to_list(I)}]}, S#state{message_id=I+1}};
handle_operation({_Operation, Session, Pdu}, From, S) ->
	io:format("*** operation - ~p ***~n", [operation:to_list(Pdu)]),
	{reply, {error, ?ESME_RINVCMDID, []}, S}.


%% @spec handle_unbind(Unbind, From, State) -> Result
%%    Unbind = {SystemId, BoundAs}
%%    SystemId = term()
%%    BoundAs = bind_receiver | bind_transmitter | bind_transceiver
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = ok | {error, Error}
%%    Error = int()
%%
%% @doc <a href="gen_smsc.html#handle_unbind-3">gen_smsc - 
%% handle_unbind/3</a> callback implementation.
%% @end
handle_unbind({unbind, Session, Pdu}, _From, S) ->
    io:format("*** unbind - ~p ***~n", [operation:to_list(Pdu)]),
    {reply, ok, S#state{rx = lists:delete(Session, S#state.rx),
                        tx = lists:delete(Session, S#state.tx)}}.


%% @spec handle_listen_error(State) -> Result
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    Reply = ok | {error, Error}
%%    Error = int()
%%
%% @doc <a href="gen_smsc.html#handle_listen_error-1">gen_smsc - 
%% handle_listen_error/1</a> callback implementation.
%% @end
handle_listen_error(State) ->
	io:format("*** listen error ***~n", []),
	{noreply, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
deliver_sm_iter(ParamList, []) ->
	ok;
deliver_sm_iter(ParamList, [H|T]) ->
	gen_smsc:deliver_sm(?SERVER, H, ParamList),
	deliver_sm_iter(ParamList, T).
