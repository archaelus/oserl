%%% Copyright (C) 2004 Enrique Marcote Peña <quique@orzan.nomasprestig.es>
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

%%% @doc Sample echo ESME.
%%%
%%% <p>This ESME echoes SM back to the caller.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <quique@orzan.nomasprestig.es>
%%%         [http://]
%%% @version 1.0, {23 Jun 2004} {@time}.
%%% @end
-module(echo_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
%%-compile(export_all).
-export([start_link/0, bind/0, bind/4, unbind/0, stop/0]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([init/1,
		 handle_operation/3,
		 handle_call/3,
		 handle_cast/2,
		 handle_info/2,
		 terminate/2,
		 code_change/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).
%-define(SMSC_ADDRESS, {193, 144, 50, 51}).
-define(SMSC_ADDRESS, {192, 168, 1, 4}).
%-define(SMPP_PORT, 10002).
-define(SMPP_PORT, ?DEFAULT_SMPP_PORT).
-define(SYSTEM_ID, "amuse").
-define(PASSWORD, "p1l0t0").
-define(SOURCE_ADDR, "1949").

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
-record(state, {rx_session, tx_session}).

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
	gen_esme:start_link({local, ?SERVER}, ?MODULE, ?DEFAULT_TIMERS, [], []).

bind() ->
	bind(?SMSC_ADDRESS, ?SYSTEM_ID, ?PASSWORD, ?SOURCE_ADDR).

bind(SMSCAddress, Id, Pwd, SourceAddr) ->
	ParamList = [{system_id, Id},{password, Pwd},{source_addr, SourceAddr}],
	{ok, Rx} = gen_esme:session(?SERVER, SMSCAddress, ?SMPP_PORT),
%	receive	after 3000 -> ok end,
	case gen_esme:bind_receiver(?SERVER, Rx, ParamList) of
		{ok, _PduRx} ->
			gen_esme:cast(?SERVER, {bound_rx, Rx});
		ErrorRx ->
			exit(ErrorRx)
	end,
	{ok, Tx} = gen_esme:session(?SERVER, SMSCAddress, ?SMPP_PORT),
%	receive	after 3000 -> ok end,
	case gen_esme:bind_transmitter(?SERVER, Tx, ParamList) of
		{ok, _PduTx} ->
			gen_esme:cast(?SERVER, {bound_tx, Tx});
		ErrorTx ->
			exit(ErrorTx)
	end.

unbind() ->
	gen_esme:cast(?SERVER, unbind).
	
%% @spec stop() -> ok
%%
%% @doc Stops the server.
%%
%% @see handle_call/3
%%
%% @equiv gen_server:call(?SERVER, die, 10000).
%% @end
stop() ->
	gen_esme:call(?SERVER, die, 10000).

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
init([]) ->
	{ok, #state{}}.


%% @spec handle_operation({CmdName, Session, Pdu}, From, State) -> Result
%%    Operation = {deliver_sm, Session, Pdu} | {data_sm, Session, Pdu}
%%    Session = pid()
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
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_esme.html#handle_operation-3">gen_esme - 
%% handle_operation/3</a> callback implementation.
%% @end
handle_operation({deliver_sm, _Session, Pdu}, From, S) ->
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    io:format("Echoing SM: ~p~n", [Mesg]),
    spawn_link(fun() -> gen_esme:submit_sm(?SERVER, S#state.tx_session, [Mesg|Dest]) end),
    {reply, {ok, []}, S};
handle_operation({data_sm, Session, Pdu}, From, S) ->
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    io:format("Echoing SM: ~p~n", [Mesg]),
    spawn(fun() -> gen_esme:data_sm(?SERVER, S#state.tx_session, [Mesg|Dest]) end), 
    {reply, {ok, []}, S}.


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
handle_call(die, _From, S) ->
	{stop, normal, ok, S}.

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
handle_cast({bound_rx, Session}, S) ->
	{noreply, S#state{rx_session = Session}};
handle_cast({bound_tx, Session}, S) ->
	{noreply, S#state{tx_session = Session}};
handle_cast(unbind, S) ->
    spawn_link(fun() -> gen_esme:unbind(?SERVER, S#state.rx_session) end),
    spawn_link(fun() -> gen_esme:unbind(?SERVER, S#state.tx_session) end),
	{noreply, S#state{rx_session = undefined, tx_session = undefined}}.


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

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
