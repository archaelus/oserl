%%% Copyright (C) 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%
%%% This library is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU Lesser General Public
%%% License as published by the Free Software Foundation; either
%%% version 2.1 of the License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%% @doc Generic SMSC.
%%%
%%% <p>A generic SMSC implemented as a gen_server.</p>
%%%
%%%
%%% <h2>Callback Function Index</h2>
%%%
%%% <p>A module implementing this behaviour must export these functions.  
%%% Leaving a callback undefined crashes the entire SMSC whenever that
%%% particular function is called.</p>
%%%
%%% <table width="100%" border="1">
%%%   <tr>
%%%     <td valign="top"><a href="#handle_bind-3">handle_bind/3</a></td>
%%%     <td>Forwards <i>bind_receiver</i>, <i>bind_transmitter</i> and 
%%%       <i>bind_transceiver</i> operations (from the peer ESMEs) to the 
%%%       callback SMSC.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_operation-3">handle_operation/3</a>
%%%     </td>
%%%     <td>Forwards <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%%%       <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%%%       <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%%%       <i>data_sm</i> operations (from the peer ESMEs) to the callback 
%%%       SMSC.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_unbind-3">handle_unbind/3</a></td>
%%%     <td>This callback forwards an unbind request (issued by peer ESMEs) 
%%%       to the SMSC.
%%%     </td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%% 
%%% <h3><a name="handle_bind-3">handle_bind/3</a></h3>
%%%
%%% <tt>handle_bind(Bind, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>Bind = {bind_receiver, SystemId, Pdu}    |
%%%                  {bind_transmitter, SystemId, Pdu} |
%%%                  {bind_transceiver, SystemId, Pdu}</tt></li>
%%%   <li><tt>SystemId= string()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>From = term()</tt></li>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {reply, Reply, NewState}          |
%%%                    {reply, Reply, NewState, Timeout} |
%%%                    {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, Reply, NewState}   |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>Reply = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>bind_receiver</i>, <i>bind_transmitter</i> and 
%%% <i>bind_transceiver</i> operations (from the peer ESMEs) to the 
%%% callback SMSC.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the bind response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the ESME in the response PDU, the callback should return the
%%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%%% desired command_status error code.</p>
%%%
%%% 
%%% <h3><a name="handle_operation-3">handle_operation/3</a></h3>
%%%
%%% <tt>handle_operation(Operation, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>Operation = {broadcast_sm, SystemId, Pdu} |
%%%                       {cancel_broadcast_sm, SystemId, Pdu} |
%%%                       {cancel_sm, SystemId, Pdu} |
%%%                       {query_broadcast_sm, SystemId, Pdu} |
%%%                       {query_sm, SystemId, Pdu} |
%%%                       {replace_sm, SystemId, Pdu} |
%%%                       {submit_multi, SystemId, Pdu} |
%%%                       {submit_sm, SystemId, Pdu} |
%%%                       {data_sm, SystemId, Pdu}</tt></li>
%%%   <li><tt>SystemId = string()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>From = term()</tt></li>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {reply, Reply, NewState}          |
%%%                    {reply, Reply, NewState, Timeout} |
%%%                    {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, Reply, NewState}   |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>Reply = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%%% <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%%% <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%%% <i>data_sm</i> operations (from the peer ESMEs) to the callback SMSC.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the ESME in the response PDU, the callback should return the
%%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%%% desired command_status error code.</p>
%%%
%%% 
%%% <h3><a name="handle_unbind-3">handle_unbind/3</a></h3>
%%%
%%% <tt>handle_unbind(Unbind, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>Unbind = {bound_rx, SystemId, Pdu} |
%%%                    {bound_tx, SystemId, Pdu} |
%%%                    {bound_trx, SystemId, Pdu}</tt></li>
%%%   <li><tt>SystemId = string()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>Result = {reply, Reply, NewState}          |
%%%                    {reply, Reply, NewState, Timeout} |
%%%                    {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, Reply, NewState}   |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>Reply = ok | {error, Error}</tt></li>
%%%   <li><tt>Error = int()</tt></li>
%%% </ul>
%%%
%%% <p>This callback forwards an unbind request (issued by peer ESMEs) to the 
%%% SMSC.</p>
%%%
%%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%%% command_status is sent to the MC and the session moves into the unbound
%%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%%% command_status and the session will remain on it's current bound state
%%% (bound_rx, bound_tx or bound_trx).</p>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 0.1, {13 May 2004} {@time}.
%%% @end
%%%
%%% %@TODO Outbind is synchronous, may block the entire SMSC during connection
%%% establishment.
-module(gen_smsc).

-behaviour(gen_server).
-behaviour(gen_smsc_session).
-behaviour(gen_connection).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% Behaviour exports
%%%-------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
%%-compile(export_all).
-export([start/4,
         start/5,
         start_link/4,
         start_link/5, 
         listen/1,
         listen/3,
         alert_notification/3,
         outbind/4,
         deliver_sm/3,
         data_sm/3,
         unbind/3,
         call/2, 
         call/3, 
         cast/2, 
         reply/2]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-------------------------------------------------------------------
%%% Internal gen_smsc_session exports
%%%-------------------------------------------------------------------
-export([handle_bind/3, handle_operation/3, handle_unbind/2]).

%%%-------------------------------------------------------------------
%%% Internal gen_connection exports
%%%-------------------------------------------------------------------
-export([handle_accept/3, handle_input/4]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SESSION_MODULE, gen_smsc_session).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Mod, ModState, Timers, Sessions}
%%    Mod           = atom()
%%    ModState      = atom()
%%    Timers        = #timers()
%%    Sessions      = ets()
%%
%% %@doc Representation of the server's state.
%%
%% <dl>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>ModState: </dt><dd>Callback module private state.</dd>
%%   <dt>Timers: </dt><dd>SMPP timers.</dd>
%%   <dt>Sessions: </dt><dd>ETS table with the active sessions.</dd>
%% </dl>
%% %@end
-record(state, {mod, mod_state, timers, listener, sessions, peers}).


%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec behaviour_info(Category) -> Info
%%    Category      = callbacks | term()
%%    Info          = CallbacksInfo | term()
%%    CallbacksInfo = [{FunctionName, Arity}]
%%    FunctionName  = atom()
%%    Arity         = int()
%%
%% @doc Gives information about the behaviour.
%% @end
behaviour_info(callbacks) ->
    [{init, 1},
     {handle_bind, 3}, 
     {handle_operation, 3}, 
     {handle_unbind, 3}, 
     {handle_session_failure, 2},
     {handle_listen_error, 1},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2},
     {code_change, 3}];
behaviour_info(_Other) ->
    undefined.


%% @spec start(Module, Timers, Args, Options) -> Result
%%    Module = atom()
%%    Timers = #timers()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_SMPP_TIMERS()</tt> to define this parameter if default SMPP 
%% timers are OK for you.</p>
%%
%% @see gen_server:start/3
%% @see start_link/4
%% @see start/5
%% @end
start(Module, Timers, Args, Options) ->
    gen_server:start(?MODULE, {Module, Timers, Args}, Options).


%% @spec start(ServerName, Module, Timers, Args, Options) -> Result
%%    ServerName = {local, Name} | {global, Name}
%%    Name       = atom()
%%    Module     = atom()
%%    Timers     = #timers()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_SMPP_TIMERS()</tt> to define this parameter if default SMPP 
%% timers are OK for you.</p>
%%
%% @see gen_server:start/4
%% @see start_link/5
%% @see start/4
%% @end
start(ServerName, Module, Timers, Args, Options) ->
    gen_server:start(ServerName, ?MODULE, {Module, Timers, Args}, Options).


%% @spec start_link(Module, Timers, Args, Options) -> Result
%%    Module = atom()
%%    Timers = #timers()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_SMPP_TIMERS()</tt> to define this parameter if default SMPP 
%% timers are OK for you.</p>
%%
%% @see gen_server:start_link/3
%% @see start_link/5
%% @see start/4
%% @end
start_link(Module, Timers, Args, Options) ->
    gen_server:start_link(?MODULE, {Module, Timers, Args}, Options).


%% @spec start_link(ServerName, Module, Timers, Args, Options) -> Result
%%    ServerName = {local, Name} | {global, Name}
%%    Name       = atom()
%%    Module     = atom()
%%    Timers     = #timers()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_SMPP_TIMERS()</tt> to define this parameter if default SMPP 
%% timers are OK for you.</p>
%%
%% @see gen_server:start_link/4
%% @see start_link/4
%% @see start/5
%% @end
start_link(ServerName, Module, Timers, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, {Module,Timers,Args}, Options).


%% @spec listen(ServerRef) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Reason = term()
%%
%% @doc Puts the SMSC Server <tt>ServerRef</tt> to listen on 
%% <tt>DEFAULT_SMPP_PORT</tt>.
%%
%% <p>Returns <tt>true</tt> if success, <tt>false</tt> otherwise.</p>
%%
%% @see listen/2
%% @equiv listen(ServerRef, DEFAULT_SMPP_PORT, infinity)
%% @end 
listen(ServerRef) -> 
    listen(ServerRef, ?DEFAULT_SMPP_PORT, infinity).


%% @spec listen(ServerRef, Port, Count) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Port = int()
%%    Count = int() | infinity
%%    Reason = term()
%%
%% @doc Puts the SMSC Server <tt>ServerRef</tt> to listen on <tt>Port</tt>
%%
%% <p>Returns <tt>true</tt> if success, <tt>false</tt> otherwise.</p>
%% @end 
listen(ServerRef, Port, Count) ->
    gen_server:call(ServerRef, {listen, Port, Count}).


%% @spec alert_notification(ServerRef, SystemId, ParamList) -> Result
%%    Sid        = atom()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues an alert notification operation on the session identified by 
%% <tt>Sid</tt>.
%% @end
alert_notification(ServerRef, SystemId, ParamList) ->
    gen_server:cast(ServerRef, {alert_notification, SystemId, ParamList}).


%% @spec outbind(ServerRef, SystemId, ParamList) -> Result
%%    Sid        = atom()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues an outbind operation on the session identified by <tt>Sid</tt>.
%% @end
outbind(ServerRef, Addr, Port, ParamList) ->
    gen_server:call(ServerRef, {outbind, Addr, Port, ParamList}, infinity).


%% @spec data_sm(ServerRef, SystemId, ParamList) -> Result
%%    Sid        = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a data_sm operation on the session identified by <tt>Sid
%% </tt>.
%% @end
data_sm(ServerRef, SystemId, ParamList) ->
    gen_server:call(ServerRef, {data_sm, SystemId, ParamList}, infinity).


%% @spec deliver_sm(ServerRef, SystemId, ParamList) -> Result
%%    Sid        = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a deliver_sm operation on the session identified by <tt>Sid
%% </tt>.
%% @end
deliver_sm(ServerRef, SystemId, ParamList) ->
    gen_server:call(ServerRef, {deliver_sm, SystemId, ParamList}, infinity).


%% @spec unbind(ServerRef, SystemId, BoundAs) -> Result
%%    Sid     = atom()
%%    Result  = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error   = int()
%%
%% @doc Issues an unbind operation on the session identified by <tt>Sid
%% </tt>.
%% @end
unbind(ServerRef, SystemId, BoundAs) ->
    gen_server:call(ServerRef, {unbind, SystemId, BoundAs}, infinity).


%% @spec call(ServerRef, Request) -> Reply
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Request = term()
%%    Reply = term()
%%
%% @doc Equivalent to gen_server:call/2.  <tt>Request</tt> is an arbitrary
%% term which is passed as one of the arguments to <tt>Module:handle_call/3
%% </tt>.
%%
%% <p>Please refer to the gen_server man page for greater details.</p>
%% @end 
call(ServerRef, Request) ->
    gen_server:call(ServerRef, {call, Request}).


%% @spec call(ServerRef, Request, Timeout) -> Reply
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Request = term()
%%    Timeout = int() > 0 | infinity
%%    Reply = term()
%%
%% @doc Equivalent to gen_server:call/3.  <tt>Request</tt> is an arbitrary
%% term which is passed as one of the arguments to <tt>Module:handle_call/3
%% </tt>.
%%
%% <p>Please refer to the gen_server man page for greater details.</p>
%% @end 
call(ServerRef, Request, Timeout) ->
    gen_server:call(ServerRef, {call, Request}, Timeout).
    

%% @spec cast(ServerRef, Request) -> Reply
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Request = term()
%%    Reply = term()
%%
%% @doc Equivalent to gen_server:cast/2.  <tt>Request</tt> is an arbitrary
%% term which is passed as one of the arguments to <tt>Module:handle_cast/2
%% </tt>.
%%
%% <p>Please refer to the gen_server man page for greater details.</p>
%% @end 
cast(ServerRef, Request) ->
    gen_server:cast(ServerRef, {cast, Request}).


%% @spec reply(Client, Reply) -> true
%%    Client = term()
%%    Reply  = term()
%%
%% @doc Equivalent to gen_server:reply/2.  Please refer to the gen_server man
%% page for greater details.
%% @end 
reply(Client, Reply) ->
    gen_server:reply(Client, Reply).


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
init({Mod, Timers, Args}) ->
	S = #state{mod      = Mod, 
			   timers   = Timers, 
			   listener = closed,
			   sessions = ets:new(sessions, []),
			   peers    = ets:new(peers, [])},
    process_flag(trap_exit, true),
    pack(Mod:init(Args), S).


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
handle_call({call, Request}, From, S) ->
    pack((S#state.mod):handle_call(Request, From, S#state.mod_state), S);
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Peer ESME requests
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
handle_call({bind_receiver, Pdu}, {Rx, _} = From, S) ->
    Id = operation:get_param(system_id, Pdu),
	R  = {bind_receiver, Id, Pdu},
	case ets:match(S#state.peers, {Id, '$1', '$2'}, 1) of
		'$end_of_table' ->
		{[[undefined, Tx]], _} ->
			ets:insert(S#state.sessions, {Rx, Id}),
			ets:insert(S#state.peers, {Id, Rx, Tx}),
			pack((S#state.mod):handle_bind(R, From, S#state.mod_state), S);


		{[[Session, _Tx]], _}     -> {rx,  Id, Pdu};
		{[[_Rx, Session]], _}     -> {tx,  Id, Pdu}
	end,

	case ets:lookup_el
handle_call({bind_transmitter, Pdu}, {Tx, _} = From, S) ->
    Id = operation:get_param(system_id, Pdu),
	Rx = ets:lookup_element(S#state.peers, Id, 2),
	R  = {bind_transmitter, Id, Pdu},
    ets:insert(S#state.sessions, {Rx, Id}),
    ets:insert(S#state.peers, {Id, Rx, Tx}),
    pack((S#state.mod):handle_bind(R, From, S#state.mod_state), S);
handle_call({bind_transceiver, Pdu}, {Trx, _} = From, S) ->
    Id = operation:get_param(system_id, Pdu),
	R  = {bind_transmitter, Id, Pdu},
    ets:insert(S#state.sessions, {Trx, Id}),
    ets:insert(S#state.peers, {Id, Trx, Trx}),
    pack((S#state.mod):handle_bind(R, From, S#state.mod_state), S);
handle_call({unbind, Pdu}, {Session, _} = From, S) ->
	Id = ets:lookup_element(S#state.sessions, Session, 2),
	R  = case ets:match(S#state.peers, {Id, '$1', '$2'}, 1) of
			 {[[Session, Session]], _} -> {trx, Id, Pdu};
			 {[[Session, _Tx]], _}     -> {rx,  Id, Pdu};
			 {[[_Rx, Session]], _}     -> {tx,  Id, Pdu}
		 end,
	pack((S#state.mod):handle_unbind(R, From, S#state.mod_state), S);
handle_call({CmdName, Pdu}, {Session, _} = From, S) ->
	Id = ets:lookup_element(S#state.sessions, Session, 2),
    R  = {CmdName, Id, Pdu},
	pack((S#state.mod):handle_operation(R, From, S#state.mod_state), S);
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% SMSC requests
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
handle_call({accept, Conn, _Socket}, From, S) ->
    case gen_smsc_session:start_link(?MODULE, Conn, S#state.timers) of
        {ok, Session} ->
            % The system_id of the peer ESME is still undefined
            ets:insert(S#state.sessions, {Session, undefined}),
            {reply, {ok, Session}, S};
        _Error ->
            {reply, error, S}
    end;
handle_call({listen, Port, Count}, From, S) ->
    case gen_connection:start_listen(?MODULE, ?SESSION_MODULE, Port, Count) of
        {ok, Listener} ->
            {reply, ok, S#state{listener = Listener}};
        Error ->
            {reply, Error, S}
    end;
handle_call({alert_notification, Id, ParamList}, From, S) ->
	Rx = ets:lookup_element(S#state.peers, Id, 2),
	{reply, gen_smsc_session:alert_notification(Rx, ParamList), S};
handle_call({outbind, Addr, Port, ParamList}, _From, S) ->
	% This shoud be asynchronous.
	case gen_connection:start_connect(?SESSION_MODULE, Addr, Port) of
		{ok, Conn} ->
			case gen_smsc_session:start_link(?MODULE, Conn, S#state.timers) of
				{ok, Session} ->
                    % Peer system_id is undefined
					ets:insert(S#state.sessions, {Session, undefined}),
					{reply, gen_smsc_session:outbind(Session, ParamList), S};
				SessionError ->
					{reply, SessionError, S}
			end;
		ConnectError ->
			{reply, ConnectError, S}
	end;
handle_call({unbind, BoundAs, Id}, From, S) ->
	Session = if
				  BoundAs == rx -> ets:lookup_element(S#state.peers, Id, 2);
				  true          -> ets:lookup_element(S#state.peers, Id, 3)
			  end,
	spawn_link(fun() -> unbind(Session, From) end),
	{noreply, S};
handle_call({CmdName, Id, ParamList}, From, S) ->
	Rx = ets:lookup_element(S#state.peers, Id, 2),
	spawn_link(fun() -> operation(CmdName, Rx, ParamList, From) end),
	{noreply, S}.


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
handle_cast({cast, Request}, S) ->
    pack((S#state.mod):handle_cast(Request, S#state.mod_state), S).


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
handle_info({'EXIT', Child, normal}, S) when Child == S#state.listener ->
    % The listener terminates with normal status.
	{noreply, S#state{listener = closed}};
handle_info({'EXIT', Child, Reason}, S) when Child == S#state.listener ->
    % The listener terminates on error.
	NewS = S#state{listener = closed},
	pack((NewS#state.mod):handle_listen_error(NewS#state.mod_state), NewS);
handle_info({'EXIT', Child, Reason} = Info, S) ->
    % A Child process terminates with normal status.
	case catch ets:lookup_element(S#state.sessions, Child, 2) of
		{'EXIT', What} ->
            pack((S#state.mod):handle_info(Info, S#state.mod_state), S);
		undefined -> % Child is a open session
            ets:delete(S#state.sessions, Child),
            {noreply, S};
		Id ->        % Child is a bound session
            ets:delete(S#state.sessions, Child),
			case ets:match(S#state.peers, {Id, '$1', '$2'}, 1) of
				{[[Rx, Child]], _} when Rx == Child; Rx == undefined ->
					ets:delete(S#state.peers, Id);
				{[[Child, Tx]], _} when Tx == Child; Tx == undefined ->
					ets:delete(S#state.peers, Id);
				{[[Rx, Child]], _} ->
					ets:insert(S#state.peers, {Id, Rx, undefined});
				{[[Child, Tx]], _} ->
					ets:insert(S#state.peers, {Id, undefined, Tx})
			end,
			if
				Reason == normal ->
					{noreply, S};
				true ->
					pack((S#state.mod):handle_session_failure(
						   R, S#state.mod_state), S)
			end
    end;
handle_info(Info, S) ->
    pack((S#state.mod):handle_info(Info, S#state.mod_state), S).


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc Shutdown the server.
%%
%% <p>Return value is ignored by <tt>gen_server</tt>.</p>
%% @end
terminate(Reason, S) ->
	io:format("*** gen_smsc terminating: ~p - ~p ***~n", [self(), Reason]),
    pack((S#state.mod):terminate(Reason, S#state.mod_state), S).


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc Convert process state when code is changed
%% @end
code_change(OldVsn, S, Extra) ->
    pack((S#state.mod):code_change(OldVsn, S#state.mod_state, Extra), S).


%%%===================================================================
%%% SMSC Session functions
%%%===================================================================
%% @spec handle_bind(SMSC, CmdName, Pdu) -> Result
%%    SMSC = pid()
%%    CmdName = bind_receiver | bind_transmitter | bind_transceiver
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#handle_bind-3">gen_smsc_session - 
%% handle_bind/3</a> callback implementation.
%% @end
handle_bind(SMSC, CmdName, Pdu) ->
    gen_server:call(SMSC, {CmdName, Pdu}, infinity).


%% @spec handle_operation(SMSC, CmdName, Pdu) -> Result
%%    SMSC = pid()
%%    CmdName = broadcast_sm |
%%              cancel_broadcast_sm |
%%              cancel_sm |
%%              query_broadcast_sm |
%%              query_sm |
%%              replace_sm |
%%              submit_multi |
%%              submit_sm |
%%              data_sm
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#handle_operation-3">gen_smsc_session - 
%% handle_operation/3</a> callback implementation.
%% @end
handle_operation(SMSC, CmdName, Pdu) ->
    gen_server:call(SMSC, {CmdName, Pdu}, infinity).


%% @spec handle_unbind(SMSC, Pdu) -> ok | {error, Error}
%%    SMSC = pid()
%%    Pdu = pdu()
%%    Error = int()
%%
%% @doc <a href="gen_smsc_session.html#handle_unbind-3">gen_smsc_session - 
%% handle_unbind/3</a> callback implementation.
%% @end
handle_unbind(SMSC, Pdu) ->
    gen_server:call(SMSC, {unbind, Pdu}, infinity).


%%%===================================================================
%%% Server gen_connection functions
%%%===================================================================
%% @spec handle_accept(Owner, Conn, Socket) -> Result
%%    Owner = Conn = NewOwner = pid()
%%    Socket = socket()
%%    Result = {ok, NewOwner} | error
%%
%% @doc <a href="gen_connection.html#handle_accept-3">gen_connection 
%% - handle_accept/3</a> callback implementation.
%% @end
handle_accept(Owner, Conn, Socket) -> 
    gen_server:call(Owner, {accept, Conn, Socket}, infinity).


%% @spec handle_input(Owner, Conn, Input, Lapse) -> {ok, RestBuffer}
%%    Pid = pid()
%%    Conn = pid()
%%    Input = binary()
%%    RestBuffer = binary()
%%    Lapse = int()
%%
%% @doc <a href="gen_connection.html#handle_input-4">gen_connection
%% - handle_input/4</a> callback implementation.
%% @end
handle_input(_Owner, _Conn, _Input, _Lapse) -> 
    exit({unexpected_callback, handle_input}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
%% @spec operation(CmdName, Session, ParamList, From) -> true
%%
%% @doc Issues a SMPP operation on the given <tt>Session</tt>.
%% @end 
operation(CmdName, Session, ParamList, From) ->
	Reply = gen_smsc_session:CmdName(Session, ParamList),
	gen_server:reply(From, Reply).


%% @spec unbind(CmdName, Session, ParamList, From) -> true
%%
%% @doc Issues an <i>unbind</i> on the given <tt>Session</tt>.
%% @end 
unbind(Session, From) ->
	Reply = gen_smsc_session:unbind(Session),
	gen_server:reply(From, Reply).


%% @spec pack(CallbackReply, State) -> Reply
%%
%% @doc The callback module replies as if I were gen_server.  Pack his
%% state into mine.
%%
%% <p>Originally written by Ulf Wiger for his dispatched_server.erl module.</p>
%% @end 
pack({reply, Reply, MS}, S) ->
    {reply, Reply, S#state{mod_state = MS}};
pack({reply, Reply, MS, Timeout}, S) ->
    {reply, Reply, S#state{mod_state = MS}, Timeout};
pack({noreply, MS}, S) ->
    {noreply, S#state{mod_state = MS}};
pack({noreply, MS, Timeout}, S) ->
    {noreply, S#state{mod_state = MS}, Timeout};
pack({stop, Reason, Reply, MS}, S) ->
    {stop, Reason, Reply, S#state{mod_state = MS}};
pack({stop, Reason, MS}, S) ->
    {stop, Reason,  S#state{mod_state = MS}};
pack({ok, MS}, S) ->
    {ok, S#state{mod_state = MS}};
pack({ok, MS, Timeout}, S) ->
    {ok, S#state{mod_state = MS}, Timeout};
pack(Other, _S) ->
    Other.
