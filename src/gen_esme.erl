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

%%% @doc Generic ESME.
%%%
%%% <p>A generic ESME implemented as a gen_server.</p>
%%%
%%%
%%% <h2>Callback Function Index</h2>
%%%
%%% <p>A module implementing this behaviour must export these functions.  
%%% Leaving a callback undefined crashes the entire ESME whenever that
%%% particular function is called.</p>
%%%
%%% <table width="100%" border="1">
%%%   <tr>
%%%     <td valign="top"><a href="#handle_outbind-3">handle_outbind/3</a></td>
%%%     <td>Forwards <i>outbind</i> operations (from the peer SMSCs) to the 
%%%       callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_alert_notification-3">
%%%       handle_alert_notification/3</a></td>
%%%     <td>Forwards <i>alert_notification</i> operations (from the peer SMSCs)
%%%       to the callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_operation-3">handle_operation/3</a>
%%%     </td>
%%%     <td>Forwards <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%%%       <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%%%       <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%%%       <i>data_sm</i> operations (from the peer SMSC) to the callback 
%%%       ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_unbind-3">handle_unbind/3</a></td>
%%%     <td>This callback forwards an unbind request (issued by peer SMSCs) 
%%%       to the callback ESME.
%%%     </td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%% 
%%% <h3><a name="handle_outbind-3">handle_outbind/3</a></h3>
%%%
%%% <tt>handle_outbind(Outbind, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>OutBind = {outbind, Session, Pdu}</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>From = term()</tt></li>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>outbind</i>, operations (from the peer SMSCs) to the 
%%% callback ESME.</p>
%%%
%%%
%%% <h3><a name="handle_alert_notification-3">handle_alert_notification/3</a>
%%% </h3>
%%%
%%% <tt>handle_alert_notification(AlertNotification, From, State) -> Result
%%% </tt>
%%% <ul>
%%%   <li><tt>AlertNotification = {alert_notification, Session, Pdu}</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>From = term()</tt></li>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>alert_notification</i>, operations (from the peer SMSCs) 
%%% to the callback ESME.</p>
%%%
%%% 
%%% <h3><a name="handle_operation-3">handle_operation/3</a></h3>
%%%
%%% <tt>handle_operation(Operation, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>Operation = {deliver_sm, Session, Pdu} |
%%%                       {data_sm, Session, Pdu}</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
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
%%% <p>Forwards <i>deliver_sm</i> and <i>data_sm</i> operations (from the peer
%%% SMSCs) to the callback ESME.</p>
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
%%%   <li><tt>Unbind = {unbind, Session, Pdu}</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
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
-module(gen_esme).

-behaviour(gen_server).
-behaviour(gen_esme_session).
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
         close_listen/1,
         session/3,
         close_session/2,
         bind_receiver/3,
         bind_transmitter/3,
         bind_transceiver/3,
         broadcast_sm/3,
         cancel_broadcast_sm/3,
         cancel_sm/3,
         data_sm/3,
         query_broadcast_sm/3,
         query_sm/3,
         replace_sm/3,
         submit_multi/3,
         submit_sm/3,
         unbind/2,
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
%%% Internal gen_esme_session exports
%%%-------------------------------------------------------------------
-export([handle_outbind/3, 
         handle_alert_notification/3, 
         handle_operation/3, 
         handle_unbind/3]).

%%%-------------------------------------------------------------------
%%% Internal gen_connection exports
%%%-------------------------------------------------------------------
-export([handle_accept/3, handle_input/4]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SESSION_MODULE, gen_esme_session).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Mod, ModState, Timers, Listener, Sessions}
%%    Mod      = atom()
%%    ModState = atom()
%%    Timers   = #timers()
%%    Listener = pid()
%%    Sessions = ets()
%%
%% %@doc Representation of the server's state.
%%
%% <dl>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>ModState: </dt><dd>Callback module private state.</dd>
%%   <dt>Timers: </dt><dd>SMPP timers.</dd>
%%   <dt>Listener: </dt><dd>Pid of the listener process.</dd>
%%   <dt>Sessions: </dt><dd>ETS table with the active sessions.</dd>
%% </dl>
%% %@end
-record(state, {mod, mod_state, timers, listener, sessions}).


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
     {handle_outbind, 3}, 
     {handle_alert_notification, 3}, 
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
%% @doc Starts the ESME server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_TIMERS()</tt> to define this parameter if default SMPP 
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
%% @doc Starts the ESME server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_TIMERS()</tt> to define this parameter if default SMPP 
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
%% @doc Starts the ESME server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_TIMERS()</tt> to define this parameter if default SMPP 
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
%% @doc Starts the ESME server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macro
%% <tt>DEFAULT_TIMERS()</tt> to define this parameter if default SMPP 
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
%% @doc Puts the ESME Server <tt>ServerRef</tt> to listen on 
%% <tt>DEFAULT_SMPP_PORT</tt>.
%%
%% <p>By default only one connection is accepted, then the listen socket
%% will be closed.</p>
%%
%% @see listen/2
%% @equiv listen(ServerRef, DEFAULT_SMPP_PORT, 1)
%% @end 
listen(ServerRef) -> 
    listen(ServerRef, ?DEFAULT_SMPP_PORT, 1).


%% @spec listen(ServerRef, Port, Count) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Port = int()
%%    Count = int() | infinity
%%    Reason = term()
%%
%% @doc Puts the ESME Server <tt>ServerRef</tt> to listen on <tt>Port</tt>
%%
%% <p><tt>Count</tt> connections are accepted.</p>
%% @end 
listen(ServerRef, Port, Count) ->
    gen_server:call(ServerRef, {listen, Port, Count}).


%% @spec close_listen(ServerRef) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%
%% @doc Stops listening.
%% @end 
close_listen(ServerRef) ->
    gen_server:cast(ServerRef, close_listen).


%% @spec session(ServerRef, Address, Port) -> Result
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session.
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%% @end 
session(ServerRef, Address, Port) ->
    gen_server:call(ServerRef, {session, Address, Port}, infinity).


%% @spec close_session(ServerRef, Session) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%
%% @doc Closes the <tt>Session</tt>.
%% @end 
close_session(ServerRef, Session) ->
    gen_server:cast(ServerRef, {close_session, Session}).


%% @spec bind_receiver(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>bind_receiver</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
bind_receiver(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {bind_receiver, Session, ParamList}, infinity).


%% @spec bind_transmitter(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>bind_transmitter</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
bind_transmitter(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {bind_transmitter,Session,ParamList}, infinity).


%% @spec bind_transceiver(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>bind_transceiver</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
bind_transceiver(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {bind_transceiver,Session,ParamList}, infinity).


%% @spec broadcast_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>broadcast_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
broadcast_sm(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {broadcast_sm, Session, ParamList}, infinity).


%% @spec cancel_broadcast_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>cancel_broadcast_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
cancel_broadcast_sm(ServerRef, Session, ParamList) ->
    gen_server:call(
      ServerRef, {cancel_broadcast_sm, Session, ParamList}, infinity).


%% @spec cancel_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>cancel_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
cancel_sm(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {cancel_sm, Session, ParamList}, infinity).


%% @spec data_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>data_sm</i> operation on the session identified by 
%% <tt>Session</tt>.
%% @end
data_sm(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {data_sm, Session, ParamList}, infinity).


%% @spec query_broadcast_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>query_broadcast_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
query_broadcast_sm(ServerRef, Session, ParamList) ->
    gen_server:call(
      ServerRef, {query_broadcast_sm, Session, ParamList}, infinity).


%% @spec query_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>query_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
query_sm(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {query_sm, Session, ParamList}, infinity).


%% @spec replace_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>replace_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
replace_sm(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {replace_sm, Session, ParamList}, infinity).


%% @spec submit_multi(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>submit_multi</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
submit_multi(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {submit_multi, Session, ParamList}, infinity).


%% @spec submit_sm(ServerRef, Session, ParamList) -> Result
%%    ServerRef  = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>submit_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
submit_sm(ServerRef, Session, ParamList) ->
    gen_server:call(ServerRef, {submit_sm, Session, ParamList}, infinity).


%% @spec unbind(ServerRef, Session) -> Result
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Session = pid()
%%    Result  = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error   = int()
%%
%% @doc Issues an <i>unbind</i> operation on the session identified by 
%% <tt>Session</tt>.
%% @end
unbind(ServerRef, Session) ->
    gen_server:call(ServerRef, {unbind_session, Session}, infinity).


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
%% @doc <a href="gen_server.html#init-1">gen_server - 
%% init/1</a> callback implementation.
%%
%% <p>Initiates the server.</p>
%% @end
init({Mod, Timers, Args}) ->
    S = #state{mod      = Mod, 
               timers   = Timers, 
               listener = closed,
               sessions = ets:new(sessions,[])},
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
%% @doc <a href="gen_server.html#handle_call-3">gen_server - 
%% handle_call/3</a> callback implementation.
%% 
%% <p>Handling call messages.</p>
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
handle_call({accept, Conn, _Socket}, From, S) ->
    case gen_esme_session:start_link(?MODULE, Conn, S#state.timers) of
        {ok, Session} ->
            ets:insert(S#state.sessions, {Session}),
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
handle_call({session, Address, Port}, From, S) ->
    case gen_connection:start_connect(?SESSION_MODULE, Address, Port) of
        {ok, Conn} ->
            % If the connection fails in the meanwhile and undesired exit
            % is received by handle_info
            case gen_esme_session:start_link(?MODULE, Conn, S#state.timers) of
                {ok, Session} ->
                    gen_connection:controlling_process(Conn, Session),
                    ets:insert(S#state.sessions, {Session}),
                    {reply, {ok, Session}, S};
                SessionError ->
                    {reply, SessionError, S}
            end;
        ConnectError ->
            {reply, ConnectError, S}
    end;
handle_call({unbind_session, Session}, From, S) ->
    spawn_link(fun() -> unbind_session(Session, From) end),
    {noreply, S};
handle_call({CmdName, Session, ParamList}, From, S) when list(ParamList) ->
    spawn_link(fun() -> operation(CmdName, Session, ParamList, From) end),
    {noreply, S};
handle_call({unbind, Session, Pdu} = R, From, S) ->
    pack((S#state.mod):handle_unbind(R, From, S#state.mod_state), S);
handle_call({CmdName, Session, Pdu} = R, From, S) ->
    pack((S#state.mod):handle_operation(R, From, S#state.mod_state), S).


%% @spec handle_cast(Request, State) -> Result
%%    Request  = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="gen_server.html#handle_cast-2">gen_server - 
%% handle_cast/2</a> callback implementation.
%%
%% <p>Handling cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_cast({cast, Request}, S) ->
    pack((S#state.mod):handle_cast(Request, S#state.mod_state), S);
handle_cast({outbind, Session, Pdu} = R, S) ->
    pack((S#state.mod):handle_outbind(R, S#state.mod_state), S);
handle_cast({alert_notification, Session, Pdu} = R, S) ->
    pack((S#state.mod):handle_alert_notification(R, S#state.mod_state), S);
handle_cast(close_listen, S) ->
    % Cleaning is done in handle_info upon trap exit
    gen_connection:stop(S#state.listener),
    {noreply, S};
handle_cast({close_session, Session}, S) ->
    % Cleaning is done by handle_info upon trap exit
    gen_esme_session:stop(Session),
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
%% @doc <a href="gen_server.html#handle_info-2">gen_server - 
%% handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info({'EXIT', P, normal}, S) when P == S#state.listener ->
    % The listener terminates with normal status.
    {noreply, S#state{listener = closed}};
handle_info({'EXIT', P, Reason}, S) when P == S#state.listener ->
    % The listener terminates on error.
    NewS = S#state{listener = closed},
    pack((NewS#state.mod):handle_listen_error(NewS#state.mod_state), NewS);
handle_info({'EXIT', P, normal} = Info, S) ->
    % Child process P terminates with normal status.
    case ets:member(S#state.sessions, P) of
        true ->
            ets:delete(S#state.sessions, P),
            {noreply, S};
        false ->
            pack((S#state.mod):handle_info(Info, S#state.mod_state), S)
    end;
handle_info({'EXIT', P, Reason} = Info, S) ->
    % Child process P terminates on error.
    case ets:member(S#state.sessions, P) of
        true ->
            ets:delete(S#state.sessions, P),
            pack((S#state.mod):handle_session_failure(P,S#state.mod_state), S);
        false ->
            pack((S#state.mod):handle_info(Info, S#state.mod_state), S)
    end;
handle_info(Info, S) ->
    pack((S#state.mod):handle_info(Info, S#state.mod_state), S).


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="gen_server.html#terminate-2">gen_server - 
%% terminate/2</a> callback implementation.
%%
%% <p>Shutdown the server.</p>
%%
%% <p>Return value is ignored by <tt>gen_server</tt>.</p>
%% @end
terminate(Reason, S) ->
    io:format("*** gen_esme terminating: ~p - ~p ***~n", [self(), Reason]),
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
%%% ESME Session functions
%%%===================================================================
%% @spec handle_outbind(ESME, Session, Pdu) -> ok
%%    ESME = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%
%% @doc <a href="gen_esme_session.html#handle_outbind-3">gen_esme_session - 
%% handle_outbind/3</a> callback implementation.
%% @end
handle_outbind(ESME, Session, Pdu) ->
    gen_server:cast(ESME, {outbind, Session, Pdu}).


%% @spec handle_alert_notification(ESME, Session, Pdu) -> ok
%%    ESME = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%
%% @doc <a href="gen_esme_session.html#handle_alert_notification-3">
%% gen_esme_session - handle_alert_notification/3</a> callback implementation.
%% @end
handle_alert_notification(ESME, Session, Pdu) ->
    gen_server:cast(ESME, {alert_notification, Session, Pdu}).


%% @spec handle_operation(ESME, Session, {CmdName, Pdu}) -> Result
%%    ESME = pid()
%%    Session = pid()
%%    CmdName = data_sm | deliver_sm
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_esme_session.html#handle_operation-3">gen_esme_session - 
%% handle_operation/3</a> callback implementation.
%% @end
handle_operation(ESME, Session, {CmdName, Pdu}) ->
    gen_server:call(ESME, {CmdName, Session, Pdu}, infinity).


%% @spec handle_unbind(ESME, Session, Pdu) -> ok | {error, Error}
%%    ESME = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Error = int()
%%
%% @doc <a href="gen_esme_session.html#handle_unbind-3">gen_esme_session - 
%% handle_unbind/3</a> callback implementation.
%% @end
handle_unbind(ESME, Session, Pdu) ->
    gen_server:call(ESME, {unbind, Session, Pdu}, infinity).


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
    Reply = gen_esme_session:CmdName(Session, ParamList),
    gen_server:reply(From, Reply).


%% @spec unbind(CmdName, Session, ParamList, From) -> true
%%
%% @doc Issues an <i>unbind</i> on the given <tt>Session</tt>.
%% @end 
unbind_session(Session, From) ->
    Reply = gen_esme_session:unbind(Session),
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
