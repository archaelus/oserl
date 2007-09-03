%%% Copyright (C) 2004 - 2005 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
%%% <p>A generic ESME implemented as a <i>gen_server</i>.</p>
%%%
%%% <p>This behaviour acts as an extended <i>gen_server</i>, homonymous 
%%% functions have the exact same meaning.</p>
%%%
%%% <p>By default sessions are NOT linked to the parent ESME, thus silently
%%% dropped if an error occurs.  To monitor sessions, ESME programmers may 
%%% either use <tt>erlang:monitor(process, Session)</tt> or explicitly create 
%%% a link to them.</p>
%%%
%%% <p>SMPP operations are directly issued upon underlying sessions, they
%%% don't go through the ESME server loop.  This means you may call the 
%%% functions <a href="#session_start-2">session_start/2</a>, 
%%% <a href="#session_start-3">session_start/3</a>, 
%%% <a href="#session_stop-1">session_stop/1</a>, 
%%% <a href="#bind_receiver-2">bind_receiver/2</a>,
%%% <a href="#bind_transmitter-2">bind_transmitter/2</a>,
%%% <a href="#bind_transceiver-2">bind_transceiver/2</a>, 
%%% <a href="#broadcast_sm-2">broadcast_sm/2</a>, 
%%% <a href="#cancel_broadcast_sm-2">cancel_broadcast_sm/2</a>, 
%%% <a href="#cancel_sm-2">cancel_sm/2</a>, <a href="#data_sm-2">data_sm/2</a>,
%%% <a href="#query_broadcast_sm-2">query_broadcast_sm/2</a>, 
%%% <a href="#query_sm-2">query_sm/2</a>, <a href="#replace_sm-2">replace_sm/2
%%% </a>, <a href="#submit_multi-2">submit_multi/2</a>, 
%%% <a href="#submit_sm-2">submit_sm/2</a> or <a href="#unbind-1">unbind/1</a>
%%% from within any callback without having the risk of blocking the ESME
%%% server.</p>
%%%
%%% <p>Please refer to <a href="examples/echo_esme.erl">echo_esme.erl</a>
%%% for a minimal ESME example.  There is also an ESME skeleton you may use
%%% as the starting point of your ESME development, find the module
%%% <a href="examples/esme_skel.erl">esme_skel.erl</a> under <tt>doc/examples
%%% </tt> directory.</p>
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
%%%     <td valign="top"><a href="#init-1">init/1</a></td>
%%%     <td>Forwards <i>gen_server:init/1</i> callbacks to the ESME server.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_outbind-3">handle_outbind/3</a></td>
%%%     <td>Forwards <i>outbind</i> operations (from the peer SMSCs) to the 
%%%       callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_alert_notification-2">
%%%       handle_alert_notification/2</a></td>
%%%     <td>Forwards <i>alert_notification</i> operations (from the peer SMSCs)
%%%       to the callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_enquire_link_failure-2">
%%%       handle_enquire_link_failure/2</a></td>
%%%     <td>Notifies <i>enquire_link</i> failures.</td>
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
%%%   <tr>
%%%     <td valign="top"><a href="#handle_listen_error-1">handle_listen_error/1
%%%       </a></td>
%%%     <td>Notifies listen socket failures to the callback ESME.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_call-3">handle_call/3</a></td>
%%%     <td>Forwards <i>gen_server:handle_call/3</i> callbacks to the ESME 
%%%       server.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td>
%%%     <td>Forwards <i>gen_server:handle_cast/2</i> callbacks to the ESME 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_info-2">handle_info/2
%%%       </a></td>
%%%     <td>Forwards <i>gen_server:handle_info/2</i> callbacks to the ESME 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#terminate-2">terminate/2
%%%       </a></td>
%%%     <td>Forwards <i>gen_server:terminate/2</i> callbacks to the ESME 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#code_change-3">code_change/3
%%%       </a></td>
%%%     <td>Forwards <i>gen_server:code_change/3</i> callbacks to the ESME 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%% 
%%% <h3><a name="init-1">init/1</a></h3>
%%%
%%% <tt>init(Args) -&gt; Result</tt>
%%%
%%% <p>Forwards <i>gen_server:init/1</i> callbacks to the SMSC server.</p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%% <h3><a name="handle_outbind-3">handle_outbind/3</a></h3>
%%%
%%% <tt>handle_outbind(Outbind, From, State) -&gt; Result</tt>
%%% <ul>
%%%   <li><tt>OutBind = {outbind, Session, Pdu, IPAddr}</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>IPAddr = {int(), int(), int(), int()}</tt></li>
%%%   <li><tt>From = term()</tt></li>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%%   <li><tt>Timeout = int()</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>outbind</i>, operations (from the peer SMSCs) to the 
%%% callback ESME.</p>
%%%
%%%
%%% <h3><a name="handle_alert_notification-2">handle_alert_notification/2</a>
%%% </h3>
%%%
%%% <tt>handle_alert_notification(AlertNotification, State) -&gt; Result
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
%%%   <li><tt>Timeout = int()</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>alert_notification</i>, operations (from the peer SMSCs) 
%%% to the callback ESME.</p>
%%%
%%%
%%% <h3><a name="handle_enquire_link_failure-2">handle_enquire_link_failure/2
%%% </a></h3>
%%%
%%% <tt>handle_enquire_link_failure(EnquireLinkFailure, State) -&gt; Result
%%% </tt>
%%% <ul>
%%%   <li><tt>EnquireLinkFailure = {enquire_link_failure, Session, CommandStatus}</tt>
%%%   </li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>CommandStatus = int()</tt></li>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>Timeout = int()</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
%%% </ul>
%%%
%%% <p>Notifies when an <i>enquire_link</i> failure occurs (i.e. the SMSC did
%%% not respond to our <i>enquire_link</i> operation).</p>
%%%
%%% 
%%% <h3><a name="handle_operation-3">handle_operation/3</a></h3>
%%%
%%% <tt>handle_operation(Operation, From, State) -&gt; Result</tt>
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
%%%   <li><tt>Timeout = int()</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
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
%%% <tt>handle_unbind(Unbind, From, State) -&gt; Result</tt>
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
%%%   <li><tt>Timeout = int()</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
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
%%% <h3><a name="handle_listen_error-1">handle_listen_error/1</a></h3>
%%%
%%% <tt>handle_listen_error(State) -&gt; Result</tt>
%%% <ul>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, NewState}</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
%%%   <li><tt>Timeout = int()</tt></li>
%%%   <li><tt>NewState = term()</tt></li>
%%%   <li><tt>Reason = term()</tt></li>
%%% </ul>
%%%
%%% <p>Notifies listen socket failures to the callback SMSC.</p>
%%%
%%%
%%% <h3><a name="handle_call-3">handle_call/3</a></h3>
%%%
%%% <tt>handle_call(Request, From, State) -&gt; Result</tt>
%%%
%%% <p>Forwards <i>gen_server:handle_call/3</i> callbacks to the SMSC server.
%%% </p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%%
%%% <h3><a name="handle_cast-2">handle_cast/2</a></h3>
%%%
%%% <tt>handle_cast(Request, State) -&gt; Result</tt>
%%%
%%% <p>Forwards <i>gen_server:handle_cast/2</i> callback to the SMSC server.
%%% </p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%%
%%% <h3><a name="handle_info-2">handle_info/2</a></h3>
%%%
%%% <tt>handle_info(Info, State) -&gt; Result</tt>
%%%
%%% <p>Forwards <i>gen_server:handle_info/2</i> callback to the SMSC server.
%%% </p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%%
%%% <h3><a name="terminate-2">terminate/2</a></h3>
%%%
%%% <tt>terminate(Reason, State)</tt>
%%%
%%% <p>Forwards <i>gen_server:terminate/2</i> callback to the SMSC server.</p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%%
%%% <h3><a name="code_change-3">code_change/3</a></h3>
%%%
%%% <tt>code_change(OldVsn, State, Extra) -&gt; {ok, NewState}</tt>
%%%
%%% <p>Forwards <i>gen_server:code_change/3</i> callback to the SMSC server.
%%% </p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%%
%%% <h2>Changes 1.1 -&gt; 1.2</h2>
%%%
%%% [7 Abr 2005]
%%%
%%% <ul>
%%%   <li>New callback <a href="#handle_enquire_link_failure-2">
%%%     handle_enquire_link_failure/2</a> added.
%%%     <br/>
%%%     <a href="http://sourceforge.net/forum/forum.php?thread_id=1206343&amp;forum_id=350015">More</a>
%%%   </li>
%%%   <li><a href="#handle_alert_notification-2">handle_alert_notification/2
%%%     </a> must have only 2 parameters and not 3, as previously declared
%%%     in behaviour_info/1.
%%%   </li>
%%%   <li>Functions <a href="#session_start_link-2">session_start_link/2</a>
%%%     and <a href="#session_start_link-3">session_start_link/3</a> added.
%%%   </li>
%%%   <li>Use <tt>proc_lib:spawn_link/1</tt> instead of <tt>spawn_link</tt>.
%%%   </li>
%%%   <li>Functions <a href="#submit_sm-3">submit_sm/3</a> and
%%%     <a href="#submit_multi-3">submit_multi/3</a> implemented.</li>
%%% </ul>
%%%
%%% [6 May 2005]
%%%
%%% <ul>
%%%   <li>Log utility added.  Functions <a href="#open_disk_log-1">
%%%     open_disk_log/1</a>, <a href="#close_disk_log-0">close_disk_log</a>,
%%%     <a href="#open_error_logger-1">open_error_logger/1</a> and
%%%     <a href="#close_error_logger-0">close_error_logger</a> implemented.
%%%     <br/>
%%%     By default neither the disk_log nor the error_logger handler are
%%%     added to the <a href="smpp_log.html">smpp_log</a> event manager, thus 
%%%     no logging is done until <a href="#open_disk_log-1">open_disk_log/1</a>
%%%     and/or <a href="#open_error_logger-1">open_error_logger/1</a> are 
%%%     called.
%%%     <br/>
%%%     Please refer to <a href="smpp_log.html">smpp_log</a> for further 
%%%     information about OSERL logs.
%%%   </li>
%%% </ul>
%%%
%%% [4 Jul 2005]
%%%
%%% <ul>
%%%   <li>Implement <i>gen_esme_session</i> callback 
%%%     <a href="gen_esme_session.html#handle_enquire_link-3">
%%%     handle_enquire_link/3</a>.
%%%     <br/>
%%%     <a href="http://sourceforge.net/forum/forum.php?thread_id=1206343&amp;forum_id=350015">More</a>
%%%   </li>
%%% </ul>
%%%
%%%
%%% <h2>Changes 1.2 -&gt; 1.3</h2>
%%%
%%% [30 Jul 2005]
%%%
%%% <ul>
%%%   <li>Add IPAddr in outbind callback.
%%%   </li>
%%% </ul>
%%%
%%% [19 Sep 2006]
%%%
%%% <ul>
%%%   <li>Add <tt>EsmeRef</tt> to 
%%%     <a href="gen_esme.html#session_start-4">session_start/4</a>,
%%%     <a href="gen_esme.html#session_start-5">session_start/5</a>, 
%%%     <a href="gen_esme.html#session_start_link-4">session_start_link/4</a> 
%%%     and <a href="gen_esme.html#session_start_link-5">session_start_link/5
%%%     </a> functions.
%%%   </li>
%%% </ul>
%%%
%%% @copyright 2004 - 2005 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.2, {13 May 2004} {@time}.
%%% @end
-module(gen_esme).

-behaviour(gen_server).
-behaviour(gen_esme_session).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% Behaviour exports
%%%-------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
%%% External ESME exports
%%%-------------------------------------------------------------------
-export([start/3,
         start/4,
         start_link/3,
         start_link/4, 
         listen_start/1,
         listen_start/4,
         listen_stop/1,
         call/2, 
         call/3, 
         cast/2, 
         close_disk_log/1,
         close_error_logger/1,
         count/4,
         match/4,
         match/5,
         open_disk_log/2,
         open_error_logger/2,
         reply/2]).

%%%-------------------------------------------------------------------
%%% External Session exports
%%%-------------------------------------------------------------------
-export([session_start/3,
         session_start/4,
         session_start_link/3,
         session_start_link/4,
         session_stop/1,
         bind_receiver/2,
         bind_transmitter/2,
         bind_transceiver/2,
         broadcast_sm/2,
         cancel_broadcast_sm/2,
         cancel_sm/2,
         data_sm/2,
         query_broadcast_sm/2,
         query_sm/2,
         replace_sm/2,
         submit_multi/2,
         submit_multi/3,
         submit_sm/2,
         submit_sm/3,
         unbind/1]).

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
         handle_enquire_link/3,
         handle_enquire_link_failure/3,
         handle_operation/3, 
         handle_unbind/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(CONNECT_TIME, 30000).
-define(CONNECT_OPTS, [binary, {packet, 0}, {active, false}]).
-define(LISTEN_OPTS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-define(DECR(X), if is_integer(X) -> X - 1; true -> X end).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Mod, ModState, LSocket, Timers, Log}
%%    Mod = atom()
%%    ModState = atom()
%%    LSocket = socket()
%%    Timers = #timers{}
%%    Log = pid()
%%
%% %@doc Representation of the server's state.
%%
%% <dl>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>ModState: </dt><dd>Callback module private state.</dd>
%%   <dt>LSocket: </dt><dd>Listener socket.</dd>
%%   <dt>Timers: </dt><dd>SMPP timers for accepted session.</dd>
%%   <dt>Log: </dt><dd>Reference of the SMPP log event manager.</dd>
%% </dl>
%% %@end
-record(state, {mod, mod_state, lsocket = closed, timers, log}).

%%%===================================================================
%%% Behaviour functions
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
     {handle_alert_notification, 2}, 
     {handle_enquire_link_failure, 2}, 
     {handle_operation, 3}, 
     {handle_unbind, 3}, 
     {handle_listen_error, 1},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {terminate, 2},
     {code_change, 3}];
behaviour_info(_Other) ->
    undefined.

%%%===================================================================
%%% External ESME functions
%%%===================================================================
%% @spec start(Module, Args, Options) -> Result
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the ESME server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% @see gen_server:start/3
%% @see start_link/3
%% @see start/4
%% @end
start(Module, Args, Options) ->
    gen_server:start(?MODULE, {Module, Args}, Options).

%% @spec start(ServerName, Module, Args, Options) -> Result
%%    ServerName = {local, Name} | {global, Name}
%%    Name       = atom()
%%    Module     = atom()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the ESME server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% @see gen_server:start/4
%% @see start_link/4
%% @see start/3
%% @end
start(ServerName, Module, Args, Options) ->
    gen_server:start(ServerName, ?MODULE, {Module, Args}, Options).

%% @spec start_link(Module, Args, Options) -> Result
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the ESME server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% @see gen_server:start_link/3
%% @see start_link/4
%% @see start/3
%% @end
start_link(Module, Args, Options) ->
    gen_server:start_link(?MODULE, {Module, Args}, Options).

%% @spec start_link(ServerName, Module, Args, Options) -> Result
%%    ServerName = {local, Name} | {global, Name}
%%    Name       = atom()
%%    Module     = atom()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the ESME server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% @see gen_server:start_link/4
%% @see start_link/3
%% @see start/4
%% @end
start_link(ServerName, Module, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Options).

%% @spec listen_start(ServerRef) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Reason = term()
%%
%% @doc Puts the ESME Server <tt>ServerRef</tt> to listen on 
%% <tt>DEFAULT_SMPP_PORT</tt>.
%%
%% <p>By default only one connection is accepted before the listen socket
%% is closed.  Started sessions are NOT linked to the SMSC.</p>
%%
%% @see listen_start/4
%% @equiv listen(ServerRef, DEFAULT_SMPP_PORT, 1, DEFAULT_SMPP_TIMERS)
%% @end 
listen_start(ServerRef) -> 
    listen_start(ServerRef, ?DEFAULT_SMPP_PORT, 1, ?DEFAULT_SMPP_TIMERS).

%% @spec listen_start(ServerRef, Port, Count, Timers) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Port = int()
%%    Count = int() | infinity
%%    Timers = timers()
%%    Reason = term()
%%
%% @doc Puts the ESME Server <tt>ServerRef</tt> to listen on <tt>Port</tt>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  Accepted session will be subject to
%% given timers.</p>
%%
%% <p><tt>Count</tt> connections are accepted.  Started sessions are NOT
%% linked to the SMSC.</p>
%% @end 
listen_start(ServerRef, Port, Count, Timers) ->
    gen_server:call(ServerRef, {listen_start, Port, Count, Timers}).

%% @spec listen_stop(ServerRef) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%
%% @doc Stops listening.
%% @end 
listen_stop(ServerRef) ->
    gen_server:cast(ServerRef, listen_stop).

%% @spec call(ServerRef, Request) -> Reply
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Request = term()
%%    Reply = term()
%%
%% @doc Equivalent to <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#call-2">gen_server:call/2</a>.  
%%
%% <p><tt>Request</tt> is an arbitrary term which is passed as one of the 
%% arguments to <tt>Module:handle_call/3</tt>.</p>
%%
%% <p>Please refer to the <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server</a> man page for greater details.</p>
%% @end 
call(ServerRef, Request) ->
    gen_server:call(ServerRef, {call, Request}).

%% @spec call(ServerRef, Request, Timeout) -> Reply
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Request = term()
%%    Timeout = int() | infinity
%%    Reply = term()
%%
%% @doc Equivalent to <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#call-3">gen_server:call/3</a>.  
%%
%% <p><tt>Request</tt> is an arbitrary term which is passed as one of the 
%% arguments to <tt>Module:handle_call/3</tt>.</p>
%%
%% <p>Please refer to the <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server</a> man page for greater details.</p>
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
%% @doc Equivalent to <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#cast-2">gen_server:cast/2</a>.  
%%
%% <p><tt>Request</tt> is an arbitrary term which is passed as one of the 
%% arguments to <tt>Module:handle_cast/2</tt>.</p>
%%
%% <p>Please refer to the <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server</a> man page for greater details.</p>
%% @end 
cast(ServerRef, Request) ->
    gen_server:cast(ServerRef, {cast, Request}).

%% @spec close_disk_log(ServerRef) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%
%% @doc Closes the ESME disk log.
%%
%% @see smpp_log:delete_disk_log_handler/0
%% @end 
close_disk_log(ServerRef) ->
    gen_server:cast(ServerRef, close_disk_log).

%% @spec close_error_logger(ServerRef) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%
%% @doc Closes the ESME error logger.
%%
%% @see smpp_log:delete_error_logger_handler/0
%% @end 
close_error_logger(ServerRef) ->
    gen_server:cast(ServerRef, close_error_logger).

%% @spec count(ServerRef, Date, From, Pred) -> int()
%%    ServerRef = pid()
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun()
%%
%% @doc PDUs are logged in the disk log as <tt>{Now, BinaryPdu, From}</tt> 
%% terms.  This function counts logged PDUs matching the given 
%% <tt>Date</tt>, <tt>From</tt> and <tt>Pred</tt>.
%%
%% <ul>
%%   <li><b>Date:</b> <tt>any</tt>, <tt>{from, Time}</tt>, <tt>{until, Time}
%%     </tt> or <tt>{lapse, FromTime,ToTime}</tt>.</li>
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or <tt>any</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% @end 
count(ServerRef, Date, From, Pred) ->
    gen_server:call(ServerRef, {count, Date, From, Pred}, infinity).

%% @spec match(ServerRef, Date, From, Pred) -> Result
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun()
%%    Continuation = start | cont()
%%    Result = {Continuation2, List} |
%%             {Continuation2, List, Badbytes} |
%%             eof |
%%             {error, Reason}
%%    List = [{Now, BinaryPdu, From}]
%%    Now = {MegaSecs, Secs, Microsecs}
%%    BinaryPdu = binary()
%%
%% @doc PDUs are logged in the disk log as <tt>{Now, BinaryPdu, From}</tt> 
%% terms.  This function gets the list of logged PDUs matching the given 
%% <tt>Date</tt>, <tt>From</tt> and <tt>Pred</tt>.
%%
%% <ul>
%%   <li><b>Date:</b> <tt>any</tt>, <tt>{from, Time}</tt>, <tt>{until, Time}
%%     </tt> or <tt>{lapse, FromTime,ToTime}</tt>.</li>
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or <tt>any</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% 
%% <p>This function internally calls <a href="#match-5">match/5</a> with 
%% <tt>start</tt> as the <tt>Continuation</tt> parameters.</p>
%%
%% @see match/4
%%
%% @equiv match(ServerRef, Date, From, Pred, start)
%% @end 
match(ServerRef, Date, From, Pred) ->
    match(ServerRef, Date, From, Pred, start).

%% @spec match(ServerRef, Date, From, Pred, Continuation) -> Result
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun()
%%    Continuation = start | cont()
%%    Result = {Continuation2, List} |
%%             {Continuation2, List, Badbytes} |
%%             eof |
%%             {error, Reason}
%%    List = [{Now, BinaryPdu, From}]
%%    Now = {MegaSecs, Secs, Microsecs}
%%    BinaryPdu = binary()
%%
%% @doc PDUs are logged in the disk log as <tt>{Now, BinaryPdu, From}</tt> 
%% terms.  This function gets the list of logged PDUs matching the given 
%% <tt>Date</tt>, <tt>From</tt> and <tt>Pred</tt>.
%%
%% <ul>
%%   <li><b>Date:</b> <tt>any</tt>, <tt>{from, Time}</tt>, <tt>{until, Time}
%%     </tt> or <tt>{lapse, FromTime,ToTime}</tt>.</li>
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or <tt>any</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% @end 
match(ServerRef, Date, From, Pred, Cont) ->
    gen_server:call(ServerRef, {match, Date, From, Pred, Cont}, infinity).

%% @spec open_disk_log(ServerRef, Args) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Args = term()
%%    File = string()
%%    Pred = fun()
%%    BinaryPdu = binary()
%%
%% @doc Opens the ESME disk log.  Refer to <a href="smpp_log.html#add_disk_log_handler-1">smpp_log:add_disk_log_handler/1</a> to see permitted <tt>Args</tt>.
%%
%% <p><tt>Pred</tt> is a fun <tt>fun(BinaryPdu) -&gt; bool()</tt></p>
%%
%% @see smpp_log:add_disk_log_handler/1
%% @end 
open_disk_log(ServerRef, Args) ->
    gen_server:cast(ServerRef, {open_disk_log, Args}).

%% @spec open_error_logger(ServerRef, Args) -> ok
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Args = term()
%%    Pred = fun()
%%    BinaryPdu = binary()
%%
%% @doc Opens the ESME error logger.  Refer to <a href="smpp_log.html#add_error_logger_handler-1">smpp_log:add_error_logger_handler/1</a> to see permitted <tt>Args</tt>.
%%
%% <p><tt>Pred</tt> is a fun <tt>fun(BinaryPdu) -&gt; bool()</tt></p>
%%
%% @see smpp_log:add_error_logger_handler/1
%% @end 
open_error_logger(ServerRef, Args) ->
    gen_server:cast(ServerRef, {open_error_logger, Args}).

%% @spec reply(Client, Reply) -> true
%%    Client = term()
%%    Reply  = term()
%%
%% @doc Equivalent to <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#reply-2">gen_server:reply/2</a>.
%%
%% <p>Please refer to the <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server</a> man page for greater details.</p>
%% @end 
reply(Client, Reply) ->
    gen_server:reply(Client, Reply).

%%%===================================================================
%%% External Session functions
%%%===================================================================
%% @spec session_start(EsmeRef, Address, Port) -> Result
%%    EsmeRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session.  By default Sessions are NOT linked to the ESME.
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @equiv session_start(EsmeRef, Address, Port, DEFAULT_SMPP_TIMERS)
%% @end 
session_start(EsmeRef, Address, Port) ->
    session_start(EsmeRef, Address, Port, ?DEFAULT_SMPP_TIMERS).

%% @spec session_start(EsmeRef, Address, Port, Timers) -> Result
%%    EsmeRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session.  By default Sessions are NOT linked to the ESME.
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.</p>
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @see session_start/2
%% @see gen_esme_session:start/3
%% @end 
session_start(EsmeRef, Address, Port, Timers) ->
    Request = {session, start, Address, Port, Timers},
    gen_server:call(EsmeRef, Request, infinity).

%% @spec session_start_link(EsmeRef, Address, Port) -> Result
%%    EsmeRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session linked to the ESME.
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @equiv session_start_link(Address, Port, DEFAULT_SMPP_TIMERS)
%% @end 
session_start_link(EsmeRef, Address, Port) ->
    session_start_link(EsmeRef, Address, Port, ?DEFAULT_SMPP_TIMERS).

%% @spec session_start_link(EsmeRef, Address, Port, Timers) -> Result
%%    EsmeRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session linked to the ESME.
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.</p>
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @see session_start_link/2
%% @see gen_esme_session:start_link/3
%% @end 
session_start_link(EsmeRef, Address, Port, Timers) ->
    Request = {session, start_link, Address, Port, Timers},
    gen_server:call(EsmeRef, Request, infinity).

%% @spec session_stop(Session) -> ok
%%    Session = pid()
%%
%% @doc Closes the <tt>Session</tt>.
%% @end 
session_stop(Session) ->
    gen_esme_session:stop(Session).

%% @spec bind_receiver(Session, ParamList) -> Result
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
bind_receiver(Session, ParamList) ->
    gen_esme_session:bind_receiver(Session, ParamList).

%% @spec bind_transmitter(Session, ParamList) -> Result
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
bind_transmitter(Session, ParamList) ->
    gen_esme_session:bind_transmitter(Session, ParamList).

%% @spec bind_transceiver(Session, ParamList) -> Result
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
bind_transceiver(Session, ParamList) ->
    gen_esme_session:bind_transceiver(Session, ParamList).

%% @spec broadcast_sm(Session, ParamList) -> Result
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
broadcast_sm(Session, ParamList) ->
    gen_esme_session:broadcast_sm(Session, ParamList).

%% @spec cancel_broadcast_sm(Session, ParamList) -> Result
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
cancel_broadcast_sm(Session, ParamList) ->
    gen_esme_session:cancel_broadcast_sm(Session, ParamList).

%% @spec cancel_sm(Session, ParamList) -> Result
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
cancel_sm(Session, ParamList) ->
    gen_esme_session:cancel_sm(Session, ParamList).

%% @spec data_sm(Session, ParamList) -> Result
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
data_sm(Session, ParamList) ->
    gen_esme_session:data_sm(Session, ParamList).

%% @spec query_broadcast_sm(Session, ParamList) -> Result
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
query_broadcast_sm(Session, ParamList) ->
    gen_esme_session:query_broadcast_sm(Session, ParamList).

%% @spec query_sm(Session, ParamList) -> Result
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
query_sm(Session, ParamList) ->
    gen_esme_session:query_sm(Session, ParamList).

%% @spec replace_sm(Session, ParamList) -> Result
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
replace_sm(Session, ParamList) ->
    gen_esme_session:replace_sm(Session, ParamList).

%% @spec submit_multi(Session, ParamList) -> Result
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
submit_multi(Session, ParamList) ->
    gen_esme_session:submit_multi(Session, ParamList).

%% @spec submit_multi(Session, ParamList, ConcatenationMethod) -> Results
%%    Session = pid()
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    ConcatenationMethod = udh | tlv
%%    Results = [Result]
%%    Result = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error = int()
%%
%% @doc Issues a <i>submit_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%%
%% <p>This submission function automatically accomplishes long messages 
%% segmentation and concatenation using the <tt>ConcatenationMethod</tt>
%% indicated.</p>
%%
%% <dl>
%%   <dt>udh</dt><dd>Concatenates the messages using UDH.  If unsure use
%%     this method, since is more portable and widely available in most SMSCs.
%%   </dd>
%%   <dt>tlv</dt><dd>Concatenates the messages using <i>sar_msg_ref_num</i>,
%%     <i>sar_total_segments</i> and <i>sar_segments_seqnum</i> TLVs.  If
%%     your SMSC supports these TLVs (not all of them do) this method is
%%     recommended, unless <i>message_payload</i> TLV is also supported of
%%     course, read below comments on this subject.</dd>
%% </dl>
%%
%% <p>The <i>short_message</i> is splited if longer than SM_MAX_SIZE macro
%% (defaults to 160), otherwise <a href="#submit_sm-2">submit_sm/2</a> is
%% called and the message is send as is.  Resulting segments are 
%% SM_SEGMENT_MAX_SIZE which defaults to 150 for either concatenation method, 
%% to allow room for the UDH.  You may want to redefine these macros to fit 
%% your particular needs, find them in <tt>oserl.hrl</tt>
%% </p>
%%
%% <p>If you are lucky and your SMSC permits the <i>message_payload</i>
%% TLV, use it with the <a href="#data_sm-2">data_sm/2</a> and
%% <a href="#submit_sm-2">submit_sm/2</a> functions instead.</p>
%%
%% @see submit_sm/2
%% @see data_sm/2
%% @see gen_esme_session:submit_sm/3
%% @see sm:split/2
%% @end
submit_multi(Session, ParamList, ConcatenationMethod) ->
    case lists:keysearch(short_message, 1, ParamList) of
        {value, {short_message, SM}} when length(SM) > ?SM_MAX_SIZE ->
            RefNum = gen_esme_session:reference_number(Session),
            F = fun(L) -> gen_esme_session:submit_multi(Session, L) end,
            lists:map(F, sm:split(ParamList, RefNum, ConcatenationMethod));
        _Otherwise ->
            [gen_esme_session:submit_multi(Session, ParamList)]
    end.

%% @spec submit_sm(Session, ParamList) -> Result
%%    Session = pid()
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    Result = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error = int()
%%
%% @doc Issues a <i>submit_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
submit_sm(Session, ParamList) ->
    gen_esme_session:submit_sm(Session, ParamList).

%% @spec submit_sm(Session, ParamList, ConcatenationMethod) -> Results
%%    Session = pid()
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    ConcatenationMethod = udh | tlv
%%    Results = [Result]
%%    Result = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error = int()
%%
%% @doc Issues a <i>submit_sm</i> operation on the session 
%% identified by <tt>Session</tt>.
%%
%% <p>This submission function automatically accomplishes long messages 
%% segmentation and concatenation using the <tt>ConcatenationMethod</tt>
%% indicated.</p>
%%
%% <dl>
%%   <dt>udh</dt><dd>Concatenates the messages using UDH.  If unsure use
%%     this method, since is more portable and widely available in most SMSCs.
%%   </dd>
%%   <dt>tlv</dt><dd>Concatenates the messages using <i>sar_msg_ref_num</i>,
%%     <i>sar_total_segments</i> and <i>sar_segments_seqnum</i> TLVs.  If
%%     your SMSC supports these TLVs (not all of them do) this method is
%%     recommended, unless <i>message_payload</i> TLV is also supported of
%%     course, read below comments on this subject.</dd>
%% </dl>
%%
%% <p>The <i>short_message</i> is splited if longer than SM_MAX_SIZE macro
%% (defaults to 160), otherwise <a href="#submit_sm-2">submit_sm/2</a> is
%% called and the message is send as is.  Resulting segments are 
%% SM_SEGMENT_MAX_SIZE which defaults to 150 for either concatenation method, 
%% to allow room for the UDH.  You may want to redefine these macros to fit 
%% your particular needs, find them in <tt>oserl.hrl</tt>
%% </p>
%%
%% <p>If you are lucky and your SMSC permits the <i>message_payload</i>
%% TLV, use it with the <a href="#data_sm-2">data_sm/2</a> and
%% <a href="#submit_sm-2">submit_sm/2</a> functions instead.</p>
%%
%% @see submit_sm/2
%% @see data_sm/2
%% @see gen_esme_session:submit_sm/3
%% @see sm:split/2
%% @end
submit_sm(Session, ParamList, ConcatenationMethod) ->
    case lists:keysearch(short_message, 1, ParamList) of
        {value, {short_message, SM}} when length(SM) > ?SM_MAX_SIZE ->
            RefNum = gen_esme_session:reference_number(Session),
            F = fun(L) -> gen_esme_session:submit_sm(Session, L) end,
            lists:map(F, sm:split(ParamList, RefNum, ConcatenationMethod));
        _Otherwise ->
            [gen_esme_session:submit_sm(Session, ParamList)]
    end.

%% @spec unbind(Session) -> Result
%%    Session = pid()
%%    Result  = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error   = int()
%%
%% @doc Issues an <i>unbind</i> operation on the session identified by 
%% <tt>Session</tt>.
%% @end
unbind(Session) ->
    gen_esme_session:unbind(Session).

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
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#init-1">gen_server - init/1</a> callback implementation.
%%
%% <p>Initiates the server.</p>
%% @end
init({Mod, Args}) ->
    {ok, Log} = smpp_log:start_link(),
    pack(Mod:init(Args), #state{mod = Mod, log = Log}).

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
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#handle_call-3">gen_server - handle_call/3</a> callback implementation.
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
handle_call({listen_start, Port, Count, Timers}, _From, S) ->
    case gen_tcp:listen(Port, ?LISTEN_OPTS) of
        {ok, LSocket} ->
            Self = self(),
            proc_lib:spawn_link(fun() -> listener(Self, LSocket, Count) end),
            {reply, true, S#state{lsocket = LSocket, timers = Timers}};
        _Error ->
            {reply, false, S}
    end;
handle_call({accept, Socket}, _From, S) ->
    R = gen_esme_session:start(self(), ?MODULE,
                               [{socket, Socket},
                                {timers, S#state.timers},
                                {log, S#state.log}]),
    {reply, R, S};
handle_call({Bind, _Session, _Pdu} = R, From, S) when Bind == bind_transceiver;
                                                      Bind == bind_transmitter;
                                                      Bind == bind_receiver ->
    pack((S#state.mod):handle_bind(R, From, S#state.mod_state), S);
handle_call({unbind, _Session, _Pdu} = R, From, S) ->
    pack((S#state.mod):handle_unbind(R, From, S#state.mod_state), S);
handle_call({_CmdName, _Session, _Pdu} = R, From, S) ->
    pack((S#state.mod):handle_operation(R, From, S#state.mod_state), S);
handle_call({session, StartFun, Addr, Port, Timers}, From, S) ->
    Self = self(),
    proc_lib:spawn_link(
      fun() ->
              case gen_tcp:connect(Addr, Port, ?CONNECT_OPTS, ?CONNECT_TIME) of
                  {ok, Socket} ->
                      A = [{socket,Socket}, {timers,Timers}, {log,S#state.log}],
                      case gen_esme_session:StartFun(Self, ?MODULE, A) of
                          {ok, Session} ->
                              gen_tcp:controlling_process(Socket, Session),
                              gen_server:reply(From, {ok, Session});
                          SessionError ->
                              gen_server:reply(From, SessionError)
                      end;
                  ConnectError ->
                      gen_server:reply(From, ConnectError)
              end
      end),
    {noreply, S};
handle_call({count, Date, From, Pred}, _From, S) ->
    Reply = smpp_log:count(S#state.log, Date, From, Pred),
    {reply, Reply, S};
handle_call({match, Date, From, Pred, Continuation}, _From, S) ->
    Reply = smpp_log:match(S#state.log, Date, From, Pred, Continuation),
    {reply, Reply, S}.



%% @spec handle_cast(Request, State) -> Result
%%    Request  = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#handle_cast-2">gen_server - handle_cast/2</a> callback implementation.
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
handle_cast({outbind, _Session, _Pdu, _IPAddr} = R, S) ->
    pack((S#state.mod):handle_outbind(R, S#state.mod_state), S);
handle_cast({alert_notification, _Session, _Pdu} = R, S) ->
    pack((S#state.mod):handle_alert_notification(R, S#state.mod_state), S);
handle_cast({enquire_link, _Session, _Pdu}, S) ->
    {noreply, S};
handle_cast({enquire_link_failure, _Session, _CommandStatus} = R, S) ->
    pack((S#state.mod):handle_enquire_link_failure(R, S#state.mod_state), S);
handle_cast(listen_error, S) when S#state.lsocket == closed ->
    {noreply, S};
handle_cast(listen_error, S) ->
    gen_tcp:close(S#state.lsocket),  % Close it anyway
    NewS = S#state{lsocket = closed},
    pack((NewS#state.mod):handle_listen_error(NewS#state.mod_state), NewS);
handle_cast(listen_stop, S) ->
    gen_tcp:close(S#state.lsocket),
    {noreply, S#state{lsocket = closed}};
handle_cast(close_disk_log, S) ->
    smpp_log:delete_disk_log_handler(S#state.log),
    {noreply, S};
handle_cast(close_error_logger, S) ->
    smpp_log:delete_error_logger_handler(S#state.log),
    {noreply, S};
handle_cast({open_disk_log, Args}, S) ->
    smpp_log:add_disk_log_handler(S#state.log, Args),
    {noreply, S};
handle_cast({open_error_logger, Args}, S) ->
    smpp_log:add_error_logger_handler(S#state.log, Args),
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
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#handle_info-2">gen_server - handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info(Info, S) ->
    pack((S#state.mod):handle_info(Info, S#state.mod_state), S).

%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#terminate-2">gen_server - terminate/2</a> callback implementation.
%%
%% <p>Shutdown the server.</p>
%%
%% <p>Return value is ignored by <tt>gen_server</tt>.</p>
%% @end
terminate(R, S) ->
    report:info(?MODULE, terminate, [{reason, R}, {pid, self()}]),
    pack((S#state.mod):terminate(R, S#state.mod_state), S),
    smpp_log:stop(S#state.log).


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html#code_change-3">gen_server - code_change/3</a> callback implementation.
%%
%% <p>Convert process state when code is changed.</p>
%% @end
code_change(OldVsn, S, Extra) ->
    pack((S#state.mod):code_change(OldVsn, S#state.mod_state, Extra), S).

%%%===================================================================
%%% ESME Session functions
%%%===================================================================
%% @spec handle_outbind(ServerRef, Session, Pdu) -> ok
%%    ServerRef = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%
%% @doc <a href="gen_esme_session.html#handle_outbind-3">gen_esme_session - 
%% handle_outbind/3</a> callback implementation.
%% @end
handle_outbind(ServerRef, Session, Pdu) ->
    gen_server:cast(ServerRef, {outbind, Session, Pdu}).

%% @spec handle_alert_notification(ServerRef, Session, Pdu) -> ok
%%    ServerRef = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%
%% @doc <a href="gen_esme_session.html#handle_alert_notification-3">
%% gen_esme_session - handle_alert_notification/3</a> callback implementation.
%% @end
handle_alert_notification(ServerRef, Session, Pdu) ->
    gen_server:cast(ServerRef, {alert_notification, Session, Pdu}).

%% @spec handle_enquire_link(ServerRef, Session, Pdu) -> ok
%%    ServerRef = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%
%% @doc <a href="gen_esme_session.html#handle_enquire_link-3">
%% gen_esme_session - handle_enquire_link/3</a> callback implementation.
%% @end
handle_enquire_link(ServerRef, Session, Pdu) ->
    gen_server:cast(ServerRef, {enquire_link, Session, Pdu}).

%% @spec handle_enquire_link_failure(ServerRef, Session, CommandStatus) -> ok
%%    ServerRef = pid()
%%    Session = pid()
%%    CommandStatus = int()
%%
%% @doc <a href="gen_esme_session.html#handle_enquire_link_failure-3">
%% gen_esme_session - handle_enquire_link_failure/3</a> callback 
%% implementation.
%% @end
handle_enquire_link_failure(ServerRef, Session, CommandStatus) ->
    gen_server:cast(ServerRef, {enquire_link_failure, Session, CommandStatus}).

%% @spec handle_operation(ServerRef, Session, {CmdName, Pdu}) -> Result
%%    ServerRef = pid()
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
handle_operation(ServerRef, Session, {CmdName, Pdu}) ->
    gen_server:call(ServerRef, {CmdName, Session, Pdu}, infinity).

%% @spec handle_unbind(ServerRef, Session, Pdu) -> ok | {error, Error}
%%    ServerRef = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Error = int()
%%
%% @doc <a href="gen_esme_session.html#handle_unbind-3">gen_esme_session - 
%% handle_unbind/3</a> callback implementation.
%% @end
handle_unbind(ServerRef, Session, Pdu) ->
    gen_server:call(ServerRef, {unbind, Session, Pdu}, infinity).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
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

%% @spec listener(ServerRef, LSocket, Count) -> void()
%%    ServerRef = pid()
%%    LSocket = socket()
%%    Count = int()
%%
%% @doc Waits until a connection is requested on <tt>LSocket</tt> or an
%% error occurs.  If successful the event <i>accept</i> is triggered. On
%% error an exit on the listen socket is reported.
%% @end
listener(ServerRef, LSocket, Count) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, ?CONNECT_OPTS),
            case gen_server:call(ServerRef, {accept, Socket}, infinity) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid),
                    if
                        Count > 1 ->
                            listener(ServerRef, LSocket, ?DECR(Count));
                        true ->
                            gen_server:cast(ServerRef, listen_stop)
                    end;
                _Error ->
                    gen_tcp:close(Socket),
                    listener(ServerRef, LSocket, Count)
            end;
        _Error ->
            gen_server:cast(ServerRef, listen_error)
    end.
