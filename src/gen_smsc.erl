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
%%% <p>A generic SMSC implemented as a <i>gen_server</i>.</p>
%%%
%%% <p>This behaviour acts as an extended <i>gen_server</i>, homonymous 
%%% functions have the exact same meaning.</p>
%%%
%%% <p>By default sessions are NOT linked to the parent SMSC, thus silently
%%% dropped if an error occurs.  To monitor sessions, SMSC programmers may 
%%% either use <tt>erlang:monitor(process, Session)</tt> or explicitly create 
%%% a link to them.</p>
%%%
%%% <p>SMPP operations are directly issued upon sessions, they don't go through
%%% the SMSC server loop.  This means you may call the functions 
%%% <a href="#session_start-2">session_start/2</a>, 
%%% <a href="#session_start-3">session_start/3</a>, 
%%% <a href="#session_stop-1">session_stop/1</a>, 
%%% <a href="#alert_notification-2">alert_notification/2</a>, 
%%% <a href="#outbind-2">outbind/2</a>, <a href="#deliver_sm-2">deliver_sm/2
%%% </a>, <a href="#data_sm-2">data_sm/2</a> or <a href="#unbind-1">unbind/1
%%% </a> from within any callback without having the risk of blocking the 
%%% SMSC server.</p>
%%%
%%% <p>Please refer to <a href="examples/test_smsc.erl">test_smsc.erl</a>
%%% for a minimal SMSC example.  There is also a SMSC skeleton you may use
%%% as the starting point of your SMSC development, find the module
%%% <a href="examples/smsc_skel.erl">smsc_skel.erl</a> under <tt>doc/examples
%%% </tt> directory.</p>
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
%%%     <td valign="top"><a href="#init-1">init/1</a></td>
%%%     <td>Forwards <i>gen_server:init/1</i> callbacks to the SMSC server.
%%%     </td>
%%%   </tr>
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
%%%   <tr>
%%%     <td valign="top"><a href="#handle_listen_error-1">handle_listen_error/1
%%%       </a></td>
%%%     <td>Notifies listen socket failures to the callback SMSC.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_call-3">handle_call/3</a></td>
%%%     <td>Forwards <i>gen_server:handle_call/3</i> callbacks to the SMSC 
%%%       server.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td>
%%%     <td>Forwards <i>gen_server:handle_cast/2</i> callbacks to the SMSC 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_info-2">handle_info/2
%%%       </a></td>
%%%     <td>Forwards <i>gen_server:handle_info/2</i> callbacks to the SMSC 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#terminate-2">terminate/2
%%%       </a></td>
%%%     <td>Forwards <i>gen_server:terminate/2</i> callbacks to the SMSC 
%%%       server.</td>
%%%     <td>.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#code_change-3">code_change/3
%%%       </a></td>
%%%     <td>Forwards <i>gen_server:code_change/3</i> callbacks to the SMSC 
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
%%% <tt>init(Args) -> Result</tt>
%%%
%%% <p>Forwards <i>gen_server:init/1</i> callbacks to the SMSC server.</p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%% 
%%% <h3><a name="handle_bind-3">handle_bind/3</a></h3>
%%%
%%% <tt>handle_bind(Bind, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>Bind = {bind_receiver, Session, Pdu, IPAddr}    |
%%%                  {bind_transmitter, Session, Pdu, IPAddr} |
%%%                  {bind_transceiver, Session, Pdu, IPAddr}</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>IPAddr = {int(),int(),int(),int()}
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
%%% <p>Forwards <i>bind_receiver</i>, <i>bind_transmitter</i> and 
%%% <i>bind_transceiver</i> operations (from the peer ESMEs) to the 
%%% callback SMSC.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the bind response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the SMSC in the response PDU, the callback should return the
%%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%%% desired command_status error code.</p>
%%%
%%% 
%%% <h3><a name="handle_operation-3">handle_operation/3</a></h3>
%%%
%%% <tt>handle_operation(Operation, From, State) -> Result</tt>
%%% <ul>
%%%   <li><tt>Operation = {broadcast_sm, Session, Pdu} |
%%%                       {cancel_broadcast_sm, Session, Pdu} |
%%%                       {cancel_sm, Session, Pdu} |
%%%                       {query_broadcast_sm, Session, Pdu} |
%%%                       {query_sm, Session, Pdu} |
%%%                       {replace_sm, Session, Pdu} |
%%%                       {submit_multi, Session, Pdu} |
%%%                       {submit_sm, Session, Pdu} |
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
%%% <p>Forwards <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%%% <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%%% <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%%% <i>data_sm</i> operations (from the peer ESMEs) to the callback SMSC.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the SMSC in the response PDU, the callback should return the
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
%%%   <li><tt>From = term()</tt></li>
%%%   <li><tt>State = term()</tt></li>
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
%%% <tt>handle_listen_error(State) -> Result</tt>
%%% <ul>
%%%   <li><tt>State = term()</tt></li>
%%%   <li><tt>Result = {noreply, NewState}               |
%%%                    {noreply, NewState, Timeout}      |
%%%                    {stop, Reason, NewState}</tt></li>
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
%%% <tt>handle_call(Request, From, State) -> Result</tt>
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
%%% <tt>handle_cast(Request, State) -> Result</tt>
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
%%% <tt>handle_info(Info, State) -> Result</tt>
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
%%% <tt>code_change(OldVsn, State, Extra) -> {ok, NewState}</tt>
%%%
%%% <p>Forwards <i>gen_server:code_change/3</i> callback to the SMSC server.
%%% </p>
%%%
%%% <p>Refer to OTP <i>gen_server</i> behaviour documentation for greater
%%% details on this callback.</p>
%%%
%%%
%%% <h2>Changes 1.2 -&gt; 1.3</h2>
%%%
%%% [19 Sep 2006]
%%%
%%% <ul>
%%%   <li>Add <tt>EsmeRef</tt> to 
%%%     <a href="gen_smsc.html#session_start-4">session_start/4</a>,
%%%     <a href="gen_smsc.html#session_start-5">session_start/5</a>, 
%%%     <a href="gen_smsc.html#session_start_link-4">session_start_link/4</a> 
%%%     and <a href="gen_smsc.html#session_start_link-5">session_start_link/5
%%%     </a> functions.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {13 May 2004} {@time}.
%%% @end
-module(gen_smsc).

-behaviour(gen_server).
-behaviour(gen_smsc_session).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% Behaviour exports
%%%-------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
%%% External SMSC exports
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
         reply/2]).

%%%-------------------------------------------------------------------
%%% External Session exports
%%%-------------------------------------------------------------------
-export([session_start/3,
         session_start/4,
         session_start_link/3,
         session_start_link/4,
         session_stop/1,
         alert_notification/2,
         outbind/2,
         deliver_sm/2,
         data_sm/2,
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
%%% Internal gen_smsc_session exports
%%%-------------------------------------------------------------------
-export([handle_bind/3, handle_operation/3, handle_unbind/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(CONNECT_TIME, 30000).
-define(LISTEN_OPTIONS, 
        [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(CONNECT_OPTIONS, 
        [binary, {packet, 0}, {active, false}]).

-define(DECR(X), if is_integer(X) -> X - 1; true -> X end).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Mod, ModState, LSocket, Timers}
%%    Mod      = atom()
%%    ModState = atom()
%%    LSocket  = socket()
%%    Timers   = #timers{}
%%
%% %@doc Representation of the server's state.
%%
%% <dl>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>ModState: </dt><dd>Callback module private state.</dd>
%%   <dt>LSocket: </dt><dd>Listener socket.</dd>
%%   <dt>Timers: </dt><dd>SMPP timers for accepted session.</dd>
%% </dl>
%% %@end
-record(state, {mod, mod_state, lsocket = closed, timers}).

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
     {handle_bind, 3}, 
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
%%% External SMSC functions
%%%===================================================================
%% @spec start(Module, Args, Options) -> Result
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
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
%% @doc Starts the SMSC server.
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
%% @doc Starts the SMSC server.
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
%% @doc Starts the SMSC server.
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
%% @doc Puts the SMSC Server <tt>ServerRef</tt> to listen on 
%% <tt>DEFAULT_SMPP_PORT</tt>.
%%
%% <p>By default infinity connections are accepted.  Started sessions are NOT
%% linked to the SMSC.</p>
%%
%% @see listen_start/4
%%
%% @equiv listen(ServerRef, DEFAULT_SMPP_PORT, infinity, DEFAULT_SMPP_TIMERS)
%% @end 
listen_start(ServerRef) -> 
    listen_start(ServerRef, ?DEFAULT_SMPP_PORT, infinity,?DEFAULT_SMPP_TIMERS).

%% @spec listen_start(ServerRef, Port, Count, Timers) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Port = int()
%%    Timers = timers()
%%    Count = int() | infinity
%%    Reason = term()
%%
%% @doc Puts the SMSC Server <tt>ServerRef</tt> to listen on <tt>Port</tt>
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  Accepted session will be subject to
%% given timers.</p>
%%
%% <p>Only <tt>Count</tt> connections are accepted.  Started sessions are NOT
%% linked to the SMSC.</p>
%%
%% @see listen_start/1
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
%% @spec session_start(SmscRef, Address, Port) -> Result
%%    SmscRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session.  By default Sessions are NOT linked to the SMSC.
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @equiv session_start(SmscRef, Address, Port, DEFAULT_SMPP_TIMERS)
%% @end 
session_start(SmscRef, Address, Port) ->
	session_start(SmscRef, Address, Port, ?DEFAULT_SMPP_TIMERS).

%% @spec session_start(SmscRef, Address, Port, Timers) -> Result
%%    SmscRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Timers = timers()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session.  By default Sessions are NOT linked to the SMSC.
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.</p>
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @see session_start/3
%% @see gen_smsc_session:start/4
%% @end 
session_start(SmscRef, Address, Port, Timers) ->
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, ?CONNECT_TIME) of
        {ok, Socket} ->
            case gen_smsc_session:start(SmscRef, ?MODULE, Socket, Timers) of
                {ok, Session} ->
                    gen_tcp:controlling_process(Socket, Session),
                    {ok, Session};
                SessionError ->
                    SessionError
            end;
        ConnectError ->
            ConnectError
    end.

%% @spec session_start_link(SmscRef, Address, Port) -> Result
%%    SmscRef = pid() | atom()
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
%%
%% @equiv session_start_link(SmscRef, Address, Port, DEFAULT_SMPP_TIMERS)
%% @end 
session_start_link(SmscRef, Address, Port) ->
	session_start_link(SmscRef, Address, Port, ?DEFAULT_SMPP_TIMERS).

%% @spec session_start_link(SmscRef, Address, Port, Timers) -> Result
%%    SmscRef = pid() | atom()
%%    Address = string() | atom() | ip_address()
%%    Port = int()
%%    Timers = timers()
%%    Result = {ok, Session} | {error, Reason}
%%    Session = pid()
%%    Reason = term()
%%
%% @doc Opens a new session.
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.</p>
%%
%% <p>Returns <tt>{ok, Session}</tt> if success, <tt>{error, Reason}</tt>
%% otherwise.</p>
%%
%% @see session_start_link/3
%% @see gen_smsc_session:start_link/4
%% @end 
session_start_link(SmscRef, Address, Port, Timers) ->
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, ?CONNECT_TIME) of
        {ok, Socket} ->
            case gen_smsc_session:start_link(SmscRef,?MODULE,Socket,Timers) of
                {ok, Session} ->
                    gen_tcp:controlling_process(Socket, Session),
                    {ok, Session};
                SessionError ->
                    SessionError
            end;
        ConnectError ->
            ConnectError
    end.

%% @spec session_stop(Session) -> ok
%%    Session = pid()
%%
%% @doc Stops the <tt>Session</tt>.
%% @end 
session_stop(Session) ->
    gen_smsc_session:stop(Session).

%% @spec alert_notification(Session, ParamList) -> ok
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc Issues an <i>alert_notification</i> operation on the session 
%% identified by <tt>Session</tt>.
%% @end
alert_notification(Session, ParamList) ->
    gen_smsc_session:alert_notification(Session, ParamList).

%% @spec outbind(Session, ParamList) -> ok
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc Issues an <i>outbind</i> operation on the session identified by 
%% <tt>Session</tt>.
%% @end
outbind(Session, ParamList) ->
    gen_smsc_session:outbind(Session, ParamList).

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
    gen_smsc_session:data_sm(Session, ParamList).

%% @spec deliver_sm(Session, ParamList) -> Result
%%    Session = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>deliver_sm</i> operation on the session identified by 
%% <tt>Session</tt>.
%% @end
deliver_sm(Session, ParamList) ->
    gen_smsc_session:deliver_sm(Session, ParamList).

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
    gen_smsc_session:unbind(Session).

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
    pack(Mod:init(Args), #state{mod = Mod}).

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
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            Self = self(),
            spawn_link(fun() -> listener(Self, LSocket, Count) end),
            {reply, true, S#state{lsocket = LSocket, timers = Timers}};
        _Error ->
            {reply, false, S}
    end;
handle_call({accept, Socket}, _From, S) ->
	{reply, gen_smsc_session:start(self(), ?MODULE, Socket, S#state.timers), S};
handle_call({Bind, _Session, _Pdu, _IPAddr} = R, From, S) 
  when Bind==bind_transceiver; Bind==bind_transmitter; Bind==bind_receiver ->
    pack((S#state.mod):handle_bind(R, From, S#state.mod_state), S);
handle_call({unbind, _Session, _Pdu} = R, From, S) ->
    pack((S#state.mod):handle_unbind(R, From, S#state.mod_state), S);
handle_call({_CmdName, _Session, _Pdu} = R, From, S) ->
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
handle_cast(listen_error, S) when S#state.lsocket == closed ->
    {noreply, S};
handle_cast(listen_error, S) ->
    gen_tcp:close(S#state.lsocket),  % Close it anyway
    NewS = S#state{lsocket = closed},
    pack((NewS#state.mod):handle_listen_error(NewS#state.mod_state), NewS);
handle_cast(listen_stop, S) ->
    gen_tcp:close(S#state.lsocket),
    {noreply, S#state{lsocket = closed}}.

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
    pack((S#state.mod):terminate(R, S#state.mod_state), S).

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
%%% SMSC Session functions
%%%===================================================================
%% @spec handle_bind(ServerRef, Session, {CmdName, Pdu, IPAddr}) -> Result
%%    ServerRef = pid()
%%    Session = pid()
%%    CmdName = bind_receiver | bind_transmitter | bind_transceiver
%%    Pdu = pdu()
%%    IPAddr = {int(),int(),int(),int()}
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#handle_bind-3">gen_smsc_session - 
%% handle_bind/3</a> callback implementation.
%% @end
handle_bind(ServerRef, Session, {CmdName, Pdu, IPAddr}) ->
    gen_server:call(ServerRef, {CmdName, Session, Pdu, IPAddr}, infinity).

%% @spec handle_operation(ServerRef, Session, {CmdName, Pdu}) -> Result
%%    ServerRef = pid()
%%    Session = pid()
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
handle_operation(ServerRef, Session, {CmdName, Pdu}) ->
    gen_server:call(ServerRef, {CmdName, Session, Pdu}, infinity).

%% @spec handle_unbind(ServerRef, Session, Pdu) -> ok | {error, Error}
%%    ServerRef = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Error = int()
%%
%% @doc <a href="gen_smsc_session.html#handle_unbind-3">gen_smsc_session - 
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
            inet:setopts(Socket, ?CONNECT_OPTIONS),
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
