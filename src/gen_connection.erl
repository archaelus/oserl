%%%
% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@udc.es>
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
%
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
% You should have received a copy of the GNU General Public License
% along with this program; if not, write to the Free Software
% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
%%

%%%
% @doc TCP connection behaviour.
%
% <p>A generic TCP connection modeled as a simple FSM.</p>
%
%
% <h2>State transitions table</h2>
%
% <p>Possible states for the Connection FSM are shown in the first row.
% Events are those in the first column.  This table shows the next state given
% an event and the current state.</p>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>&#160;</small></th>
%     <th><small>closed</small></th>
%     <th><small>listening</small></th>
%     <th><small>connected</small></th>
%     <th><small>failure</small></th>
%   </tr>
%   <tr> 
%     <th align="left"><small>accept</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>close</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect</small></th>
%     <td valign="top" align="center"><small>connected/failure</small></td>
%     <td valign="top" align="center"><small>connected/failure</small></td>
%     <td valign="top" align="center"><small>connected/failure</small></td>
%     <td valign="top" align="center"><small>connected/failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>die</small></th>
%     <td valign="top" align="center"><small>STOP</small></td>
%     <td valign="top" align="center"><small>STOP</small></td>
%     <td valign="top" align="center"><small>STOP</small></td>
%     <td valign="top" align="center"><small>STOP</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>disable_retry</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>enable_retry</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>fail</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>failure/listening</small></td>
%     <td valign="top" align="center"><small>failure/connected</small></td>
%     <td valign="top" align="center"><small>failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen</small></th>
%     <td valign="top" align="center"><small>listening/failure</small></td>
%     <td valign="top" align="center"><small>listening/failure</small></td>
%     <td valign="top" align="center"><small>listening/failure</small></td>
%     <td valign="top" align="center"><small>listening/failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>recv</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>retry_connect (async)</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>connected/failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>retry_listen (async)</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>listening/failure</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>send</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>connected</small></td>
%     <td valign="top" align="center"><small>failure</small></td>
%   </tr>
% </table>
%
% <h2>Callback Function Index</h2>
%
% <p>A module implementing this behaviour may export these functions.  Leaving 
% a function undefined preserves the default behaviour.</p>
%
% <table width="100%" border="1">
%   <tr>
%     <td valign="top"><a href="#handle_accept-2">handle_accept/2</a></td>
%     <td>A connection was accepted.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#handle_input-4">handle_input/4</a></td>
%     <td>New input data available.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#handle_listen_error-3">handle_listen_error/3</a>
%     </td>
%     <td>Error occurs when listening.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#handle_connect_error-4">handle_connect_error/4</a>
%     </td>
%     <td>Error occurs when connected.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#handle_listen_recovery-3">handle_listen_recovery/3</a>
%     </td>
%     <td>Listen error recovery.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#handle_connect_recovery-4">handle_connect_recovery/4</a>
%     </td>
%     <td>Connect error recovery.</td>
%   </tr>
% </table>
%
%
% <h2>Callback Function Details</h2>
%
% <h3><a name="handle_accept-2">handle_accept/2</a></h3>
%
% <tt>handle_accept(Pid, Cid) -> ok</tt>
% <ul>
%   <li><tt>Pid = Cid = pid()</tt></li>
% </ul>
%
% <p>Whenever an incoming connection is accepted under a listening state, 
% the gen_connection turns into connect state and triggers this callback to 
% notify such an event to the parent of the connection. <tt>Cid</tt> is
% the pid of the connection, <tt>Pid</tt> is the connection's 
% parent pid.</p>
%
% <p><b>See also:</b> <tt>callback_handle_accept/1</tt></p>
%
%
% <h3><a name="handle_input-4">handle_input/4</a></h3>
%
% <tt>handle_input(Pid, Cid, Input, Lapse) -> {ok, RestBuffer}</tt>
% <ul>
%   <li><tt>Pid = Cid = pid()</tt></li>
%   <li><tt>Input = RestBuffer = binary()</tt></li>
%   <li><tt>Lapse = int()</tt></li>
% </ul>
%
% <p>Every time the connection receives new data, this function is called to 
% let the callback module consume data from the <tt>Input</tt> and from
% the stored <tt>Buffer</tt>.  This function should return the unused data.
% <tt>Cid</tt> is the pid of the connection, <tt>Pid</tt> is the
% connection's parent pid.</p>
%
% <p><tt>Lapse</tt> are the microseconds waited until input came.</p>
%
% <p><b>See also:</b> <tt>callback_handle_input/3</tt></p>
%
%
% <h3><a name="handle_listen_error-3">handle_listen_error/3</a></h3>
%
% <tt>handle_listen_error(Pid, Cid, Port) -> ok</tt>
% <ul>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Pid = Cid = pid()</tt></li>
% </ul>
%
% <p>If the connection is listening on port <tt>Port</tt> and an error
% occurs, this function reports the failure to the callback module so it can
% take the appropriate actions.  The connection may try to recover itself
% every <tt>#state.retry_time</tt> milliseconds, if this timer wasn't
% set to the atom <tt>infinity</tt>, in such a case the callback module
% should take care of this situation. <tt>Cid</tt> is the pid of the
% connection, <tt>Pid</tt> is the connection's parent pid.</p>
%
% <p>Return value is ignored by the connection.</p>
%
% <p><b>See also:</b> <tt>callback_handle_listen_error/1</tt></p>
%
%
% <h3><a name="handle_connect_error-4">handle_connect_error/4</a></h3>
%
% <tt>handle_connect_error(Pid, Cid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Pid = Cid = pid()</tt></li>
% </ul>
%
% <p>If the connection is active (sending/receiving data to/from address
% <tt>Address</tt> on port <tt>Port</tt>) and an error occurs, this 
% function reports the failure to the callback module so it can take the
% appropriate actions.  The connection may try to recover itself every
% <tt>#state.retry_time</tt> milliseconds, if this timer wasn't set
% to the atom <tt>infinity</tt>, in such a case the callback module should
% take care of this situation.  <tt>Cid</tt> is the pid of the connection,
% <tt>Pid</tt> is the connection's parent pid.</p>
%
% <p>Return value is ignored by the connection.</p>
%
% <p><b>See also:</b> <tt>callback_handle_connect_error/1</tt></p>
%
%
% <h3><a name="handle_listen_recovery-3">handle_listen_recovery/3</a></h3>
%
% <tt>handle_listen_recovery(Pid, Cid, Port) -> ok</tt>
% <ul>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Pid = Cid = pid()</tt></li>
% </ul>
%
% <p>Notifies the callback module than the connection is again, after a 
% failure, up and listening on port <tt>Port</tt>. <tt>Cid</tt> is the
% pid of the connection, <tt>Pid</tt> is the connection's parent pid.</p>
%
% <p>Return value is ignored by the connection.</p>
%
% <p><b>See also:</b> <tt>callback_handle_listen_recovery/1</tt></p>
%
%
% <h3><a name="handle_connect_recovery-4">handle_connect_recovery/4</a></h3>
%
% <tt>handle_connect_recovery(Pid, Cid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Pid = Cid = pid()</tt></li>
% </ul>
%
% <p>Notifies the callback module than the connection is again, after a 
% failure, up and ready to send/receive data to address <tt>Address</tt> 
% on port <tt>Port</tt> . <tt>Cid</tt> is the pid of the connection. 
% <tt>Pid</tt> is the connection's parent pid.</p>
%
% <p>Return value is ignored by the connection.</p>
%
% <p><b>See also:</b> <tt>callback_handle_connect_recovery/1</tt></p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [22 Feb 2004]
%
% <ul>
%   <li><tt>retry_status</tt> is <tt>disabled</tt> on <tt>die</tt> event.</li>
%   <li>If already listening on <tt>Port</tt>, the term
%     <tt>{error, {already_listening, Port}}</tt> is returned for a request to
%     <tt>listen</tt> on <tt>Port</tt> (instead of <tt>ok</tt>).
%   </li>
%   <li>If already connected to <tt>Address</tt> on <tt>Port</tt>, the term
%     <tt>{error, {already_listening, Address, Port}}</tt> is returned for a
%     request to <tt>connect</tt> to <tt>Address</tt> on <tt>Port</tt> 
%     (instead of <tt>ok</tt>).
%   </li>
% </ul>
%
% [27 Feb 2004]
%
% <ul>
%   <li>Changes in comments.</li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.2 alpha, { 1 Jun 2003} {@time}.
% @end
%
% %@TODO ¿Output buffer?  Buffer output on send operations under a connect 
% failure, flush buffer on recovery.
%%
-module(gen_connection).

-behaviour(gen_fsm).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Behaviour exports
%%--------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([start_link/2, 
         start_link/3,
         connect/3, 
         connect/4, 
         disable_retry/1,
         enable_retry/1,
         listen/2,
         send/2, 
         close/1, 
         stop/1]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
-export([init/1,
         closed/2,
         listening/2,
         connected/2,
         failure/2,
         closed/3,
         listening/3,
         connected/3,
         failure/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(RETRY_TIME, 5000).
-define(CONNECT_TIME, 30000).
-define(LISTEN_OPTIONS, 
        [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(CONNECT_OPTIONS, 
        [binary, {packet, 0}, {active, false}]).

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%
% %@spec {state, 
%         Parent, 
%         Self,
%         CallbackModule, 
%         Address, 
%         Port, 
%         Socket, 
%         Buffer, 
%         ConnectTime,
%         RetryTime, 
%         RetryStatus,
%         Role}
%    Parent         = pid()
%    Self           = pid()
%    CallbackModule = atom()
%    Address        = string() | atom() | ip_address()
%    Port           = int()
%    Socket         = socket()
%    Buffer         = binary()
%    ConnectTime    = int() | infinity
%    RetryTime      = int() | infinity
%    RetryStatus    = enabled | disabled
%    Role           = client | server
%
% %@doc Representation of the fsm's state
%
% <dl>
%   <dt>Parent: </dt><dd>Pid of the parent process.  Is passed in the
%     callback functions to help identify the owner of the connection.
%   </dd>
%   <dt>Self: </dt><dd>The Pid of our gen_connection process.  Is passed in the
%     callback functions to help identify the gen_connection.  Since
%     a callback function might be called from a spawned procces, we want to
%     keep a reference to our main process.
%   </dd>
%   <dt>CallbackModule: </dt><dd>Module where callbacks are defined.</dd>
%   <dt>Address: </dt><dd>IP Address of the host to connect to.  Can be either 
%     a hostname, or an IP address.
%   </dd>
%   <dt>Port: </dt><dd>Listen (acting as server) or connection port number 
%     (acting as client).</dd>
%   <dt>Socket: </dt><dd>Active socket of the connection.  Either a listen
%     or a connection socket.
%   </dd>
%   <dt>Buffer: </dt><dd>Received data is stored in this buffer.  This buffer
%     is passed to the callback module via the handle_input/4 function every
%     time new input is received.  The callback module may consume all or
%     part of the buffer.
%   </dd>
%   <dt>ConnectTime: </dt><dd>Timer for the connect operation timeout.  On 
%     expiration the connect operation fails (default is ?CONNECT_TIME).
%   </dd>
%   <dt>RetryTime: </dt><dd>Retry timer.  If a failure occurs, the connection
%     will try to reconnect every <tt>RetryTime</tt> milliseconds.  A
%     <tt>infinity</tt> value permanently disables retrying (default is 
%     ?RETRY_TIME).
%   </dd>
%   <dt>RetryStatus: </dt><dd>Flag used to dynamically enable/disable retry
%     mechanisms.  This field might be used to disable retrying under certain
%     especial circumstances.  Accepted values are <tt>enabled</tt> and
%     <tt>disabled</tt> (default is <tt>enabled</tt>).
%   </dd>
%   <dt>Role: </dt><dd>The connection may be acting as a client or server.
%     A server connection listens on <tt>Port</tt> and accepts incoming
%     connections, a connection has a client role if the connect/1 function
%     is explicitly called.
%   </dd>
% </dl>
% %@end
%%
-record(state, 
        {parent,
         self,
         callback_module,
         address, 
         port, 
         socket,
         buffer       = <<>>,
         connect_time = ?CONNECT_TIME,
         retry_time   = ?RETRY_TIME,
         retry_status = enabled,
         role}).

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec behaviour_info(Category) -> Info
%    Category      = callbacks | term()
%    Info          = CallbacksInfo | term()
%    CallbacksInfo = [{FunctionName, Arity}]
%    FunctionName  = atom()
%    Arity         = int()
%
% @doc Gives information about the behaviour.
% @end
%
% %@see
%
% %@equiv
%%
behaviour_info(callbacks) ->
    [{handle_accept, 2}, 
     {handle_input, 4}, 
     {handle_listen_error, 3}, 
     {handle_connect_error, 4},
     {handle_listen_recovery, 3}, 
     {handle_connect_recovery, 4}];

behaviour_info(_Other) ->
    undefined.


%%%
% @spec start_link(Module, RTime) -> Result
%    Module = atom()
%    RTime  = int() | infinity
%    Result = {ok, Pid} | ignore | {error, Error}
%    Pid    = pid()
%    Error  = {already_started, Pid} | term()
%
% @doc Starts the connection server.
%
% <p><tt>Module</tt> is the callback module and <tt>RTime</tt> the
% retry timer, the atom <tt>infinity</tt> disables the timeout.</p>
%
% <p>The gen_connection is not registered.</p>
%
% @see gen_fsm:start_link/3
% @see start_link/3
% @end
%%
start_link(Module, RTime) ->
    gen_fsm:start_link(?MODULE, [self(), Module, RTime], []).


%%%
% @spec start_link(CName, Module, RTime) -> Result
%    CName  = {local, Name} | {global, Name}
%    Name   = atom()
%    Module = atom()
%    RTime  = int() | infinity
%    Result = {ok, Pid} | ignore | {error, Error}
%    Pid    = pid()
%    Error  = {already_started, Pid} | term()
%
% @doc Starts the connection server.
%
% <p><tt>Module</tt> is the callback module and <tt>RTime</tt> the
% retry timer, the atom <tt>infinity</tt> disables the timeout.</p>
%
% <p>If <tt>CName = {local, Name}</tt>, the gen_connection is registered
% locally as <tt>Name</tt>.  If <tt>CName = {global, Name}</tt>, the
% gen_connection is registered globally as <tt>Name</tt>.</p>
%
% @see gen_fsm:start_link/4
% @see start_link/2
% @end
%%
start_link(CName, Module, RTime) ->
    gen_fsm:start_link(CName, ?MODULE, [self(), Module, RTime], []).


%%%
% @spec connect(Cid, Address, Port) -> ok | {error, Reason}
%    Cid     = pid()
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    Reason  = term()
%
% @doc
% @see connect/4
%
% @equiv connect(Cid, Address, Port, CONNECT_TIME)
% @end
%%
connect(Cid, Address, Port) ->
    connect(Cid, Address, Port, ?CONNECT_TIME).


%%%
% @spec connect(Cid, Address, Port, CTime) -> ok | {error, Reason}
%    Cid     = pid()
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    CTime   = int() | infinity
%    Reason  = term()
%
% @doc Connects  to  a server on TCP port <tt>Port</tt> on the host
% with IP address <tt>Address</tt> (the address fields can be either 
% a hostname, or an IP address) waiting on the connect during 
% <tt>CTime</tt>.  If <tt>CTime</tt> is not supplied the default
% is assumed (defined by the macro ?CONNECT_TIME).
%
% <p>If the connection is already connected to <tt>Address</tt> on
% <tt>Port</tt>, this function returns <tt>ok</tt> and nothing really
% happens (no reconnection is done).</p>
%
% @see gen_fsm:syn_send_event/3
% @see connect/3
% @end
%
% %@equiv
%%
connect(Cid, Address, Port, CTime) ->
    gen_fsm:sync_send_event(Cid, {connect, Address, Port, CTime}, infinity).


%%%
% @spec disable_retry(Cid) -> ok 
%    Cid    = pid()
%
% @doc Disables retrying.  Under certain circumstances the automatic recovery
% mechanisms are better disabled.  Use enable_retry/1 to enable them back up.
%
% @see gen_fsm:sync_send_all_state_event/3
% @end
%
% %@equiv
%%
disable_retry(Cid) ->
    gen_fsm:sync_send_all_state_event(Cid, disable_retry, infinity).


%%%
% @spec enable_retry(Cid) -> ok 
%    Cid    = pid()
%
% @doc Enables retrying.
%
% @see gen_fsm:sync_send_all_state_event/3
% @end
%
% %@equiv
%%
enable_retry(Cid) ->
    gen_fsm:sync_send_all_state_event(Cid, enable_retry, infinity).



%%%
% @spec listen(Cid, Port) -> ok | {error, Reason}
%    Cid    = pid()
%    Port   = int()
%    Reason = term()
%
% @doc  Sets up socket to listen on <tt>Port</tt> of the local host.
%
% <p>If already listening on <tt>Port</tt>, the atom <tt>ok</tt> is
% returned and nothing else done.</p>
%
% @see gen_fsm:sync_send_event/3
% @end
%
% %@equiv
%%
listen(Cid, Port) ->
    gen_fsm:sync_send_event(Cid, {listen, Port}, infinity).


%%%
% @spec send(Cid, Output) -> ok | {error, Reason}
%    Cid    = pid()
%    Output = binary()
%    Reason = term()
%
% @doc Sends a binary <tt>Output</tt> through the connection socket.
%
% @see gen_fsm:sync_send_event/3
% @end
%
% %@equiv
%%
send(Cid, Output) ->
    gen_fsm:sync_send_event(Cid, {send, Output}, infinity).


%%%
% @spec close(Cid) -> ok | {error, Error}
%    Cid   = pid()
%    Error = term()
%
% @doc Closes the connection.  The result of this function is the term
% returned by gen_tcp:close/1.
%
% @see gen_fsm:sync_send_event/3
% @end
%
% %@equiv
%%
close(Cid) ->
    gen_fsm:sync_send_event(Cid, close, infinity).


%%%
% @spec stop(Cid) -> ok
%    Cid = pid()
%
% @doc Stops the fsm.  Notice that the connection should have been previously
% closed by <tt>close/1</tt> function.
%
% @see gen_fsm:send_all_state_event/2
% @end
%
% %@equiv
%%
stop(Cid) ->
    gen_fsm:send_all_state_event(Cid, die).


%%%===================================================================
% Server functions
%%====================================================================
%%%
% @spec init(Args) -> Result
%    Args       = term()
%    Result     = {ok, StateName, StateData}          |
%                 {ok, StateName, StateData, Timeout} |
%                 ignore                              |
%                 {stop, StopReason}                   
%    StateName  = atom()
%    StateData  = term()
%    Timeout    = int()
%    StopReason = term()
%
% @doc Initializes the the fsm.
% @end
%%
init([Pid, Module, RTime]) ->
    StateData = #state{parent          = Pid,
                       self            = self(),
                       callback_module = Module,
                       retry_time      = RTime},
    process_flag(trap_exit, true),
    {ok, closed, StateData}.


%%%
% @spec closed(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = gen_connection:state()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name closed.
% @end
%%
closed(_Event, StateData) ->
    {next_state, closed, StateData}.


%%%
% @spec listening(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = gen_connection:state()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name listening.
% @end
%%
listening(_Event, StateData) ->
    {next_state, listening, StateData}.


%%%
% @spec connected(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = gen_connection:state()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name connected.
% @end
%%
connected(_Event, StateData) ->
    {next_state, connected, StateData}.


%%%
% @spec failure(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = gen_connection:state()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name failure.
% @end
%%
failure(retry_connect, #state{retry_status=R} = StateData) when R /= enabled ->
    retry_connect_after(StateData#state.retry_time),
    {next_state, failure, StateData};

failure(retry_connect, #state{address=A,port=P,connect_time=T} = StateData) ->
    case gen_tcp:connect(A, P, ?CONNECT_OPTIONS, T) of 
        {ok, Socket} ->
            spawn(fun() -> callback_handle_connect_recovery(StateData) end),
            spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
            {next_state, connected, StateData#state{socket = Socket}};
        _Error ->
            retry_connect_after(StateData#state.retry_time),
            {next_state, failure, StateData}
    end;

failure(retry_listen, #state{retry_status=R} = StateData) when R /= enabled ->
    retry_listen_after(StateData#state.retry_time),
    {next_state, failure, StateData};

failure(retry_listen, StateData) ->
    case gen_tcp:listen(StateData#state.port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            spawn(fun() -> callback_handle_listen_recovery(StateData) end),
            spawn_link(fun() -> wait_accept(StateData#state.self,LSocket) end),
            {next_state, listening, StateData#state{socket = LSocket}};
        _Error ->
            retry_listen_after(StateData#state.retry_time),
            {next_state, failure, StateData}
    end.


%%%
% @spec closed(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name closed.
% @end
%%
closed({accept, _Socket}, _From, StateData) ->
    {reply, ok, closed, StateData};

closed(close, _From, StateData) ->
    {reply, ok, closed, StateData};

closed({connect, Address, Port, CTime}, _From, StateData) ->
    NewStateData = StateData#state{address      = Address,
                                   port         = Port,
                                   connect_time = CTime,
                                   role         = client},
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, CTime) of 
        {ok, Socket} ->
            spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
            {reply, ok, connected, NewStateData#state{socket = Socket}};
        Error ->
            retry_connect_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

closed({fail, _Socket}, _From, StateData) ->
    {reply, ok, closed, StateData};

closed({listen, Port}, _From, StateData) ->
    NewStateData = StateData#state{port = Port, role = server},
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            spawn_link(fun() -> wait_accept(StateData#state.self,LSocket) end),
            {reply, ok, listening, NewStateData#state{socket = LSocket}};
        Error ->
            retry_listen_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

closed({recv, Input, _Lapse}, _From, StateData) ->
    Buffer = concat_binary([StateData#state.buffer, Input]),
    {reply, ok, closed, StateData#state{buffer = Buffer}};

closed({send, _Output}, _From, StateData) ->
    {reply, {error, closed}, closed, StateData}.


%%%
% @spec listening(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name listening.
% @end
%%
listening({accept, Socket}, _From, StateData) ->
    gen_tcp:close(StateData#state.socket),  % Close listen socket.
    spawn(fun() -> callback_handle_accept(StateData) end),
    spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
    {reply, ok, connected, StateData#state{socket = Socket}};

listening(close, _From, StateData) ->
    {reply, gen_tcp:close(StateData#state.socket), closed, StateData};

listening({connect, Address, Port, CTime}, _From, StateData) ->
    NewStateData = StateData#state{address      = Address,
                                   port         = Port,
                                   connect_time = CTime,
                                   role         = client},
    gen_tcp:close(StateData#state.socket),  % Close listen socket.
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, CTime) of 
        {ok, Socket} ->
            spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
            {reply, ok, connected, NewStateData#state{socket = Socket}};
        Error ->
            retry_connect_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

listening({fail, S}, _From, #state{socket = S} = StateData) ->
    % Failing Socket is active
    spawn(fun() -> callback_handle_listen_error(StateData) end),
    retry_listen_after(StateData#state.retry_time),
    {reply, ok, failure, StateData};

listening({fail, _Socket}, _From, StateData) ->
    % Socket was closed and replaced, ignore failure
    {reply, ok, listening, StateData};

listening({listen, P}, _From, #state{port = P} = StateData) ->
    {reply, {error, {already_listening, P}}, listening, StateData};

listening({listen, Port}, _From, StateData) ->
    NewStateData = StateData#state{port = Port},
    gen_tcp:close(StateData#state.socket),  % Close actual listen socket.
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            spawn_link(fun() -> wait_accept(StateData#state.self,LSocket) end),
            {reply, ok, listening, NewStateData#state{socket = LSocket}};
        Error ->
            retry_listen_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

listening({recv, Input, _Lapse}, _From, StateData) ->
    Buffer = concat_binary([StateData#state.buffer, Input]),
    {reply, ok, listening, StateData#state{buffer = Buffer}};

listening({send, _Output}, _From, StateData) ->
    {reply, {error, listening}, listening, StateData}.


%%%
% @spec connected(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name connected.
% @end
%%
connected({accept, _Socket},  _From, StateData) ->
    {reply, {error, connected}, connected, StateData};

connected(close, _From, StateData) ->
    {reply, gen_tcp:close(StateData#state.socket), closed, StateData};

connected({connect, A, P, _},  _From, #state{address=A, port=P} = StateData) ->
    {reply, {error, {already_connected, A, P}}, connected, StateData};

connected({connect, Address, Port, CTime}, _From, StateData) ->
    NewStateData = StateData#state{address      = Address,
                                   port         = Port,
                                   connect_time = CTime,
                                   role         = client},
    gen_tcp:close(StateData#state.socket),  % Close actual connection socket
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, CTime) of 
        {ok, Socket} ->
            spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
            {reply, ok, connected, NewStateData#state{socket = Socket}};
        Error ->
            retry_connect_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

connected({fail, S}, _From, #state{socket = S} = StateData) ->
    % Failing Socket is active
    case StateData#state.role of
        client ->
            spawn(fun() -> callback_handle_connect_error(StateData) end),
            retry_connect_after(StateData#state.retry_time);
        _Server ->
            spawn(fun() -> callback_handle_listen_error(StateData) end),
            retry_listen_after(StateData#state.retry_time)
    end,
    {reply, ok, failure, StateData};

connected({fail, _Socket}, _From, StateData) ->
    % Failing Socket was already closed and replaced, ignore failure
    {reply, ok, connected, StateData};

connected({listen, Port}, _From, StateData) ->
    NewStateData = StateData#state{port = Port, role = server},
    gen_tcp:close(StateData#state.socket),  % Close connection socket
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            spawn_link(fun() -> wait_accept(StateData#state.self,LSocket) end),
            {reply, ok, listening, NewStateData#state{socket = LSocket}};
        Error ->
            retry_listen_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

connected({recv, Input, Lapse}, _From, StateData) ->
    RestBuffer = callback_handle_input(StateData, Input, Lapse),
    {reply, ok, connected, StateData#state{buffer = RestBuffer}};

connected({send, Output}, _From, #state{socket = S} = StateData) ->
    {reply, gen_tcp:send(S, Output), connected, StateData}.


%%%
% @spec failure(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name failure.
% @end
%%
failure({accept, Socket}, _From, StateData) ->
    spawn(fun() -> callback_handle_connect_recovery(StateData) end),
    spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
    {reply, ok, connected, StateData#state{socket = Socket}};

failure(close, _From, StateData) ->
    {reply, ok, closed, StateData};

failure({connect, Address, Port, CTime}, _From, StateData) ->
    NewStateData = StateData#state{address      = Address,
                                   port         = Port,
                                   connect_time = CTime,
                                   role         = client},
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, CTime) of 
        {ok, Socket} ->
            spawn_link(fun() -> wait_recv(StateData#state.self, Socket) end),
            {reply, ok, connected, NewStateData#state{socket = Socket}};
        Error ->
            retry_connect_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

failure({fail, _Socket}, _From, StateData) ->
    {reply, ok, failure, StateData};

failure({listen, Port}, _From, StateData) ->
    NewStateData = StateData#state{port = Port, role = server},
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            spawn_link(fun() -> wait_accept(StateData#state.self,LSocket) end),
            {reply, ok, listening, NewStateData#state{socket = LSocket}};
        Error ->
            retry_listen_after(StateData#state.retry_time),
            {reply, Error, failure, NewStateData}
    end;

failure({recv, Input, _Lapse}, _From, StateData) ->
    Buffer = concat_binary([StateData#state.buffer, Input]),
    {reply, ok, failure, StateData#state{buffer = Buffer}};

failure({send, _Output}, _From, StateData) ->
    {reply, {error, failure}, failure, StateData}.


%%%
% @spec handle_event(Event, StateName, StateData) -> Result
%    Event         = die | term()
%    StateName     = atom()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                         
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events received by <tt>gen_fsm:send_all_state_event/2</tt>.
% @end
%%
handle_event(die, _StateName, StateData) ->
    {stop, normal, StateData#state{retry_status = disabled}}.


%%%
% @spec handle_sync_event(Event, From, StateName, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateName     = atom()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events received via 
% <tt>gen_fsm:sync_send_all_state_event/2,3</tt>.
% @end
%%
handle_sync_event(disable_retry, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData#state{retry_status = disabled}};

handle_sync_event(enable_retry, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData#state{retry_status = enabled}};

handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.


%%%
% @spec handle_info(Info, StateName, StateData) -> Result
%    Info          = term()
%    StateName     = atom()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                         
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Call on reception of any other messages than a synchronous or
% asynchronous event.
% @end
%%
handle_info({'EXIT', _Child, Reason}, StateName, StateData) 
  when Reason /= normal, StateName /= failure ->
    % If a wait_accept or wait_recv child crashes the connection is stopped.
    gen_tcp:close(StateData#state.socket),  % close active socket, necessary?
    {stop, Reason, StateData};

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%%
% @spec terminate(Reason, StateName, StateData) -> true
%    Reason    = normal | shutdown | term()
%    StateName = atom()
%    StateData = term()
%
% @doc Shutdown the fsm.
%
% <p>Return value is ignored by the server.</p>
% @end
%%
terminate(_Reason, _StateName, #state{self = S} = StateData) ->
    callback_handle_input(StateData, <<>>, 0),
    case process_info(S, registered_name) of
        {registered_name, Name} ->
            unregister(Name);
        _NotRegistered ->
            true
    end.


%%%
% @spec code_change(OldVsn, StateName, StateData, Extra) -> Result
%    OldVsn        = undefined | term()
%    StateName     = term()
%    StateData     = term()
%    Extra         = term()
%    Result        = {ok, NextStateName, NewStateData}
%    NextStateName = atom()
%    NewStateData  = term()
%
% @doc Convert process state when code is changed.
% @end
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%-------------------------------------------------------------------
% Internal server calls
%%--------------------------------------------------------------------
%%%
% @spec accept(Cid, Socket) -> ok
%    Cid    = pid()
%    Socket = socket()
%
% @doc Accepts an incoming connection request on the listen socket.
% @end
%
% %@see
%
% %@equiv
%%
accept(Cid, Socket) ->
    gen_fsm:sync_send_event(Cid, {accept, Socket}, infinity).


%%%
% @spec fail(Cid, Socket) -> ok
%    Cid    = pid()
%    Socket = socket()
%
% @doc Reports a failure on <tt>Socket</tt>.
% @end
%
% %@see
%
% %@equiv
%%
fail(Cid, Socket) ->
    gen_fsm:sync_send_event(Cid, {fail, Socket}, infinity).


%%%
% @spec recv(Cid, Input, Lapse) -> ok
%    Cid   = pid()
%    Input = binary()
%    Lapse = int()
%
% @doc Receives binary <tt>Input</tt> on the connection's socket and
% triggers the <tt>handle_input/4</tt> callback.  <tt>Lapse</tt>
% indicates the microsends waiting for the input to come.
% @end
%
% %@see
%
% %@equiv
%%
recv(Cid, Input, Lapse) ->
    gen_fsm:sync_send_event(Cid, {recv, Input, Lapse}, infinity).


%%%-------------------------------------------------------------------
% Internal functions
%%--------------------------------------------------------------------
%%%
% @spec retry_connect_after(Time) -> void()
%    Time = int()
%
% @doc Spawns a process to wait <tt>Time</tt> miliseconds and retry to 
% wake up the connection.
% @end
%
% %@see
%
% %@equiv
%%
retry_connect_after(infinity) ->
    ok;

retry_connect_after(Time) ->
    send_event_after(Time, retry_connect).


%%%
% @spec retry_listen_after(Time) -> void()
%    Time = int()
%
% @doc Spawns a process to wait <tt>Time</tt> miliseconds and retry to 
% wake up the listen socket.
% @end
%
% %@see
%
% %@equiv
%%
retry_listen_after(infinity) ->
    ok;

retry_listen_after(Time) ->
    send_event_after(Time, retry_listen).


%%%
% @spec send_event_after(Time, Event) -> Ref
%    Time  = int()
%    Event = term()
%    Ref   = pid()
%
% @doc This funcion is superceded by gen_fsm:send_event_after/2 on stdlib
% version 1.12.  Is is provided for forward compatibility.
% @end
%
% %@see
%
% %@equiv
%
% %@TODO Replace by gen_fsm:send_event_after/2 if using stdlib 1.12 or greater.
%%
send_event_after(Time, Event) ->
    WaitSend = fun(C, T, E) ->
                       receive after T -> gen_fsm:send_event(C, E) end
               end,
    Cid = self(),
    spawn(fun() -> WaitSend(Cid, Time, Event) end).


%%%
% @spec wait_accept(Cid, LSocket) -> void()
%    Cid     = pid()
%    LSocket = socket()
%
% @doc Waits until a connection is requested on <tt>LSocket</tt> or an
% error occurs.  If successful the event <i>accept</i> is triggered, on
% error a failure on the listen socket is reported.
% @end
%
% %@see
%
% %@equiv
%%
wait_accept(Cid, LSocket) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, ?CONNECT_OPTIONS),  % Needed?
            accept(Cid, Socket);
        {error, Reason} ->
            fail(Cid, LSocket)
    end.


%%%
% @spec wait_recv(Cid, Socket) -> void()
%    Cid    = pid()
%    Socket = socket()
%
% @doc Waits until new data is received on <tt>Socket</tt> and starts a 
% receive loop to get bulk input.  All data received on the same loop 
% triggers the event <i>recv</i> with the same timestamp (with a 0 time lapse).
%
% <p>If the <tt>Socket</tt> is closed a failure is reported.</p>
% @end
%
% %@see
%
% %@equiv
%%
wait_recv(Cid, Socket) ->
    Timestamp = now(),
    case gen_tcp:recv(Socket, 0) of
        {ok, Input} ->
            recv(Cid, Input, my_calendar:time_since(Timestamp)),
            recv_loop(Cid, Socket);
        {error, Reason} ->
            fail(Cid, Socket)
    end.

%%%
% @doc Auxiliary function for wait_recv/2
%
% @see wait_recv/2
% @end
%%
recv_loop(Cid, Socket) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Input} ->                    % Some input waiting already 
            recv(Cid, Input, 0),
            recv_loop(Cid, Socket);
        {error, timeout} ->               % No data inmediately available
            wait_recv(Cid, Socket);
        {error, Reason} ->
            fail(Cid, Socket)
    end.

%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Callback wrappers
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% @spec callback_handle_accept(StateData) -> ok
%    StateData = state()
%
% @doc Wrapper for CallbackModule:handle_accept/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_handle_accept(StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Cid = StateData#state.self,
    case catch Mod:handle_accept(Pid, Cid) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_handle_input(StateData, Input, Lapse) -> RestBuffer
%    StateData = state()
%    Input = binary()
%    RestBuffer = binary()
%    Lapse = int()
%
% @doc Wrapper for CallbackModule:handle_input/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_handle_input(StateData, Input, Lapse) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Cid = StateData#state.self,
    Buf = concat_binary([StateData#state.buffer, Input]),
    case catch Mod:handle_input(Pid, Cid, Buf, Lapse) of
        {ok, RestBuffer} when binary(RestBuffer) ->
            RestBuffer;
        _Otherwise ->
            <<>>
    end.


%%%
% @spec callback_handle_listen_error(StateData) -> ok
%    StateData = state()
%
% @doc Wrapper for CallbackModule:handle_listen_error/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_handle_listen_error(StateData) ->
    Port = StateData#state.port,
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Cid = StateData#state.self,
    case catch Mod:handle_listen_error(Pid, Cid, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_handle_connect_error(StateData) -> ok
%    StateData = state() 
%
% @doc Wrapper for CallbackModule:handle_connect_error/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_handle_connect_error(StateData) ->
    Address = StateData#state.address,
    Port = StateData#state.port,
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Cid = StateData#state.self,
    case catch Mod:handle_connect_error(Pid, Cid, Address, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_handle_listen_recovery(StateData) -> ok
%    StateData = state()
%
% @doc Wrapper for CallbackModule:handle_listen_recovery/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_handle_listen_recovery(StateData) ->
    Port = StateData#state.port,
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Cid = StateData#state.self,
    case catch Mod:handle_listen_recovery(Pid, Cid, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_handle_connect_recovery(StateData) -> ok
%    StateData = state()
%
% @doc Wrapper for CallbackModule:handle_connect_recovery/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_handle_connect_recovery(StateData) ->
    Address = StateData#state.address,
    Port = StateData#state.port,
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Cid = StateData#state.self,
    case catch Mod:handle_connect_recovery(Pid, Cid, Address, Port) of
        _Whatever ->
            ok
    end.
