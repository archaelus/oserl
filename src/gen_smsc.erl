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
%%%
%%% <p>A module implementing this behaviour must export these functions.</p>
%%%
%%% <table width="100%" border="1">
%%%   <tr>
%%%     <td valign="top">
%%%       <a href="#bind_receiver_resp-3">bind_receiver_resp/3</a>
%%%     </td>
%%%     <td>Forwards bind_receiver responses.</td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%%
%%% <h3><a name="bind_receiver_resp-3">bind_receiver_resp/3</a></h3>
%%%
%%% <tt>bind_receiver_resp(Pid, Sid, Resp) -> ok</tt>
%%% <ul>
%%%   <li><tt>Pid = Eid = pid()</tt></li>
%%%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%%%   <li><tt>PduResp = pdu()</tt></li>
%%%   <li><tt>Error = int()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards bind_receiver responses.</p>
%%%
%%% <p>Returning value is ignored by the ESME. The callback module may start
%%% some initialization on response to this callback.</p>
%%% 
%%% <p><tt>Error</tt> is the SMPP error returned by the bind operation.</p>
%%%
%%% <p><tt>PduResp</tt> is the PDU response sent by the MC. <tt>Pid</tt> is the
%%% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%%%
%%% <p><b>See also:</b> <tt>callback_bind_receiver_resp/2</tt></p>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 0.1, {13 May 2004} {@time}.
%%% @end
-module(gen_smsc).

-behaviour(gen_server).
-behaviour(gen_smsc_session).
-behaviour(gen_tcp_connection).

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
         listen/2,
         call/2, 
         call/3, 
         cast/2, 
         stop/1]).

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
%%% Internal gen_connection exports
%%%-------------------------------------------------------------------
-export([handle_accept/3, handle_input/4]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Mod, ModState, SystemId, Password, Timers, Sessions}
%%    Mod           = atom()
%%    ModState      = atom()
%%    SystemId      = string()
%%    Password      = string()
%%    Timers        = #timers()
%%    Sessions      = ets()
%%
%% %@doc Representation of the server's state.
%%
%% <dl>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>ModState: </dt><dd>Callback module private state.</dd>
%%   <dt>SystemId: </dt><dd>ESME identifier.  C-octet string, null terminated 
%%     string (default value is ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>Password: </dt><dd>ESME password.  C-octet string, null terminated 
%%     string (default value is ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>Timers: </dt><dd>SMPP timers.</dd>
%%   <dt>Sessions: </dt><dd>ETS table with the active sessions.</dd>
%% </dl>
%% %@end
-record(state, 
		{mod, 
		 mod_state, 
		 system_id, 
		 password, 
		 timers, 
		 listener,
		 sessions}).


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
      {handle_ubind, 3}, 
      {handle_session_failure, 2},
	  {handle_listen_error, 1},
      {handle_call, 3},
      {handle_cast, 2},
      {handle_info, 2},
      {terminate, 2},
      {code_change, 3}];
behaviour_info(_Other) ->
    undefined.


%% @spec start(Module, Setup, Args, Options) -> Result
%%    Module     = atom()
%%    Setup      = #smsc_setup()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% <p><tt>Setup</tt> is a <tt>smsc_setup</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macros
%% <tt>DEFAULT_SMSC_SETUP()</tt> or <tt>SMSC_SETUP(SystemId, Password)</tt> to
%% define this parameter if default SMPP timers are OK for you.</p>
%%
%% @see gen_server:start/3
%% @see start_link/4
%% @see start/5
%% @end
start(Module, Setup, Args, Options) ->
    gen_server:start(?MODULE, {Module, Setup, Args}, Options).


%% @spec start(ServerName, Module, Setup, Args, Options) -> Result
%%    ServerName = {local, Name} | {global, Name}
%%    Name       = atom()
%%    Module     = atom()
%%    Setup      = #smsc_setup()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% <p><tt>Setup</tt> is a <tt>smsc_setup</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macros
%% <tt>DEFAULT_SMSC_SETUP()</tt> or <tt>SMSC_SETUP(SystemId, Password)</tt> to
%% define this parameter if default SMPP timers are OK for you.</p>
%%
%% @see gen_server:start/4
%% @see start_link/5
%% @see start/4
%% @end
start(ServerName, Module, Setup, Args, Options) ->
    gen_server:start(ServerName, ?MODULE, {Module, Setup, Args}, Options).


%% @spec start_link(Module, Setup, Args, Options) -> Result
%%    Module     = atom()
%%    Setup      = #smsc_setup()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt> have the exact same
%% meaning as in gen_server behavior.</p>
%%
%% <p><tt>Setup</tt> is a <tt>smsc_setup</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macros
%% <tt>DEFAULT_SMSC_SETUP()</tt> or <tt>SMSC_SETUP(SystemId, Password)</tt> to
%% define this parameter if default SMPP timers are OK for you.</p>
%%
%% @see gen_server:start_link/3
%% @see start_link/5
%% @see start/4
%% @end
start_link(Module, Setup, Args, Options) ->
    gen_server:start_link(?MODULE, {Module, Setup, Args}, Options).


%% @spec start_link(ServerName, Module, Setup, Args, Options) -> Result
%%    ServerName = {local, Name} | {global, Name}
%%    Name       = atom()
%%    Module     = atom()
%%    Setup      = #smsc_setup()
%%    Result     = {ok, Pid} | ignore | {error, Error}
%%    Pid        = pid()
%%    Error      = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% <p><tt>ServerName</tt>, <tt>Module</tt>, <tt>Args</tt> and <tt>Options</tt>
%% have the exact same meaning as in gen_server behavior.</p>
%%
%% <p><tt>Setup</tt> is a <tt>smsc_setup</tt> record as declared in 
%% <a href="oserl.html">oserl.hrl</a>.  You may use the macros
%% <tt>DEFAULT_SMSC_SETUP()</tt> or <tt>SMSC_SETUP(SystemId, Password)</tt> to
%% define this parameter if default SMPP timers are OK for you.</p>
%%
%% @see gen_server:start_link/4
%% @see start_link/4
%% @see start/5
%% @end
start_link(ServerName, Module, Setup, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, {Module, Setup, Args}, Options).


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
%% @equiv listen(ServerRef, DEFAULT_SMPP_PORT)
%% @end 
listen(ServerRef) -> 
	listen(ServerRef, ?DEFAULT_SMPP_PORT).


%% @spec listen(ServerRef, Port) -> ok | {error, Reason}
%%    ServerRef = Name | {Name, Node} | {global, Name} | pid()
%%    Name = atom()
%%    Node = atom()
%%    Port = int()
%%    Reason = term()
%%
%% @doc Puts the SMSC Server <tt>ServerRef</tt> to listen on <tt>Port</tt>
%%
%% <p>Returns <tt>true</tt> if success, <tt>false</tt> otherwise.</p>
%% @end 
listen(ServerRef, Port) ->
	gen_server:call(ServerRef, {listen, Port}).


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


%% @spec stop() -> ok
%%
%% @doc Stops the server.
%%
%% @see handle_call/3
%%
%% @equiv gen_server:call(ServerRef, die, 10000).
%% @end
stop(ServerRef) ->
    gen_server:call(ServerRef, die, 10000).

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
init({Mod, #smsc_setup{system_id = I, password = P, timers = T}, Args}) ->
	S = #state{system_id = I, password = P, timers = T},
	case Mod:init(Args) of
		{ok, ModState} ->
			{ok, S#state{mod = Mod, mod_state = ModState}};
		{ok, ModState, Timeout} ->
			{ok, S#state{mod = Mod, mod_state = ModState}, Timeout};
		Other ->
			Other
	end.


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
% Peer ESME SMPP requests 
handle_call({Request, Args}, From, S) when Request == handle_operation;
										   Request == handle_bind;
										   Request == handle_unbind ->
    Self = self(),
    spawn_link(fun() -> 
					   apply_callback(Self, From, S#state.mod, Request, Args) 
			   end),
    {noreply, State};

% Internal requests
handle_call({listen, Port}, From, S) ->
	case gen_tcp_connection(?MODULE, Port, infinity) of
		{ok, Listener} ->
			{reply, ok, S#state{listener = Listener}};
		Error ->
			{reply, Error, S}
	end;

% % Asynchronous replies (Update Mod State).
% handle_call({reply, Reply, NewMS}, From, S) ->
%     {reply, Reply, S#state{mod_state = NewMS}};
% handle_call({reply, Reply, NewMS, Timeout}, From, S) ->
%     {reply, Reply, S#state{mod_state = NewMS}, Timeout};
% handle_call({noreply, NewMS}, From, S) ->
%     {noreply, S#state{mod_state = NewMS}};
% handle_call({noreply, NewMS, Timeout}, From, S) ->
%     {noreply, S#state{mod_state = NewMS}, Timeout};
% handle_call({stop, Reason, Reply, NewMS}, From, S) ->
%     {stop, Reason, Reply, S#state{mod_state = NewMS}};
% handle_call({stop, Reason, NewMS}, From, S) ->
%     {stop, Reason,  S#state{mod_state = NewMS}};

% Callback SMSC custom calls
handle_call({call, Request}, From, S) ->
    case (S#state.mod):handle_call(Request, From, S#state.mod_state) of
        {reply, Reply, NewMS} ->
            {reply, Reply, S#state{mod_state = NewMS}};
        {reply, Reply, NewMS, Timeout} ->
            {reply, Reply, S#state{mod_state = NewMS}, Timeout};
        {noreply, NewMS} ->
            {noreply, S#state{mod_state = NewMS}};
        {noreply, NewMS, Timeout} ->
            {noreply, S#state{mod_state = NewMS}, Timeout};
        {stop, Reason, Reply, NewMS} ->
            {stop, Reason, Reply, S#state{mod_state = NewMS}};
        {stop, Reason, NewMS} ->
            {stop, Reason,  S#state{mod_state = NewMS}}
    end.

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
% Asynchronous replies (Update Mod State)
handle_cast({noreply, NewMS}, From, S) ->
    {noreply, S#state{mod_state = NewMS}};
handle_cast({noreply, NewMS, Timeout}, From, S) ->
    {noreply, S#state{mod_state = NewMS}, Timeout};
handle_cast({stop, Reason, NewMS}, From, S) ->
    {stop, Reason,  S#state{mod_state = NewMS}};

handle_cast({cast, Request}, S) ->
    case (S#state.mod):handle_cast(Request, S#state.mod_state) of
        {noreply, NewMS} ->
            {noreply, S#state{mod_state = NewMS}};
        {noreply, NewMS, Timeout} ->
            {noreply, S#state{mod_state = NewMS}, Timeout};
        {stop, Reason, NewMS} ->
            {stop, Reason,  S#state{mod_state = NewMS}}
    end;

handle_cast({mod_state, NewMs}, S) ->
    {noreply, S#state{mod_state = NewMS}}.


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
handle_info(Info, S) ->
    case (S#state.mod):handle_info(Info, S#state.mod_state) of
        {noreply, NewMS} ->
            {noreply, S#state{mod_state = NewMS}};
        {noreply, NewMS, Timeout} ->
            {noreply, S#state{mod_state = NewMS}, Timeout};
        {stop, Reason, NewMS} ->
            {stop, Reason, S#state{mod_state = NewMS}}
    end.


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc Shutdown the server.
%%
%% <p>Return value is ignored by <tt>gen_server</tt>.</p>
%% @end
terminate(Reason, S) ->
    (S#state.mod):terminate(Reason, S#state.mod_state).


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc Convert process state when code is changed
%% @end
code_change(OldVsn, #state{mod=M, mod_state=S} = State, Extra) ->
    {ok, NewS} = M:code_change(OldVsn, S, Extra),
    {ok, State#state{mod_state = NewS}};
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%%%===================================================================
%%% SMSC Session functions
%%%===================================================================
%% @spec handle_bind(SMSC, Session, Bind) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Bind = {bind_receiver, Pdu} |
%%           {bind_transmitter, Pdu} |
%%           {bind_transceiver, Pdu}
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#handle_bind-3">gen_smsc_session - 
%% handle_bind/3</a> callback implementation.
%% @end
handle_bind(SMSC, Session, Bind) ->
    gen_server:call(SMSC, {handle_bind, [Session, Bind]}, infinity).


%% @spec handle_operation(SMSC, Session, Operation) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Operation = {broadcast_sm, Pdu} |
%%                {cancel_broadcast_sm, Pdu} |
%%                {cancel_sm, Pdu} |
%%                {query_broadcast_sm, Pdu} |
%%                {query_sm, Pdu} |
%%                {replace_sm, Pdu} |
%%                {submit_multi, Pdu} |
%%                {submit_sm, Pdu} |
%%                {data_sm, Pdu}
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#handle_operation-3">gen_smsc_session - 
%% handle_operation/3</a> callback implementation.
%% @end
handle_operation(SMSC, Session, Operation) ->
    gen_server:call(SMSC, {handle_operation, [Session, Operation]}, infinity).


%% @spec handle_unbind(SMSC, Session, Unbind) -> ok | {error, Error}
%%    SMSC = pid()
%%    Unbind = {unbind, Pdu}
%%    Pdu = pdu()
%%    Session = pid()
%%    Error = int()
%%
%% @doc <a href="gen_smsc_session.html#handle_unbind-3">gen_smsc_session - 
%% handle_unbind/3</a> callback implementation.
%% @end
handle_unbind(SMSC, Session, Unbind) ->
    gen_server:call(SMSC, {handle_unbind, [Session, Unbind]}, infinity).


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
handle_accept(Owner, Conn, _Socket) -> 
	% Start a new session.
	case gen_smsc_session:start_link(?MODULE) of
		{ok, Session} ->
			gen_server:cast(Owner, {register_session, Session}),
			{ok, Session};
		_Error ->
			error
	end.


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

% apply_sync_callback(Pid, From, Mod, Callback, Args) ->
%     Reply = gen_server:call(Pid, apply(Mod, Callback, Args)),
%     gen_server:reply(From, Reply).

% apply_async_callback(Pid, Mod, Callback, Args) ->
%     gen_server:cast(Pid, apply(Mod, Callback, Args)).

apply_callback(Pid, From, Mod, Callback, Args) ->
    case apply(Mod, Callback, Args) of
        {reply, Reply, NewMS} ->
            gen_server:cast(Pid, {noreply, NewMS}),
            gen_server:reply(From, Reply);
        {reply, Reply, NewMS, Timeout} ->
            gen_server:cast(Pid, {noreply, NewMS, Timeout}),
            gen_server:reply(From, Reply);
        {stop, Reason, Reply, NewMS} ->
            gen_server:reply(From, Reply), % reply first
            gen_server:cast(Pid, {stop, Reason, NewMS});
        WithoutReply ->
            gen_server:cast(Pid, WithoutReply)
    end.
