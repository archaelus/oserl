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

%%% @doc Generic TCP connection.
%%%
%%% <p></p>
%%%
%%%
%%% <h2>Callback Function Index</h2>
%%%
%%% <p>A module implementing this behaviour must export these functions.</p>
%%%
%%% <table width="100%" border="1">
%%%   <tr>
%%%     <td valign="top"><a href="#handle_accept-3">handle_accept/3</a></td>
%%%     <td>A new incomming connection is available.</td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_input-4">handle_input/4</a></td>
%%%     <td>New input data available.</td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%%
%%% <h3><a name="handle_accept-3">handle_accept/3</a></h3>
%%%
%%% <tt>handle_accept(Owner, Conn, Socket) -> true | false</tt>
%%% <ul>
%%%   <li><tt>Owner = Conn = pid()</tt></li>
%%%   <li><tt>Socket = socket()</tt></li>
%%% </ul>
%%%
%%% <p>A new incomming connection is available.  <tt>Conn</tt> is the pid of 
%%% the gen_connection, <tt>Owner</tt> the pid of the controlling process.</p>
%%%
%%% <p>The callback must return <tt>true</tt> if the connection is accepted,
%%% <tt>false</tt> otherwise.</p>
%%%
%%%
%%% <h3><a name="handle_input-4">handle_input/4</a></h3>
%%%
%%% <tt>handle_input(Owner, Conn, Input, Lapse) -> RestInput</tt>
%%% <ul>
%%%   <li><tt>Owner = Conn = pid()</tt></li>
%%%   <li><tt>Input = RestInput = binary()</tt></li>
%%%   <li><tt>Lapse = int()</tt></li>
%%% </ul>
%%%
%%% <p>New input data available.  The callback module should consume desired
%%% data from <tt>Input</tt> and return unused data.</p>
%%%
%%% <tt>Conn</tt> is the pid of the gen_connection, <tt>Owner</tt> the pid of 
%%% the controlling process.</p>
%%%
%%% <p><tt>Lapse</tt> are the microseconds waited for input to come.  This
%%% parameter may be used to control congestion.</p>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 1.0 beta, {24 May 2004} {@time}.
%%% @end
-module(gen_tcp_connection).

-behaviour(gen_server).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Behaviour exports
%%%-------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
%%-compile(export_all).
-export([start_connect/3, 
         start_connect/4,
         start_listen/3,
         start_listen/4,
         controlling_process/2,
         send/2,
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
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(CONNECT_TIME, 30000).
-define(ACCEPT_TIME, 30000).
-define(LISTEN_OPTIONS, 
        [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(CONNECT_OPTIONS, 
        [binary, {packet, 0}, {active, false}]).

-define(DECR(X), if is_integer(X) -> X - 1; true -> X end).
                         

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Owner, Mod, Buffer}
%%    Owner = pid()
%%    Mod    = atom()
%%    Buffer = binary()
%%
%% %@doc Representation of the fsm's state
%%
%% <dl>
%%   <dt>Owner: </dt><dd>Pid of the controlling process.  Is passed in the
%%     callback functions to help identify the owner of the connection.
%%   </dd>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>Socket: </dt><dd>Active socket of the connection.  Either a listen
%%     or a connection socket.
%%   </dd>
%%   <dt>Buffer: </dt><dd>Received data is stored in this buffer.  This buffer
%%     is passed to the callback module via the handle_input/4 function every
%%     time new input is received.  The callback module may consume all or
%%     part of the buffer.
%%   </dd>
%% </dl>
%% %@end
-record(state, {owner, mod, socket, buffer = <<>>}).

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
    [{handle_accept, 3}, {handle_input, 4}];
behaviour_info(_Other) ->
    undefined.


%% @spec start_connect(Module, Address, Port) -> Result
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server opening a connection to <tt>Address:Port</tt>.
%%
%% <p><tt>Module</tt> is the callback module.
%%
%% <p>The gen_connection is not registered.</p>
%%
%% @see gen_server:start_link/3
%% @see start_link/4
%% @end
start_connect(Module, Address, Port) ->
    ConnectMode = {connect, Address, Port},
    gen_server:start_link(?MODULE, [self(), Module, ConnectMode], []).


%% @spec start_connect(CName, Module, Address, Port) -> Result
%%    CName  = {local, Name} | {global, Name}
%%    Name   = atom()
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server opening a connection to <tt>Address:Port</tt>.
%%
%% <p><tt>Module</tt> is the callback module.
%%
%% <p>If <tt>CName = {local, Name}</tt>, the gen_connection is registered
%% locally as <tt>Name</tt>.  If <tt>CName = {global, Name}</tt>, the
%% gen_connection is registered globally as <tt>Name</tt>.</p>
%%
%% @see gen_server:start_link/4
%% @see start_link/3
%% @end
start_connect(CName, Module, Address, Port) ->
    ConnectMode = {connect, Address, Port},
    gen_server:start_link(CName, ?MODULE, [self(), Module, ConnectMode], []).


%% @spec start_listen(Module, Port, Count) -> Result
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server setting up a socket to listen on <tt>Port</tt>.
%%
%% <p><tt>Module</tt> is the callback module.
%%
%% <p>The gen_connection is not registered.</p>
%%
%% @see gen_server:start_link/3
%% @see start_link/4
%% @end
start_listen(Module, Port, Count) ->
    ListenMode = {listen, Port, Count},
    gen_server:start_link(?MODULE, [self(), Module, ListenMode], []).


%% @spec start_listen(CName, Module, Port, Count) -> Result
%%    CName  = {local, Name} | {global, Name}
%%    Name   = atom()
%%    Module = atom()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server setting up a socket to listen on <tt>Port</tt>.
%%
%% <p><tt>Module</tt> is the callback module.
%%
%% <p>If <tt>CName = {local, Name}</tt>, the gen_connection is registered
%% locally as <tt>Name</tt>.  If <tt>CName = {global, Name}</tt>, the
%% gen_connection is registered globally as <tt>Name</tt>.</p>
%%
%% @see gen_server:start_link/4
%% @see start_link/3
%% @end
start_listen(CName, Module, Port, Count) ->
    ListenMode = {listen, Port, Count},
    gen_server:start_link(CName, ?MODULE, [self(), Module, ListenMode], []).


%% @spec controlling_process(Conn, NewOwner) -> ok | {error, eperm}
%%    Conn = pid()
%%    NewOwner = pid()
%%
%% @doc Assigns a new controlling process  to  Socket.  The controlling 
%% process is the process which will receive callbacks from the connection. 
%% If called by any other process than the current owner {error, eperm}
%% will be returned.
%% @end 
controlling_process(Conn, NewOwner) ->
    gen_server:call(Conn, {owner, NewOwner}, infinity).


%% @spec send(Conn, Output) -> ok | {error, Reason}
%%    Conn   = pid()
%%    Output = binary()
%%    Reason = term()
%%
%% @doc Sends a binary <tt>Output</tt> through the connection socket.
%%
%% @see gen_server:call/3
%% @end
send(Conn, Output) ->
    gen_server:call(Conn, {send, Output}, infinity).


%% @spec stop() -> ok
%%
%% @doc Stops the server.
%%
%% @see handle_call/3
%%
%% @equiv gen_server:call(?SERVER, die, 10000).
%% @end
stop(Conn) ->
    gen_server:call(Conn, die, 10000).

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
init([Pid, Module, {connect, Address, Port}]) ->
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, ?CONNECT_TIME) of 
        {ok, Socket} ->
            Self = self(),
            process_flag(trap_exit, true),
            spawn_link(fun() -> wait_recv(Self, Socket) end),
            {ok, #state{owner = Pid, socket = Socket, mod = Module}};
        Error ->
            {stop, Error}
    end;
init([Pid, Module, {listen, Port, Count}]) ->
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
            Self = self(),
            process_flag(trap_exit, true),
            spawn_link(fun() -> wait_accept(Self, LSocket, Count) end),
            {ok, #state{owner = Pid, socket = LSocket, mod = Module}};
        Error ->
            {stop, Error}
    end;
init([Pid, Module]) ->
    process_flag(trap_exit, true),
    {ok, #state{owner = Pid, mod = Module}}.


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
handle_call({send, Output}, From, S) ->
    {reply, gen_tcp:send(S#state.socket, Output), S};
handle_call({accept, Socket}, _From, S) ->
    Self = self(),
    case (S#state.mod):handle_accept(S#state.owner, Self, Socket) of
        true ->
            spawn_link(fun() -> wait_recv(Self, Socket) end),
            {reply, {ok, Self}, S#state{socket = Socket}};
        false ->
            {reply, {error, reject}, S}
    end;
handle_call({spawn_accept, Socket}, From, S) ->
    spawn(fun() -> start_accept(Socket, From, S#state.owner, S#state.mod) end),
    {noreply, S};
handle_call({owner, NewOwner}, From, #state{owner = From} = S) ->
    {reply, ok, S#state{owner = NewOwner}};
handle_call({owner, NewOwner}, From, S) ->
    {reply, {error, eperm}, S};
handle_call(die, _From, S) ->
    process_flag(trap_exit, false),
    gen_tcp:close(S#state.socket),
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
handle_cast({recv, Socket, Input, Lapse}, S) ->
    Buf = concat_binary([S#state.buffer, Input]),
    case (S#state.mod):handle_input(S#state.owner, self(), Buf, Lapse) of
        RestBuffer when binary(RestBuffer) ->
            {noreply, S#state{buffer = RestBuffer}};
        _Otherwise ->
            {noreply, S#state{buffer = <<>>}}
    end.


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
handle_info({'EXIT', _AcceptLoop, {accept_error, Reason}}, State) ->
    % A wait_accept loop exits on error
    {stop, Reason, State};
handle_info({'EXIT', _RecvLoop, {recv_error, Reason}}, State) ->
    % A wait_recv loop exits on error
    {stop, Reason, State};
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
terminate(Reason, #state{buffer = <<>>} = S) ->
    case process_info(self(), registered_name) of
        {registered_name, Name} ->
            unregister(Name);
        _NotRegistered ->
            ok
    end;
terminate(Reason, S) ->
    (S#state.mod):handle_input(S#state.owner, self(), S#state.buffer, 0),
    terminate(Reason, S#state{buffer = <<>>}).

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
%% @spec start_accept(Socket, From, Owner, Module) -> Result
%%    Socket = socket()
%%
%% @doc Starts a separate gen_connection with the given <tt>Socket</tt>.
%% @end 
start_accept(Socket, From, Owner, Module) ->
    case gen_server:start_link(?MODULE, [Owner, Module], []) of
        {ok, Pid} ->
            Reply = gen_server:call(Pid, {accept, Socket}),
            gen_server:reply(From, Reply);
        Error ->
            gen_server:reply(From, Error)
    end.


%% @spec wait_accept(Conn, LSocket) -> void()
%%    Conn     = pid()
%%    LSocket = socket()
%%
%% @doc Waits until a connection is requested on <tt>LSocket</tt> or an
%% error occurs.  If successful the event <i>accept</i> is triggered, on
%% error a failure on the listen socket is reported.
%% @end
wait_accept(Conn, LSocket, Count) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} when Count == 1 -> % Handle last Socket in this connection
            inet:setopts(Socket, ?CONNECT_OPTIONS),
            case gen_server:call(Conn, {accept, Socket}, ?ACCEPT_TIME) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid),
                    gen_tcp:close(LSocket);
                {error, reject} ->
                    gen_tcp:close(Socket),
                    wait_accept(Conn, LSocket, Count)
            end;
        {ok, Socket} ->                 % Spawn a new connection to handle it
            inet:setopts(Socket, ?CONNECT_OPTIONS),
            case gen_server:call(Conn, {spawn_accept, Socket}, ?ACCEPT_TIME) of
                {ok, Pid} ->
                    gen_tcp:controlling_process(Socket, Pid),
                    wait_accept(Conn, LSocket, ?DECR(Count));
                {error, reject} ->
                    gen_tcp:close(Socket),
                    wait_accept(Conn, LSocket, Count);
                SpawnError ->
                    gen_tcp:close(Socket),
                    gen_tcp:close(LSocket),
                    exit({accept_error, SpawnError})
            end;
        Error ->
            exit({accept_error, Error})
    end.


%% @spec wait_recv(Conn, Socket) -> void()
%%    Conn   = pid()
%%    Socket = socket()
%%
%% @doc Waits until new data is received on <tt>Socket</tt> and starts a 
%% receive loop to get bulk input.  All data received on the same loop triggers
%% the event <i>recv</i> with the same timestamp (with a 0 time lapse).
%%
%% <p>If the <tt>Socket</tt> is closed a failure is reported.</p>
%% @end
wait_recv(Conn, Socket) ->
    Timestamp = now(),
    case gen_tcp:recv(Socket, 0) of
        {ok, Input} ->
            Lapse = my_calendar:time_since(Timestamp),
            gen_server:cast(Conn, {recv, Socket, Input, Lapse}),
            recv_loop(Conn, Socket);
        Error ->
            exit({recv_error, Error})
    end.

%% @doc Auxiliary function for wait_recv/2
%%
%% @see wait_recv/2
%% @end
recv_loop(Conn, Socket) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Input} ->                    % Some input waiting already 
            gen_server:cast(Conn, {recv, Socket, Input, 0}),
            recv_loop(Conn, Socket);
        {error, timeout} ->               % No data inmediately available
            wait_recv(Conn, Socket);
        Error ->
            exit({recv_error, Error})
    end.
