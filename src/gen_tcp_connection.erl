%%% Copyright (C) 2004 Enrique Marcote Peña <quique@argos>
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

%%% @doc Generic TCP connection.
%%%
%%% <p></p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <quique@argos>
%%%         [http://]
%%% @version , {24 May 2004} {@time}.
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
		 start_listen/2,
		 start_listen/3,
		 stop/0]).

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
-define(LISTEN_OPTIONS, 
        [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(CONNECT_OPTIONS, 
        [binary, {packet, 0}, {active, false}]).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, Parent, Mod, Buffer}
%%    Parent = pid()
%%    Mod    = atom()
%%    Buffer = binary()
%%
%% %@doc Representation of the fsm's state
%%
%% <dl>
%%   <dt>Parent: </dt><dd>Pid of the parent process.  Is passed in the
%%     callback functions to help identify the owner of the connection.
%%   </dd>
%%   <dt>Mod: </dt><dd>Callback module.</dd>
%%   <dt>Buffer: </dt><dd>Received data is stored in this buffer.  This buffer
%%     is passed to the callback module via the handle_input/4 function every
%%     time new input is received.  The callback module may consume all or
%%     part of the buffer.
%%   </dd>
%% </dl>
%% %@end
-record(state, {parent, mod, buffer = <<>>}).

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
    [{handle_accept, 2}, 
     {handle_input, 4}, 
     {handle_listen_error, 3}, 
     {handle_connect_error, 4}];
behaviour_info(_Other) ->
    undefined.


%% @spec start_connect() -> Result
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
	Request = {connect, Address, Port},
    gen_server:start_link(?MODULE, [self(), Module, Request], []).


%% @spec start_connect() -> Result
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
	Request = {connect, Address, Port},
    gen_server:start_link(CName, ?MODULE, [self(), Module, Request], []).


%% @spec send(Cid, Output) -> ok | {error, Reason}
%%    Cid    = pid()
%%    Output = binary()
%%    Reason = term()
%%
%% @doc Sends a binary <tt>Output</tt> through the connection socket.
%%
%% @see gen_server:call/3
%% @end
send(Cid, Output) ->
    gen_server:call(Cid, {send, Output}, infinity).


%% @spec stop() -> ok
%%
%% @doc Stops the server.
%%
%% @see handle_call/3
%%
%% @equiv gen_server:call(?SERVER, die, 10000).
%% @end
stop(Cid) ->
	gen_server:call(Cid, die, 10000).

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
init([[Pid, Module, {connect, Address, Port}]) ->
    case gen_tcp:connect(Address, Port, ?CONNECT_OPTIONS, ?CONNECT_TIME) of 
        {ok, Socket} ->
			Self = self(),
			process_flag(trap_exit, true),
            spawn_link(fun() -> wait_recv(Self, Socket) end),
			{ok, #state{parent = Pid, mod = Module}};
        Error ->
            {stop, Error}
    end;
init([[Pid, Module, {listen, Port, Count}]) ->
    case gen_tcp:listen(Port, ?LISTEN_OPTIONS) of
        {ok, LSocket} ->
			Self = self(),
			process_flag(trap_exit, true),
            spawn_link(fun() -> wait_accept(Self, LSocket, Count) end),
			{ok, #state{parent = Pid, mod = Module}};
        Error ->
            {stop, Error}
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
handle_call(Request, From, State) ->
	Reply = ok,
	{reply, Reply, State};
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
handle_cast({accept, Socket}, State) ->
	{noreply, State};
handle_cast({recv, Socket, Input, Lapse}, State) ->
    Buf = concat_binary([StateData#state.buffer, Input]),
    case catch Mod:handle_input(Pid, Cid, Buf, Lapse) of
        {ok, RestBuffer} when binary(RestBuffer) ->
            RestBuffer;
        _Otherwise ->
            <<>>
    end.

	{noreply, State}.

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
%% @spec wait_accept(Cid, LSocket) -> void()
%%    Cid     = pid()
%%    LSocket = socket()
%%
%% @doc Waits until a connection is requested on <tt>LSocket</tt> or an
%% error occurs.  If successful the event <i>accept</i> is triggered, on
%% error a failure on the listen socket is reported.
%% @end
wait_accept(Cid, LSocket, Count) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            inet:setopts(Socket, ?CONNECT_OPTIONS),
            gen_tcp:controlling_process(Socket, Cid),
			gen_server:cast(Cid, {accept, Socket}),
			if
				Count == infinity -> 
					wait_accept(Cid, LSocket, Count);
				Count >  0 -> 
					wait_accept(Cid, LSocket, Count - 1);
				true -> 
					gen_tcp:close(LSocket)
			end;
        Error ->
			exit({fail_accept, LSocket, Error})
    end.


%% @spec wait_recv(Cid, Socket) -> void()
%%    Cid    = pid()
%%    Socket = socket()
%%
%% @doc Waits until new data is received on <tt>Socket</tt> and starts a 
%% receive loop to get bulk input.  All data received on the same loop triggers
%% the event <i>recv</i> with the same timestamp (with a 0 time lapse).
%%
%% <p>If the <tt>Socket</tt> is closed a failure is reported.</p>
%% @end
wait_recv(Cid, Socket) ->
    Timestamp = now(),
    case gen_tcp:recv(Socket, 0) of
        {ok, Input} ->
			Lapse = my_calendar:time_since(Timestamp),
			gen_server:cast(Cid, {recv, Socket, Input, Lapse}),
            recv_loop(Cid, Socket);
        Error ->
			exit({fail_recv, Socket, Error})
    end.

%% @doc Auxiliary function for wait_recv/2
%%
%% @see wait_recv/2
%% @end
recv_loop(Cid, Socket) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Input} ->                    % Some input waiting already 
			gen_server:cast(Cid, {recv, Socket, Input, 0}),
            recv_loop(Cid, Socket);
        {error, timeout} ->               % No data inmediately available
            wait_recv(Cid, Socket);
        Error ->
			exit({fail_recv, Socket, Error})
    end.
