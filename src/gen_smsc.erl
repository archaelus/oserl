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
-export([start/3,
         start/4,
         start_link/3,
         start_link/4, 
         alert/3,
         call/2, 
         call/3, 
         cast/2, 
         request/3,
         response/3,
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
%-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state}
%%
%% %@doc Representation of the server's state
%%
%% <dl>
%%   <dt>access: </dt>public | private<dd>
%%   </dd>
%% </dl>
%% %@end
-record(state, {mod, mod_state}).

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
      {bind_receiver, 3}, 
      {bind_transmitter, 3}, 
      {bind_transceiver, 3}, 
      {broadcast_sm, 3},
      {cancel_broadcast_sm, 3},
      {cancel_sm, 3},
      {query_broadcast_sm, 3},
      {query_sm, 3},
      {replace_sm, 3},
      {submit_multi, 3},
      {deliver_sm, 3},
      {data_sm, 3},
      {unbind, 2},
      {handle_session_failure, 1},
      {handle_call, 3},
      {handle_cast, 2},
      {handle_info, 2},
      {terminate, 2},
      {code_change, 3}];
behaviour_info(_Other) ->
    undefined.



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
start(Module, Args, Options) ->
    gen_server:start(?MODULE, {Module, Args}, Options).

start(ServerName, Module, Args, Options) ->
    gen_server:start(ServerName, ?MODULE, {Module, Args}, Options).

start_link(Module, Args, Options) ->
    gen_server:start_link(?MODULE, {Module, Args}, Options).

start_link(ServerName, Module, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, {Module, Args}, Options).

alert(ServerRef, User, Data) ->
    gen_server:cast(ServerRef, {alert, User, Data}).




call(SMSC, Request) ->
    gen_server:call(SMSC, {call, Request}).

call(SMSC, Request, Timeout) ->
    gen_server:call(SMSC, {call, Request}, Timeout).
    
cast(SMSC, Request) ->
    gen_server:cast(SMSC, {cast, Request}).




request(ServerRef, User, Data) ->
    gen_server:cast(ServerRef, {request, User, Data}).

response(ServerRef, User, Data) ->
    gen_server:cast(ServerRef, {response, User, Data}).

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
init({Mod, Args}) ->
    case Mod:init(Args) of
        {ok, ModState} ->
            {ok, #state{mod = Mod, mod_state = ModState}};
        {ok, ModState, Timeout} ->
            {ok, #state{mod = Mod, mod_state = ModState}, Timeout};
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
handle_call({Id, Session, Pdu}, From, S) when Id == bind_receiver;
                                              Id == bind_transmitter;
                                              Id == bind_transceiver;
                                              Id == broadcast_sm;
                                              Id == cancel_broadcast_sm;
                                              Id == cancel_sm;
                                              Id == query_broadcast_sm;
                                              Id == query_sm;
                                              Id == replace_sm;
                                              Id == submit_multi;
                                              Id == deliver_sm;
                                              Id == data_sm ->
    Self = self(),
    Args = [Self, Session, Pdu],
    spawn_link(fun() -> apply_callback(Self, From, S#state.mod, Id, Args) end),
    {noreply, State};
handle_call({unbind, Session}, From, S) ->
    (S#state.mod):unbind(self(), Session);

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
handle_cast({request, User, Data}, S) ->
    R = (S#state.mod):handle_parse_request(Data),
    case account_mgr:lookup(User) of
        {ok, enabled} -> 
            spawn_link(fun() -> subscriber_broker(self(), User, R, S) end);
        _Otherwise -> 
            spawn_link(fun() -> visitor_broker(self(), User, R, S) end)
    end,
    {noreply, S};
handle_cast({response, User, Data}, S) ->
    amusement:send_sms(User, Data),
    {noreply, S};
handle_cast({alert, User, Data}, S) ->
    amusement:send_sms(User, Data),
    {noreply, S};

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
%% @spec bind_receiver(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
bind_receiver(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {bind_receiver, Session, Pdu}, infinity).


%% @spec bind_transmitter(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
bind_transmitter(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {bind_transmitter, Session, Pdu}, infinity).


%% @spec bind_transceiver(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
bind_transceiver(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {bind_transceiver, Session, Pdu}, infinity).


%% @spec broadcast_sm(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
broadcast_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {broadcast_sm, Session, Pdu}, infinity).


%% @spec cancel_broadcast_sm(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
cancel_broadcast_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {cancel_broadcast_sm, Session, Pdu}, infinity).


%% @spec cancel_sm(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
cancel_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {cancel_sm, Session, Pdu}, infinity).


%% @spec query_broadcast_sm(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
query_broadcast_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {query_broadcast_sm, Session, Pdu}, infinity).


%% @spec query_sm(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
query_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {query_sm, Session, Pdu}, infinity).


%% @spec replace_sm(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
replace_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {replace_sm, Session, Pdu}, infinity).


%% @spec submit_multi(SMSC, Session, Pdu) -> Result
%%    SMSC = pid()
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
submit_multi(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {submit_multi, Session, Pdu}, infinity).


%% @spec deliver_sm(SMSC, Session, Pdu) -> Result
%%    SMCS       = pid()
%%    Session    = pid()
%%    Pdu        = pdu()
%%    Result     = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
deliver_sm(SMSC, Session, Pdu) -> 
    gen_server:call(SMSC, {deliver_sm, Session, Pdu}, infinity).


%% @spec data_sm(SMSC, Session, Pdu) -> Result
%%    SMSC       = pid()
%%    Session    = pid()
%%    Pdu        = pdu()
%%    Result     = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
data_sm(SMSC, Session, Pdu) ->
    gen_server:call(SMSC, {data_sm, Session, Pdu}, infinity).


%% @spec unbind(SMSC, Session) -> ok | {error, Error}
%%    SMSC = pid()
%%    Session = pid()
%%    Error = int()
%%
%% @doc <a href="gen_smsc_session.html#outbind-3">gen_smsc_session - 
%% /3</a> callback implementation.
%% @end
unbind(SMSC, Session) ->
    gen_server:call(SMSC, {unbind, Session}, infinity).


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



%% @spec 
%%
%% @doc 
%%
%% % @see
%%
%% % @equiv
%% @end 
subscriber_broker(Parent, User, Req, #state{mod=M, mod_state=S}) ->
    {noreply, NewS} = case M:handle_price_request(Req) of
                          free ->
                              M:handle_subscriber_request(Req, User, S);
                          {Comment, Price} ->
                              case account_mgr:withdraw(User,Price,Comment) of
                                  ok ->
                                      M:handle_subscriber_request(Req,User,S);
                                  Error ->
                                      M:handle_error(Error, User, S)
                              end
                      end,
    % Need a cast to update service's state from broker process.
    % @see handle_cast({mod_state, ...}...) above 
    cast(Parent, {mod_state, NewS}).


%% @spec 
%%
%% @doc 
%%
%% % @see
%%
%% % @equiv
%% @end 
visitor_broker(Parent, User, Req, #state{mod=M, mod_state=S}) ->
    {noreply, NewS} = M:handle_visitor_request(Req, User, S),
    % Need a cast to update service's state from broker process.
    % @see handle_cast({mod_state, ...}...) above 
    cast(Parent, {mod_state, NewS}).
