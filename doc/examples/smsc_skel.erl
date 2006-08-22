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

%%% @doc SMSC Skeleton.
%%%
%%% <p>You may use this skeleton as the starting point to implement your
%%% own SMSCs.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, { 6 Jul 2004} {@time}.
%%% @end
-module(smsc_skel).

-behaviour(gen_smsc).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start_link/0]).

%%%-------------------------------------------------------------------
%%% Internal SMSC exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_bind/3,
         handle_operation/3,
         handle_unbind/3,
         handle_listen_error/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SMPP_PORT, ?DEFAULT_SMPP_PORT).
-define(SYSTEM_ID, atom_to_list(?MODULE)).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state}
%%
%% %@doc Representation of the SMSC server state.
%%
%% <dl>
%%   <dt>: </dt><dd>
%%   </dd>
%% </dl>
%% %@end
-record(state, {}).

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start_link() -> Result
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% @see gen_smsc
%% @see start/0
%% @end
start_link() ->
    gen_smsc:start_link({local, ?SERVER}, ?MODULE, [], []).

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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#init-1">
%% gen_smsc - init/1</a> callback implementation.
%% 
%% <p>Initiates the server.</p>
%% @end
init([]) ->
    % You may start sessions and issue bind requests here.
    {ok, #state{}}.

%% @spec handle_bind(Bind, From, State) -> Result
%%    Bind = {CmdName, Session, Pdu, IpAddr}
%%    CmdName = bind_receiver | bind_transmitter | bind_transceiver
%%    Session = pid()
%%    Pdu = pdu()
%%    IpAddr = {int(), int(), int(), int()}
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
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_bind-3">gen_smsc - handle_bind/3</a> callback implementation.
%%
%% <p>Handle <i>bind</i> requests from the peer ESME.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the bind response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the SMSC in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_bind({_CmdName, _Session, _Pdu, _IpAddr}, _From, State) ->
    ParamList = [{system_id, ?SYSTEM_ID}],
    {reply, {ok, ParamList}, State}.

%% @spec handle_operation(Operation, From, State) -> Result
%%    Operation = {CmdName, Session, Pdu}
%%    CmdName = broadcast_sm        |
%%              cancel_broadcast_sm |
%%              cancel_sm           |
%%              query_broadcast_sm  |
%%              query_sm            |
%%              replace_sm          |
%%              submit_multi        |
%%              submit_sm           |
%%              data_sm
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
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_operation-3">gen_smsc - handle_operation/3</a> callback implementation.
%%
%% <p>Handle <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%% <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%% <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%% <i>data_sm</i> operations from peer ESMEs.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the SMSC in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_operation({_CmdName, _Session, _Pdu}, _From, S) ->
    % Don't know how to handle CmdName
    {reply, {error, ?ESME_RINVCMDID, []}, S}.

%% @spec handle_unbind(Unbind, From, State) -> Result
%%    Unbind = {unbind, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = ok | {error, Error}
%%    Error = int()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_unbind-3">gen_smsc - handle_unbind/3</a> callback implementation.
%%
%% <p>Handle <i>unbind</i> requests from the peer ESMEs.</p>
%%
%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%% command_status is sent to the MC and the session moves into the unbound
%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%% command_status and the session will remain on it's current bound state
%% (bound_rx, bound_tx or bound_trx).</p>
%% @end
handle_unbind({unbind, _Session, _Pdu}, _From, State) -> 
    {reply, ok, State}.

%% @spec handle_listen_error(State) -> Result
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_listen_error-1">gen_smsc - handle_listen_error/1</a> callback implementation.
%%
%% <p>Handle listen failures.</p>
%% @end
handle_listen_error(State) ->
    {noreply, State}.

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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_call-3">gen_smsc - handle_call/3</a> callback implementation.
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% @spec handle_cast(Request, State) -> Result
%%    Request  = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_cast-2"> gen_smsc - handle_cast/2</a> callback implementation.
%%
%% <p>Handling cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_cast(_Request, State) ->
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_info-2"> gen_smsc - handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#terminate-2">
%% gen_smsc - terminate/2</a> callback implementation.
%%
%% <p>Shutdown the SMSC server.</p>
%%
%% <p>Return value is ignored by <tt>gen_smsc</tt>.</p>
%% @end
terminate(_Reason, _State) ->
    % You may stop sessions and issue unbind requests here.
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#code_change-2"> gen_smsc - code_change/2</a> callback implementation.
%%
%% <p>Convert process state when code is changed.</p>
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
