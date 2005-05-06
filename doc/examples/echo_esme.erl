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

%%% @doc Sample echo ESME.
%%%
%%% <p>This ESME echoes SMs back to the caller.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {23 Jun 2004} {@time}.
%%% @end
-module(echo_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start_link/0, stop/0]).

%%%-------------------------------------------------------------------
%%% Internal ESME exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_outbind/3,
         handle_alert_notification/2,
         handle_enquire_link_failure/2,
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
-define(SMSC_ADDRESS, {127, 0, 0, 1}).
-define(SYSTEM_ID, atom_to_list(?MODULE)).
-define(PASSWORD, "secret").
-define(SOURCE_ADDR, "1948").

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, TrxSession}
%%
%% %@doc Representation of the ESME server state.
%%
%% <dl>
%%   <dt>TrxSession: </dt><dd>Pid of the transceiver session.</dd>
%% </dl>
%% %@end
-record(state, {trx_session}).


%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start_link() -> Result
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the ESME server.
%%
%% @see gen_esme
%% @see start/0
%% @end
start_link() ->
    gen_esme:start_link({local, ?SERVER}, ?MODULE, [], []).


%% @spec stop() -> ok
%%
%% @doc Stops the ESME server.
%%
%% @see handle_call/3
%%
%% @equiv gen_esme:call(SERVER, die, 10000)
%% @end
stop() ->
    gen_esme:call(?SERVER, die, 10000).

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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#init-1">
%% gen_esme - init/1</a> callback implementation.
%% 
%% <p>Initiates the server.</p>
%% @end
init([]) ->
    gen_esme:open_disk_log([]),
    gen_esme:open_error_logger([]),
    case gen_esme:session_start(?SMSC_ADDRESS, ?DEFAULT_SMPP_PORT) of
        {ok, Trx} ->
            erlang:monitor(process, Trx),
            ParamList = [{system_id, ?SYSTEM_ID},
                         {password, ?PASSWORD},
                         {source_addr, ?SOURCE_ADDR}],
            case gen_esme:bind_transceiver(Trx, ParamList) of
                {ok, _PduResp} ->
                    {ok, #state{trx_session = Trx}};
                BindError ->
                    {stop, BindError}
            end;
        SessionError ->
            {stop, SessionError}
    end.


%% @spec handle_outbind(Outbind, From, State) -> Result
%%    OutBind = {outbind, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_outbind-3">gen_esme - handle_outbind/3</a> callback implementation.
%%
%% <p>Handle <i>oubind</i> requests from the peer SMSC.</p>
%% @end
handle_outbind({outbind, _Session, _Pdu}, _From, State) ->
    {noreply, State}.


%% @spec handle_alert_notification(AlertNotification, State) -> Result
%%    AlertNotification = {alert_notification, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_alert_notification-2">gen_esme - handle_alert_notification/2</a> callback implementation.
%%
%% <p>Handle <i>alert_notification</i> requests from the peer SMSC.</p>
%% @end
handle_alert_notification({alert_notification, _Session, _Pdu}, State) -> 
    {noreply, State}.

%% @spec handle_enquire_link_failure(EnquireLinkFailure, State) -> Result
%%    EnquireLinkFailure = {enquire_link_failure, Session, CommandStatus}
%%    Session = pid()
%%    CommandStatus = int()
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    Timeout = int()
%%    NewState = term()
%%    Reason = term()
%%
%% @doc Notifies when an <i>enquire_link</i> failure occurs (i.e. the SMSC did
%% not respond to our <i>enquire_link</i> operation).
%% @end
handle_enquire_link_failure({enquire_link_failure,_Session,_Status}, State) ->
    {stop, enquire_link_failure, State}.


%% @spec handle_operation(Operation, From, State) -> Result
%%    Operation = {deliver_sm, Session, Pdu} | {data_sm, Session, Pdu}
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_operation-3">gen_esme - handle_operation/3</a> callback implementation.
%%
%% <p>Handle <i>deliver_sm</i> and <i>data_sm</i> operations (from the peer
%% SMSCs) to the callback ESME.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the ESME in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_operation({deliver_sm, _Session, Pdu}, _From, S) ->
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    report:info(?MODULE, echo_deliver_sm, [Mesg|Dest]),
    spawn(fun() -> gen_esme:submit_sm(S#state.trx_session, [Mesg|Dest]) end),
    {reply, {ok, []}, S};
handle_operation({data_sm, _Session, Pdu}, _From, S) ->
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    report:info(?MODULE, echo_data_sm, [Mesg|Dest]),
    spawn(fun() -> gen_esme:data_sm(S#state.trx_session, [Mesg|Dest]) end),
    {reply, {ok, []}, S};
handle_operation({_CmdName, _Session, Pdu}, _From, S) ->
    % Don't know how to handle CmdName
    report:info(?MODULE, cannot_handle_operation, operation:to_list(Pdu)),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_unbind-3">gen_esme - handle_unbind/3</a> callback implementation.
%%
%% <p>Handle <i>unbind</i> requests from the peer SMSC.</p>
%%
%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%% command_status is sent to the MC and the session moves into the unbound
%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%% command_status and the session will remain on it's current bound state
%% (bound_rx, bound_tx or bound_trx).</p>
%% @end
handle_unbind({unbind, Trx, _Pdu}, _From, #state{trx_session = Trx} = S) -> 
    {stop, normal, S#state{trx_session = undefined}}.


%% @spec handle_listen_error(State) -> Result
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_listen_error-1">gen_esme - handle_listen_error/1</a> callback implementation.
%%
%% <p>Handle listen failures.</p>
%% @end
handle_listen_error(State) ->
    {stop, {error, listen_error}, State}.


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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_call-3">gen_esme - handle_call/3</a> callback implementation.
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_cast-2"> gen_esme - handle_cast/2</a> callback implementation.
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_info-2"> gen_esme - handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info({'DOWN', _, process, Trx, Info}, #state{trx_session = Trx} = S) ->
    {stop, Info, S};
handle_info(_Info, S) ->
    {noreply, S}.


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#terminate-2">
%% gen_esme - terminate/2</a> callback implementation.
%%
%% <p>Shutdown the ESME server.</p>
%%
%% <p>Return value is ignored by <tt>gen_esme</tt>.</p>
%% @end
terminate(kill, _S) ->
    ok;
terminate(_Reason, S) ->
    catch gen_smsc:unbind(S#state.trx_session),
    catch gen_smsc:session_stop(S#state.trx_session).


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#code_change-2"> gen_esme - code_change/2</a> callback implementation.
%%
%% <p>Convert process state when code is changed.</p>
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

