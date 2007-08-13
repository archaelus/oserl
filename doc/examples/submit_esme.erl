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

%%% @doc Submit SM example.
%%%
%%% <p>A very simple ESME for submitting short messages.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {09 Feb 2004} {@time}.
%%% @end
-module(submit_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start_link/0, submit_sm/2, stop/0]).

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
-define(SMPP_PORT, ?DEFAULT_SMPP_PORT).
-define(SYSTEM_ID, atom_to_list(?MODULE)).
-define(PASSWORD, "secret").
-define(SOURCE_ADDR, "1950").

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, TxSession}
%%
%% %@doc Representation of the server's state
%%
%% <dl>
%%   <dt>TxSession: </dt><dd>Pid of the transmitter session.</dd>
%% </dl>
%% %@end
-record(state, {tx_session}).

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

%% @spec submit_sm(MobileNumber, UserMessage) -> ok
%%     MobileNumber = string()
%%     UserMessage  = string()
%%
%% @doc Submits a Short Message with the text <tt>UserMessage</tt> to
%% <tt>MobileNumber</tt>.
%% @end
submit_sm(MobileNumber, UserMessage) ->
    gen_esme:call(?SERVER, {submit_sm, MobileNumber, UserMessage}, infinity).

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
    case gen_esme:session_start(?SMSC_ADDRESS, ?SMPP_PORT) of
        {ok, Tx} ->
            ParamList = [{system_id, ?SYSTEM_ID},
                         {password, ?PASSWORD},
                         {source_addr, ?SOURCE_ADDR}],
            case gen_esme:bind_transmitter(Tx, ParamList) of
                {ok, _PduResp} ->
                    {ok, #state{tx_session = Tx}};
                BindError ->
                    {stop, BindError}
            end;
        SessionError ->
            SessionError
    end.

%% @spec handle_outbind(Outbind, From, State) -> Result
%%    OutBind = {outbind, Session, Pdu, IpAddr}
%%    Session = pid()
%%    Pdu = pdu()
%%    IpAddr = {int(), int(), int(), int()}
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
handle_outbind({outbind, _Session, _Pdu, _IpAddr}, _From, State) ->
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_alert_notification-3">gen_esme - handle_alert_notification/2</a> callback implementation.
%%
%% <p>Handle <i>alert_notification</i> requests from the peer SMSC.</p>
%% @end
handle_alert_notification({alert_notification, _Session, _Pdu}, State) -> 
    {noreply, State}.

%% @spec handle_enquire_link_failure(EnquireLinkFailure, State) -> Result
%%     EnquireLinkFailure = {enquire_link_failure, Session, CommandStatus}
%%     Session = pid()
%%     CommandStatus = int()
%%     State = term()
%%     Result = {noreply, NewState}               |
%%              {noreply, NewState, Timeout}      |
%%              {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_enquire_link_failure-2">gen_esme - handle_enquire_link_failure/2</a> callback implementation.
%%
%% <p>Notifies when an <i>enquire_link</i> failure occurs (i.e. the SMSC did
%% not respond to our <i>enquire_link</i> operation).</p>
%% @end
handle_enquire_link_failure({enquire_link_failure, _Session, _Status}, State) -> 
    {noreply, State}.

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
handle_operation({CmdName, _Session, _Pdu}, _From, S) ->
    % Don't know how to handle CmdName
    io:format("Don't know how to handle ~p~n", [CmdName]),
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
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_esme.html#handle_listen_error-1">gen_esme - handle_listen_error/1</a> callback implementation.
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
handle_call({submit_sm, MobileNumber, UserMessage}, _From, S) ->
    % <p>This function is a direct translation into erlang from the java code 
    % below.
    % </p>
    %
    % <p>This sample code has too much I/O, which is very similar in both 
    % languajes, that's why resulting code is almost identical.  In other kind
    % of examples, with less I/O, I believe erlang code results much more 
    % compact and readable than java.
    % </p>
    %
    % <pre>
    % public String  submit(String mobileNumber, String userMessage) {
    %     SubmitSM request = new SubmitSM();
    %     SubmitSMResp response;
    %  
    %     // input values
    %     serviceType = getParam("Service type", serviceType);
    %     sourceAddress = getAddress("Source",sourceAddress);
    %     destAddress = getAddress("Destination",new Address(mobileNumber));
    %     replaceIfPresentFlag = getParam("Replace if present flag", 
    %                                      replaceIfPresentFlag);
    %     shortMessage = userMessage;
    %     scheduleDeliveryTime = getParam("Schedule delivery time", 
    %                                     scheduleDeliveryTime);
    %     validityPeriod = getParam("Validity period", validityPeriod);
    %     esmClass = getParam("Esm class", esmClass);
    %     protocolId = 0;
    %     priorityFlag = getParam("Priority flag", priorityFlag);
    %     registeredDelivery = getParam("Registered delivery", 
    %                                    registeredDelivery);
    %     dataCoding = 0x10; // For Flash Message
    %     smDefaultMsgId = getParam("Sm default msg id", smDefaultMsgId);
    %
    %     // set values
    %     request.setServiceType(serviceType);
    %     request.setSourceAddr(sourceAddress);
    %     request.setDestAddr(destAddress);
    %     request.setReplaceIfPresentFlag(replaceIfPresentFlag);
    %     request.setShortMessage(shortMessage);
    %     request.setScheduleDeliveryTime(scheduleDeliveryTime);
    %     request.setValidityPeriod(validityPeriod);
    %     request.setEsmClass(esmClass);
    %     request.setProtocolId(protocolId);
    %     request.setPriorityFlag(priorityFlag);
    %     request.setRegisteredDelivery(registeredDelivery);
    %     request.setDataCoding(dataCoding);
    %     request.setSmDefaultMsgId(smDefaultMsgId);
    %
    %     // send the request
    %     int count = 1;
    %     System.out.println();
    %     count = getParam("How many times to submit this message (load test)",
    %                      count);
    %     for (int i = 0; i&lt;count; i++) {
    %         request.assignSequenceNumber(true);
    %         //System.out.print("#"+i+"  ");
    %         System.out.println("Submit request " + request.debugString());
    %         if (asynchronous) {
    %             //session.submit(request);  
    %             response = session.submit(request);
    %             messageId = response.getMessageId();
    %             //System.out.println();
    %             return messageId;
    %         } else {
    %             response = session.submit(request);                    
    %             messageId = response.getMessageId();
    %             System.out.println("Submit response " + 
    %                                response.debugString() + 
    %                                " MESSAGE ID : " + 
    %                                messageId);
    %             return messageId;
    %         }
    %     }   
    %     return messageId;            
    % }
    % </pre>
    ServiceType = read_string("Service type> "),
    SourceAddress = read_string("Source> "),
    ReplaceIfPresentFlag = read_decimal("Replace if present flag> "),
    ScheduleDeliveryTime = read_string("Schedule delivery time> "),
    ValidityPeriod = read_string("Validity period> "),
    EsmClass = read_decimal("Esm class> "),
    ProtocolId = 0,
    PriorityFlag = read_decimal("Priority flag> "),
    RegisteredDelivery = read_decimal("Registered delivery> "),
    DataCoding = 2#10,
    SmDefaultMsgId = read_decimal("Sm default msg id> "),

    % Create the ParamList (set values)
    ParamList = [{service_type, ServiceType},
                 {source_addr, SourceAddress},
                 {destination_addr, MobileNumber},
                 {replace_if_present_flag, ReplaceIfPresentFlag},
                 {short_message, UserMessage},
                 {schedule_delivery_time, ScheduleDeliveryTime},
                 {validity_period, ValidityPeriod},
                 {esm_class, EsmClass},
                 {protocol_id, ProtocolId},
                 {priority_flag, PriorityFlag},
                 {registered_delivery, RegisteredDelivery},
                 {data_coding, DataCoding},
                 {sm_default_msg_id, SmDefaultMsgId}],

    % send the request

    % The framework provided by OSERL is completely asynchronous by nature,
    % gen_esme_session and gen_esme behave asynchronously, but you
    % may want to wait the responses synchronously or asynchronously in your 
    % application.  Both examples are shown below 
    %
    % @see submit_sm_async_iter/2 (asynchronous/default)
    % @see submit_sm_sync_iter/2 (synchronous)
    case read_decimal("How many times to submit this message (load test)") of
        Count when integer(Count) -> 
            submit_sm_iter(ParamList, Count, S#state.tx_session);
        _Error -> 
            submit_sm_iter(ParamList, 1, S#state.tx_session)
    end,
    {reply, ok, S};
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
handle_info(_Info, State) ->
    {noreply, State}.

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
    catch gen_smsc:unbind(S#state.tx_session),
    catch gen_smsc:session_stop(S#state.tx_session).

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
%% @spec submit_sm_iter(ParamList, Count, Session) -> ok
%%
%% @doc Sends a SM <tt>Count</tt> times over a given transmitter 
%% <tt>Session</tt>.
%% @end 
submit_sm_iter(_ParamList, 0, _Session) -> 
    ok;
submit_sm_iter(ParamList, Count, Session) ->
    spawn(fun() ->
                  case gen_esme:submit_sm(Session, ParamList) of
                      {ok, Response} -> 
                          % See how to get a parameter value
                          Id = operation:get_param(message_id, Response),
                          io:format("Message ID: ~p~n", [Id]);
                      {error, Error} ->
                          io:format("submit_sm failed with ~p~n", [Error])
                  end
          end),
    submit_sm_iter(ParamList, Count - 1, Session).


%% @spec read_string(Prompt) -> Result
%%     Prompt = string()
%%     Result = string | {error, What} | eof
%%     What   = term()
%%
%% @doc Reads a string from the standard input.
%% @end 
read_string(Prompt) ->
    case io:fread(Prompt, "~s") of
        {ok, InputList} ->
            hd(InputList);
        Error ->
            Error
    end.


%% @spec read_decimal(Prompt) -> Result
%%     Prompt = string()
%%     Result = int() | {error, What} | eof
%%     What   = term()
%%
%% @doc Reads a decimal number from the standard input.
%% @end 
read_decimal(Prompt) ->
    case io:fread(Prompt, "~d") of
        {ok, InputList} ->
            hd(InputList);
        Error ->
            Error
    end.
