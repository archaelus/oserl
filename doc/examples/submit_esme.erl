%%%
% Copyright (C) 2004 Enrique Marcote Peña <mpquique@udc.es>
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
% @doc Submit SM example.
%
% <p>A very simple ESME for submitting short messages.</p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [18 Feb 2004]
% 
% <ul>
%   <li>Trailing $\0 removed from the C-Octet String values.</li>
%   <li>Absolute/relative time implemented as pure C-Octet Strings.  Time 
%     record finally removed.  Time manipulation is now much more natural... 
%     and easy.
%   </li>
% </ul>
%
% [26 Feb 2004]
%
% <ul>
%   <li>Last changes in <a href="gen_esme.html">gen_esme.erl</a> adopted.</li>
% </ul>
%
% [28 Feb 2004]
%
% <ul>
%   <li>Uses new callbacks defined in <a href="gen_esme.html">gen_esme.erl</a>.
%   </li>
% </ul>
%
%
% @copyright 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique]
% @version 1.0, {09 Feb 2004} {@time}.
% @end
%%
-module(submit_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("gen_esme.hrl").

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([start/0, start_link/5, stop/0, submit_sm/2]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
% Internal gen_esme exports
%%--------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
-define(MC_ADDR, {193, 144, 50, 51}).
-define(ESME_NAME, submit_esme).

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec start() -> ok
%
% @doc Starts the submit ESME.
% @end
%
% %@see
%
% %@equiv
%%
start() ->
    start_link("submit_esme", "secret", "1950", "1950", ?MC_ADDR).
    
%%%
% @spec start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) -> ok
%    SystemId = string()
%    Password = string()
%    AddrRange = string()
%    SourceAddr = string()
%    McAddr = string() | atom() | ip_address()
%
% @doc Starts the submit ESME and binds as a receiver and transmitter to 
% McAddr.
% @end
%
% %@see
%
% %@equiv
%%
start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) ->
    Setup = ?GEN_ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    case gen_esme:start_link({local, ?ESME_NAME}, ?MODULE, Setup) of
        {ok, Eid} ->
            gen_esme:open_transmitter(?ESME_NAME, McAddr),
            gen_esme:bind_transmitter(?ESME_NAME),
            {ok, Eid};
        Error ->
            Error
    end.


%%%
% @spec stop() -> ok
%
% @doc Unbinds and stops the Submit SM ESME.
% @end
%
% %@see
%
% %@equiv
%%
stop() ->
    gen_esme:unbind_transmitter(?ESME_NAME),
    gen_esme:close_transmitter(?ESME_NAME),
    gen_esme:stop(?ESME_NAME).


%%%
% @spec submit_sm(MobileNumber, UserMessage) -> MessageId
%     MobileNumber = string()
%     UserMessage  = string()
%     MessageId    = string()
%
% @doc Submits a Short Message with the text <tt>UserMessage</tt> to
% <tt>MobileNumber</tt>.
%
% <p>This function is a direct translation into erlang from the java code 
% below.
% </p>
%
% <p>This sample code has too much I/O, which is very similar in both 
% languajes, that's why resulting code is almost identical.  In other kind
% of examples, with less I/O, I believe erlang code results much more compact 
% and readable than java.
% </p>
%
% <p>Besides, you should consider that the erlang code in this module, is more
% than just equivalent to the java function below.  In fact this module is a 
% complete ESME implementation.  This ESME detects and recovers from 
% connection failures, handles SMPP timers, controls congestion and so forth.
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
% @end 
%
% % @see
%
% % @equiv
%%
submit_sm(MobileNumber, UserMessage) ->
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
        Count when integer(Count) -> submit_sm_async_iter(ParamList, Count);
        _Error                    -> submit_sm_async_iter(ParamList, 1)
    end.


%%%
% @doc Auxiliary function for submit_sm/2
%
% <p>Asynchronous submit iter.</p>
% @end 
% % @see
%%
submit_sm_async_iter(ParamList, 0) -> 
    ok;
submit_sm_async_iter(ParamList, Count) ->
    spawn(fun() -> submit_sm(ParamList) end),
    submit_sm_async_iter(ParamList, Count - 1).


%%%
% @doc Auxiliary function for submit_sm/2
%
% <p>Synchronous submit iter.</p>
% @end 
% % @see
%%
submit_sm_sync_iter(ParamList, 0) -> 
    ok;
submit_sm_sync_iter(ParamList, Count) ->
    submit_sm(ParamList),
    submit_sm_sync_iter(ParamList, Count - 1).

%%%===================================================================
% ESME functions
%%====================================================================

%%%===================================================================
% Internal functions
%%====================================================================
%%%
% @spec submit_sm(ParamList) -> ok
%
% @doc Submits a SM and waits for the submit_sm_resp.
% @end 
%
% % @see
%
% % @equiv
%%
submit_sm(ParamList) -> 
    case gen_esme:submit_sm(?ESME_NAME, ParamList) of
        {ok, Response} -> 
            % See how to get a parameter value
            MessageId = operation:get_param(message_id, Response),
            io:format("Message ID: ~p~n", [MessageId]);
        {error, Error} ->
            io:format("Submit operation failed with ~p~n", [Error])
    end.


%%%
% @spec read_string(Prompt) -> Result
%     Prompt = string()
%     Result = string | {error, What} | eof
%     What   = term()
%
% @doc Reads a string from the standard input.
% @end 
%
% % @see
%
% % @equiv
%%
read_string(Prompt) ->
    case io:fread(Prompt, "~s") of
        {ok, InputList} ->
            hd(InputList);
        Error ->
            Error
    end.


%%%
% @spec read_decimal(Prompt) -> Result
%     Prompt = string()
%     Result = int() | {error, What} | eof
%     What   = term()
%
% @doc Reads a decimal number from the standard input.
% @end 
%
% % @see
%
% % @equiv
%%
read_decimal(Prompt) ->
    case io:fread(Prompt, "~d") of
        {ok, InputList} ->
            hd(InputList);
        Error ->
            Error
    end.
