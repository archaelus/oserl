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
%   <li>Trailing $\0 removed from the c_octet_string values.</li>
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

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) -> ok
%    SystemId = string()
%    Password = string()
%    AddrRange = string()
%    SourceAddr = string()
%    McAddr = string() | atom() | ip_address()
%
% @doc Starts the echo ESME and binds as a receiver and transmitter to McAddr.
% @end
%
% %@see
%
% %@equiv
%%
start() ->
    start_link("banana", "secret", "1950", "1950", {193, 144, 50, 51}).
    
start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) ->
    Setup = ?GEN_ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    case gen_esme:start_link({local, submit_esme}, ?MODULE, Setup) of
        {ok, Eid} ->
            gen_esme:open_transmitter(submit_esme, McAddr),
            gen_esme:bind_transmitter(submit_esme),
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
    gen_esme:unbind_transmitter(submit_esme),
    gen_esme:close_transmitter(submit_esme),
    gen_esme:stop(submit_esme).


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
%     debug.enter(this, "SMPPTest.submit()");        
%     try {
%         if (!bound) {
%             //System.out.println("Binding to SMSC......");
%             bind();
%         }
%         SubmitSM request = new SubmitSM();
%         SubmitSMResp response;
%   
%         // input values
%         serviceType = getParam("Service type", serviceType);
%         sourceAddress = getAddress("Source",sourceAddress);
%         //System.out.println("Source Address : " + sourceAddress);
%         //destAddress = getAddress("Destination",destAddress);
% 
%         destAddress = getAddress("Destination",new Address(mobileNumber));
%    
%         replaceIfPresentFlag = getParam("Replace if present flag", 
%                                         replaceIfPresentFlag);
%         //shortMessage = getParam("The short message", shortMessage);
%         shortMessage = userMessage;
%         scheduleDeliveryTime = getParam("Schedule delivery time", 
%                                         scheduleDeliveryTime);
%
%         validityPeriod = getParam("Validity period", validityPeriod);
%         esmClass = getParam("Esm class", esmClass);
%         //protocolId = getParam("Protocol id", protocolId);
%         //protocolId=00;
%         priorityFlag = getParam("Priority flag", priorityFlag);
%         registeredDelivery = getParam("Registered delivery", 
%                                       registeredDelivery);
%         //dataCoding = getParam("Data encoding", dataCoding);
%         //dataCoding = 0x10; // For Flash Message
%         smDefaultMsgId = getParam("Sm default msg id", smDefaultMsgId);
%
%         //System.out.println ("\nRegistered_Delivery : "+registeredDelivery);
%         // set values
%         request.setServiceType(serviceType);
%         request.setSourceAddr(sourceAddress);
%         request.setDestAddr(destAddress);
%         request.setReplaceIfPresentFlag(replaceIfPresentFlag);
%         request.setShortMessage(shortMessage);
%         request.setScheduleDeliveryTime(scheduleDeliveryTime);
%         request.setValidityPeriod(validityPeriod);
%         request.setEsmClass(esmClass);
%         request.setProtocolId(protocolId);
%         request.setPriorityFlag(priorityFlag);
%         request.setRegisteredDelivery(registeredDelivery);
%         request.setDataCoding(dataCoding);
%         request.setSmDefaultMsgId(smDefaultMsgId);
%         // send the request
%         int count = 1;
%         System.out.println();
%         count = getParam("How many times to submit this message (load test)",
%                          count);
%
%         for (int i = 0; i<count; i++) {
%             request.assignSequenceNumber(true);
%             //System.out.print("#"+i+"  ");
%             System.out.println("Submit request " + request.debugString());
%             if (asynchronous) {
%                 //session.submit(request);  
%                 response = session.submit(request);
%                 messageId = response.getMessageId();
%                 //System.out.println();
%                 return messageId;
%             } else {
%                 response = session.submit(request);                    
%                 messageId = response.getMessageId();
%                 System.out.println("Submit response " + 
%                                    response.debugString() + 
%                                    " MESSAGE ID : " + 
%                                    messageId);
%                 return messageId;
%             }
%         }    
%         return messageId;            
%     } catch (Exception e) {
%         event.write(e,"");
%         debug.write("Submit operation failed. " + e);
%         //System.out.println("Submit operation failed. " + e);
%         bind();
%         return messageId;
%     } finally {
%         debug.exit(this);
%     }
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
% <p>Asynchronous submit iter.
% </p>
% @end 
% @see
%%
submit_sm_async_iter(ParamList, 0) -> 
    ok;
submit_sm_async_iter(ParamList, Count) ->
    spawn(fun() -> submit_sm(ParamList) end),
    submit_sm_async_iter(ParamList, Count - 1).

%%%
% @doc Auxiliary function for submit_sm/2
%
% <p>Synchronous submit iter.
% </p>
% @see
% @end 
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
    case gen_esme:submit_sm(submit_esme, ParamList) of
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

% Erlang shell

% 1>  submit_esme:start_link("banana\0", "secret\0", "1948\0", "1948\0", {192, 168, 1, 2}).
% {ok,<0.39.0>}
% 2> submit_esme:submit_sm("600123456\0", "Hello there").
% "Service type> "CMT
% "Source> "1948
% "Replace if present flag> "1
% "Schedule delivery time> "040209203214000R
% "Validity period> "040210203214000R
% "Esm class> "0
% "Priority flag> "0
% "Registered delivery> "0
% "Sm default msg id> "1
% "How many times to submit this message (load test)"20
% ok
% Message ID: [50,50,54,52,48,54,51,54,0]
% Message ID: [49,57,50,56,54,55,57,48,0]
% Message ID: [55,48,51,53,53,53,55,52,0]
% Message ID: [57,57,57,54,53,52,51,54,0]
% Message ID: [51,56,56,57,54,54,53,52,0]
% Message ID: [56,56,51,55,55,48,57,50,0]
% Message ID: [56,51,54,54,48,51,50,57,0]
% Message ID: [51,49,53,48,55,51,51,52,0]
% Message ID: [53,50,50,54,57,54,56,53,0]
% Message ID: [52,56,52,51,50,49,48,52,0]
% Message ID: [55,55,48,48,55,49,55,54,0]
% Message ID: [50,54,49,57,56,49,56,56,0]
% Message ID: [49,51,49,56,49,48,57,48,0]
% Message ID: [49,54,50,54,51,48,51,49,0]
% Message ID: [53,50,55,49,57,54,53,50,0]
% Message ID: [50,52,56,49,56,54,55,53,0]
% Message ID: [50,56,55,53,53,49,50,53,0]
% Message ID: [52,56,53,56,55,56,57,55,0]
% Message ID: [49,56,50,48,50,49,51,53,0]
% Message ID: [54,55,52,55,57,56,53,55,0]
% 3> submit_esme:stop().
% ok
% 4> 

% SMSC log

% 2004.02.06 23:52:06 367 INFO    ConnectionHandler CH15 accepted a connection
% 2004.02.06 23:52:07 130 INFO    CH15: BIND_TRANSMITTER:
% 2004.02.06 23:52:07 147 INFO    Hex dump (39) bytes:
% 2004.02.06 23:52:07 200 INFO    00000027:00000002:00000000:00000001:
% 2004.02.06 23:52:07 260 INFO    62616E61:6E610073:65637265:74000050:
% 2004.02.06 23:52:07 261 INFO    01013139:343800
% 2004.02.06 23:52:07 262 INFO
% 2004.02.06 23:52:07 950 INFO    CH15: BIND_TRANSMITTER_RESP:
% 2004.02.06 23:52:07 951 INFO    Hex dump (24) bytes:
% 2004.02.06 23:52:07 953 INFO    00000018:80000002:00000000:00000001:
% 2004.02.06 23:52:07 954 INFO    534D5050:53696D00:
% 2004.02.06 23:52:07 955 INFO
% 2004.02.06 23:53:07 981 INFO    CH15: ENQUIRE_LINK:
% 2004.02.06 23:53:08 013 INFO    Hex dump (16) bytes:
% 2004.02.06 23:53:08 032 INFO    00000010:00000015:00000000:00000002:
% 2004.02.06 23:53:08 033 INFO
% 2004.02.06 23:53:08 036 INFO
% 2004.02.06 23:53:08 134 INFO    CH15: ENQUIRE_LINK_RESP:
% 2004.02.06 23:53:08 137 INFO    Hex dump (16) bytes:
% 2004.02.06 23:53:08 139 INFO    00000010:80000015:00000000:00000002:
% 2004.02.06 23:53:08 142 INFO
% 2004.02.06 23:53:08 144 INFO
% 2004.02.06 23:53:21 993 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:22 019 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:22 804 INFO    0000009E:00000004:00000000:00000003:
% 2004.02.06 23:53:22 827 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:22 830 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:22 832 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:22 835 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:22 838 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:22 841 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:22 843 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:22 846 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:22 870 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:22 873 INFO
% 2004.02.06 23:53:23 051 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 053 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 056 INFO    00000019:80000004:00000000:00000003:
% 2004.02.06 23:53:23 058 INFO    32323634:30363336:00
% 2004.02.06 23:53:23 061 INFO
% 2004.02.06 23:53:23 090 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 115 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 118 INFO    0000009E:00000004:00000000:00000004:
% 2004.02.06 23:53:23 121 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 123 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 126 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 128 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 131 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 134 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 137 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 139 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 142 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 145 INFO
% 2004.02.06 23:53:23 159 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 162 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 164 INFO    00000019:80000004:00000000:00000004:
% 2004.02.06 23:53:23 167 INFO    31393238:36373930:00
% 2004.02.06 23:53:23 182 INFO
% 2004.02.06 23:53:23 211 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 214 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 217 INFO    0000009E:00000004:00000000:00000005:
% 2004.02.06 23:53:23 219 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 222 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 225 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 227 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 230 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 233 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 235 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 238 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 241 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 243 INFO
% 2004.02.06 23:53:23 271 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 273 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 286 INFO    00000019:80000004:00000000:00000005:
% 2004.02.06 23:53:23 288 INFO    37303335:35353734:00
% 2004.02.06 23:53:23 291 INFO
% 2004.02.06 23:53:23 301 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 303 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 306 INFO    0000009E:00000004:00000000:00000006:
% 2004.02.06 23:53:23 309 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 311 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 314 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 317 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 320 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 322 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 325 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 328 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 331 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 333 INFO
% 2004.02.06 23:53:23 363 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 366 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 369 INFO    00000019:80000004:00000000:00000006:
% 2004.02.06 23:53:23 371 INFO    39393936:35343336:00
% 2004.02.06 23:53:23 373 INFO
% 2004.02.06 23:53:23 384 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 386 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 389 INFO    0000009E:00000004:00000000:00000007:
% 2004.02.06 23:53:23 392 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 394 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 397 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 400 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 402 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 404 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 407 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 410 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 413 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 417 INFO
% 2004.02.06 23:53:23 432 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 434 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 437 INFO    00000019:80000004:00000000:00000007:
% 2004.02.06 23:53:23 439 INFO    33383839:36363534:00
% 2004.02.06 23:53:23 442 INFO
% 2004.02.06 23:53:23 452 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 454 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 457 INFO    0000009E:00000004:00000000:00000008:
% 2004.02.06 23:53:23 460 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 462 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 465 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 467 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 470 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 473 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 476 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 478 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 481 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 483 INFO
% 2004.02.06 23:53:23 498 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 500 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 503 INFO    00000019:80000004:00000000:00000008:
% 2004.02.06 23:53:23 505 INFO    38383337:37303932:00
% 2004.02.06 23:53:23 508 INFO
% 2004.02.06 23:53:23 518 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 520 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 523 INFO    0000009E:00000004:00000000:00000009:
% 2004.02.06 23:53:23 525 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 528 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 531 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 533 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 536 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 539 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 541 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 544 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 547 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 559 INFO
% 2004.02.06 23:53:23 662 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 664 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 667 INFO    00000019:80000004:00000000:00000009:
% 2004.02.06 23:53:23 669 INFO    38333636:30333239:00
% 2004.02.06 23:53:23 671 INFO
% 2004.02.06 23:53:23 692 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 695 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 697 INFO    0000009E:00000004:00000000:0000000A:
% 2004.02.06 23:53:23 700 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 702 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 705 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 707 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 711 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 713 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 716 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 719 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 721 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 724 INFO
% 2004.02.06 23:53:23 738 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 741 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 744 INFO    00000019:80000004:00000000:0000000A:
% 2004.02.06 23:53:23 746 INFO    33313530:37333334:00
% 2004.02.06 23:53:23 748 INFO
% 2004.02.06 23:53:23 759 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 761 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 766 INFO    0000009E:00000004:00000000:0000000B:
% 2004.02.06 23:53:23 769 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 772 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 774 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 777 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 780 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 782 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 785 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 788 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 790 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 793 INFO
% 2004.02.06 23:53:23 808 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 810 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 813 INFO    00000019:80000004:00000000:0000000B:
% 2004.02.06 23:53:23 815 INFO    35323236:39363835:00
% 2004.02.06 23:53:23 818 INFO
% 2004.02.06 23:53:23 828 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 831 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 833 INFO    0000009E:00000004:00000000:0000000C:
% 2004.02.06 23:53:23 836 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 839 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 841 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 845 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 848 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 850 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 853 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 856 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 859 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 861 INFO
% 2004.02.06 23:53:23 876 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 878 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 881 INFO    00000019:80000004:00000000:0000000C:
% 2004.02.06 23:53:23 883 INFO    34383433:32313034:00
% 2004.02.06 23:53:23 886 INFO
% 2004.02.06 23:53:23 896 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 899 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 901 INFO    0000009E:00000004:00000000:0000000D:
% 2004.02.06 23:53:23 904 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 907 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 909 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 912 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 914 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 917 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 920 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 923 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 925 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 928 INFO
% 2004.02.06 23:53:23 942 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:23 945 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:23 948 INFO    00000019:80000004:00000000:0000000D:
% 2004.02.06 23:53:23 950 INFO    37373030:37313736:00
% 2004.02.06 23:53:23 952 INFO
% 2004.02.06 23:53:23 963 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:23 965 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:23 968 INFO    0000009E:00000004:00000000:0000000E:
% 2004.02.06 23:53:23 971 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:23 973 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:23 976 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:23 979 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:23 982 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:23 984 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:23 987 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:23 990 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:23 992 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:23 995 INFO
% 2004.02.06 23:53:24 009 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 012 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 025 INFO    00000019:80000004:00000000:0000000E:
% 2004.02.06 23:53:24 028 INFO    32363139:38313838:00
% 2004.02.06 23:53:24 030 INFO
% 2004.02.06 23:53:24 041 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 043 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 046 INFO    0000009E:00000004:00000000:0000000F:
% 2004.02.06 23:53:24 049 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 051 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 054 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 057 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 060 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 062 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 065 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 068 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 070 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 073 INFO
% 2004.02.06 23:53:24 088 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 090 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 093 INFO    00000019:80000004:00000000:0000000F:
% 2004.02.06 23:53:24 096 INFO    31333138:31303930:00
% 2004.02.06 23:53:24 098 INFO
% 2004.02.06 23:53:24 130 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 133 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 136 INFO    0000009E:00000004:00000000:00000010:
% 2004.02.06 23:53:24 138 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 141 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 144 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 146 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 149 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 151 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 154 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 157 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 160 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 162 INFO
% 2004.02.06 23:53:24 275 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 278 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 280 INFO    00000019:80000004:00000000:00000010:
% 2004.02.06 23:53:24 283 INFO    31363236:33303331:00
% 2004.02.06 23:53:24 285 INFO
% 2004.02.06 23:53:24 297 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 303 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 306 INFO    0000009E:00000004:00000000:00000011:
% 2004.02.06 23:53:24 308 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 311 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 313 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 316 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 319 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 322 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 324 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 327 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 330 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 332 INFO
% 2004.02.06 23:53:24 366 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 373 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 376 INFO    00000019:80000004:00000000:00000011:
% 2004.02.06 23:53:24 378 INFO    35323731:39363532:00
% 2004.02.06 23:53:24 381 INFO
% 2004.02.06 23:53:24 397 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 399 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 402 INFO    0000009E:00000004:00000000:00000012:
% 2004.02.06 23:53:24 411 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 413 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 416 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 419 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 421 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 424 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 427 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 432 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 434 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 437 INFO
% 2004.02.06 23:53:24 457 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 460 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 462 INFO    00000019:80000004:00000000:00000012:
% 2004.02.06 23:53:24 465 INFO    32343831:38363735:00
% 2004.02.06 23:53:24 467 INFO
% 2004.02.06 23:53:24 477 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 481 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 484 INFO    0000009E:00000004:00000000:00000013:
% 2004.02.06 23:53:24 486 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 497 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 499 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 502 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 505 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 507 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 516 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 519 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 527 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 529 INFO
% 2004.02.06 23:53:24 553 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 555 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 558 INFO    00000019:80000004:00000000:00000013:
% 2004.02.06 23:53:24 560 INFO    32383735:35313235:00
% 2004.02.06 23:53:24 562 INFO
% 2004.02.06 23:53:24 575 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 578 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 581 INFO    0000009E:00000004:00000000:00000014:
% 2004.02.06 23:53:24 592 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 594 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 597 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 599 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 602 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 605 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 608 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 610 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 614 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 621 INFO
% 2004.02.06 23:53:24 636 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 638 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 640 INFO    00000019:80000004:00000000:00000014:
% 2004.02.06 23:53:24 657 INFO    34383538:37383937:00
% 2004.02.06 23:53:24 660 INFO
% 2004.02.06 23:53:24 680 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 683 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 685 INFO    0000009E:00000004:00000000:00000015:
% 2004.02.06 23:53:24 688 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 690 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 693 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 698 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 700 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 703 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 706 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 708 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 711 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 713 INFO
% 2004.02.06 23:53:24 745 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 748 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 750 INFO    00000019:80000004:00000000:00000015:
% 2004.02.06 23:53:24 753 INFO    31383230:32313335:00
% 2004.02.06 23:53:24 765 INFO
% 2004.02.06 23:53:24 795 INFO    CH15: SUBMIT_SM:
% 2004.02.06 23:53:24 797 INFO    Hex dump (158) bytes:
% 2004.02.06 23:53:24 800 INFO    0000009E:00000004:00000000:00000016:
% 2004.02.06 23:53:24 802 INFO    434D5400:01013139:34380001:01363030:
% 2004.02.06 23:53:24 805 INFO    31323334:35360000:00003034:30323039:
% 2004.02.06 23:53:24 807 INFO    32303332:31343030:30520030:34303231:
% 2004.02.06 23:53:24 812 INFO    30323033:32313430:30305200:00010201:
% 2004.02.06 23:53:24 815 INFO    0B48656C:6C6F2074:68657265:00050001:
% 2004.02.06 23:53:24 818 INFO    00000700:01010006:00010100:08000200:
% 2004.02.06 23:53:24 820 INFO    00042400:00001900:0100020F:00010102:
% 2004.02.06 23:53:24 944 INFO    0E000101:04210001:01000D00:0100000F:
% 2004.02.06 23:53:24 950 INFO    00010100:0E000101:00100002:0000
% 2004.02.06 23:53:24 952 INFO
% 2004.02.06 23:53:24 977 INFO    CH15:SUBMIT_SM_RESP:
% 2004.02.06 23:53:24 987 INFO    Hex dump (25) bytes:
% 2004.02.06 23:53:24 990 INFO    00000019:80000004:00000000:00000016:
% 2004.02.06 23:53:24 992 INFO    36373437:39383537:00
% 2004.02.06 23:53:24 994 INFO
% 2004.02.06 23:53:36 075 INFO    CH15: UNBIND
% 2004.02.06 23:53:36 076 INFO    Hex dump (16) bytes:
% 2004.02.06 23:53:36 077 INFO    00000010:00000006:00000000:00000017:
% 2004.02.06 23:53:36 078 INFO
% 2004.02.06 23:53:36 079 INFO
% 2004.02.06 23:53:36 167 INFO    CH15: UNBIND_RESP
% 2004.02.06 23:53:36 170 INFO    Hex dump (16) bytes:
% 2004.02.06 23:53:36 172 INFO    00000010:80000006:00000000:00000017:
% 2004.02.06 23:53:36 175 INFO
% 2004.02.06 23:53:36 177 INFO
% 2004.02.06 23:53:36 180 INFO    CH15 closing connection
% 2004.02.06 23:53:36 183 INFO    ConnectionHandler CH15 waiting for connection

