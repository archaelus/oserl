%%% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc SMPP PDU Macros.
%%%
%%% <p>SMPP PDU descriptors using the pdu syntax.</p>
%%%
%%% <p>As a guideline, some comments include a section number referencing 
%%% the corresponding section number on [SMPP 5.0].</p>
%%%
%%%
%%% <h2>PDU definition<h2>
%%%
%%% <p>Instead of defining a common type specifier for each group of command 
%%% PDUs, happening to have identical syntax in SMPP specification version 5.0.
%%% *Every* PDU was defined by it's own specifier identified by the associated 
%%% <tt>command_name</tt> and <tt>command_id fields</tt>.</p>
%%%
%%% <p>With this approach it'll be easier to update this implementation to 
%%% later versions of the protocol, where the definition of any PDU may change,
%%% PDUs that today are identical could differ in the future.
%%%
%%% 
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [17 Feb 2004]
%%% 
%%% <ul>
%%%   <li>Default values moved to smpp_param.hrl.</li>
%%%   <li>Field <i>number_of_messages</i> added to <i>submit_sm</i>, 
%%%     <i>data_sm</i> and <i>submit_multi_sm</i>.
%%%   </li>
%%%   <li>CDMA and TDMA specific TLVs are no longer commented out.</li>
%%% </ul>
%%%
%%% [01 Mar 2004]
%%%
%%% <ul>
%%%   <li><i>command_id</i> added to PDU descriptors.</li>
%%% </ul>
%%%
%%%
%%% <h2>References</h2>
%%% <dl>
%%%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification.
%%%     Version 5.0. SMS Forum.
%%%   </dd>
%%%   <dt>[3GPP TS 23.040]</dt><dd>Technical Realization of the Short Message
%%%     Service (SMS) Release 4.  Version 5.0.0. 
%%%     <a href="http://www.3gpp.org">3GPP</a>.
%%%   </dd>
%%% </dl>
%%%
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 0.2, {12 May 2003} {@time}.
%%% @end
-ifndef(smpp_pdu).
-define(smpp_pdu, true).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_globals.hrl").  % Some global definitions
-include("smpp_param.hrl").    % The parameters declaration
-include("pdu_syntax.hrl").    % The syntax used in this file

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
%%% %@doc SMPP PDU Type Declarations.
%%%
%%% <p>Macros declaring SMPP PDU definitions and their default values.  Refer 
%%% to <a href="operation.html">operation.erl</a> to find out how these macros
%%% are used.</p>
%%%
%%%
%%% <h2>PDU declaration</h2>
%%%
%%% <p>Every PDU declaration consists of two lists.</p>
%%%
%%% <dl>
%%%   <dt>Command Id: </dt><dd>SMPP <i>command_id</i>.</dd>
%%%   <dt>Standard body parameters declaration: </dt><dd>A list with the
%%%     declaration of every standard parameter of the PDU body.  Header 
%%%     parameters are *not* included on the PDU declaration.  The standard
%%%     parameters are packed/unpacked in the order specified on this list, 
%%%     thus the order must be the one declared on [SMPP 5.0].
%%%   </dd>
%%%   <dt>TLV parameters declaration: </dt><dd>A list with the declaration
%%%     of every TLV parameter of the PDU.  
%%%
%%%     <p>Programmers may want to comment out, uncomment or even add any 
%%%     desired TLV in order to fit the needs of a particular implementation.
%%%     Notice that having declared unused TLVs doesn't do any harm, comment
%%%     them out or remove'em from the TLV list on the PDU declaration only for
%%%     efficiency sake.</p>
%%%   </dd>
%%% </dl>
%%%
%%% <p>Given the following declaration:</p>
%%% 
%%% <pre>
%%% -define(BIND_TRANSMITTER_RESP,
%%%         ?PDU(?COMMAND_ID_BIND_TRANSMITTER_RESP,
%%%              [?SYSTEM_ID],
%%%              [?CONGESTION_STATE,
%%%               ?SC_INTERFACE_VERSION])).
%%% </pre>
%%%
%%% <p>A dictionary representing this PDU must have the elements:</p>
%%%
%%% <pre>
%%% [{command_id, ?COMMAND_ID_BIND_TRANSMITTER_RESP},
%%%  {command_status, CommandStatus},
%%%  {sequence_number, SequenceNumber},
%%%  {system_id, SystemId}]
%%% </pre>
%%%
%%% <p>The pair <tt>{system_id, SystemId}</tt> might be ignored if 
%%% <tt>command_status</tt> is not 0, the following pairs are also optional:
%%% </p>
%%%
%%% <pre>
%%% [{congestion_state, CongestionState}, 
%%%  {sc_interface_version, ScInterfaceVersion}]
%%% </pre>
%%%
%%% <p>Notice that a <tt>command_length</tt> element is not included on the 
%%% dictionary.</p>
%%%
%%% %@see sections 4.1 to 4.6 on [SMPP 5.0]
%%% %@end
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%% bind_transmitter
-define(BIND_TRANSMITTER, 
        ?PDU(?COMMAND_ID_BIND_TRANSMITTER,
             [?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [])).

%% bind_transmitter_resp
-define(BIND_TRANSMITTER_RESP,
        ?PDU(?COMMAND_ID_BIND_TRANSMITTER_RESP,
             [?SYSTEM_ID],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

%% bind_receiver
-define(BIND_RECEIVER, 
        ?PDU(?COMMAND_ID_BIND_RECEIVER,
             [?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [])).

%% bind_receiver_resp
-define(BIND_RECEIVER_RESP, 
        ?PDU(?COMMAND_ID_BIND_RECEIVER_RESP,
             [?SYSTEM_ID],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

%% bind_transceiver
-define(BIND_TRANSCEIVER, 
        ?PDU(?COMMAND_ID_BIND_TRANSCEIVER,
             [?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [])).

%% bind_transceiver_resp
-define(BIND_TRANSCEIVER_RESP, 
        ?PDU(?COMMAND_ID_BIND_TRANSCEIVER_RESP,
             [?SYSTEM_ID],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

%% outbind
-define(OUTBIND, 
        ?PDU(?COMMAND_ID_OUTBIND,
             [?SYSTEM_ID, 
              ?PASSWORD],
             [])).

%% unbind
-define(UNBIND, 
        ?PDU(?COMMAND_ID_UNBIND,
             [], 
             [])).

%% unbind_resp
-define(UNBIND_RESP, 
        ?PDU(?COMMAND_ID_UNBIND_RESP,
             [],
             [?CONGESTION_STATE])).

%% enquire_link
-define(ENQUIRE_LINK, 
        ?PDU(?COMMAND_ID_ENQUIRE_LINK,
             [],
             [])).

%% enquire_link_resp
-define(ENQUIRE_LINK_RESP, 
        ?PDU(?COMMAND_ID_ENQUIRE_LINK_RESP,
             [],
             [?CONGESTION_STATE])).

%% alert_notification
-define(ALERT_NOTIFICATION, 
        ?PDU(?COMMAND_ID_ALERT_NOTIFICATION,
             [?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_65,
              ?ESME_ADDR_TON,
              ?ESME_ADDR_NPI,
              ?ESME_ADDR],
             [?MS_AVAILABILITY_STATUS])).

%% generic_nack
-define(GENERIC_NACK, 
        ?PDU(?COMMAND_ID_GENERIC_NACK,
             [],
             [])).

%% submit_sm
-define(SUBMIT_SM,
        ?PDU(?COMMAND_ID_SUBMIT_SM,
             [?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21,
              ?ESM_CLASS,
              ?PROTOCOL_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              ?DISPLAY_TIME,              % (CDMA, TDMA)
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?OPTIONAL_MESSAGE_PAYLOAD,  % Notice that it's optional
              ?MORE_MESSAGES_TO_SEND,
              ?MS_MSG_WAIT_FACILITIES,
              ?MS_VALIDITY,               % (CDMA, TDMA)
              ?NUMBER_OF_MESSAGES,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,
              ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

%% submit_sm_resp
%%
%% %@TODO Remove ?DELIVERY_FAILURE_REASON if never needed
-define(SUBMIT_SM_RESP,
        ?PDU(?COMMAND_ID_SUBMIT_SM_RESP,
             [?MESSAGE_ID],
             [?ADDITIONAL_STATUS_INFO_TEXT,         
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,   % data_sm_resp only
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

%% data_sm Syntax
-define(DATA_SM,
        ?PDU(?COMMAND_ID_DATA_SM,
             [?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_65,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_65,
              ?ESM_CLASS,
              ?REGISTERED_DELIVERY,
              ?DATA_CODING],
             [?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              ?DISPLAY_TIME,              % (CDMA, TDMA)
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MANDATORY_MESSAGE_PAYLOAD, % Notice that it's mandatory
              ?MORE_MESSAGES_TO_SEND,
              ?MS_MSG_WAIT_FACILITIES,
              ?MS_VALIDITY,               % (CDMA, TDMA)
              ?NUMBER_OF_MESSAGES,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,
              ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

%% data_sm_resp
-define(DATA_SM_RESP,
        ?PDU(?COMMAND_ID_DATA_SM_RESP,
             [?MESSAGE_ID],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

%% submit_multi
%%
%% %@TODO Remove ?MORE_MESSAGES_TO_SEND and ?SET_DPF if never needed.
-define(SUBMIT_MULTI,
        ?PDU(?COMMAND_ID_SUBMIT_MULTI,
             [?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDRESS,
              ?ESM_CLASS,
              ?PROTOCOL_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              ?DISPLAY_TIME,              % (CDMA, TDMA)
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?OPTIONAL_MESSAGE_PAYLOAD,  % Notice that it's optional
              ?MORE_MESSAGES_TO_SEND,     % makes sense on submit multi?
              ?MS_MSG_WAIT_FACILITIES,
              ?MS_VALIDITY,               % (CDMA, TDMA)
              ?NUMBER_OF_MESSAGES,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,                   % makes sense on submit multi?
              ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

%% %@doc submit_multi_resp.
%%
%% <p>Unlike some simulators, like SMPPSim 1.1, which they ignore the field
%% <tt>no_unsuccess</tt> if no destination was unsuccessful, this 
%% implementation sets to O this field if every destination was reached 
%% (remember that UNSUCCESS_SME was declared as a multivalue and this base type
%% automatically encodes the number of elements at the head of the binary).</p>
%%
%% <p>Notice that the SMS forum recommends the approach followed here.  Excerpt
%% from the SMS Forum:</p>
%%
%% <i>
%% <p>When all dests are sucessful, the no_unsuccess field will be set to 0. 
%% There are no unsuccessful SME fields then included.</p>
%%
%% <p>...here are two examples of the no_unsuccess file encodings for 0 and
%% then 2 failed destinations</p>
%%
%% <pre>
%% Body:
%% 41424300        Message id "ABC"
%% 00              0 unsuccessful SMEs
%% </pre>
%%
%% <pre>
%% 41424300        Message id "ABC"
%% 02              2 unsuccessful SMEs
%% 01              TON=1
%% 01              NPI=1
%% 353535353500    SME "55555"
%% 00000005        SMPP Error 0x00000005
%% 02              TON=2
%% 08              NPI=3
%% 363534353500    SME "65455"
%% 00000015        SMPP Error 0x00000015
%% </pre>
%%
%% <p>So if the <tt>no_unsuccess</tt> field is non zero, then that number of 
%% SMEs must be appended in the form of TON, NPI & Address similarly to how 
%% source and destination addresses are normally encoded. Also included per 
%% failed address is the 4-octet error code (command status value) associated 
%% with the failed address.</p>
%% </i>
%%
%% %@TODO Remove ?DELIVERY_FAILURE_REASON and ?DPF_RESULT if never needed
%% %@end
-define(SUBMIT_MULTI_RESP,
        ?PDU(?COMMAND_ID_SUBMIT_MULTI_RESP,
             [?MESSAGE_ID, 
              ?UNSUCCESS_SME],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

%% deliver_sm
-define(DELIVER_SM,
        ?PDU(?COMMAND_ID_DELIVER_SM,
             [?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21,
              ?ESM_CLASS,
              ?PROTOCOL_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [?CALLBACK_NUM,
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,         % (TDMA)
              ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_NETWORK_ID,
              ?DEST_NODE_ID,
              ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_PORT,
              ?DPF_RESULT,
              ?ITS_REPLY_TYPE,            % (CDMA)
              ?ITS_SESSION_INFO,          % (CDMA)
              ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?OPTIONAL_MESSAGE_PAYLOAD,  % Notice that it's optional
              ?OPTIONAL_MESSAGE_STATE,
              ?NETWORK_ERROR_CODE,
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?RECEIPTED_MESSAGE_ID,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?USER_MESSAGE_REFERENCE,
              ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

%% deliver_sm_resp Syntax
-define(DELIVER_SM_RESP, 
        ?PDU(?COMMAND_ID_DELIVER_SM_RESP,
             [?MESSAGE_ID],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?NETWORK_ERROR_CODE])).

%% broadcast_sm Syntax
%%
%% @TODO Confirm that the <tt>message_payload</tt> is mandatory
-define(BROADCAST_SM,
        ?PDU(?COMMAND_ID_BROADCAST_SM,
             [?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?MESSAGE_ID,
              ?PRIORITY_FLAG,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REPLACE_IF_PRESENT_FLAG,
              ?DATA_CODING,
              ?SM_DEFAULT_MSG_ID],
             [?BROADCAST_AREA_IDENTIFIER,
              ?MANDATORY_BROADCAST_CONTENT_TYPE,
              ?BROADCAST_REP_NUM,
              ?BROADCAST_FREQUENCY_INTERVAL,
              ?ALERT_ON_MESSAGE_DELIVERY,   % (CDMA)
              ?BROADCAST_CHANNEL_INDICATOR,
              ?BROADCAST_CONTENT_TYPE_INFO, % (CDMA, TDMA)
              ?BROADCAST_MESSAGE_CLASS,
              ?BROADCAST_SERVICE_GROUP,     % (CDMA, TDMA)
              ?CALLBACK_NUM,
              ?CALLBACK_NUM_ATAG,           % (TDMA)
              ?CALLBACK_NUM_PRES_IND,       % (TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_SUBADDRESS,             % (CDMA, TDMA)
              ?DEST_PORT,
              ?DISPLAY_TIME,                % (CDMA, TDMA)
              ?LANGUAGE_INDICATOR,          % (CDMA, TDMA)
              ?MANDATORY_MESSAGE_PAYLOAD,   % Notice that it's mandatory
              ?MS_VALIDITY,                 % (CDMA, TDMA)
              ?PAYLOAD_TYPE,
              ?PRIVACY_INDICATOR,           % (CDMA, TDMA)
              ?SMS_SIGNAL,                  % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_PORT,
              ?SOURCE_SUBADDRESS,           % (CDMA, TDMA)
              ?USER_MESSAGE_REFERENCE])).

%% broadcast_sm_resp Syntax
-define(BROADCAST_SM_RESP,
        ?PDU(?COMMAND_ID_BROADCAST_SM_RESP,
             [?MESSAGE_ID],
             [?BROADCAST_ERROR_STATUS,
              ?CONGESTION_STATE, 
              ?FAILED_BROADCAST_AREA_IDENTIFIER])).

%% cancel_sm Syntax
-define(CANCEL_SM,
        ?PDU(?COMMAND_ID_CANCEL_SM,
             [?SERVICE_TYPE,
              ?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21],
             [])).

%% cancel_sm_resp Syntax
-define(CANCEL_SM_RESP, 
        ?PDU(?COMMAND_ID_CANCEL_SM_RESP,
             [],
             [?CONGESTION_STATE])).

%% query_sm Syntax
-define(QUERY_SM,
        ?PDU(?COMMAND_ID_QUERY_SM,
             [?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [])).

%% query_sm_resp Syntax
-define(QUERY_SM_RESP,
        ?PDU(?COMMAND_ID_QUERY_SM_RESP,
             [?MESSAGE_ID,
              ?FINAL_DATE,
              ?MESSAGE_STATE,
              ?ERROR_CODE],
             [?CONGESTION_STATE])).

%% replace_sm Syntax
-define(REPLACE_SM,
        ?PDU(?COMMAND_ID_REPLACE_SM,
             [?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [?OPTIONAL_MESSAGE_PAYLOAD])).  % Notice that it's optional

%% replace_sm_resp Syntax
-define(REPLACE_SM_RESP, 
        ?PDU(?COMMAND_ID_REPLACE_SM_RESP,
             [],
             [?CONGESTION_STATE])).

%% query_broadcast_sm Syntax
-define(QUERY_BROADCAST_SM,
        ?PDU(?COMMAND_ID_QUERY_BROADCAST_SM,
             [?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [?USER_MESSAGE_REFERENCE])).

%% query_broadcast_sm_resp Syntax
-define(QUERY_BROADCAST_SM_RESP,
        ?PDU(?COMMAND_ID_QUERY_BROADCAST_SM_RESP,
             [?MESSAGE_ID],
             [?CONGESTION_STATE,
              ?MANDATORY_MESSAGE_STATE,
              ?BROADCAST_AREA_IDENTIFIER,
              ?BROADCAST_AREA_SUCCESS,
              ?BROADCAST_END_TIME, 
              ?USER_MESSAGE_REFERENCE])).

%% cancel_broadcast_sm Syntax
-define(CANCEL_BROADCAST_SM,
        ?PDU(?COMMAND_ID_CANCEL_BROADCAST_SM,
             [?SERVICE_TYPE,
              ?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [?OPTIONAL_BROADCAST_CONTENT_TYPE, 
              ?USER_MESSAGE_REFERENCE])).

%% cancel_broadcast_sm_resp Syntax
-define(CANCEL_BROADCAST_SM_RESP,
        ?PDU(?COMMAND_ID_CANCEL_BROADCAST_SM_RESP,
             [],
             [?CONGESTION_STATE])).


%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

-endif.  % -ifndef(smpp_pdu)

