%%%
% Copyright (C) 2003 Enrique Marcote Peña <mpquique@udc.es>
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
% @doc SMPP PDU Macros.
%
% <p>SMPP PDU descriptors using the pdu syntax.</p>
%
% <p>As a guideline, some comments include a section number referencing 
% the corresponding section number on [SMPP 5.0].</p>
%
%
% <h2>PDU definition<h2>
%
% <p>Instead of defining a common type specifier for each group of command 
% PDUs, happening to have identical syntax in SMPP specification version 5.0.
% *Every* PDU was defined by it's own specifier identified by the associated 
% <tt>command_name</tt> and <tt>command_id fields</tt>.</p>
%
% <p>With this approach it'll be easier to update this implementation to later 
% versions of the protocol, where the definition of any PDU may change, so
% PDUs that today are identical, could be different in the future.
%
% <h2>References</h2>
% <dl>
%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification. 
%     Version 5.0. SMS Forum.
%   </dd>
%   <dt>[3GPP TS 23.040]</dt><dd>Technical Realization of the Short Message
%     Service (SMS) Release 4.  Version 5.0.0. 
%     <a href="http://www.3gpp.org">3GPP</a>.
%   </dd>
% </dl>
%%
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {12 May 2003} {@time}.
% @end
%%

-ifndef(smpp_pdu).
-define(smpp_pdu, true).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("smpp_globals.hrl").  % Some global definitions
-include("smpp_param.hrl").    % The parameters declaration
-include("pdu_syntax.hrl").    % The syntax used in this file

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% %@doc SMPP PDU Type Declarations.
%
% <p>Macros declaring SMPP PDU definitions and their default values.  Refer 
% to <b>operation.erl</b> to find out how these macros are used.</p>
%
%
% <h2>PDU declaration</h2>
%
% <p>Every PDU declaration consists of two lists.</p>
%
% <dl>
%   <dt>Standard body parameters declaration: </dt><dd>A list with the
%     declaration of every standard parameter of the PDU body.  Header 
%     parameters are *not* included on the PDU declaration.  The standard
%     parameters are packed/unpacked in the order specified on this list, thus
%     the order must be the one declared on [SMPP 5.0].
%   </dd>
%   <dt>TLV parameters declaration: </dt><dd>A list with the declaration
%     of every TLV parameter of the PDU.  
%
%     <p>Programmers may want to comment out, uncomment or even add any 
%     desired TLV in order to fit the needs of a particular implementation.
%     Notice that having declared unused TLVs doesn't do any harm, comment
%     them out or remove'em from the TLV list on the PDU declaration only for
%     efficiency sake.
%   </dd>
% </dl>
%
% <p>Given the following declaration:</p>
% 
% <pre>
% -define(BIND_TRANSMITTER_RESP,
%         ?PDU([?SYSTEM_ID],
%              [?CONGESTION_STATE,
%               ?SC_INTERFACE_VERSION])).
% </pre>
%
% <p>A dictionary representing this PDU must have the elements:</p>
%
% <pre>
% [{command_id, ?COMMAND_ID_BIND_TRANSMITTER_RESP},
%  {command_status, CommandStatus},
%  {sequence_number, SequenceNumber},
%  {system_id, SystemId}]
% </pre>
%
% <p>The pair <tt>{system_id, SystemId}</tt> might be ignored if 
% <tt>command_status</tt> is not 0, the following pairs are also optional:</p>
%
% <pre>
% [{congestion_state, CongestionState}, 
%  {sc_interface_version, ScInterfaceVersion}]
% </pre>
%
% <p>Notice that a <tt>command_length</tt> element is not included on the 
% dictionary.
%
%
% <h2>PDU default value list</h2>
%
% <p>Feel free to customize the default value list of any PDU to fit the
% particular needs of your implementation.  Just take care that the
% values are given in the form <tt>{ParamName, ParamValue}</tt>, where
% <tt>ParamName</tt> must be the name given for that particular parameter
% on its type specifier, found in <b>smpp_param.hrl</b>.</p>
%
% <b>Important Note: </b><i>Don't forget to recompile the module
% <b>operation.erl</b> for the changes on this file to take effect.</i>
%
% %@see sections 4.1 to 4.6 on [SMPP 5.0]
% %@end
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% bind_transmitter
%%
-define(BIND_TRANSMITTER, 
        ?PDU([?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [])).

-define(BIND_TRANSMITTER_DEFAULTS, 
        [{password,          ?NULL_C_OCTET_STRING},
         {system_type,       ?NULL_C_OCTET_STRING},
         {interface_version, ?SMPP_VERSION_5_0},
         {addr_ton,          ?TON_INTERNATIONAL},
         {addr_npi,          ?NPI_ISDN},
         {address_range,     ?NULL_C_OCTET_STRING}]).

%%%
% bind_transmitter_resp
%%
-define(BIND_TRANSMITTER_RESP,
        ?PDU([?SYSTEM_ID],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

-define(BIND_TRANSMITTER_RESP_DEFAULTS, 
        [{sc_interface_version, ?SMPP_VERSION_5_0}]).

%%%
% bind_receiver
%%
-define(BIND_RECEIVER, 
        ?PDU([?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [])).

-define(BIND_RECEIVER_DEFAULTS, 
        [{password,          ?NULL_C_OCTET_STRING},
         {system_type,       ?NULL_C_OCTET_STRING},
         {interface_version, ?SMPP_VERSION_5_0},
         {addr_ton,          ?TON_INTERNATIONAL},
         {addr_npi,          ?NPI_ISDN},
         {address_range,     ?NULL_C_OCTET_STRING}]).

%%%
% bind_receiver_resp
%%
-define(BIND_RECEIVER_RESP, 
        ?PDU([?SYSTEM_ID],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

-define(BIND_RECEIVER_RESP_DEFAULTS, 
        [{sc_interface_version, ?SMPP_VERSION_5_0}]).

%%%
% bind_transceiver
%%
-define(BIND_TRANSCEIVER, 
        ?PDU([?SYSTEM_ID,
              ?PASSWORD,
              ?SYSTEM_TYPE,
              ?INTERFACE_VERSION,
              ?ADDR_TON,
              ?ADDR_NPI,
              ?ADDRESS_RANGE],
             [])).

-define(BIND_TRANSCEIVER_DEFAULTS, 
        [{password,          ?NULL_C_OCTET_STRING},
         {system_type,       ?NULL_C_OCTET_STRING},
         {interface_version, ?SMPP_VERSION_5_0},
         {addr_ton,          ?TON_INTERNATIONAL},
         {addr_npi,          ?NPI_ISDN},
         {address_range,     ?NULL_C_OCTET_STRING}]).

%%%
% bind_transceiver_resp
%%
-define(BIND_TRANSCEIVER_RESP, 
        ?PDU([?SYSTEM_ID],
             [?CONGESTION_STATE,
              ?SC_INTERFACE_VERSION])).

-define(BIND_TRANSCEIVER_RESP_DEFAULTS, 
        [{sc_interface_version, ?SMPP_VERSION_5_0}]).

%%%
% outbind
%%
-define(OUTBIND, 
        ?PDU([?SYSTEM_ID, 
              ?PASSWORD],
             [])).


-define(OUTBIND_DEFAULTS, 
        [{password, ?NULL_C_OCTET_STRING}]).

%%%
% unbind
%%
-define(UNBIND, 
        ?PDU([], 
             [])).

-define(UNBIND_DEFAULTS, []).

%%%
% unbind_resp
%%
-define(UNBIND_RESP, 
        ?PDU([],
             [?CONGESTION_STATE])).

-define(UNBIND_RESP_DEFAULTS, []).

%%%
% enquire_link
%%
-define(ENQUIRE_LINK, 
        ?PDU([],
             [])).

-define(ENQUIRE_LINK_DEFAULTS, []).

%%%
% enquire_link_resp
%%
-define(ENQUIRE_LINK_RESP, 
        ?PDU([],
             [?CONGESTION_STATE])).

-define(ENQUIRE_LINK_RESP_DEFAULTS, []).

%%%
% alert_notification
%%
-define(ALERT_NOTIFICATION, 
        ?PDU([?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_65,
              ?ESME_ADDR_TON,
              ?ESME_ADDR_NPI,
              ?ESME_ADDR],
             [?MS_AVAILABILITY_STATUS])).

-define(ALERT_NOTIFICATION_DEFAULTS, 
        [{source_addr_ton,        ?TON_INTERNATIONAL},
         {source_addr_npi,        ?NPI_ISDN},
         {source_addr,            ?NULL_C_OCTET_STRING},
         {esme_addr_ton,          ?TON_INTERNATIONAL},
         {esme_addr_npi,          ?NPI_ISDN},
         {esme_addr,              ?NULL_C_OCTET_STRING},
         {ms_availability_status, ?MS_AVAILABILITY_STATUS_AVAILABLE}]).

%%%
% generic_nack
%%
-define(GENERIC_NACK, 
        ?PDU([],
             [])).

-define(GENERIC_NACK_DEFAULTS, []). 

%%%
% submit_sm
%%
-define(SUBMIT_SM,
        ?PDU([?SERVICE_TYPE,
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
             [% ?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              % ?CALLBACK_NUM_ATAG,         % (TDMA)
              % ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              % ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              % ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              % ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              % ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              % ?DISPLAY_TIME,              % (CDMA, TDMA)
              % ?ITS_REPLY_TYPE,            % (CDMA)
              % ?ITS_SESSION_INFO,          % (CDMA)
              % ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?OPTIONAL_MESSAGE_PAYLOAD,  % Notice that it's optional
              ?MORE_MESSAGES_TO_SEND,
              ?MS_MSG_WAIT_FACILITIES,
              % ?MS_VALIDITY,               % (CDMA, TDMA)
              ?PAYLOAD_TYPE,
              % ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,
              % ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              % ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              % ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(SUBMIT_SM_DEFAULTS, 
        [{service_type,            ?SERVICE_TYPE_NULL},
         {source_addr_ton,         ?TON_INTERNATIONAL},
         {source_addr_npi,         ?NPI_ISDN},
         {source_addr,             ?NULL_C_OCTET_STRING},
         {dest_addr_ton,           ?TON_INTERNATIONAL},
         {dest_addr_npi,           ?NPI_ISDN},
         {esm_class,               ?ESM_CLASS_DEFAULT},
         {protocol_id,             ?PROTOCOL_IDENTIFIER_GSM},
         {priority_flag,           ?PRIORITY_FLAG_GSM_SMS_NON_PRIORITY},
         {schedule_delivery_time,  ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
         {validity_period,         ?VALIDITY_PERIOD_DEFAULT},
         {registered_delivery,     ?REGISTERED_DELIVERY_DEFAULT},
         {replace_if_present_flag, ?REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE},
         {data_coding,             ?ENCODING_SCHEME_LATIN_1},
         {sm_default_msg_id,       ?NULL_INTEGER},
         {short_message,           ?NULL_OCTET_STRING},
         % {alert_on_msg_delivery,   ?ALERT_ON_MESSAGE_DELIVERY_DEFAULT},
         {callback_num,            []},
         % {callback_num_atag,       []},
         % {callback_num_pres_ind,   []},
         % {dest_addr_np_country,    ?COUNTRY_CODE_SPAIN},
         % {dest_addr_np_resolution, ?DEST_ADDR_NP_RESOLUTION_NO_QUERY_PERFORMED},
         {dest_addr_subunit,       ?ADDR_SUBUNIT_UNKNOWN},
         {dest_bearer_type,        ?BEARER_TYPE_SMS},
         {dest_network_type,       ?NETWORK_TYPE_GSM},
         {dest_telematics_id,      #telematics_id{}},  % record defaults
         % {display_time,            ?DISPLAY_TIME_DEFAULT},
         % {language_indicator,      ?LANGUAGE_INDICATOR_UNSPECIFIED},
         {message_payload,         ?NULL_OCTET_STRING},
         % {ms_validity,             #ms_validity_absolute{}},  % record defaults
         {payload_type,            ?PAYLOAD_TYPE_DEFAULT},
         % {privacy_indicator,       ?PRIVACY_INDICATOR_NOT_RESTRICTED},
         {sar_segment_seqnum,      1},
         {sar_total_segments,      1},
         {set_dpf,                 ?SET_DPF_REQUESTED},
         {source_addr_subunit,     ?ADDR_SUBUNIT_UNKNOWN},
         {source_bearer_type,      ?BEARER_TYPE_SMS},
         {source_network_type,     ?NETWORK_TYPE_GSM},
         {source_telematics_id,    #telematics_id{}}]).  % record defaults

%%%
% submit_sm_resp
%
% %@TODO Remove ?DELIVERY_FAILURE_REASON if never needed
%%
-define(SUBMIT_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [?ADDITIONAL_STATUS_INFO_TEXT,         
              ?CONGESTION_STATE,
              % ?DELIVERY_FAILURE_REASON,   % data_sm_resp only
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

-define(SUBMIT_SM_RESP_DEFAULTS, 
        [{additional_status_info_text, ?NULL_C_OCTET_STRING},
         {dpf_result,                  ?DPF_RESULT_NOT_SET}]).

%%%
% data_sm Syntax
%%
-define(DATA_SM,
        ?PDU([?SERVICE_TYPE,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_65,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_65,
              ?ESM_CLASS,
              ?REGISTERED_DELIVERY,
              ?DATA_CODING],
             [% ?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              % ?CALLBACK_NUM_ATAG,         % (TDMA)
              % ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              % ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              % ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              % ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              % ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              % ?DISPLAY_TIME,              % (CDMA, TDMA)
              % ?ITS_REPLY_TYPE,            % (CDMA)
              % ?ITS_SESSION_INFO,          % (CDMA)
              % ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MANDATORY_MESSAGE_PAYLOAD, % Notice that it's mandatory
              ?MORE_MESSAGES_TO_SEND,
              ?MS_MSG_WAIT_FACILITIES,
              % ?MS_VALIDITY,               % (CDMA, TDMA)
              ?PAYLOAD_TYPE,
              % ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SET_DPF,
              % ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              % ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              % ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(DATA_SM_DEFAULTS, 
        [{service_type,            ?SERVICE_TYPE_NULL},
         {source_addr_ton,         ?TON_INTERNATIONAL},
         {source_addr_npi,         ?NPI_ISDN},
         {source_addr,             ?NULL_C_OCTET_STRING},
         {dest_addr_ton,           ?TON_INTERNATIONAL},
         {dest_addr_npi,           ?NPI_ISDN},
         {esm_class,               ?ESM_CLASS_DEFAULT},
         {registered_delivery,     ?REGISTERED_DELIVERY_DEFAULT},
         {data_coding,             ?ENCODING_SCHEME_LATIN_1},
         % {alert_on_msg_delivery,   ?ALERT_ON_MESSAGE_DELIVERY_DEFAULT},
         {callback_num,            []},
         % {callback_num_atag,       []},
         % {callback_num_pres_ind,   []},
         % {dest_addr_np_country,    ?COUNTRY_CODE_SPAIN},
         % {dest_addr_np_resolution, ?DEST_ADDR_NP_RESOLUTION_NO_QUERY_PERFORMED},
         {dest_addr_subunit,       ?ADDR_SUBUNIT_UNKNOWN},
         {dest_bearer_type,        ?BEARER_TYPE_SMS},
         {dest_network_type,       ?NETWORK_TYPE_GSM},
         {dest_telematics_id,      #telematics_id{}},  % record defaults
         % {display_time,            ?DISPLAY_TIME_DEFAULT},
         % {language_indicator,      ?LANGUAGE_INDICATOR_UNSPECIFIED},
         {message_payload,         ?NULL_OCTET_STRING},
         {more_messages_to_send,   ?MORE_MESSAGES_TO_SEND_YES},
         % {ms_validity,             #ms_validity_absolute{}},  % record defaults
         {payload_type,            ?PAYLOAD_TYPE_DEFAULT},
         % {privacy_indicator,       ?PRIVACY_INDICATOR_NOT_RESTRICTED},
         {sar_segment_seqnum,      1},
         {sar_total_segments,      1},
         {set_dpf,                 ?SET_DPF_REQUESTED},
         {source_addr_subunit,     ?ADDR_SUBUNIT_UNKNOWN},
         {source_bearer_type,      ?BEARER_TYPE_SMS},
         {source_network_type,     ?NETWORK_TYPE_GSM},
         {source_telematics_id,    #telematics_id{}}]).  % record defaults

%%%
% data_sm_resp
%%
-define(DATA_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

-define(DATA_SM_RESP_DEFAULTS, 
        [{message_id,                  ?NULL_C_OCTET_STRING},
         {additional_status_info_text, ?NULL_C_OCTET_STRING},
         {dpf_result,                  ?DPF_RESULT_NOT_SET}]).

%%%
% submit_multi
%
% %@TODO Remove ?MORE_MESSAGES_TO_SEND and ?SET_DPF if never needed.
%%
-define(SUBMIT_MULTI,
        ?PDU([?SERVICE_TYPE,
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
             [% ?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BILLING_IDENTIFICATION,
              ?CALLBACK_NUM,
              % ?CALLBACK_NUM_ATAG,         % (TDMA)
              % ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              % ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              % ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              % ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_BEARER_TYPE,
              ?DEST_NETWORK_ID,
              ?DEST_NETWORK_TYPE,
              ?DEST_NODE_ID,
              % ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_TELEMATICS_ID,
              ?DEST_PORT,
              % ?DISPLAY_TIME,              % (CDMA, TDMA)
              % ?ITS_REPLY_TYPE,            % (CDMA)
              % ?ITS_SESSION_INFO,          % (CDMA)
              % ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?OPTIONAL_MESSAGE_PAYLOAD,  % Notice that it's optional
              % ?MORE_MESSAGES_TO_SEND,   % makes sense on submit multi?
              ?MS_MSG_WAIT_FACILITIES,
              % ?MS_VALIDITY,               % (CDMA, TDMA)
              ?PAYLOAD_TYPE,
              % ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?QOS_TIME_TO_LIVE,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              % ?SET_DPF,                 % makes sense on submit multi?
              % ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_BEARER_TYPE,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NETWORK_TYPE,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              % ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?SOURCE_TELEMATICS_ID,
              ?USER_MESSAGE_REFERENCE,
              % ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(SUBMIT_MULTI_DEFAULTS, 
        [{service_type,            ?SERVICE_TYPE_NULL},
         {source_addr_ton,         ?TON_INTERNATIONAL},
         {source_addr_npi,         ?NPI_ISDN},
         {source_addr,             ?NULL_C_OCTET_STRING},
         {esm_class,               ?ESM_CLASS_DEFAULT},
         {protocol_id,             ?PROTOCOL_IDENTIFIER_GSM},
         {priority_flag,           ?PRIORITY_FLAG_GSM_SMS_NON_PRIORITY},
         {schedule_delivery_time,  ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
         {validity_period,         ?VALIDITY_PERIOD_DEFAULT},
         {registered_delivery,     ?REGISTERED_DELIVERY_DEFAULT},
         {replace_if_present_flag, ?REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE},
         {data_coding,             ?ENCODING_SCHEME_LATIN_1},
         {sm_default_msg_id,       ?NULL_INTEGER},
         {short_message,           ?NULL_OCTET_STRING},
         % {alert_on_msg_delivery,   ?ALERT_ON_MESSAGE_DELIVERY_DEFAULT},
         {callback_num,            []},
         % {callback_num_atag,       []},
         % {callback_num_pres_ind,   []},
         % {dest_addr_np_country,    ?COUNTRY_CODE_SPAIN},
         % {dest_addr_np_resolution, ?DEST_ADDR_NP_RESOLUTION_NO_QUERY_PERFORMED},
         {dest_addr_subunit,       ?ADDR_SUBUNIT_UNKNOWN},
         {dest_bearer_type,        ?BEARER_TYPE_SMS},
         {dest_network_type,       ?NETWORK_TYPE_GSM},
         {dest_telematics_id,      #telematics_id{}},  % record defaults
         % {display_time,            ?DISPLAY_TIME_DEFAULT},
         % {language_indicator,      ?LANGUAGE_INDICATOR_UNSPECIFIED},
         {message_payload,         ?NULL_OCTET_STRING},
         % {more_messages_to_send,   ?MORE_MESSAGES_TO_SEND_YES},
         % {ms_validity,             #ms_validity_absolute{}},  % record defaults
         {payload_type,            ?PAYLOAD_TYPE_DEFAULT},
         % {privacy_indicator,       ?PRIVACY_INDICATOR_NOT_RESTRICTED},
         {sar_segment_seqnum,      1},
         {sar_total_segments,      1},
         % {set_dpf,                 ?SET_DPF_REQUESTED},
         {source_addr_subunit,     ?ADDR_SUBUNIT_UNKNOWN},
         {source_bearer_type,      ?BEARER_TYPE_SMS},
         {source_network_type,     ?NETWORK_TYPE_GSM},
         {source_telematics_id,    #telematics_id{}}]).  % record defaults

%%%
% %@doc submit_multi_resp.
%
% <p>Unlike some simulators, like SMPPSim 1.1, which they ignore the field
% <tt>no_unsuccess</tt> if no destination was unsuccessful, this implementation
% sets a O on this field if every destination was reached (remember that
% UNSUCCESS_SME was declared as a multivalue and this base type automatically
% encodes the number of elements at the head of the binary).</p>
%
% <p>Notice that the SMS forum recommends the approach followed here.  Excerpt
% from the SMS Forum:</p>
%
% <i>
% <p>When all dests are sucessful, the no_unsuccess field will be set to 0. 
% There are no unsuccessful SME fields then included.</p>
%
% <p>...here are two examples of the no_unsuccess file encodings for 0 and
% then 2 failed destinations</p>
%
% <pre>
% Body:
% 41424300        Message id "ABC"
% 00              0 unsuccessful SMEs
% </pre>
%
% <pre>
% 41424300        Message id "ABC"
% 02              2 unsuccessful SMEs
% 01              TON=1
% 01              NPI=1
% 353535353500    SME "55555"
% 00000005        SMPP Error 0x00000005
% 02              TON=2
% 08              NPI=3
% 363534353500    SME "65455"
% 00000015        SMPP Error 0x00000015
% </pre>
%
% <p>So if the <tt>no_unsuccess</tt> field is non zero, then that number of 
% SMEs must be appended in the form of TON, NPI & Address similarly to how 
% source and destination addresses are normally encoded. Also included per 
% failed address is the 4-octet error code (command status value) associated 
% with the failed address.</p>
% </i>
%
% %@TODO Remove ?DELIVERY_FAILURE_REASON and ?DPF_RESULT if never needed
% %@end
%%
-define(SUBMIT_MULTI_RESP,
        ?PDU([?MESSAGE_ID, 
              ?UNSUCCESS_SME],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?DPF_RESULT,
              ?NETWORK_ERROR_CODE])).

-define(SUBMIT_MULTI_RESP_DEFAULTS, 
        [{additional_status_info_text, ?NULL_C_OCTET_STRING},
         {dpf_result,                  ?DPF_RESULT_NOT_SET}]).

%%%
% deliver_sm
%%
-define(DELIVER_SM,
        ?PDU([?SERVICE_TYPE,
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
              % ?CALLBACK_NUM_ATAG,         % (TDMA)
              % ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              % ?DEST_ADDR_NP_COUNTRY,      % (CDMA, TDMA)
              % ?DEST_ADDR_NP_INFORMATION,  % (CDMA, TDMA)
              % ?DEST_ADDR_NP_RESOLUTION,   % (CDMA, TDMA)
              ?DEST_ADDR_SUBUNIT,
              ?DEST_NETWORK_ID,
              ?DEST_NODE_ID,
              % ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_PORT,
              ?DPF_RESULT,
              % ?ITS_REPLY_TYPE,            % (CDMA)
              % ?ITS_SESSION_INFO,          % (CDMA)
              % ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?OPTIONAL_MESSAGE_PAYLOAD,  % Notice that it's optional
              ?OPTIONAL_MESSAGE_STATE,
              ?NETWORK_ERROR_CODE,
              ?PAYLOAD_TYPE,
               % ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              ?RECEIPTED_MESSAGE_ID,
              ?SAR_MSG_REF_NUM,
              ?SAR_SEGMENT_SEQNUM,
              ?SAR_TOTAL_SEGMENTS,
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_NETWORK_ID,
              ?SOURCE_NODE_ID,
              ?SOURCE_PORT,
              % ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?USER_MESSAGE_REFERENCE,
              % ?USER_RESPONSE_CODE,        % (CDMA, TDMA)
              ?USSD_SERVICE_OP])).

-define(DELIVER_SM_DEFAULTS,
        [{service_type,            ?SERVICE_TYPE_NULL},
         {source_addr_ton,         ?TON_INTERNATIONAL},
         {source_addr_npi,         ?NPI_ISDN},
         {source_addr,             ?NULL_C_OCTET_STRING},
         {dest_addr_ton,           ?TON_INTERNATIONAL},
         {dest_addr_npi,           ?NPI_ISDN},
         {esm_class,               ?ESM_CLASS_DEFAULT},
         {protocol_id,             ?PROTOCOL_IDENTIFIER_GSM},
         {priority_flag,           ?PRIORITY_FLAG_GSM_SMS_NON_PRIORITY},
         {schedule_delivery_time,  ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
         {validity_period,         ?VALIDITY_PERIOD_DEFAULT},
         {registered_delivery,     ?REGISTERED_DELIVERY_DEFAULT},
         {replace_if_present_flag, ?REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE},
         {data_coding,             ?ENCODING_SCHEME_LATIN_1},
         {sm_default_msg_id,       ?NULL_INTEGER},
         {short_message,           ?NULL_OCTET_STRING},
         {callback_num,            []},
         % {callback_num_atag,       []},
         % {callback_num_pres_ind,   []},
         % {dest_addr_np_country,    ?COUNTRY_CODE_SPAIN},
         % {dest_addr_np_resolution, ?DEST_ADDR_NP_RESOLUTION_NO_QUERY_PERFORMED},
         {dest_addr_subunit,       ?ADDR_SUBUNIT_UNKNOWN},
         {dpf_result,              ?DPF_RESULT_NOT_SET},
         % {language_indicator,      ?LANGUAGE_INDICATOR_UNSPECIFIED},
         {message_payload,         ?NULL_OCTET_STRING},
         {payload_type,            ?PAYLOAD_TYPE_DEFAULT},
         % {privacy_indicator,       ?PRIVACY_INDICATOR_NOT_RESTRICTED},
         {sar_segment_seqnum,      1},
         {sar_total_segments,      1},
         {source_addr_subunit,     ?ADDR_SUBUNIT_UNKNOWN}]).

%%%
% deliver_sm_resp Syntax
%%
-define(DELIVER_SM_RESP, 
        ?PDU([?MESSAGE_ID],
             [?ADDITIONAL_STATUS_INFO_TEXT,
              ?CONGESTION_STATE,
              ?DELIVERY_FAILURE_REASON,
              ?NETWORK_ERROR_CODE])).

-define(DELIVER_SM_RESP_DEFAULTS, 
        [{message_id,                  ?NULL_C_OCTET_STRING},
         {additional_status_info_text, ?NULL_C_OCTET_STRING}]).

%%%
% broadcast_sm Syntax
%
% %@TODO Confirm that the <tt>message_payload</tt> is mandatory
%%
-define(BROADCAST_SM,
        ?PDU([?SERVICE_TYPE,
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
              % ?ALERT_ON_MESSAGE_DELIVERY, % (CDMA)
              ?BROADCAST_CHANNEL_INDICATOR,
              % ?BROADCAST_CONTENT_TYPE_INFO, (CDMA, TDMA)
              ?BROADCAST_MESSAGE_CLASS,
              % ?BROADCAST_SERVICE_GROUP,   % (CDMA, TDMA)
              ?CALLBACK_NUM,
              % ?CALLBACK_NUM_ATAG,         % (TDMA)
              % ?CALLBACK_NUM_PRES_IND,     % (TDMA)
              ?DEST_ADDR_SUBUNIT,
              % ?DEST_SUBADDRESS,           % (CDMA, TDMA)
              ?DEST_PORT,
              % ?DISPLAY_TIME,              % (CDMA, TDMA)
              % ?LANGUAGE_INDICATOR,        % (CDMA, TDMA)
              ?MANDATORY_MESSAGE_PAYLOAD,  % Notice that it's mandatory
              % ?MS_VALIDITY,               % (CDMA, TDMA)
              ?PAYLOAD_TYPE,
              % ?PRIVACY_INDICATOR,         % (CDMA, TDMA)
              % ?SMS_SIGNAL,                % (TDMA)
              ?SOURCE_ADDR_SUBUNIT,
              ?SOURCE_PORT,
              % ?SOURCE_SUBADDRESS,         % (CDMA, TDMA)
              ?USER_MESSAGE_REFERENCE])).

-define(BROADCAST_SM_DEFAULTS, 
        [{service_type,              ?SERVICE_TYPE_NULL},
         {source_addr_ton,           ?TON_INTERNATIONAL},
         {source_addr_npi,           ?NPI_ISDN},
         {source_addr,               ?NULL_C_OCTET_STRING},
         {message_id,                ?NULL_C_OCTET_STRING},
         {priority_flag,             ?PRIORITY_FLAG_GSM_SMS_NON_PRIORITY},
         {schedule_delivery_time,    ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
         {validity_period,           ?VALIDITY_PERIOD_DEFAULT},
         {replace_if_present_flag,   ?REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE},
         {data_coding,               ?ENCODING_SCHEME_LATIN_1},
         {sm_default_msg_id,         ?NULL_INTEGER},
         {broadcast_area_identifier, []},
         {broadcast_content_type,    #broadcast_content_type{}},
         {broadcast_rep_num,         ?BROADCAST_REP_NUM_DEFAULT},
         % {alert_on_msg_delivery,       ?ALERT_ON_MESSAGE_DELIVERY_DEFAULT},
         {broadcast_channel_indicator, ?BROADCAST_CHANNEL_INDICATOR_BASIC},
         {broadcast_message_class,     ?BROADCAST_MESSAGE_CLASS_NO_CLASS},
         {callback_num,            []},
         % {callback_num_atag,       []},
         % {callback_num_pres_ind,   []},
         {dest_addr_subunit,       ?ADDR_SUBUNIT_UNKNOWN},
         % {display_time,            ?DISPLAY_TIME_DEFAULT},
         % {language_indicator,      ?LANGUAGE_INDICATOR_UNSPECIFIED},
         {message_payload,         ?NULL_OCTET_STRING},
         % {ms_validity,             #ms_validity_absolute{}},
         {payload_type,            ?PAYLOAD_TYPE_DEFAULT},
         % {privacy_indicator,       ?PRIVACY_INDICATOR_NOT_RESTRICTED},
         {source_addr_subunit,     ?ADDR_SUBUNIT_UNKNOWN}]).

%%%
% broadcast_sm_resp Syntax
%%
-define(BROADCAST_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [?BROADCAST_ERROR_STATUS,
              ?CONGESTION_STATE, 
              ?FAILED_BROADCAST_AREA_IDENTIFIER])).

-define(BROADCAST_SM_RESP_DEFAULTS, []).

%%%
% cancel_sm Syntax
%%
-define(CANCEL_SM,
        ?PDU([?SERVICE_TYPE,
              ?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?DEST_ADDR_TON,
              ?DEST_ADDR_NPI,
              ?DESTINATION_ADDR_21],
             [])).

-define(CANCEL_SM_DEFAULTS, 
        [{service_type,    ?SERVICE_TYPE_NULL},
         {source_addr_ton, ?TON_INTERNATIONAL},
         {source_addr_npi, ?NPI_ISDN},
         {source_addr,     ?NULL_C_OCTET_STRING},
         {dest_addr_ton,   ?TON_INTERNATIONAL},
         {dest_addr_npi,   ?NPI_ISDN}]).

%%%
% cancel_sm_resp Syntax
%%
-define(CANCEL_SM_RESP, 
        ?PDU([],
             [?CONGESTION_STATE])).

-define(CANCEL_SM_RESP_DEFAULTS, []). 

%%%
% query_sm Syntax
%%
-define(QUERY_SM,
        ?PDU([?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [])).

-define(QUERY_SM_DEFAULTS, 
        [{source_addr_ton, ?TON_INTERNATIONAL},
         {source_addr_npi, ?NPI_ISDN},
         {source_addr,     ?NULL_C_OCTET_STRING}]).

%%%
% query_sm_resp Syntax
%%
-define(QUERY_SM_RESP,
        ?PDU([?MESSAGE_ID,
              ?FINAL_DATE,
              ?MESSAGE_STATE,
              ?ERROR_CODE],
             [?CONGESTION_STATE])).

-define(QUERY_SM_RESP_DEFAULTS, 
        [{final_date,    ?FINAL_DATE_FINAL_STATE_NOT_REACHED},
         {message_state, ?MESSAGE_STATE_ENROUTE},
         {error_code,    ?NULL_INTEGER}]).

%%%
% replace_sm Syntax
%%
-define(REPLACE_SM,
        ?PDU([?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21,
              ?SCHEDULE_DELIVERY_TIME,
              ?VALIDITY_PERIOD,
              ?REGISTERED_DELIVERY,
              ?SM_DEFAULT_MSG_ID,
              ?SHORT_MESSAGE],
             [?OPTIONAL_MESSAGE_PAYLOAD])).  % Notice that it's optional

-define(REPLACE_SM_DEFAULTS, 
        [{source_addr_ton,        ?TON_INTERNATIONAL},
         {source_addr_npi,        ?NPI_ISDN},
         {source_addr,            ?NULL_C_OCTET_STRING},
         {schedule_delivery_time, ?SCHEDULE_DELIVERY_TIME_IMMEDIATE},
         {validity_period,        ?VALIDITY_PERIOD_DEFAULT},
         {registered_delivery,    ?REGISTERED_DELIVERY_DEFAULT},
         {sm_default_msg_id,      ?NULL_INTEGER},
         {short_message,          ?NULL_OCTET_STRING},
         {message_payload,        ?NULL_OCTET_STRING}]).

%%%
% replace_sm_resp Syntax
%%
-define(REPLACE_SM_RESP, 
        ?PDU([],
             [?CONGESTION_STATE])).

-define(REPLACE_SM_RESP_DEFAULTS, []).

%%%
% query_broadcast_sm Syntax
%%
-define(QUERY_BROADCAST_SM,
        ?PDU([?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [?USER_MESSAGE_REFERENCE])).

-define(QUERY_BROADCAST_SM_DEFAULTS, 
        [{source_addr_ton, ?TON_INTERNATIONAL},
         {source_addr_npi, ?NPI_ISDN},
         {source_addr,     ?NULL_C_OCTET_STRING}]).

%%%
% query_broadcast_sm_resp Syntax
%%
-define(QUERY_BROADCAST_SM_RESP,
        ?PDU([?MESSAGE_ID],
             [?CONGESTION_STATE,
              ?MANDATORY_MESSAGE_STATE,
              ?BROADCAST_AREA_IDENTIFIER,
              ?BROADCAST_AREA_SUCCESS,
              ?BROADCAST_END_TIME, 
              ?USER_MESSAGE_REFERENCE])).

-define(QUERY_BROADCAST_SM_RESP_DEFAULTS, 
        [{message_state,             ?MESSAGE_STATE_ENROUTE},
         {broadcast_area_identifier, []},
         {broadcast_area_success,    []}]).

%%%
% cancel_broadcast_sm Syntax
%%
-define(CANCEL_BROADCAST_SM,
        ?PDU([?SERVICE_TYPE,
              ?MESSAGE_ID,
              ?SOURCE_ADDR_TON,
              ?SOURCE_ADDR_NPI,
              ?SOURCE_ADDR_21],
             [?OPTIONAL_BROADCAST_CONTENT_TYPE, 
              ?USER_MESSAGE_REFERENCE])).


-define(CANCEL_BROADCAST_SM_DEFAULTS, 
        [{service_type,    ?SERVICE_TYPE_NULL},
         {source_addr_ton, ?TON_INTERNATIONAL},
         {source_addr_npi, ?NPI_ISDN},
         {source_addr,     ?NULL_C_OCTET_STRING}]).

%%%
% cancel_broadcast_sm_resp Syntax
%%
-define(CANCEL_BROADCAST_SM_RESP,
        ?PDU([],
             [?CONGESTION_STATE])).

-define(CANCEL_BROADCAST_SM_RESP_DEFAULTS, []).


%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%
% %@spec 
%
% %@doc 
%
% <dl>
%   <dt>: </dt><dd>
%   </dd>
% </dl>
%%
%-record(, {}).

-endif.  % -ifndef(smpp_pdu)

