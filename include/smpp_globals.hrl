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
% @doc SMPP Global definitions.
%
% <p>Some global definitions used in the SMPP protocol implementation.</p>
%
% <p>As a guideline, some comments reference the section number of the
% document [SMPP 5.0].</p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [18 Feb 2004]
% 
% <ul>
%   <li>$\0 removed from the NULL_C_OCTET_STRING macro.</li>
% </ul>
%
%
% <h2>References</h2>
% <dl>
%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification.
%     Version 5.0. SMS Forum.
%   </dd>
% </dl>
%
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {27 Apr 2003} {@time}.
% @end
%%

-ifndef(smpp_globals).
-define(smpp_globals, true).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
%%%
% Command Ids
%
% %@see section 4.7.5 on [SMPP 5.0]
%%
-define(COMMAND_ID_BIND_RECEIVER,            16#00000001).
-define(COMMAND_ID_BIND_TRANSMITTER,         16#00000002).
-define(COMMAND_ID_QUERY_SM,                 16#00000003).
-define(COMMAND_ID_SUBMIT_SM,                16#00000004).
-define(COMMAND_ID_DELIVER_SM,               16#00000005).
-define(COMMAND_ID_UNBIND,                   16#00000006).
-define(COMMAND_ID_REPLACE_SM,               16#00000007).
-define(COMMAND_ID_CANCEL_SM,                16#00000008).
-define(COMMAND_ID_BIND_TRANSCEIVER,         16#00000009).
-define(COMMAND_ID_OUTBIND,                  16#0000000B).
-define(COMMAND_ID_ENQUIRE_LINK,             16#00000015).
-define(COMMAND_ID_SUBMIT_MULTI,             16#00000021).
-define(COMMAND_ID_ALERT_NOTIFICATION,       16#00000102).
-define(COMMAND_ID_DATA_SM,                  16#00000103).
-define(COMMAND_ID_BROADCAST_SM,             16#00000111).
-define(COMMAND_ID_QUERY_BROADCAST_SM,       16#00000112).
-define(COMMAND_ID_CANCEL_BROADCAST_SM,      16#00000113).
-define(COMMAND_ID_GENERIC_NACK,             16#80000000).
-define(COMMAND_ID_BIND_RECEIVER_RESP,       16#80000001).
-define(COMMAND_ID_BIND_TRANSMITTER_RESP,    16#80000002).
-define(COMMAND_ID_QUERY_SM_RESP,            16#80000003).
-define(COMMAND_ID_SUBMIT_SM_RESP,           16#80000004).
-define(COMMAND_ID_DELIVER_SM_RESP,          16#80000005).
-define(COMMAND_ID_UNBIND_RESP,              16#80000006).
-define(COMMAND_ID_REPLACE_SM_RESP,          16#80000007).
-define(COMMAND_ID_CANCEL_SM_RESP,           16#80000008).
-define(COMMAND_ID_BIND_TRANSCEIVER_RESP,    16#80000009).
-define(COMMAND_ID_ENQUIRE_LINK_RESP,        16#80000015).
-define(COMMAND_ID_SUBMIT_MULTI_RESP,        16#80000021).
-define(COMMAND_ID_DATA_SM_RESP,             16#80000103).
-define(COMMAND_ID_BROADCAST_SM_RESP,        16#80000111).
-define(COMMAND_ID_QUERY_BROADCAST_SM_RESP,  16#80000112).
-define(COMMAND_ID_CANCEL_BROADCAST_SM_RESP, 16#80000113).

%%%
% Default Port Number (Standardised by IANA)
%
% %@see section 2.2 on [SMPP 5.0]
%% 
-define(DEFAULT_SMPP_PORT, 2775).

%%%
% Error Codes
%
% %@see section 4.7.6 on [SMPP 5.0]
%%
-define(ESME_ROK,                 16#00000000). % No Error
-define(ESME_RINVMSGLEN,          16#00000001). % Message Length is invalid
-define(ESME_RINVCMDLEN,          16#00000002). % Command Length is invalid
-define(ESME_RINVCMDID,           16#00000003). % Invalid Command ID
-define(ESME_RINVBNDSTS,          16#00000004). % Incorrect BIND Status for 
                                                % given command
-define(ESME_RALYBND,             16#00000005). % ESME Already in Bound State
-define(ESME_RINVPRTFLG,          16#00000006). % Invalid Priority Flag
-define(ESME_RINVREGDLVFLG,       16#00000007). % Invalid Registered Delivery
                                                % Flag
-define(ESME_RSYSERR,             16#00000008). % System Error
-define(ESME_RINVSRCADR,          16#0000000A). % Invalid Source Address
-define(ESME_RINVDSTADR,          16#0000000B). % Invalid Dest Addr
-define(ESME_RINVMSGID,           16#0000000C). % Message ID is invalid
-define(ESME_RBINDFAIL,           16#0000000D). % Bind Failed
-define(ESME_RINVPASWD,           16#0000000E). % Invalid Password
-define(ESME_RINVSYSID,           16#0000000F). % Invalid System ID
-define(ESME_RCANCELFAIL,         16#00000011). % Cancel SM Failed
-define(ESME_RREPLACEFAIL,        16#00000013). % Replace SM Failed
-define(ESME_RMSGQFUL,            16#00000014). % Message Queue Full
-define(ESME_RINVSERTYP,          16#00000015). % Invalid Service Type
-define(ESME_RINVNUMDESTS,        16#00000033). % Invalid destinations number
-define(ESME_RINVDLNAME,          16#00000034). % Invalid Distribution List
                                                % name
-define(ESME_RINVDESTFLAG,        16#00000040). % Invalid Destination flag
                                                % (submit_multi)
-define(ESME_RINVSUBREP,          16#00000042). % Invalid submit with replace
-define(ESME_RINVESMCLASS,        16#00000043). % Invalid esm_class field data
-define(ESME_RCNTSUBDL,           16#00000044). % Cannot Submit to Distribution
                                                % List
-define(ESME_RSUBMITFAIL,         16#00000045). % submit_sm or submit_multi 
                                                % failed
-define(ESME_RINVSRCTON,          16#00000048). % Invalid Source address TON
-define(ESME_RINVSRCNPI,          16#00000049). % Invalid Source address NPI
-define(ESME_RINVDSTTON,          16#00000050). % Invalid Destination addr TON
-define(ESME_RINVDSTNPI,          16#00000051). % Invalid Destination addr NPI
-define(ESME_RINVSYSTYP,          16#00000053). % Invalid system_type field
-define(ESME_RINVREPFLAG,         16#00000054). % Invalid replace_if_present 
                                                % Flag
-define(ESME_RINVNUMMSGS,         16#00000055). % Invalid number of messages
-define(ESME_RTHROTTLED,          16#00000058). % Throttling error (ESME has 
                                                % exceeded allowed msg limits)
-define(ESME_RINVSCHED,           16#00000061). % Invalid Scheduled Delivery
                                                % Time
-define(ESME_RINVEXPIRY,          16#00000062). % Invalid message validity 
                                                % period (Expiry time)
-define(ESME_RINVDFTMSGID,        16#00000063). % Predefined Message Invalid or
                                                % Not Found
-define(ESME_RX_T_APPN,           16#00000064). % ESME Receiver Temporary App Err
-define(ESME_RX_P_APPN,           16#00000065). % ESME Receiver Permanent App Err
-define(ESME_RX_R_APPN,           16#00000066). % ESME Receiver Reject Message
-define(ESME_RQUERYFAIL,          16#00000067). % query_sm request failed
-define(ESME_RINVTLVSTREAM,       16#000000C0). % Error in the optional part 
                                                % of the PDU Body
-define(ESME_RTLVNOTALLWD,        16#000000C1). % TLV not allowed
-define(ESME_RINVTLVLEN,          16#000000C2). % Invalid Parameter Length
-define(ESME_RMISSINGTLV,         16#000000C3). % Expected TLV missing
-define(ESME_RINVTLVVAL,          16#000000C4). % Invalid TLV Value
-define(ESME_RDELIVERYFAILURE,    16#000000FE). % Transaction Delivery Failure
-define(ESME_RUNKNOWNERR,         16#000000FF). % Unknown Error
-define(ESME_RSERTYPUNAUTH,       16#00000100). % ESME Not authorised to use
                                                % specified service_type
-define(ESME_RPROHIBITED,         16#00000101). % ESME Prohibited from using
                                                % specified operation
-define(ESME_RSERTYPUNAVAIL,      16#00000102). % Specified service_type is
                                                % unavailable
-define(ESME_RSERTYPDENIED,       16#00000103). % Specified service_type denied
-define(ESME_RINVDCS,             16#00000104). % Invalid Data Coding Scheme
-define(ESME_RINVSRCADDRSUBUNIT,  16#00000105). % Source Address Sub unit
                                                % is invalid
-define(ESME_RINVDSTADDRSUBUNIT,  16#00000106). % Destination Address Sub unit
                                                % is invalid
-define(ESME_RINVBCASTFREQINT,    16#00000107). % Broadcast Frequency Interval
                                                % is invalid
-define(ESME_RINVBCASTALIAS_NAME, 16#00000108). % Invalid Broadcast Alias Name
-define(ESME_RINVBCASTAREAFMT,    16#00000109). % Invalid Broadcast Area Format
-define(ESME_RINVNUMBCAST_AREAS,  16#0000010A). % Number of Broadcast Areas 
                                                % is invalid
-define(ESME_RINVBCASTCNTTYPE,    16#0000010B). % Invalid Broadcast Content 
                                                % Type
-define(ESME_RINVBCASTMSGCLASS,   16#0000010C). % Broadcast Message Class 
                                                % is invalid
-define(ESME_RBCASTFAIL,          16#0000010D). % broadcast_sm operation failed
-define(ESME_RBCASTQUERYFAIL,     16#0000010E). % query_broadcast_sm failed
-define(ESME_RBCASTCANCELFAIL,    16#0000010F). % cancel_broadcast_sm failed
-define(ESME_RINVBCAST_REP,       16#00000110). % Number of Repeated Broadcasts
                                                % is invalid
-define(ESME_RINVBCASTSRVGRP,     16#00000111). % Broadcast Service Group
                                                % is invalid
-define(ESME_RINVBCASTCHANIND,    16#00000112). % Broadcast Channel Indicator
                                                % is invalid

% SMPP 3.4 Error Code Synonyms
%
% %@deprecated For backward compatibility only.
-define(ESME_RINVOPTPARSTREAM,    16#000000C0). % Error in the optional part of
                                                % the PDU Body
-define(ESME_ROPTPARNOTALLWD,     16#000000C1). % Optional Parameter not
                                                % allowed
-define(ESME_RINVPARLEN,          16#000000C2). % Invalid Parameter Length
-define(ESME_RMISSINGOPTPARAM,    16#000000C3). % Expected Optional Parameter
                                                % missing
-define(ESME_RINVOPTPARAMVAL,     16#000000C4). % Invalid Optional Parameter
                                                % Value

%%%
% Null Settings
%
% %@see section 3.1.1 on [SMPP 5.0]
%%
-define(NULL_CHARACTER, 0).
-define(NULL_INTEGER,   0).
-define(NULL_C_OCTET_STRING, "").
-define(NULL_OCTET_STRING,   "").


%%%
% Timers default values
%
% %@doc Besides the timers declared on [SMPP 5.0], a rebind timer default is
% defined.  This timer sets the time lapse in which the ESME should try to
% rebind once the MC becomes unavailable.</p>
%
% %@TODO Review these default values.
%
% %@see section 2.7 on [SMPP 5.0]
%%
-define(SESSION_INIT_TIME, 180000).  % 3 minutes
-define(ENQUIRE_LINK_TIME,  60000).  % 1 minute
-define(INACTIVITY_TIME, infinity).  % No timeout, never drop the session.
-define(RESPONSE_TIME,      60000).  % 1 minute
-define(REBIND_TIME,         5000).  % Tries to rebind every 5 seconds.

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

-endif.  % -ifndef(smpp_globals)
