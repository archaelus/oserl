%%%
% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
% @doc SMPP Operation library.
%
% <p>SMPP Operation PDU definitions.</p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [10 Feb 2004]
%
% <ul>
%   <li>Implemented <tt>get_param/2</tt> and <tt>set_param/3</tt>, replacements
%     for <tt>pdu_syntax:get_value/2</tt> and <tt>pdu_syntax:set_value/3</tt> 
%     respectively.
%   </li>
% </ul>
%
% [17 Feb 2004]
%
% <ul>
%   <li>new_pdu/5 function removed.  PDU defaults MACROS do no longer exist.
%   </li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.2 alpha, {24 May 2003} {@time}.
% @end
%%
-module(operation).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("smpp_globals.hrl").
-include("smpp_pdu.hrl").

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([get_param/2, 
         set_param/3, 
         merge_params/2,
         new_bind_transmitter/2,
         new_bind_transmitter_resp/3,
         new_bind_receiver/2,
         new_bind_receiver_resp/3,
         new_bind_transceiver/2,
         new_bind_transceiver_resp/3,
         new_outbind/2,
         new_unbind/2,
         new_unbind_resp/3,
         new_enquire_link/2,
         new_enquire_link_resp/3,
         new_alert_notification/2,
         new_generic_nack/3,
         new_submit_sm/2,
         new_submit_sm_resp/3,
         new_data_sm/2,
         new_data_sm_resp/3,
         new_submit_multi/2,
         new_submit_multi_resp/3,
         new_deliver_sm/2,
         new_deliver_sm_resp/3,
         new_broadcast_sm/2,
         new_broadcast_sm_resp/3,
         new_cancel_sm/2,
         new_cancel_sm_resp/3,
         new_query_sm/2,
         new_query_sm_resp/3,
         new_replace_sm/2,
         new_replace_sm_resp/3,
         new_query_broadcast_sm/2,
         new_query_broadcast_sm_resp/3,
         new_cancel_broadcast_sm/2,
         new_cancel_broadcast_sm_resp/3,
         pack/1,
         esme_pack/1,
         mc_pack/1,
         unpack/1,
         esme_unpack/1,
         mc_unpack/1,
         response_command_id/1,
         request_command_id/1,
         request_failure_code/1]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
%%%
% %@doc Some macros used to simplify packing/unpacking functions
%
% <p>Notice that the commands are proven in order of declaration, it'll be
% important to have the PDU ids sorted by frequency (most frequent operations
% should appear first in the list).</p>
%
% <p>This macros may be redefined to fit special needs of the ESME or MC to
% be implemented, for example, unused operations could be removed or placed
% at the end of the list.</p>
%
% <p>Packing operation is a little bit more efficient, to speed up unpacking
% and since this library was initially conceived for the development of
% ESMEs, usually operations issued by the MC (unpacked by the ESME) appear 
% first on the lists.</p>
%
% <dl>
%   <dt>ESME_ISSUED_OPERATIONS</dt><dd>Every PDU sent by an ESME (received by
%     MC) matches one type specifier of the list. 
%
%     <p>This macro is used by functions esme_pack and mc_unpack; while 
%     packing on an ESME (unpacking on a MC) only need to worry about these
%     PDUs.
%   </dd>
%   <dt>MC_ISSUED_OPERATIONS</dt><dd>Every PDU received by an ESME (sent by a
%     MC) matches one type specifier on this list. 
%
%     <p>This macro is used by function esme_unpack and mc_pack; while 
%     unpacking on an ESME (packing on a MC) only need to worry about these
%     PDUs.
%   </dd>
%   <dt>ANY_OPERATION</dt><dd>Every PDU matches this type specifier list.
%
%     <p>This macro is used by functions pack and unpack; these generic 
%     functions are able to handle any PDU.
%   </dd>
% </dl>
% %@end
%%
-define(ESME_ISSUED_OPERATIONS, 
        [?COMMAND_ID_DATA_SM,                  % Both sides
         ?COMMAND_ID_DATA_SM_RESP,             % Both sides
         ?COMMAND_ID_DELIVER_SM_RESP,          % Issued by ESME only
         ?COMMAND_ID_SUBMIT_SM,                % Issued by ESME only
         ?COMMAND_ID_SUBMIT_MULTI,             % Issued by ESME only
         ?COMMAND_ID_BROADCAST_SM,             % Issued by ESME only
         ?COMMAND_ID_QUERY_SM,                 % Issued by ESME only
         ?COMMAND_ID_REPLACE_SM,               % Issued by ESME only
         ?COMMAND_ID_CANCEL_SM,                % Issued by ESME only
         ?COMMAND_ID_QUERY_BROADCAST_SM,       % Issued by ESME only
         ?COMMAND_ID_CANCEL_BROADCAST_SM,      % Issued by ESME only
         ?COMMAND_ID_ENQUIRE_LINK,             % Both sides
         ?COMMAND_ID_ENQUIRE_LINK_RESP,        % Both sides
         ?COMMAND_ID_GENERIC_NACK,             % Both sides
         ?COMMAND_ID_UNBIND,                   % Both sides
         ?COMMAND_ID_UNBIND_RESP,              % Both sides
         ?COMMAND_ID_BIND_RECEIVER,            % Issued by ESME only
         ?COMMAND_ID_BIND_TRANSCEIVER,         % Issued by ESME only
         ?COMMAND_ID_BIND_TRANSMITTER]).       % Issued by ESME only
-define(MC_ISSUED_OPERATIONS,
        [?COMMAND_ID_DATA_SM,                  % Both sides
         ?COMMAND_ID_DATA_SM_RESP,             % Both sides
         ?COMMAND_ID_DELIVER_SM,               % Issued by MC only
         ?COMMAND_ID_SUBMIT_SM_RESP,           % Issued by MC only
         ?COMMAND_ID_SUBMIT_MULTI_RESP,        % Issued by MC only
         ?COMMAND_ID_ALERT_NOTIFICATION,       % Issued by MC only
         ?COMMAND_ID_BROADCAST_SM_RESP,        % Issued by MC only
         ?COMMAND_ID_QUERY_SM_RESP,            % Issued by MC only
         ?COMMAND_ID_REPLACE_SM_RESP,          % Issued by MC only
         ?COMMAND_ID_CANCEL_SM_RESP,           % Issued by MC only
         ?COMMAND_ID_QUERY_BROADCAST_SM_RESP,  % Issued by MC only
         ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP, % Issued by MC only
         ?COMMAND_ID_ENQUIRE_LINK,             % Both sides
         ?COMMAND_ID_ENQUIRE_LINK_RESP,        % Both sides
         ?COMMAND_ID_GENERIC_NACK,             % Both sides
         ?COMMAND_ID_UNBIND,                   % Both sides
         ?COMMAND_ID_UNBIND_RESP,              % Both sides
         ?COMMAND_ID_BIND_RECEIVER_RESP,       % Issued by MC only
         ?COMMAND_ID_BIND_TRANSCEIVER_RESP,    % Issued by MC only
         ?COMMAND_ID_BIND_TRANSMITTER_RESP,    % Issued by MC only
         ?COMMAND_ID_OUTBIND]).                % Issued by MC only
-define(ANY_OPERATION, 
        [?COMMAND_ID_DATA_SM,                  % Both sides
         ?COMMAND_ID_DATA_SM_RESP,             % Both sides
         ?COMMAND_ID_DELIVER_SM,               % Issued by MC only
         ?COMMAND_ID_SUBMIT_SM_RESP,           % Issued by MC only
         ?COMMAND_ID_SUBMIT_MULTI_RESP,        % Issued by MC only
         ?COMMAND_ID_ALERT_NOTIFICATION,       % Issued by MC only
         ?COMMAND_ID_BROADCAST_SM_RESP,        % Issued by MC only
         ?COMMAND_ID_QUERY_SM_RESP,            % Issued by MC only
         ?COMMAND_ID_REPLACE_SM_RESP,          % Issued by MC only
         ?COMMAND_ID_CANCEL_SM_RESP,           % Issued by MC only
         ?COMMAND_ID_QUERY_BROADCAST_SM_RESP,  % Issued by MC only
         ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP, % Issued by MC only
         ?COMMAND_ID_DELIVER_SM_RESP,          % Issued by ESME only
         ?COMMAND_ID_SUBMIT_SM,                % Issued by ESME only
         ?COMMAND_ID_SUBMIT_MULTI,             % Issued by ESME only
         ?COMMAND_ID_BROADCAST_SM,             % Issued by ESME only
         ?COMMAND_ID_QUERY_SM,                 % Issued by ESME only
         ?COMMAND_ID_REPLACE_SM,               % Issued by ESME only
         ?COMMAND_ID_CANCEL_SM,                % Issued by ESME only
         ?COMMAND_ID_QUERY_BROADCAST_SM,       % Issued by ESME only
         ?COMMAND_ID_CANCEL_BROADCAST_SM,      % Issued by ESME only
         ?COMMAND_ID_ENQUIRE_LINK,             % Both sides
         ?COMMAND_ID_ENQUIRE_LINK_RESP,        % Both sides
         ?COMMAND_ID_GENERIC_NACK,             % Both sides
         ?COMMAND_ID_UNBIND,                   % Both sides
         ?COMMAND_ID_UNBIND_RESP,              % Both sides
         ?COMMAND_ID_BIND_RECEIVER_RESP,       % Issued by MC only
         ?COMMAND_ID_BIND_TRANSCEIVER_RESP,    % Issued by MC only
         ?COMMAND_ID_BIND_TRANSMITTER_RESP,    % Issued by MC only
         ?COMMAND_ID_OUTBIND,                  % Issued by MC only
         ?COMMAND_ID_BIND_RECEIVER,            % Issued by ESME only
         ?COMMAND_ID_BIND_TRANSCEIVER,         % Issued by ESME only
         ?COMMAND_ID_BIND_TRANSMITTER]).       % Issued by ESME only


%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec get_param(ParamName, PduDict) -> ParamValue
%    ParamName  = atom()
%    PduDict    = dictionary()
%    ParamValue = term()
%
% @doc Gets the value of a parameter from a PDU dictionary given the parameter
% name.  If the parameter is not defined on the PDU the atom <tt>undefined
% </tt> is returned.
% @end
%
% %@see
%
% %@equiv
%%
get_param(ParamName, PduDict) ->
    case dict:find(ParamName, PduDict) of
        {ok, ParamValue} ->
            ParamValue;
        error ->
            undefined
    end.


%%%
% @spec set_param(ParamName, ParamValue, PduDict) -> NewPduDict
%    ParamName  = atom()
%    ParamValue = term()
%    PduDict    = dictionary()
%    NewPduDict = dictionary()
%
% @doc Sets the value of a parameter on a PDU dictionary given the parameter
% name, the new PDU dictionary is returned.
% @end
%
% %@see
%
% %@equiv
%%
set_param(ParamName, ParamValue, PduDict) ->
    dict:store(ParamName, ParamValue, PduDict).


%%%
% @spec merge_params(ParamList1, ParamList2) -> NewParamList
%    ParamList1 = [{ParamName, ParamValue}]
%    ParamList2 = [{ParamName, ParamValue}]
%    NewParamList = [{ParamName, ParamValue}]
%    ParamName = atom()
%    ParamValue = term()
%
% @doc Merge two parameter lists.  If an parameter appears on both lists,
% the value from the first list will be taken.
% @end
%
% %@see
%
% %@equiv
%%
merge_params(ParamList1, ParamList2) ->
    merge_params(lists:keysort(1,ParamList1), lists:keysort(1,ParamList2), []).


%%%
% @doc Auxiliary function for merge_params/2
%
% @see merge_params/2
% @end
%%
merge_params([], ParamList2, MergedParamList) ->
    MergedParamList ++ ParamList2;

merge_params(ParamList1, [], MergedParamList) ->
    MergedParamList ++ ParamList1;

merge_params([{Id,V1}|T1], [{Id,V2}|T2], MergedParamList) ->
    merge_params(T1, T2, [{Id, V1}|MergedParamList]);

merge_params([{I1,V1}|T1], [{I2,V2}|T2], MergedParamList) when I1 < I2 ->
    merge_params(T1, [{I2,V2}|T2], [{I1, V1}|MergedParamList]);

merge_params([{I1,V1}|T1], [{I2,V2}|T2], MergedParamList) ->
    merge_params([{I1,V1}|T1], T2, [{I2, V2}|MergedParamList]).


%%%
% @spec new_bind_transmitter(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new bind_transmitter PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_bind_transmitter(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSMITTER, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_bind_transmitter_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new bind_transmitter_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_bind_transmitter_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSMITTER_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_bind_receiver(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new bind_receiver PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_bind_receiver(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_RECEIVER, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_bind_receiver_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new bind_receiver_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_bind_receiver_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_RECEIVER_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_bind_transceiver(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new bind_transceiver PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_bind_transceiver(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSCEIVER, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_bind_transceiver_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new bind_transceiver_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_bind_transceiver_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSCEIVER_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_outbind(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new outbind PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_outbind(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_OUTBIND, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_unbind(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new unbind PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_unbind(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_UNBIND, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_unbind_resp(CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new unbind_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_unbind_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_UNBIND_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_enquire_link(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new enquire_link PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_enquire_link(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_ENQUIRE_LINK, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_enquire_link_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new enquire_link_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_enquire_link_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_ENQUIRE_LINK_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_alert_notification(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new alert_notification PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_alert_notification(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_ALERT_NOTIFICATION, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_generic_nack(CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new generic_nack PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_generic_nack(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_GENERIC_NACK, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_submit_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new submit_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_submit_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_submit_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new submit_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_submit_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_data_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new data_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_data_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DATA_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_data_sm_resp(CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new data_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_data_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DATA_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_submit_multi(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new submit_multi PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_submit_multi(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_MULTI, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_submit_multi_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new submit_multi_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_submit_multi_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_MULTI_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_deliver_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new deliver_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_deliver_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DELIVER_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_deliver_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new deliver_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_deliver_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DELIVER_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_broadcast_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new broadcast_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_broadcast_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BROADCAST_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_broadcast_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new broadcast_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_broadcast_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BROADCAST_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_cancel_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new cancel_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_cancel_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_cancel_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new cancel_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_cancel_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_query_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new query_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_query_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_query_sm_resp(CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new query_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_query_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_replace_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new replace_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_replace_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_REPLACE_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_replace_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new replace_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_replace_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_REPLACE_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_query_broadcast_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new query_broadcast_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_query_broadcast_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_BROADCAST_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_query_broadcast_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new query_broadcast_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_query_broadcast_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_BROADCAST_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_cancel_broadcast_sm(SequenceNumber, InitParams) -> PduDict
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new cancel_broadcast_sm PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_cancel_broadcast_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_BROADCAST_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec new_cancel_broadcast_sm_resp(
%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%    CommandStatus  = int()
%    SequenceNumber = int()
%    InitParams     = [{ParamName, ParamValue}]
%    ParamName      = atom()
%    ParamValue     = term()
%    PduDict        = dictionary()
%
% @doc Creates a new cancel_broadcast_sm_resp PDU dictionary with the given 
% <tt>InitParams</tt> and the default values defined for this PDU.
%
% @see pdu_syntax:new_pdu/4
% @end
%
% %@equiv
%%
new_cancel_broadcast_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_BROADCAST_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%%%
% @spec pack(PduDict) -> Result
%    PduDict        = dictionary()
%    Result         = {ok, BinaryPdu} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    BinaryPdu      = bin()
%    Error          = int()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
% 
% @doc Packs any SMPP PDU dictionary into the corresponding byte stream.  This
% function handles any operation.
%
% <p>Before using this function consider esme_pack/1 or mc_pack/1.  This
% generic packing function should only be used on special occasions 
% (implementing a Routing Entity).</p>
%
% @see pack/2
% @see esme_pack/1
% @see mc_pack/1
%
% @equiv operation:pack(Pdu, ANY_OPERATION)
% @end
%%
pack(PduDict) ->
    pack(PduDict, ?ANY_OPERATION).


%%%
% @spec esme_pack(PduDict) -> Result
%    PduDict        = dictionary()
%    Result         = {ok, BinaryPdu} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    BinaryPdu      = bin()
%    Error          = int()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
%
% @doc Packs an SMPP PDU dictionary into the corresponding byte stream.
%
% <p>This function is optimized for ESME implementations, thus only handles
% operations issued by an ESME, any other PDU generates an 
% <tt>?ESME_RINVCMDID</tt> error code.</p>
%
% @see pack/2
%
% @equiv pack(PduDict, ESME_ISSUED_OPERATIONS)
% @end
%%
esme_pack(PduDict) ->
    pack(PduDict, ?ESME_ISSUED_OPERATIONS).


%%%
% @spec mc_pack(PduDict) -> Result
%    PduDict        = dictionary()
%    Result         = {ok, BinaryPdu} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    BinaryPdu      = bin()
%    Error          = int()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
%
% @doc Packs an SMPP PDU dictionary into the corresponding byte stream.
%
% <p>This function is optimized for MC implementations, thus only handles
% operations issued by an MC, any other PDU generates an 
% <tt>?ESME_RINVCMDID</tt> error code.</p>
%
% @see pack/2
%
% @equiv pack(PduDict, MC_ISSUED_OPERATIONS)
% @end
%%
mc_pack(PduDict) ->
    pack(PduDict, ?MC_ISSUED_OPERATIONS).


%%%
% @spec unpack(BinaryPdu) -> Result
%    BinaryPdu      = bin()
%    Result         = {ok, PduDict} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    PduDict        = dictionary()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
% 
% @doc Unpacks a binary SMPP PDU into the corresponding PDU dictionary.  This
% function handles any operation.
%
% <p>Before using this function consider esme_unpack/1 or mc_unpack/1.  This
% generic unpacking function should only be used on very special occasions 
% (such us implementing a Routing Entity).</p>
%
% @see unpack/2
% @see esme_unpack/1
% @see mc_unpack/1
%
% @equiv unpack(BinaryPdu, ANY_OPERATION)
% @end
%%
unpack(BinaryPdu) ->
    unpack(BinaryPdu, ?ANY_OPERATION).


%%%
% @spec esme_unpack(BinaryPdu) -> Result
%    BinaryPdu      = bin()
%    Result         = {ok, PduDict} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    PduDict        = dictionary()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
% 
% @doc Unpacks a binary SMPP PDU into the corresponding PDU dictionary.
%
% <p>This function is optimized for ESME implementation, thus only capable of
% unpacking PDUs issued by a MC, any other PDU produces an
% <tt>?ESME_RINVCMDID</tt> error code.</p>
%
% @see unpack/2
%
% @equiv unpack(BinaryPdu, MC_ISSUED_OPERATIONS)
% @end
%%
esme_unpack(BinaryPdu) ->
    unpack(BinaryPdu, ?MC_ISSUED_OPERATIONS).


%%%
% @spec mc_unpack(BinaryPdu) -> Result
%    BinaryPdu      = bin()
%    Result         = {ok, PduDict} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    PduDict        = dictionary()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
% 
% @doc Unpacks a binary SMPP PDU into the corresponding PDU dictionary.
%
% <p>This function is optimized for MC implementation, thus only capable of
% unpacking PDUs issued by an ESME, any other PDU produces an
% <tt>?ESME_RINVCMDID</tt> error code.</p>
%
% @see unpack/2
%
% @equiv unpack(BinaryPdu, ESME_ISSUED_OPERATIONS)
% @end
%%
mc_unpack(BinaryPdu) ->
    unpack(BinaryPdu, ?ESME_ISSUED_OPERATIONS).


%%%
% @spec response_command_id(RequestCommandId) -> ResponseCommandId
%    RequestCommandId  = int()
%    ResponseCommandId = int()
%
% @doc Computes the response command id related to a given request command id.
% @end
%
% %@see
%
% %@equiv
%%
response_command_id(RequestCommandId) ->
    RequestCommandId + 16#80000000.


%%%
% @spec request_command_id(ResponseCommandId) -> RequestCommandId
%     ResponseCommandId = int()
%     RequestCommandId  = int()
%
% @doc Computes the request command id related to a given response command id.
% @end
%
% %@see
%
% %@equiv
%%
request_command_id(ResponseCommandId) ->
    ResponseCommandId - 16#80000000.


%%%
% @spec request_failure_code(CommandId) -> CommandStatus
%    CommandId     = int()
%    CommandStatus = int()
%
% @doc Returns the error status code associated to a request failure (Most
% of the requests have a generic failure code, otherwise ?ESME_RUNKNOWNERR is
% returned).
% @end
%
% %@see
%
% %@equiv
%%
request_failure_code(?COMMAND_ID_BIND_RECEIVER)      -> ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BIND_TRANSCEIVER)   -> ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BIND_TRANSMITTER)   -> ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BROADCAST_SM)       -> ?ESME_RBCASTFAIL;
request_failure_code(?COMMAND_ID_CANCEL_BROADCAST_SM)-> ?ESME_RBCASTCANCELFAIL;
request_failure_code(?COMMAND_ID_CANCEL_SM)          -> ?ESME_RCANCELFAIL;
request_failure_code(?COMMAND_ID_SUBMIT_SM)          -> ?ESME_RSUBMITFAIL;
request_failure_code(?COMMAND_ID_DATA_SM)            -> ?ESME_RSUBMITFAIL;
request_failure_code(?COMMAND_ID_SUBMIT_MULTI)       -> ?ESME_RSUBMITFAIL;
request_failure_code(?COMMAND_ID_QUERY_BROADCAST_SM) -> ?ESME_RBCASTQUERYFAIL;
request_failure_code(?COMMAND_ID_QUERY_SM)           -> ?ESME_RQUERYFAIL;
request_failure_code(?COMMAND_ID_REPLACE_SM)         -> ?ESME_RREPLACEFAIL;
request_failure_code(CommandId)                      -> ?ESME_RUNKNOWNERR.


%%%===================================================================
% Internal functions
%%====================================================================
%%%
% @spec pdu_type(CommandId) -> pdu()
%    CommandId = int()
%
% @doc Gets the pdu format given the <tt>CommandId</tt>.
% @end
%
% %@see
%
% %@equiv
%%
pdu_type(?COMMAND_ID_BIND_RECEIVER)            -> ?BIND_RECEIVER;
pdu_type(?COMMAND_ID_BIND_TRANSMITTER)         -> ?BIND_TRANSMITTER;
pdu_type(?COMMAND_ID_QUERY_SM)                 -> ?QUERY_SM;
pdu_type(?COMMAND_ID_SUBMIT_SM)                -> ?SUBMIT_SM;
pdu_type(?COMMAND_ID_DELIVER_SM)               -> ?DELIVER_SM;
pdu_type(?COMMAND_ID_UNBIND)                   -> ?UNBIND;
pdu_type(?COMMAND_ID_REPLACE_SM)               -> ?REPLACE_SM;
pdu_type(?COMMAND_ID_CANCEL_SM)                -> ?CANCEL_SM;
pdu_type(?COMMAND_ID_BIND_TRANSCEIVER)         -> ?BIND_TRANSCEIVER;
pdu_type(?COMMAND_ID_OUTBIND)                  -> ?OUTBIND;
pdu_type(?COMMAND_ID_ENQUIRE_LINK)             -> ?ENQUIRE_LINK;
pdu_type(?COMMAND_ID_SUBMIT_MULTI)             -> ?SUBMIT_MULTI;
pdu_type(?COMMAND_ID_ALERT_NOTIFICATION)       -> ?ALERT_NOTIFICATION;
pdu_type(?COMMAND_ID_DATA_SM)                  -> ?DATA_SM;
pdu_type(?COMMAND_ID_BROADCAST_SM)             -> ?BROADCAST_SM;
pdu_type(?COMMAND_ID_QUERY_BROADCAST_SM)       -> ?QUERY_BROADCAST_SM;
pdu_type(?COMMAND_ID_CANCEL_BROADCAST_SM)      -> ?CANCEL_BROADCAST_SM;
pdu_type(?COMMAND_ID_GENERIC_NACK)             -> ?GENERIC_NACK;
pdu_type(?COMMAND_ID_BIND_RECEIVER_RESP)       -> ?BIND_RECEIVER_RESP;
pdu_type(?COMMAND_ID_BIND_TRANSMITTER_RESP)    -> ?BIND_TRANSMITTER_RESP;
pdu_type(?COMMAND_ID_QUERY_SM_RESP)            -> ?QUERY_SM_RESP;
pdu_type(?COMMAND_ID_SUBMIT_SM_RESP)           -> ?SUBMIT_SM_RESP;
pdu_type(?COMMAND_ID_DELIVER_SM_RESP)          -> ?DELIVER_SM_RESP;
pdu_type(?COMMAND_ID_UNBIND_RESP)              -> ?UNBIND_RESP;
pdu_type(?COMMAND_ID_REPLACE_SM_RESP)          -> ?REPLACE_SM_RESP;
pdu_type(?COMMAND_ID_CANCEL_SM_RESP)           -> ?CANCEL_SM_RESP;
pdu_type(?COMMAND_ID_BIND_TRANSCEIVER_RESP)    -> ?BIND_TRANSCEIVER_RESP;
pdu_type(?COMMAND_ID_ENQUIRE_LINK_RESP)        -> ?ENQUIRE_LINK_RESP;
pdu_type(?COMMAND_ID_SUBMIT_MULTI_RESP)        -> ?SUBMIT_MULTI_RESP;
pdu_type(?COMMAND_ID_DATA_SM_RESP)             -> ?DATA_SM_RESP;
pdu_type(?COMMAND_ID_BROADCAST_SM_RESP)        -> ?BROADCAST_SM_RESP;
pdu_type(?COMMAND_ID_QUERY_BROADCAST_SM_RESP)  -> ?QUERY_BROADCAST_SM_RESP;
pdu_type(?COMMAND_ID_CANCEL_BROADCAST_SM_RESP) -> ?CANCEL_BROADCAST_SM_RESP.


%%%
% @spec pack(PduDict, SupportedPdus) -> Result
%    PduDict        = dictionary()
%    SupportedPdus  = [CommandId]
%    CommandId      = int()
%    Result         = {ok, BinaryPdu} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    BinaryPdu      = bin()
%    Error          = int()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
%
% 
% @doc Packs an SMPP PDU dictionary into the corresponding byte stream
% using the PDU format associated to first matching command_id on the supported
% PDUs id list (<tt>SupportedPdus</tt>).
%
% <p>If an error occurs the tuple <tt>{error, CommandId, CommandStatus, 
% SequenceNumber}</tt> is returned.</p>
%
% <p>This function generates an exception if <tt>command_id</tt>, 
% <tt>command_status</tt>, <tt>sequence_number</tt> or any of the 
% mandatory parameters are not present on the PDU.</p>
%
% @see pdu_syntax:pack/3
% @end
%
% %@equiv
%%
pack(PduDict, [CommandId]) ->
    pdu_syntax:pack(PduDict, CommandId, pdu_type(CommandId));

pack(PduDict, [CommandId|CommandIds]) ->
    case pdu_syntax:pack(PduDict, CommandId, pdu_type(CommandId)) of
        {ok, BinaryPdu} ->
            {ok, BinaryPdu};
        {error, CmdId, _Status, _SeqNum} = Error when CmdId == CommandId ->
            % The command_id matches but some error occurred while packing
            Error;
        _OtherError ->
            % CommandId didn't match the PduDict command_id, try next.
            pack(PduDict, CommandIds)
    end.


%%%
% @spec unpack(BinaryPdu, SupportedPdus) -> Result
%    BinaryPdu      = bin()
%    SupportedPdus  = [CommandId]
%    CommandId      = int()
%    Result         = {ok, PduDict} | 
%                     {error, CommandId, CommandStatus, SequenceNumber}
%    PduDict        = dictionary()
%    CommandId      = undefined | int()
%    CommandStatus  = int()
%    SequenceNumber = int()
% 
% @doc Unpacks an SMPP binary PDU into the corresponding PDU  dictionary
% using the PDU format associated to first matching command_id on the supported
% PDUs id list (<tt>SupportedPdus</tt>).
%
% <p>If an error occurs the tuple <tt>{error, CommandId, CommandStatus, 
% SequenceNumber}</tt> is returned.</p>
% 
% @see pdu_syntax:unpack/3
% @end
%
% %@equiv
%%
unpack(BinaryPdu, [CommandId]) ->
    pdu_syntax:unpack(BinaryPdu, CommandId, pdu_type(CommandId));

unpack(BinaryPdu, [CommandId|CommandIds]) ->
    case pdu_syntax:unpack(BinaryPdu, CommandId, pdu_type(CommandId)) of
        {ok, PduDict} ->
            {ok, PduDict};
        {error, CmdId, _Status, _SeqNum} = Error when CmdId == CommandId;
                                                      CmdId == undefined ->
            % The command_id matches and some error occurred while 
            % unpacking or the PDU is deemed invalid (CmdId undefined).
            Error;
        _OtherError ->
            % CommandId didn't match the PduDict command_id, try next.
            unpack(BinaryPdu, CommandIds)
    end.
