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

%%% @doc SMPP Operation library.
%%%
%%% <p>SMPP Operation PDU definitions.</p>
%%%
%%%
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [10 Feb 2004]
%%%
%%% <ul>
%%%   <li>Implemented <tt>get_param/2</tt> and <tt>set_param/3</tt>, 
%%%     replacements for <tt>pdu_syntax:get_value/2</tt> and 
%%%     <tt>pdu_syntax:set_value/3</tt> respectively.
%%%   </li>
%%% </ul>
%%%
%%% [17 Feb 2004]
%%%
%%% <ul>
%%%   <li><tt>new_pdu/5</tt> function removed.  PDU defaults MACROS do no 
%%%     longer exist.
%%%   </li>
%%% </ul>
%%%
%%% [01 Mar 2004]
%%%
%%% <ul>
%%%   <li><a href="#pack-1">pack/1</a>, <a href="#pack_esme-1">pack_esme/1</a>,
%%%     <a href="#pack_smsc-1">pack_smsc/1</a>, 
%%%     <a href="#unpack-1">unpack/1</a>,
%%%     <a href="#unpack_esme-1">unpack_esme/1</a> and 
%%%     <a href="#unpack_smsc-1">unpack_smsc/1</a> functions redefined.
%%%   </li>
%%% </ul>
%%%
%%%
%%% [10 Jun 2004]
%%%
%%% <ul>
%%%   <li><tt>pack_mc/1</tt> is now called 
%%%     <a href="#pack_smsc-1">pack_smsc/1</a>.
%%%   </li>
%%%   <li><tt>unpack_mc/1</tt> is now called 
%%%     <a href="#unpack_smsc-1">unpack_smsc/1</a>.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 0.2 alpha, {24 May 2003} {@time}.
%%% @end
-module(operation).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_globals.hrl").
-include("smpp_pdu.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([command_id/1,
         command_name/1,
         get_param/2, 
         set_param/3, 
         merge_params/2,
         new/3,
         new/4,
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
         smsc_pack/1,
         unpack/1,
         esme_unpack/1,
         smsc_unpack/1,
         request_command_id/1,
         request_command_name/1,
         response_command_id/1,
         response_command_name/1,
         request_failure_code/1]).


%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec command_id(CommandName) -> CommandId
%%    CommandName = bind_transmitter |
%%                  bind_transmitter |
%%                  bind_transmitter_resp |
%%                  bind_receiver |
%%                  bind_receiver_resp |
%%                  bind_transceiver |
%%                  bind_transceiver_resp |
%%                  outbind |
%%                  unbind |
%%                  unbind_resp |
%%                  enquire_link |
%%                  enquire_link_resp |
%%                  alert_notification |
%%                  generic_nack |
%%                  submit_sm |
%%                  submit_sm_resp |
%%                  data_sm |
%%                  data_sm_resp |
%%                  submit_multi |
%%                  submit_multi_resp |
%%                  deliver_sm |
%%                  deliver_sm_resp |
%%                  broadcast_sm |
%%                  broadcast_sm_resp |
%%                  cancel_sm |
%%                  cancel_sm_resp |
%%                  query_sm |
%%                  query_sm_resp |
%%                  replace_sm |
%%                  replace_sm_resp |
%%                  query_broadcast_sm |
%%                  query_broadcast_sm_resp |
%%                  cancel_broadcast_sm |
%%                  cancel_broadcast_sm_resp |
%%    CommandId   = int()
%%
%% @doc Returns the <tt>CommandId</tt> for a given <tt>CommandName</tt>.
%% @end 
command_id(bind_transmitter)         -> ?COMMAND_ID_BIND_TRANSMITTER;
command_id(bind_transmitter_resp)    -> ?COMMAND_ID_BIND_TRANSMITTER_RESP;
command_id(bind_receiver)            -> ?COMMAND_ID_BIND_RECEIVER;
command_id(bind_receiver_resp)       -> ?COMMAND_ID_BIND_RECEIVER_RESP;
command_id(bind_transceiver)         -> ?COMMAND_ID_BIND_TRANSCEIVER;
command_id(bind_transceiver_resp)    -> ?COMMAND_ID_BIND_TRANSCEIVER_RESP;
command_id(outbind)                  -> ?COMMAND_ID_OUTBIND;
command_id(unbind)                   -> ?COMMAND_ID_UNBIND;
command_id(unbind_resp)              -> ?COMMAND_ID_UNBIND_RESP;
command_id(enquire_link)             -> ?COMMAND_ID_ENQUIRE_LINK;
command_id(enquire_link_resp)        -> ?COMMAND_ID_ENQUIRE_LINK_RESP;
command_id(alert_notification)       -> ?COMMAND_ID_ALERT_NOTIFICATION;
command_id(generic_nack)             -> ?COMMAND_ID_GENERIC_NACK;
command_id(submit_sm)                -> ?COMMAND_ID_SUBMIT_SM;
command_id(submit_sm_resp)           -> ?COMMAND_ID_SUBMIT_SM_RESP;
command_id(data_sm)                  -> ?COMMAND_ID_DATA_SM;
command_id(data_sm_resp)             -> ?COMMAND_ID_DATA_SM_RESP;
command_id(submit_multi)             -> ?COMMAND_ID_SUBMIT_MULTI;
command_id(submit_multi_resp)        -> ?COMMAND_ID_SUBMIT_MULTI_RESP;
command_id(deliver_sm)               -> ?COMMAND_ID_DELIVER_SM;
command_id(deliver_sm_resp)          -> ?COMMAND_ID_DELIVER_SM_RESP;
command_id(broadcast_sm)             -> ?COMMAND_ID_BROADCAST_SM;
command_id(broadcast_sm_resp)        -> ?COMMAND_ID_BROADCAST_SM_RESP;
command_id(cancel_sm)                -> ?COMMAND_ID_CANCEL_SM;
command_id(cancel_sm_resp)           -> ?COMMAND_ID_CANCEL_SM_RESP;
command_id(query_sm)                 -> ?COMMAND_ID_QUERY_SM;
command_id(query_sm_resp)            -> ?COMMAND_ID_QUERY_SM_RESP;
command_id(replace_sm)               -> ?COMMAND_ID_REPLACE_SM;
command_id(replace_sm_resp)          -> ?COMMAND_ID_REPLACE_SM_RESP;
command_id(query_broadcast_sm)       -> ?COMMAND_ID_QUERY_BROADCAST_SM;
command_id(query_broadcast_sm_resp)  -> ?COMMAND_ID_QUERY_BROADCAST_SM_RESP;
command_id(cancel_broadcast_sm)      -> ?COMMAND_ID_CANCEL_BROADCAST_SM;
command_id(cancel_broadcast_sm_resp) -> ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP.


%% @spec command_name(CommandId) -> CommandName
%%    CommandId   = int()
%%    CommandName = atom()
%%
%% @doc Returns the <tt>CommandName</tt> for a given <tt>CommandId</tt>.
%%
%% <p>Refer to <a href="#command_id-1">command_id/1</a> for a complete list
%% of command names.</p>
%% @end 
command_name(?COMMAND_ID_BIND_TRANSMITTER)         -> bind_transmitter;
command_name(?COMMAND_ID_BIND_TRANSMITTER_RESP)    -> bind_transmitter_resp;
command_name(?COMMAND_ID_BIND_RECEIVER)            -> bind_receiver;
command_name(?COMMAND_ID_BIND_RECEIVER_RESP)       -> bind_receiver_resp;
command_name(?COMMAND_ID_BIND_TRANSCEIVER)         -> bind_transceiver;
command_name(?COMMAND_ID_BIND_TRANSCEIVER_RESP)    -> bind_transceiver_resp;
command_name(?COMMAND_ID_OUTBIND)                  -> outbind;
command_name(?COMMAND_ID_UNBIND)                   -> unbind;
command_name(?COMMAND_ID_UNBIND_RESP)              -> unbind_resp;
command_name(?COMMAND_ID_ENQUIRE_LINK)             -> enquire_link;
command_name(?COMMAND_ID_ENQUIRE_LINK_RESP)        -> enquire_link_resp;
command_name(?COMMAND_ID_ALERT_NOTIFICATION)       -> alert_notification;
command_name(?COMMAND_ID_GENERIC_NACK)             -> generic_nack;
command_name(?COMMAND_ID_SUBMIT_SM)                -> submit_sm;
command_name(?COMMAND_ID_SUBMIT_SM_RESP)           -> submit_sm_resp;
command_name(?COMMAND_ID_DATA_SM)                  -> data_sm;
command_name(?COMMAND_ID_DATA_SM_RESP)             -> data_sm_resp;
command_name(?COMMAND_ID_SUBMIT_MULTI)             -> submit_multi;
command_name(?COMMAND_ID_SUBMIT_MULTI_RESP)        -> submit_multi_resp;
command_name(?COMMAND_ID_DELIVER_SM)               -> deliver_sm;
command_name(?COMMAND_ID_DELIVER_SM_RESP)          -> deliver_sm_resp;
command_name(?COMMAND_ID_BROADCAST_SM)             -> broadcast_sm;
command_name(?COMMAND_ID_BROADCAST_SM_RESP)        -> broadcast_sm_resp;
command_name(?COMMAND_ID_CANCEL_SM)                -> cancel_sm;
command_name(?COMMAND_ID_CANCEL_SM_RESP)           -> cancel_sm_resp;
command_name(?COMMAND_ID_QUERY_SM)                 -> query_sm;
command_name(?COMMAND_ID_QUERY_SM_RESP)            -> query_sm_resp;
command_name(?COMMAND_ID_REPLACE_SM)               -> replace_sm;
command_name(?COMMAND_ID_REPLACE_SM_RESP)          -> replace_sm_resp;
command_name(?COMMAND_ID_QUERY_BROADCAST_SM)       -> query_broadcast_sm;
command_name(?COMMAND_ID_QUERY_BROADCAST_SM_RESP)  -> query_broadcast_sm_resp;
command_name(?COMMAND_ID_CANCEL_BROADCAST_SM)      -> cancel_broadcast_sm;
command_name(?COMMAND_ID_CANCEL_BROADCAST_SM_RESP) -> cancel_broadcast_sm_resp.


%% @spec get_param(ParamName, PduDict) -> ParamValue
%%    ParamName  = atom()
%%    PduDict    = dictionary()
%%    ParamValue = term()
%%
%% @doc Gets the value of a parameter from a PDU dictionary given the parameter
%% name.  If the parameter is not defined on the PDU the atom <tt>undefined
%% </tt> is returned.
%% @end
get_param(ParamName, PduDict) ->
    case dict:find(ParamName, PduDict) of
        {ok, ParamValue} ->
            ParamValue;
        error ->
            undefined
    end.


%% @spec set_param(ParamName, ParamValue, PduDict) -> NewPduDict
%%    ParamName  = atom()
%%    ParamValue = term()
%%    PduDict    = dictionary()
%%    NewPduDict = dictionary()
%%
%% @doc Sets the value of a parameter on a PDU dictionary given the parameter
%% name, the new PDU dictionary is returned.
%% @end
set_param(ParamName, ParamValue, PduDict) ->
    dict:store(ParamName, ParamValue, PduDict).


%% @spec merge_params(ParamList1, ParamList2) -> NewParamList
%%    ParamList1 = [{ParamName, ParamValue}]
%%    ParamList2 = [{ParamName, ParamValue}]
%%    NewParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%
%% @doc Merge two parameter lists.  If an parameter appears on both lists,
%% the value from the first list will be taken.
%% @end
merge_params(ParamList1, ParamList2) ->
    merge_params(lists:keysort(1,ParamList1), lists:keysort(1,ParamList2), []).

%% @doc Auxiliary function for merge_params/2
%%
%% @see merge_params/2
%% @end
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


%% @spec new(CommandId, SequenceNumber, InitParams) -> PduDict
%%    CommandId      = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new PDU dictionary of type <tt>PdyType</tt> with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% <p>The <i>command_status</i> is set to <tt>ESME_ROK</tt>.</p>
%%
%% @see new/4
%% @end
new(CommandId, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(CommandId, ?ESME_ROK, SequenceNumber, InitParams).


%% @spec new(CommandId, SequenceNumber, InitParams) -> PduDict
%%    CommandId      = int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new PDU dictionary of type <tt>PdyType</tt> with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% <p>The <i>command_status</i> is set to <tt>CommandStatus</tt>.</p>
%%
%% @see new/3
%% @end
new(CommandId, CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(CommandId, CommandStatus, SequenceNumber, InitParams).


%% @spec new_bind_transmitter(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new bind_transmitter PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_bind_transmitter(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSMITTER, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_bind_transmitter_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new bind_transmitter_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_bind_transmitter_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSMITTER_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_bind_receiver(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new bind_receiver PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_bind_receiver(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_RECEIVER, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_bind_receiver_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new bind_receiver_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_bind_receiver_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_RECEIVER_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_bind_transceiver(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new bind_transceiver PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_bind_transceiver(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSCEIVER, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_bind_transceiver_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new bind_transceiver_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_bind_transceiver_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BIND_TRANSCEIVER_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_outbind(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new outbind PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_outbind(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_OUTBIND, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_unbind(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new unbind PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_unbind(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_UNBIND, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_unbind_resp(CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new unbind_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_unbind_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_UNBIND_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_enquire_link(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new enquire_link PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_enquire_link(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_ENQUIRE_LINK, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_enquire_link_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new enquire_link_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_enquire_link_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_ENQUIRE_LINK_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_alert_notification(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new alert_notification PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_alert_notification(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_ALERT_NOTIFICATION, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_generic_nack(CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new generic_nack PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_generic_nack(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_GENERIC_NACK, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_submit_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new submit_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_submit_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_submit_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new submit_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_submit_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_data_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new data_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_data_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DATA_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_data_sm_resp(CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new data_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_data_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DATA_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_submit_multi(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new submit_multi PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_submit_multi(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_MULTI, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_submit_multi_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new submit_multi_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_submit_multi_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_SUBMIT_MULTI_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_deliver_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new deliver_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_deliver_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DELIVER_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_deliver_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new deliver_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_deliver_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_DELIVER_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_broadcast_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new broadcast_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_broadcast_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BROADCAST_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_broadcast_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new broadcast_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_broadcast_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_BROADCAST_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_cancel_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new cancel_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_cancel_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_cancel_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new cancel_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_cancel_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_query_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new query_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_query_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_query_sm_resp(CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new query_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_query_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_replace_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new replace_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_replace_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_REPLACE_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_replace_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new replace_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_replace_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_REPLACE_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_query_broadcast_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new query_broadcast_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_query_broadcast_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_BROADCAST_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_query_broadcast_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new query_broadcast_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_query_broadcast_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_QUERY_BROADCAST_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec new_cancel_broadcast_sm(SequenceNumber, InitParams) -> PduDict
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new cancel_broadcast_sm PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_cancel_broadcast_sm(SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_BROADCAST_SM, 
                       ?ESME_ROK, 
                       SequenceNumber,
                       InitParams).


%% @spec new_cancel_broadcast_sm_resp(
%%           CommandStatus, SequenceNumber, InitParams) -> PduDict
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    InitParams     = [{ParamName, ParamValue}]
%%    ParamName      = atom()
%%    ParamValue     = term()
%%    PduDict        = dictionary()
%%
%% @doc Creates a new cancel_broadcast_sm_resp PDU dictionary with the given 
%% <tt>InitParams</tt> and the default values defined for this PDU.
%%
%% @see pdu_syntax:new_pdu/4
%% @end
new_cancel_broadcast_sm_resp(CommandStatus, SequenceNumber, InitParams) ->
    pdu_syntax:new_pdu(?COMMAND_ID_CANCEL_BROADCAST_SM_RESP, 
                       CommandStatus, 
                       SequenceNumber,
                       InitParams).


%% @spec pack(PduDict) -> Result
%%    PduDict        = dictionary()
%%    Result         = {ok, BinaryPdu} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    BinaryPdu      = bin()
%%    Error          = int()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%% 
%% @doc Packs any SMPP PDU dictionary into the corresponding byte stream.  This
%% function handles any operation.
%%
%% <p>Before using this function consider esme_pack/1 or smsc_pack/1.  This
%% generic packing function should only be used on special occasions 
%% (implementing a Routing Entity).</p>
%%
%% @see pdu_syntax:pack/2
%% @see esme_pack/1
%% @see smsc_pack/1
%% @end
pack(PduDict) ->
    case pdu_syntax:command_id(PduDict) of
        ?COMMAND_ID_DATA_SM -> 
            pdu_syntax:pack(PduDict, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?DATA_SM_RESP);
        ?COMMAND_ID_DELIVER_SM -> 
            pdu_syntax:pack(PduDict, ?DELIVER_SM);
        ?COMMAND_ID_SUBMIT_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_SM_RESP);
        ?COMMAND_ID_SUBMIT_MULTI_RESP -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_MULTI_RESP);
        ?COMMAND_ID_ALERT_NOTIFICATION -> 
            pdu_syntax:pack(PduDict, ?ALERT_NOTIFICATION);
        ?COMMAND_ID_BROADCAST_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?BROADCAST_SM_RESP);
        ?COMMAND_ID_QUERY_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?QUERY_SM_RESP);
        ?COMMAND_ID_REPLACE_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?REPLACE_SM_RESP);
        ?COMMAND_ID_CANCEL_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?CANCEL_SM_RESP);
        ?COMMAND_ID_QUERY_BROADCAST_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?QUERY_BROADCAST_SM_RESP);
        ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?CANCEL_BROADCAST_SM_RESP);
        ?COMMAND_ID_DELIVER_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?DELIVER_SM_RESP);
        ?COMMAND_ID_SUBMIT_SM -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_SM);
        ?COMMAND_ID_SUBMIT_MULTI -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_MULTI);
        ?COMMAND_ID_BROADCAST_SM -> 
            pdu_syntax:pack(PduDict, ?BROADCAST_SM);
        ?COMMAND_ID_QUERY_SM -> 
            pdu_syntax:pack(PduDict, ?QUERY_SM);
        ?COMMAND_ID_REPLACE_SM -> 
            pdu_syntax:pack(PduDict, ?REPLACE_SM);
        ?COMMAND_ID_CANCEL_SM -> 
            pdu_syntax:pack(PduDict, ?CANCEL_SM);
        ?COMMAND_ID_QUERY_BROADCAST_SM -> 
            pdu_syntax:pack(PduDict, ?QUERY_BROADCAST_SM);
        ?COMMAND_ID_CANCEL_BROADCAST_SM -> 
            pdu_syntax:pack(PduDict, ?CANCEL_BROADCAST_SM);
        ?COMMAND_ID_ENQUIRE_LINK -> 
            pdu_syntax:pack(PduDict, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP -> 
            pdu_syntax:pack(PduDict, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK -> 
            pdu_syntax:pack(PduDict, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND -> 
            pdu_syntax:pack(PduDict, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP -> 
            pdu_syntax:pack(PduDict, ?UNBIND_RESP);
        ?COMMAND_ID_BIND_RECEIVER_RESP -> 
            pdu_syntax:pack(PduDict, ?BIND_RECEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSCEIVER_RESP -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSCEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSMITTER_RESP -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSMITTER_RESP);
        ?COMMAND_ID_OUTBIND -> 
            pdu_syntax:pack(PduDict, ?OUTBIND);
        ?COMMAND_ID_BIND_RECEIVER -> 
            pdu_syntax:pack(PduDict, ?BIND_RECEIVER);
        ?COMMAND_ID_BIND_TRANSCEIVER -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSCEIVER);
        ?COMMAND_ID_BIND_TRANSMITTER -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSMITTER);
        Other ->
            {error, Other, ?ESME_RINVCMDID,pdu_syntax:sequence_number(PduDict)}
    end.


%% @spec esme_pack(PduDict) -> Result
%%    PduDict        = dictionary()
%%    Result         = {ok, BinaryPdu} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    BinaryPdu      = bin()
%%    Error          = int()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%
%% @doc Packs an SMPP PDU dictionary into the corresponding byte stream.
%%
%% <p>This function is optimized for ESME implementations, thus only handles
%% operations issued by an ESME, any other PDU generates an 
%% <tt>?ESME_RINVCMDID</tt> error code.</p>
%%
%% @see pdu_syntax:pack/2
%% @end
esme_pack(PduDict) ->
    case pdu_syntax:command_id(PduDict) of
        ?COMMAND_ID_DATA_SM -> 
            pdu_syntax:pack(PduDict, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?DATA_SM_RESP);
        ?COMMAND_ID_DELIVER_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?DELIVER_SM_RESP);
        ?COMMAND_ID_SUBMIT_SM -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_SM);
        ?COMMAND_ID_SUBMIT_MULTI -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_MULTI);
        ?COMMAND_ID_BROADCAST_SM -> 
            pdu_syntax:pack(PduDict, ?BROADCAST_SM);
        ?COMMAND_ID_QUERY_SM -> 
            pdu_syntax:pack(PduDict, ?QUERY_SM);
        ?COMMAND_ID_REPLACE_SM -> 
            pdu_syntax:pack(PduDict, ?REPLACE_SM);
        ?COMMAND_ID_CANCEL_SM -> 
            pdu_syntax:pack(PduDict, ?CANCEL_SM);
        ?COMMAND_ID_QUERY_BROADCAST_SM -> 
            pdu_syntax:pack(PduDict, ?QUERY_BROADCAST_SM);
        ?COMMAND_ID_CANCEL_BROADCAST_SM -> 
            pdu_syntax:pack(PduDict, ?CANCEL_BROADCAST_SM);
        ?COMMAND_ID_ENQUIRE_LINK -> 
            pdu_syntax:pack(PduDict, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP -> 
            pdu_syntax:pack(PduDict, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK -> 
            pdu_syntax:pack(PduDict, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND -> 
            pdu_syntax:pack(PduDict, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP -> 
            pdu_syntax:pack(PduDict, ?UNBIND_RESP);
        ?COMMAND_ID_BIND_RECEIVER -> 
            pdu_syntax:pack(PduDict, ?BIND_RECEIVER);
        ?COMMAND_ID_BIND_TRANSCEIVER -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSCEIVER);
        ?COMMAND_ID_BIND_TRANSMITTER -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSMITTER);
        Other ->
            {error, Other, ?ESME_RINVCMDID,pdu_syntax:sequence_number(PduDict)}
    end.


%% @spec smsc_pack(PduDict) -> Result
%%    PduDict        = dictionary()
%%    Result         = {ok, BinaryPdu} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    BinaryPdu      = bin()
%%    Error          = int()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%
%% @doc Packs an SMPP PDU dictionary into the corresponding byte stream.
%%
%% <p>This function is optimized for SMSC implementations, thus only handles
%% operations issued by an SMSC, any other PDU generates an 
%% <tt>?ESME_RINVCMDID</tt> error code.</p>
%%
%% @see pdu_syntax:pack/2
%% @end
smsc_pack(PduDict) ->
    case pdu_syntax:command_id(PduDict) of
        ?COMMAND_ID_DATA_SM -> 
            pdu_syntax:pack(PduDict, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?DATA_SM_RESP);
        ?COMMAND_ID_DELIVER_SM -> 
            pdu_syntax:pack(PduDict, ?DELIVER_SM);
        ?COMMAND_ID_SUBMIT_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_SM_RESP);
        ?COMMAND_ID_SUBMIT_MULTI_RESP -> 
            pdu_syntax:pack(PduDict, ?SUBMIT_MULTI_RESP);
        ?COMMAND_ID_ALERT_NOTIFICATION -> 
            pdu_syntax:pack(PduDict, ?ALERT_NOTIFICATION);
        ?COMMAND_ID_BROADCAST_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?BROADCAST_SM_RESP);
        ?COMMAND_ID_QUERY_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?QUERY_SM_RESP);
        ?COMMAND_ID_REPLACE_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?REPLACE_SM_RESP);
        ?COMMAND_ID_CANCEL_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?CANCEL_SM_RESP);
        ?COMMAND_ID_QUERY_BROADCAST_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?QUERY_BROADCAST_SM_RESP);
        ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP -> 
            pdu_syntax:pack(PduDict, ?CANCEL_BROADCAST_SM_RESP);
        ?COMMAND_ID_ENQUIRE_LINK -> 
            pdu_syntax:pack(PduDict, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP -> 
            pdu_syntax:pack(PduDict, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK -> 
            pdu_syntax:pack(PduDict, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND -> 
            pdu_syntax:pack(PduDict, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP -> 
            pdu_syntax:pack(PduDict, ?UNBIND_RESP);
        ?COMMAND_ID_BIND_RECEIVER_RESP -> 
            pdu_syntax:pack(PduDict, ?BIND_RECEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSCEIVER_RESP -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSCEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSMITTER_RESP -> 
            pdu_syntax:pack(PduDict, ?BIND_TRANSMITTER_RESP);
        ?COMMAND_ID_OUTBIND -> 
            pdu_syntax:pack(PduDict, ?OUTBIND);
        Other ->
            {error, Other, ?ESME_RINVCMDID,pdu_syntax:sequence_number(PduDict)}
    end.


%% @spec unpack(BinaryPdu) -> Result
%%    BinaryPdu      = bin()
%%    Result         = {ok, PduDict} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    PduDict        = dictionary()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%% 
%% @doc Unpacks a binary SMPP PDU into the corresponding PDU dictionary.  This
%% function handles any operation.
%%
%% <p>Before using this function consider esme_unpack/1 or smsc_unpack/1.  This
%% generic unpacking function should only be used on very special occasions 
%% (such us implementing a Routing Entity).</p>
%%
%% @see pdu_syntax:unpack/2
%% @see esme_unpack/1
%% @see smsc_unpack/1
%% @end
unpack(BinaryPdu) ->
    case pdu_syntax:command_id(BinaryPdu) of
        ?COMMAND_ID_DATA_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?DATA_SM_RESP);
        ?COMMAND_ID_DELIVER_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?DELIVER_SM);
        ?COMMAND_ID_SUBMIT_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_SM_RESP);
        ?COMMAND_ID_SUBMIT_MULTI_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_MULTI_RESP);
        ?COMMAND_ID_ALERT_NOTIFICATION -> 
            pdu_syntax:unpack(BinaryPdu, ?ALERT_NOTIFICATION);
        ?COMMAND_ID_BROADCAST_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BROADCAST_SM_RESP);
        ?COMMAND_ID_QUERY_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_SM_RESP);
        ?COMMAND_ID_REPLACE_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?REPLACE_SM_RESP);
        ?COMMAND_ID_CANCEL_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_SM_RESP);
        ?COMMAND_ID_QUERY_BROADCAST_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_BROADCAST_SM_RESP);
        ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_BROADCAST_SM_RESP);
        ?COMMAND_ID_DELIVER_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?DELIVER_SM_RESP);
        ?COMMAND_ID_SUBMIT_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_SM);
        ?COMMAND_ID_SUBMIT_MULTI -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_MULTI);
        ?COMMAND_ID_BROADCAST_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?BROADCAST_SM);
        ?COMMAND_ID_QUERY_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_SM);
        ?COMMAND_ID_REPLACE_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?REPLACE_SM);
        ?COMMAND_ID_CANCEL_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_SM);
        ?COMMAND_ID_QUERY_BROADCAST_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_BROADCAST_SM);
        ?COMMAND_ID_CANCEL_BROADCAST_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_BROADCAST_SM);
        ?COMMAND_ID_ENQUIRE_LINK -> 
            pdu_syntax:unpack(BinaryPdu, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK -> 
            pdu_syntax:unpack(BinaryPdu, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND -> 
            pdu_syntax:unpack(BinaryPdu, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?UNBIND_RESP);
        ?COMMAND_ID_BIND_RECEIVER_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_RECEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSCEIVER_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSCEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSMITTER_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSMITTER_RESP);
        ?COMMAND_ID_OUTBIND -> 
            pdu_syntax:unpack(BinaryPdu, ?OUTBIND);
        ?COMMAND_ID_BIND_RECEIVER -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_RECEIVER);
        ?COMMAND_ID_BIND_TRANSCEIVER -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSCEIVER);
        ?COMMAND_ID_BIND_TRANSMITTER -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSMITTER);
        Other ->
            {error,Other,?ESME_RINVCMDID,pdu_syntax:sequence_number(BinaryPdu)}
    end.


%% @spec esme_unpack(BinaryPdu) -> Result
%%    BinaryPdu      = bin()
%%    Result         = {ok, PduDict} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    PduDict        = dictionary()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%% 
%% @doc Unpacks a binary SMPP PDU into the corresponding PDU dictionary.
%%
%% <p>This function is optimized for ESME implementation, thus only capable of
%% unpacking PDUs issued by a SMSC, any other PDU produces an
%% <tt>?ESME_RINVCMDID</tt> error code.</p>
%%
%% @see pdu_syntax:unpack/2
%% @end
esme_unpack(BinaryPdu) ->
    case pdu_syntax:command_id(BinaryPdu) of
        ?COMMAND_ID_DATA_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?DATA_SM_RESP);
        ?COMMAND_ID_DELIVER_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?DELIVER_SM);
        ?COMMAND_ID_SUBMIT_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_SM_RESP);
        ?COMMAND_ID_SUBMIT_MULTI_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_MULTI_RESP);
        ?COMMAND_ID_ALERT_NOTIFICATION -> 
            pdu_syntax:unpack(BinaryPdu, ?ALERT_NOTIFICATION);
        ?COMMAND_ID_BROADCAST_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BROADCAST_SM_RESP);
        ?COMMAND_ID_QUERY_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_SM_RESP);
        ?COMMAND_ID_REPLACE_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?REPLACE_SM_RESP);
        ?COMMAND_ID_CANCEL_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_SM_RESP);
        ?COMMAND_ID_QUERY_BROADCAST_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_BROADCAST_SM_RESP);
        ?COMMAND_ID_CANCEL_BROADCAST_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_BROADCAST_SM_RESP);
        ?COMMAND_ID_ENQUIRE_LINK -> 
            pdu_syntax:unpack(BinaryPdu, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK -> 
            pdu_syntax:unpack(BinaryPdu, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND -> 
            pdu_syntax:unpack(BinaryPdu, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?UNBIND_RESP);
        ?COMMAND_ID_BIND_RECEIVER_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_RECEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSCEIVER_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSCEIVER_RESP);
        ?COMMAND_ID_BIND_TRANSMITTER_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSMITTER_RESP);
        ?COMMAND_ID_OUTBIND -> 
            pdu_syntax:unpack(BinaryPdu, ?OUTBIND);
        Other ->
            {error,Other,?ESME_RINVCMDID,pdu_syntax:sequence_number(BinaryPdu)}
    end.


%% @spec smsc_unpack(BinaryPdu) -> Result
%%    BinaryPdu      = bin()
%%    Result         = {ok, PduDict} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    PduDict        = dictionary()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%% 
%% @doc Unpacks a binary SMPP PDU into the corresponding PDU dictionary.
%%
%% <p>This function is optimized for SMSC implementation, thus only capable of
%% unpacking PDUs issued by an ESME, any other PDU produces an
%% <tt>?ESME_RINVCMDID</tt> error code.</p>
%%
%% @see unpack/2
%%
%% @equiv unpack(BinaryPdu, ESME_ISSUED_OPERATIONS)
%% @end
smsc_unpack(BinaryPdu) ->
    case pdu_syntax:command_id(BinaryPdu) of
        ?COMMAND_ID_DATA_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?DATA_SM);
        ?COMMAND_ID_DATA_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?DATA_SM_RESP);
        ?COMMAND_ID_DELIVER_SM_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?DELIVER_SM_RESP);
        ?COMMAND_ID_SUBMIT_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_SM);
        ?COMMAND_ID_SUBMIT_MULTI -> 
            pdu_syntax:unpack(BinaryPdu, ?SUBMIT_MULTI);
        ?COMMAND_ID_BROADCAST_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?BROADCAST_SM);
        ?COMMAND_ID_QUERY_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_SM);
        ?COMMAND_ID_REPLACE_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?REPLACE_SM);
        ?COMMAND_ID_CANCEL_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_SM);
        ?COMMAND_ID_QUERY_BROADCAST_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?QUERY_BROADCAST_SM);
        ?COMMAND_ID_CANCEL_BROADCAST_SM -> 
            pdu_syntax:unpack(BinaryPdu, ?CANCEL_BROADCAST_SM);
        ?COMMAND_ID_ENQUIRE_LINK -> 
            pdu_syntax:unpack(BinaryPdu, ?ENQUIRE_LINK);
        ?COMMAND_ID_ENQUIRE_LINK_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?ENQUIRE_LINK_RESP);
        ?COMMAND_ID_GENERIC_NACK -> 
            pdu_syntax:unpack(BinaryPdu, ?GENERIC_NACK);
        ?COMMAND_ID_UNBIND -> 
            pdu_syntax:unpack(BinaryPdu, ?UNBIND);
        ?COMMAND_ID_UNBIND_RESP -> 
            pdu_syntax:unpack(BinaryPdu, ?UNBIND_RESP);
        ?COMMAND_ID_BIND_RECEIVER -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_RECEIVER);
        ?COMMAND_ID_BIND_TRANSCEIVER -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSCEIVER);
        ?COMMAND_ID_BIND_TRANSMITTER -> 
            pdu_syntax:unpack(BinaryPdu, ?BIND_TRANSMITTER);
        Other ->
            {error,Other,?ESME_RINVCMDID,pdu_syntax:sequence_number(BinaryPdu)}
    end.


%% @spec request_command_id(ResponseCommandId) -> RequestCommandId
%%     ResponseCommandId = int()
%%     RequestCommandId  = int()
%%
%% @doc Computes the request command id related to a given response command id.
%% @end
request_command_id(ResponseCommandId) when integer(ResponseCommandId) ->
    ResponseCommandId - 16#80000000;
request_command_id(bind_transmitter_resp) -> 
	?COMMAND_ID_BIND_TRANSMITTER;
request_command_id(bind_receiver_resp) -> 
	?COMMAND_ID_BIND_RECEIVER;
request_command_id(bind_transceiver_resp) ->
	?COMMAND_ID_BIND_TRANSCEIVER;
request_command_id(unbind_resp) -> 
	?COMMAND_ID_UNBIND;
request_command_id(enquire_link_resp) ->
	?COMMAND_ID_ENQUIRE_LINK;
request_command_id(submit_sm_resp) ->
	?COMMAND_ID_SUBMIT_SM;
request_command_id(data_sm_resp) ->
	?COMMAND_ID_DATA_SM;
request_command_id(submit_multi_resp) -> 
	?COMMAND_ID_SUBMIT_MULTI;
request_command_id(deliver_sm_resp) ->
	?COMMAND_ID_DELIVER_SM;
request_command_id(broadcast_sm_resp) -> 
	?COMMAND_ID_BROADCAST_SM;
request_command_id(cancel_sm_resp) ->
	?COMMAND_ID_CANCEL_SM;
request_command_id(query_sm_resp) ->
	?COMMAND_ID_QUERY_SM;
request_command_id(replace_sm_resp) ->
	?COMMAND_ID_REPLACE_SM;
request_command_id(query_broadcast_sm_resp) ->
	?COMMAND_ID_QUERY_BROADCAST_SM;
request_command_id(cancel_broadcast_sm_resp) ->
	?COMMAND_ID_CANCEL_BROADCAST_SM.


%% @spec request_command_name(ResponseCommandName) -> RequestCommandName
%%    ResponseCommandName = bind_transmitter_resp   |
%%                          bind_receiver_resp      |
%%                          bind_transceiver_resp   |
%%                          unbind_resp             |
%%                          enquire_link_resp       |
%%                          submit_sm_resp          |
%%                          data_sm_resp            |
%%                          submit_multi_resp       |
%%                          deliver_sm_resp         |
%%                          broadcast_sm_resp       |
%%                          cancel_sm_resp          |
%%                          query_sm_resp           |
%%                          replace_sm_resp         |
%%                          query_broadcast_sm_resp |
%%                          cancel_broadcast_sm_resp
%%    RequestCommandName  = bind_transmitter   |
%%                          bind_receiver      |
%%                          bind_transceiver   |
%%                          unbind             |
%%                          enquire_link       |
%%                          submit_sm          |
%%                          data_sm            |
%%                          submit_multi       |
%%                          deliver_sm         |
%%                          broadcast_sm       |
%%                          cancel_sm          |
%%                          query_sm           |
%%                          replace_sm         |
%%                          query_broadcast_sm |
%%                          cancel_broadcast_sm
%%
%% @doc Returns the request command name related to a given response command
%% name.
%% @end
request_command_name(bind_transmitter_resp)    -> bind_transmitter;
request_command_name(bind_receiver_resp)       -> bind_receiver;
request_command_name(bind_transceiver_resp)    -> bind_transceiver;
request_command_name(unbind_resp)              -> unbind;
request_command_name(enquire_link_resp)        -> enquire_link;
request_command_name(submit_sm_resp)           -> submit_sm;
request_command_name(data_sm_resp)             -> data_sm;
request_command_name(submit_multi_resp)        -> submit_multi;
request_command_name(deliver_sm_resp)          -> deliver_sm;
request_command_name(broadcast_sm_resp)        -> broadcast_sm;
request_command_name(cancel_sm_resp)           -> cancel_sm;
request_command_name(query_sm_resp)            -> query_sm;
request_command_name(replace_sm_resp)          -> replace_sm;
request_command_name(query_broadcast_sm_resp)  -> query_broadcast_sm;
request_command_name(cancel_broadcast_sm_resp) -> cancel_broadcast_sm.


%% @spec response_command_id(RequestCommandId) -> ResponseCommandId
%%    RequestCommandId  = int()
%%    ResponseCommandId = int()
%%
%% @doc Computes the response command id related to a given request command id.
%% @end
response_command_id(RequestCommandId) when integer(RequestCommandId) ->
    RequestCommandId + 16#80000000;
response_command_id(bind_transmitter) -> 
	?COMMAND_ID_BIND_TRANSMITTER_RESP;
response_command_id(bind_receiver) -> 
	?COMMAND_ID_BIND_RECEIVER_RESP;
response_command_id(bind_transceiver) ->
	?COMMAND_ID_BIND_TRANSCEIVER_RESP;
response_command_id(unbind) -> 
	?COMMAND_ID_UNBIND_RESP;
response_command_id(enquire_link) ->
	?COMMAND_ID_ENQUIRE_LINK_RESP;
response_command_id(submit_sm) ->
	?COMMAND_ID_SUBMIT_SM_RESP;
response_command_id(data_sm) ->
	?COMMAND_ID_DATA_SM_RESP;
response_command_id(submit_multi) -> 
	?COMMAND_ID_SUBMIT_MULTI_RESP;
response_command_id(deliver_sm) ->
	?COMMAND_ID_DELIVER_SM_RESP;
response_command_id(broadcast_sm) -> 
	?COMMAND_ID_BROADCAST_SM_RESP;
response_command_id(cancel_sm) ->
	?COMMAND_ID_CANCEL_SM_RESP;
response_command_id(query_sm) ->
	?COMMAND_ID_QUERY_SM_RESP;
response_command_id(replace_sm) ->
	?COMMAND_ID_REPLACE_SM_RESP;
response_command_id(query_broadcast_sm) ->
	?COMMAND_ID_QUERY_BROADCAST_SM_RESP;
response_command_id(cancel_broadcast_sm) ->
	?COMMAND_ID_CANCEL_BROADCAST_SM_RESP.


%% @spec response_command_name(RequestCommand) -> ResponseCommandName
%%    RequestCommand      = int() |
%%                          bind_transmitter   |
%%                          bind_receiver      |
%%                          bind_transceiver   |
%%                          unbind             |
%%                          enquire_link       |
%%                          submit_sm          |
%%                          data_sm            |
%%                          submit_multi       |
%%                          deliver_sm         |
%%                          broadcast_sm       |
%%                          cancel_sm          |
%%                          query_sm           |
%%                          replace_sm         |
%%                          query_broadcast_sm |
%%                          cancel_broadcast_sm
%%    ResponseCommandName = bind_transmitter_resp   |
%%                          bind_receiver_resp      |
%%                          bind_transceiver_resp   |
%%                          unbind_resp             |
%%                          enquire_link_resp       |
%%                          submit_sm_resp          |
%%                          data_sm_resp            |
%%                          submit_multi_resp       |
%%                          deliver_sm_resp         |
%%                          broadcast_sm_resp       |
%%                          cancel_sm_resp          |
%%                          query_sm_resp           |
%%                          replace_sm_resp         |
%%                          query_broadcast_sm_resp |
%%                          cancel_broadcast_sm_resp
%%
%% @doc Computes the response command id related to a given request command 
%% name or id.
%% @end
response_command_name(Cmd) when Cmd == bind_transmitter;
							    Cmd == ?COMMAND_ID_BIND_TRANSMITTER -> 
	bind_transmitter_resp;
response_command_name(Cmd) when Cmd == bind_receiver;
							    Cmd == ?COMMAND_ID_BIND_RECEIVER -> 
	bind_receiver_resp;
response_command_name(Cmd) when Cmd == bind_transceiver;
							    Cmd == ?COMMAND_ID_BIND_TRANSCEIVER -> 
	bind_transceiver_resp;
response_command_name(Cmd) when Cmd == unbind;
								Cmd == ?COMMAND_ID_UNBIND -> 
	unbind_resp;
response_command_name(Cmd) when Cmd == enquire_link;
								Cmd == ?COMMAND_ID_ENQUIRE_LINK -> 
	enquire_link_resp;
response_command_name(Cmd) when Cmd == submit_sm;
								Cmd == ?COMMAND_ID_SUBMIT_SM -> 
	submit_sm_resp;
response_command_name(Cmd) when Cmd == data_sm;
							    Cmd == ?COMMAND_ID_DATA_SM ->
	data_sm_resp;
response_command_name(Cmd) when Cmd == submit_multi;
								Cmd == ?COMMAND_ID_SUBMIT_MULTI -> 
	submit_multi_resp;
response_command_name(Cmd) when Cmd == deliver_sm;
							    Cmd == ?COMMAND_ID_DELIVER_SM -> 
	deliver_sm_resp;
response_command_name(Cmd) when Cmd == broadcast_sm;
							    Cmd == ?COMMAND_ID_BROADCAST_SM -> 
	broadcast_sm_resp;
response_command_name(Cmd) when Cmd == cancel_sm;
							    Cmd == ?COMMAND_ID_CANCEL_SM -> 
	cancel_sm_resp;
response_command_name(Cmd) when Cmd == query_sm;
								Cmd == ?COMMAND_ID_QUERY_SM -> 
	query_sm_resp;
response_command_name(Cmd) when Cmd == replace_sm;
								Cmd == ?COMMAND_ID_REPLACE_SM -> 
	replace_sm_resp;
response_command_name(Cmd) when Cmd == query_broadcast_sm;
								Cmd == ?COMMAND_ID_QUERY_BROACAST_SM -> 
	query_broadcast_sm_resp;
response_command_name(Cmd) when Cmd == cancel_broadcast_sm;
							    Cmd == ?COMMAND_ID_CANCEL_BROADCAST_SM -> 
	cancel_broadcast_sm_resp.


%% @spec request_failure_code(CommandId) -> CommandStatus
%%    CommandId     = int()
%%    CommandStatus = int()
%%
%% @doc Returns the error status code associated to a request failure (Most
%% of the requests have a generic failure code, otherwise ?ESME_RUNKNOWNERR is
%% returned).
%% @end
request_failure_code(?COMMAND_ID_BIND_TRANSMITTER)   -> ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BIND_RECEIVER)      -> ?ESME_RBINDFAIL;
request_failure_code(?COMMAND_ID_BIND_TRANSCEIVER)   -> ?ESME_RBINDFAIL;
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
%%% Internal functions
%%%===================================================================
