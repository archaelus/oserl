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
%%% [16 Jun 2004]
%%%
%%% <ul>
%%%   <li><tt>new_xxx/2</tt> and <tt>new_xxx/3</tt> functions removed.  See
%%%     generic functions <a href="#new-3">new/3</a> and 
%%%     <a href="#new-4">new/4</a> below.
%%%   </li>
%%%   <li><tt>request_command_id/1</tt> and <tt>response_command_id/1</tt> 
%%%     redefined as macros in 
%%%     <a href="smpp_globals.html">smpp_globals.hrl</a>.
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
-export([get_param/2, 
         set_param/3, 
         merge_params/2,
         new/3,
         new/4,
         pack/1,
         esme_pack/1,
         smsc_pack/1,
         unpack/1,
         esme_unpack/1,
         smsc_unpack/1,
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
