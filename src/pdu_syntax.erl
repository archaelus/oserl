%%% Copyright (C) 2003 - 2005 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc SMPP PDU Library
%%%
%%% <p>Library functions for the SMPP PDU manipulation.</p>
%%%
%%%
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [10 Feb 2004]
%%%
%%% <ul>
%%%   <li>Removed <tt>pdu_syntax:get_value/2</tt> and <tt>
%%%     pdu_syntax:set_value/3</tt>.  Using dictionary API instead.<br/>
%%%     <br/>
%%%     pdu_syntax functions are no longer used beyond operation API.
%%%    </li>
%%% </ul>
%%%
%%% [01 Mar 2004]
%%%
%%% <ul>
%%%   <li><i>command_id</i> is now included into the PDU descriptor.</li>
%%%   <li>New functions <a href="#command_id-1">command_id/1</a> and.
%%%     <a href="#sequence_number-1">sequence_number/1</a> 
%%%   </li>
%%% </ul>
%%%
%%%
%%% <h2>Changes 0.2 -&gt; 1.2</h2>
%%%
%%% [13 Jul 2004]
%%%
%%% <ul>
%%%   <li>Condition <tt>CommandId</tt> of <tt>16#800000000</tt> removed from 
%%%     guards in functions <a href="#pack-2">pack/2</a> and 
%%%     <a href="#unpack-2">unpack/2</a>.
%%%     <br/>
%%%     There was one 0 too many on that conditions.
%%%     <br/>
%%%     Now the command_status is assumed to be always 0 on request PDUs.
%%%    </li>
%%% </ul>
%%%
%%% [7 May 2005]
%%%
%%% <ul>
%%%   <li>Function <a href="#command_status-1">command_status/1</a> added.</li>
%%% </ul>
%%%
%%% [7 Jul 2005]
%%%
%%% <ul>
%%%   <li>Small change in <a href="#unpack_tlvs-2">unpack_tlvs/2</a>.  Now
%%%     call <a href="#unpack_tlvs-3">unpack_tlvs/3</a> instead of looping
%%%     back to <a href="#unpack_tlvs-2">unpack_tlvs/2</a> when discarding
%%%     unsupported TLVs.
%%%   </li>
%%% </ul>
%%%
%%% [30 Jul 2005 Anders Nygren]
%%%
%%% <ul>
%%%   <li>Do not build a binary in <a href="#pack-2">pack/2</a>, 
%%%       build an iolist, it is more efficient.
%%%   </li>
%%%   <li>pack_tlvs, there is no need to reverse the list of TLVs. According to
%%%       SMPP 5.0, 3.2.1.6 TLV Parameters they can be in any order.
%%%   </li>
%%%   <li>unpack_stds and unpack_tlvs, there is no need to reverse the list 
%%%       of parameters. The list of parameters will be converted to a dictionary.
%%%   </li>
%%%
%%% [5 Aug 2005 Anders Nygren]
%%%
%%% <ul>
%%%   <li>Fix bug in pack/2, there is no BIF to find the length of 
%%%       an io_list, so it is still necessary to convert the message
%%%       body to a binary in order to calculate the PDU length.
%%%   </li>
%%% </ul>
%%% @copyright 2003 - 2005 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.2, {19 Mar 2003} {@time}.
%%% @end
-module(pdu_syntax).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_globals.hrl").
-include("pdu_syntax.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([command_id/1, 
         command_status/1, 
         sequence_number/1, 
         new_pdu/4, 
         pack/2, 
         unpack/2]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
%-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec command_id(Pdu) -> int()
%%    Pdu = bin() | dictionary()
%%
%% @doc Gets the <i>command_id</i> of a PDU (binary or dictionary).
%%
%% <p>If the PDU is malformed and the <i>command_id</i> can not be found, the
%% function crashes.</p>
%% @end 
command_id(<<_Len:32, CmdId:32, _Status:32, _SeqNum:32, _Body/binary>>) ->
    CmdId;
command_id(PduDict) -> 
    dict:fetch(command_id,  PduDict).


%% @spec command_status(Pdu) -> int()
%%    Pdu = bin() | dictionary()
%%
%% @doc Gets the <i>command_status</i> of a PDU (binary or dictionary).
%%
%% <p>If the PDU is malformed and the <i>command_status</i> can not be found, 
%% the function exits.</p>
%% @end 
command_status(<<_Len:32, _CmdId:32, Status:32, _SeqNum:32, _Body/binary>>) ->
    Status;
command_status(PduDict) -> 
    dict:fetch(command_status,  PduDict).


%% @spec sequence_number(Pdu) -> int()
%%    Pdu = bin() | dictionary()
%%
%% @doc Gets the <i>sequence_number</i> of a PDU (binary or dictionary).
%%
%% <p>If the PDU is malformed and the <i>sequence_number</i> can not be found, 
%% the function crashes.</p>
%% @end 
sequence_number(<<_Len:32, _CmdId:32, _Status:32, SeqNum:32, _Body/binary>>) ->
    SeqNum;
sequence_number(PduDict) -> 
    dict:fetch(sequence_number,  PduDict).


%% @spec new_pdu(CommandId, CommandStatus, SequenceNumber, Body) -> PduDict
%%    CommandId      = int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%%    Body           = [{Key, Value}]
%%    PduDict        = dictionary()
%%
%% @doc Creates a new PDU dictionary for a given the <tt>CommandId</tt>, a
%% <tt>CommandStatus</tt>, <tt>SequenceNumber</tt> and a list of
%% initial body values.  Every pair of the <tt>Body</tt> list must be on 
%% the form <tt>{Key, Value}</tt>.
%%
%% <p>The <tt>Body</tt> parameter list is ignored whenever 
%% <tt>CommandStatus</tt> is different from 0.</p>
%% @end
new_pdu(CommandId, CommandStatus, SequenceNumber, Body) ->
    Header = [{command_id,      CommandId},
              {command_status,  CommandStatus}, 
              {sequence_number, SequenceNumber}],
    case CommandStatus of
        0 ->
            dict:from_list(Header ++ Body);
        _ ->
            dict:from_list(Header)
    end.


%% @spec pack(PduDict, PduType) -> Result
%%    PduDict        = dictionary()
%%    PduType        = {pdu, StdsTypes, TlvsTypes}
%%    StdsTypes      = [standard()]
%%    TlvsTypes      = [tlv()]
%%    Result         = {ok, BinaryPdu} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    BinaryPdu      = bin()
%%    CommandId      = CmdId | Other
%%    Other          = int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%% 
%% @doc Packs an SMPP PDU dictionary into the corresponding byte stream
%% given the <tt>PduType</tt>.
%%
%% <p>This function generates an exception if <tt>command_id</tt>, 
%% <tt>command_status</tt>, <tt>sequence_number</tt> are not present
%% on the PDU dictionary.</p>
%%
%% <p>Possible return values are:</p>
%%
%% <ul>
%%   <li><tt>{error, Other, ?ESME_RINVCMDID, SequenceNumber}</tt> if the
%%     command_id on the <tt>PduDict</tt> is <tt>Other</tt> and doesn't
%%     match the command_id given as the parameter <tt>CmdId</tt>.
%%   </li>
%%   <li><tt>{error, CmdId, CommandStatus, SequenceNumber}</tt> where
%%     <tt>CommandStatus</tt> is <tt> ?ESME_RUNKNOWNERR</tt> or an
%%     error code associated to the parameter that caused the failure.  The
%%     command_id matches <tt>CmdId</tt> but a packing error was
%%     encountered.
%%   </li>
%%   <li><tt>{ok, BinaryPdu}</tt> if the PDU was successfully packed.
%%   </li>
%% </ul>
%%
%% @see pack_body/3
%% @end
pack(PduDict, Type) ->
    PackBody = 
        fun (CommandStatus, Dict) when CommandStatus == 0 ->
                pack_body(Dict, Type#pdu.stds_types, Type#pdu.tlvs_types);
            (_CommandStatus, _Dict) ->   
                % Do NOT pack the body if CommandStatus != 0
                {ok, []}
        end,
    Status = dict:fetch(command_status,  PduDict),
    SeqNum = dict:fetch(sequence_number, PduDict),
    case dict:fetch(command_id, PduDict) of
        CmdId when CmdId == Type#pdu.command_id ->
            case PackBody(Status, PduDict) of
                {ok, BodyL} ->
		    Body=list_to_binary(BodyL), 
                    Len = size(Body) + 16,
                    {ok, [<<Len:32,CmdId:32,Status:32,SeqNum:32>>,Body]};
                {error, Error} ->
                    {error, CmdId, Error, SeqNum}
            end;
        Other ->
            {error, Other, ?ESME_RINVCMDID, SeqNum}
    end.


%% @spec unpack(BinaryPdu, PduType) -> Result
%%    BinaryPdu      = bin()
%%    PduType        = {pdu, StdsTypes, TlvsTypes}
%%    StdsTypes      = [standard()]
%%    TlvsTypes      = [tlv()]
%%    Result         = {ok, PduDict} | 
%%                     {error, CommandId, CommandStatus, SequenceNumber}
%%    PduDict        = dictionary()
%%    Error          = int()
%%    CommandId      = undefined | int()
%%    CommandStatus  = int()
%%    SequenceNumber = int()
%% 
%% @doc Unpacks an SMPP Binary PDU (octet stream) into the corresponding 
%% PDU dictionary according to <tt>PduType</tt>.
%%
%% <p>This function returns:</p>
%%
%% <ul>
%%   <li><tt>{error, CommandId, ?ESME_RINVCMDID, SequenceNumber}</tt> if
%%     the command_id on the <tt>PduDict</tt> doesn't match the command_id
%%     given on <tt>PduType</tt>
%%   </li>
%%   <li><tt>{error, CommandId, ?ESME_RINVCMDLEN, SequenceNumber}</tt> if
%%     the PDU is malformed.  The <tt>CommandId</tt> might be the atom
%%     <tt>undefined</tt> and the <tt>SequenceNumber</tt> 0 if the
%%     PDU is completly unreadable.
%%   </li>
%%   <li><tt>{error, CmdId, CommandStatus, SequenceNumber}</tt> where 
%%     <tt>CommandStatus</tt> is <tt>?ESME_RUNKNOWNERR</tt> or an error
%%     code associated to the parameter that caused the failure.  The 
%%     <i>command_id</i> <tt>CmdId</tt> and the <tt>SequenceNumber</tt> are 
%%     also included in the error report.
%%   </li>
%%   <li><tt>{ok, PduDict}</tt> if the PDU was successfully unpacked.
%%   </li>
%% </ul>
%%
%% @see unpack_body/3
%% @end
unpack(<<Len:32, Rest/binary>>, Type) when Len == size(Rest) + 4, Len >= 16 ->
    UnpackBody = 
        fun (CommandStatus, Bin) when CommandStatus == 0 ->
                unpack_body(Bin, Type#pdu.stds_types, Type#pdu.tlvs_types);
            (_CommandStatus, _Bin) ->   
                % Do NOT unpack the body if CommandStatus != 0
                {ok, []}
        end,
    case Rest of
        <<CmdId:32, Status:32, SeqNum:32, Body/binary>> 
          when CmdId == Type#pdu.command_id ->
            case UnpackBody(Status, Body) of
                {ok, BodyPairs} ->
                    {ok, new_pdu(CmdId, Status, SeqNum, BodyPairs)};
                {error, Error} ->
                    {error, CmdId, Error, SeqNum}
            end;
        <<Other:32, _Status:32, SeqNum:32, _Body/binary>> ->
            {error, Other, ?ESME_RINVCMDID, SeqNum}
    end;
unpack(_BinaryPdu, _PduType) ->
    {error, undefined, ?ESME_RINVCMDLEN, 0}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @spec pack_body(BodyDict, StdsTypes, TlvsTypes) -> Result
%%    Pdu        = dictionary()
%%    StdsTypes  = [standard()]
%%    TlvsTypes  = [tlv()]
%%    Result     = {ok, BinaryBody} | {error, Error}
%%    BinaryBody = IOlist()
%%    Error      = int()
%% 
%% @doc Packs the body's parameter dictionary of a PDU according to their
%% corresponding types.
%%
%% @see pack_stds/2
%% @see pack_tlvs/2
%% @end
pack_body(BodyDict, StdsTypes, TlvsTypes) ->
    case pack_stds(BodyDict, StdsTypes) of
        {ok, BinaryStdsValues} ->
            case pack_tlvs(BodyDict, TlvsTypes) of
                {ok, BinaryTlvsValues} ->
                    {ok, [BinaryStdsValues, BinaryTlvsValues]};
                TlvError ->
                    TlvError
            end;
        StdError ->
            StdError
    end.


%% @doc Auxiliary function for pack_body/3
%%
%% @see pack_stds/3
%% @end
pack_stds(BodyDict, StdsTypes) ->
    pack_stds(BodyDict, StdsTypes, []).


%% @doc Auxiliary function for pack_stds/2
%%
%% @see param_syntax:get_name/1
%% @see param_syntax:encode/2
%% @end
pack_stds(_BodyDict, [], Acc) ->
    {ok, lists:reverse(Acc)};
pack_stds(BodyDict, [Type|Types], Acc) ->
    Value = case dict:find(param_syntax:get_name(Type), BodyDict) of
                {ok, ParamValue} -> 
                    ParamValue;
                error -> 
                    % See how param_syntax:encode/2 handles undefined
                    undefined
            end,
    case param_syntax:encode(Value, Type) of
        {ok, BinaryValue} ->
            pack_stds(BodyDict, Types, [BinaryValue|Acc]);
        Error ->
            Error
    end.


%% @doc Auxiliary function for pack_body/3
%%
%% @see pack_tlvs/3
%% @end
pack_tlvs(BodyDict, TlvsTypes) ->
    pack_tlvs(BodyDict, TlvsTypes, []).


%% @doc Auxiliary function for pack_tlvs/2
%%
%% @see param_syntax:get_name/1
%% @see param_syntax:encode/2
%% @end
pack_tlvs(_BodyDict, [], Acc) ->
    {ok, Acc};
pack_tlvs(BodyDict, [Type|Types], Acc) ->
    Value = case dict:find(param_syntax:get_name(Type), BodyDict) of
                {ok, ParamValue} ->
                    ParamValue;
                error ->
                    % See how param_syntax:encode/2 handles undefined
                    undefined
            end,
    case param_syntax:encode(Value, Type) of
        {ok, BinaryValue} ->
            pack_tlvs(BodyDict, Types, [BinaryValue|Acc]);
        Error ->
            Error
    end.


%% @spec unpack_body(BinaryBody, StdsTypes, TlvTypes) -> Result
%%    BinaryBody = bin()
%%    StdsTypes  = [standard()]
%%    TlvsTypes  = [tlv()]
%%    Result     = {ok, BodyPairs} | {error, Error}
%%    BodyPairs  = [{Key, Value}]
%%    Error      = int()
%%
%% @doc Unpacks the <tt>BinaryBody</tt> of a PDU according to the types 
%% lists of the standard and TLV parameters specifier (<tt>StdsTypes</tt> 
%% and <tt>TlvsTypes</tt> respectively).
%%
%% <p>First the standard parameters are decoded from the head of the 
%% <tt>BinaryBody</tt> following the sequence determined by <tt>StdsTypes
%% </tt>, the remainder binary contains the TLVs.  Even TLVs may came in any
%% order, they're extracted in the order determined by <tt>TlvsTypes</tt>.
%% </p>
%%
%% @see unpack_stds/2
%% @see unpack_tlvs/2
%% @end
unpack_body(BinaryBody, StdsTypes, TlvsTypes) ->
    case unpack_stds(BinaryBody, StdsTypes) of
        {ok, StdsValues, BinaryTlvs} ->
            case unpack_tlvs(BinaryTlvs, TlvsTypes) of
                {ok, TlvsValues} ->
                    {ok, StdsValues ++ TlvsValues};
                TlvError ->
                    TlvError
            end;
        StdError ->
            StdError
    end.


%% @doc Auxiliary function for unpack_body/3
%%
%% @see unpack_stds/3
%% @end
unpack_stds(BinaryBody, StdsTypes) ->
    unpack_stds(BinaryBody, StdsTypes, []).


%% @doc Auxiliary function for unpack_stds/2
%%
%% @see param_syntax:get_name/1
%% @see param_syntax:decode/2
%% @end
unpack_stds(BinaryTlvs, [], Acc) ->
    {ok, Acc, BinaryTlvs};
unpack_stds(BinaryBody, [Type|Types], Acc) ->
    case param_syntax:decode(BinaryBody, Type) of
        {ok, Value, RestBinaryBody} ->
            Name = param_syntax:get_name(Type),
            unpack_stds(RestBinaryBody, Types, [{Name, Value}|Acc]);
        Error ->
            Error
    end.


%% @doc Auxiliary function for unpack_body/3
%%
%% @see unpack_tlvs/3
%% @end
unpack_tlvs(BinaryTlvs, []) ->
    case param_syntax:chop_tlv(BinaryTlvs) of
        {ok, _Tlv, RestUnusedTlvs} ->
            % Remaining octets seem to be a collection of unsupported TLVs.
            % Following compatibility guidelines recommend to silently discard
            % unsupported TLVs (if they are well-formed).
            %
            % After the first TLV was chopped, we call unpack_tlvs/3 cause, in
            % case of an error, we rather return the ?ESME_RINVTLVSTREAM error
            % instead of the ?ESME_RINVCMDLEN value returned by this function.
            unpack_tlvs(RestUnusedTlvs, [], []);
        {error, <<>>} ->
            {ok, []};
        _Error ->
            % Guess that no unsupported TLVs were appended to the body,
            % just dealing with a malformed PDU.
            {error, ?ESME_RINVCMDLEN}
    end;
unpack_tlvs(BinaryTlvs, TlvsTypes) ->
    unpack_tlvs(BinaryTlvs, TlvsTypes, []).


%% @doc Auxiliary function for unpack_tlvs/2
%%
%% @see param_syntax:get_name/1
%% @see param_syntax:decode/2
%% @see param_syntax:chop_tlv/1
%% @end
unpack_tlvs(<<>>, [], Acc) ->
    {ok, Acc};
unpack_tlvs(UnusedTlvs, [], Acc) ->
    case param_syntax:chop_tlv(UnusedTlvs) of
        {ok, _Tlv, RestUnusedTlvs} ->
            unpack_tlvs(RestUnusedTlvs, [], Acc);
        _Error ->  % Malformed TLV
            {error, ?ESME_RINVTLVSTREAM}
    end;
unpack_tlvs(BinaryTlvs, [Type|Types], Acc) ->
    case param_syntax:decode(BinaryTlvs, Type) of
        {ok, undefined, RestBinaryTlvs} -> % Ignore undefined TLVs
            unpack_tlvs(RestBinaryTlvs, Types, Acc);
        {ok, Value, RestBinaryTlvs} ->
            Name = param_syntax:get_name(Type),
            unpack_tlvs(RestBinaryTlvs, Types, [{Name, Value}|Acc]);
        Error ->
            Error
    end.
