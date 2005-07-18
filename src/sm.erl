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

%%% @doc Short Message Library Module.
%%%
%%% <p>Utility functions for short messages on SMPP requests/responses.</p>
%%%
%%%
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [10 Feb 2004]
%%%
%%% <ul>
%%%   <li>Calls to <tt>pdu_syntax:get_value/2</tt> replaced by 
%%%     <tt>operation:get_param/2</tt>.
%%%   </li>
%%% </ul>
%%%
%%% <h2>Changes 0.2 -&gt; 1.2</h2>
%%%
%%% [13 Feb 2005]
%%%
%%% <ul>
%%%   <li>Functions <a href="#reply_destination_address-1">
%%%     reply_destination_address/1</a>, <a href="#reply_source_address-1">
%%%     reply_source_address/1</a> and <a href="#reply_address-1">
%%%     reply_addresses/1</a> added.
%%%
%%%     <a href="#reply_address-1">reply_address/1</a> will be removed, use
%%%     <a href="#reply_destination_address-1">reply_destination_address/1</a>
%%%     instead.
%%%   </li>
%%% </ul>
%%%
%%% [7 Abr 2005]
%%%
%%% <p>Add support for message concatenation.</p>
%%%
%%% <ul>
%%%   <li>Functions <a href="#set_udhi-1">set_udhi/1</a> and 
%%%     <a href="#split_user_data-2">split_user_data/2</a> added.
%%%   </li>
%%% </ul>
%%%
%%% [11 Abr 2005]
%%%
%%% <p>Add support for message concatenation.</p>
%%%
%%% <ul>
%%%   <li>Function set_udhi/1 replaced by <a href="#udhi-2">udhi/2</a>.</li>
%%%   <li>Function <a href="#udhi-1">udhi/1</a> implemented.</li>
%%%   <li>Function <a href="#chop_udh-1">chop_udh/1</a> implemented.</li>
%%%   <li>Function <a href="#reference_number-1">reference_number/1</a> 
%%%     implemented.
%%%   </li>
%%%   <li>Function <a href="#join_user_data-1">join_user_data/1</a> added.
%%%   </li>
%%%   <li>Function <a href="#total_segments-1">total_segments/1</a> 
%%%     implemented.
%%%   </li>
%%% </ul>
%%%
%%% [11 Jul 2005]
%%%
%%% <p>Add support for message concatenation.</p>
%%%
%%% <ul>
%%%   <li>Function <a href="#split_user_data_tlv-1">split_user_data_tlv/1</a>
%%%     added.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2003 - 2005 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.2, {24 Jul 2003} {@time}.
%%% @end
-module(sm).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_base.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([chop_udh/1,
         join_user_data/1,
         message_user_data/1, 
         reference_number/1,
         reply_addresses/1,
         reply_destination_address/1,
         reply_source_address/1,
         split_user_data/2,
         split_user_data_tlv/1,
         total_segments/1,
         udhi/1,
         udhi/2]).

%%%-------------------------------------------------------------------
%%% Undocumented exports
%%%-------------------------------------------------------------------
-export([reply_address/1]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
%-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(UDHL, 5).  % UDH Length excluded length for Concatenated SMs
-define(IEI, 0).   % Information Element Identifier for Concatenated SMs
-define(IEIDL, 3). % IEI Data length excluded length
-define(REF_NUM(N), N rem 256).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec chop_udh(Data) -> {Udh, Rest}
%%     Data = string()
%%     Rest = string()
%%     Udh = string()
%%
%% @doc Chops the User Data Header from the User Data of a short message.
%% @end 
chop_udh([Udhl|_] = Data) ->
    lists:split(Udhl + 1, Data).


%% @spec join_user_data(Segments) -> {Data, RefNum}
%%    Segments = [Segment]
%%    Segment = string()
%%    Data = string()
%%    RefNum = int()
%%
%% @doc Joins the <tt>Segments</tt> of a concatenated message.
%%
%% @see split_user_data/2
%% @end 
join_user_data(Segments) ->
    Choped = lists:map(fun(S) -> chop_udh(S) end, Segments), % UDH + Data
    P = fun({[_,_,_,Ref,_,X],_}, {[_,_,_,Ref,_,Y],_}) -> X < Y end,
    [{[_,_,_,Ref,_,_], Data}|T] = lists:sort(P, Choped),     % Get Ref from UDH
    lists:foldl(fun({_,D}, {Acc, R}) -> {Acc ++ D, R} end, {Data, Ref}, T).


%% @spec message_user_data(Pdu) -> UserData
%%    Pdu        = pdu()
%%    UserData   = {ParamName, ParamValue}
%%    ParamName  = short_message | message_payload
%%    ParamValue = undefined | string()
%%
%% @doc Gets the message user data from a PDU.  The message user data may came 
%% in the <i>short_message</i> or in the <i>message_payload</i> parameter.
%% @end
message_user_data(Pdu) ->
    case operation:get_param(short_message, Pdu) of
        ShortMessage when ShortMessage == ""; ShortMessage == undefined ->
            {message_payload, operation:get_param(message_payload, Pdu)};
        ShortMessage ->
            {short_message, ShortMessage}
    end.


%% @spec reference_number(Pdu) -> ReferenceNumber
%%    Pdu = pdu()
%%    ReferenceNumber = int()
%%
%% @doc Returns the <tt>ReferenceNumber</tt> of a concatenated message.
%%
%% <p>The function will fail if <tt>Pdu</tt> does not correspond with a valid
%% concatenated message.</p>
%% @end 
reference_number(Pdu) ->
    ShortMessage = operation:get_param(short_message, Pdu),
    {[?UDHL, ?IEI, ?IEIDL, RefNum|_], _Rest} = chop_udh(ShortMessage),
    RefNum.


%% @spec reply_addresses(Pdu) -> ParamList
%%    Pdu = pdu()
%%    ParamList = [DestAddrTon | [DestAddrNpi | [DestinationAddr]]]
%%    DestAddrTon = {dest_addr_ton, int()}
%%    DestAddrNpi = {dest_addr_npi, int()}
%%    DestinationAddr = {destination_addr, string()}
%%
%% @doc Creates reply addresses (<i>dest_addr_ton</i>, <i>dest_addr_npi</i>,
%% <i>destination_addr</i>, <i>source_addr_ton</i>, <i>source_addr_npi</i>
%% and <i>source_addr</i>) from the source address and destination address
%% given in <tt>Pdu</tt>.
%% @end
reply_addresses(Pdu) ->
    reply_source_address(Pdu) ++ reply_destination_address(Pdu).


%% @spec reply_destination_address(Pdu) -> ParamList
%%    Pdu = pdu()
%%    ParamList = [DestAddrTon | [DestAddrNpi | [DestinationAddr]]]
%%    DestAddrTon = {dest_addr_ton, int()}
%%    DestAddrNpi = {dest_addr_npi, int()}
%%    DestinationAddr = {destination_addr, string()}
%%
%% @doc Creates destination address parameters (<i>ton</i>, <i>npi</i> and 
%% <i>addr</i>) from the source address parameters of a given <tt>Pdu</tt>.
%% @end
reply_destination_address(Pdu) ->
    [{dest_addr_ton, operation:get_param(source_addr_ton, Pdu)},
     {dest_addr_npi, operation:get_param(source_addr_npi, Pdu)},
     {destination_addr, operation:get_param(source_addr, Pdu)}].


%% @spec reply_source_address(Pdu) -> ParamList
%%    Pdu = pdu()
%%    ParamList = [DestAddrTon | [DestAddrNpi | [DestinationAddr]]]
%%    DestAddrTon = {dest_addr_ton, int()}
%%    DestAddrNpi = {dest_addr_npi, int()}
%%    DestinationAddr = {destination_addr, string()}
%%
%% @doc Creates source address parameters (<i>ton</i>, <i>npi</i> and 
%% <i>addr</i>) from the destination address parameters of a given 
%% <tt>Pdu</tt>.
%% @end
reply_source_address(Pdu) ->
    [{source_addr_ton, operation:get_param(dest_addr_ton, Pdu)},
     {source_addr_npi, operation:get_param(dest_addr_npi, Pdu)},
     {source_addr, operation:get_param(destination_addr, Pdu)}].


%% @spec split_user_data(Data, RefNum) -> Segments
%%    Data = string()
%%    RefNum = int()
%%    Segments = [Segment]
%%    Segment = string()
%%
%% @doc Splits the user data into several segments for message concatenation.
%%
%% <p>Resulting <tt>Segments</tt> contain the appropriated UDH for message
%% concatenation, thus they are at most SM_SEGMENT_MAX_SIZE + UDH length (6)
%% long.</p>
%%
%% <p><tt>Segments</tt> are returned in the right order.</p>
%%
%% @see join_user_data/1
%% @end 
split_user_data(Data, RefNum) ->
    split_user_data(Data, ?REF_NUM(RefNum), []).

%% @doc Auxiliary function for split_user_data/2
%%
%% @see split_user_data/2
%% @end 
split_user_data(Data, RefNum, Acc) when length(Data) > ?SM_SEGMENT_MAX_SIZE ->
    {Segment, Rest} = lists:split(?SM_SEGMENT_MAX_SIZE, Data),
    split_user_data(Rest, RefNum, [Segment|Acc]);
split_user_data(Data, RefNum, Acc) ->
    append_udh([Data|Acc], RefNum).


%% @spec split_user_data_tlv(Data) -> Segments
%%    Data = string()
%%    RefNum = int()
%%    Segments = [Segment]
%%    Segment = string()
%%
%% @doc Splits the user data into several segments for message concatenation
%% with <i>sar</i> TLVs.
%%
%% <p>No UDH is appended to the resulting segments.  Use this function with
%% <i>sar</i> parameters.  Use <a href="#split_user_data-2">split_user_data/2
%% </a> if you are using UDHI method for concatenation.</p>
%%
%% <p><tt>Segments</tt> are returned in the right order.  Resulting segments
%% are at most SM_SEGMENT_MAX_SIZE long (must leave room for the SMSC to
%% add the headers).</p>
%%
%% @see split_user_data/2
%% @end 
split_user_data_tlv(Data) ->
    split_user_data_tlv(Data, []).

%% @doc Auxiliary function for split_user_data_tlv/2
%%
%% @see split_user_data_tlv/2
%% @end 
split_user_data_tlv(Data, Acc) when length(Data) > ?SM_SEGMENT_MAX_SIZE ->
    {Segment, Rest} = lists:split(?SM_SEGMENT_MAX_SIZE, Data),
    split_user_data_tlv(Rest, [Segment|Acc]);
split_user_data_tlv(Data, Acc) ->
    lists:reverse([Data|Acc]).


%% @spec total_segments(Pdu) -> TotalSegments
%%    Pdu = pdu()
%%    TotalSegments = int()
%%
%% @doc Returns the total number of segments on a concatenated message.
%%
%% <p>The function will fail if <tt>Pdu</tt> does not correspond with a valid
%% concatenated message.</p>
%% @end 
total_segments(Pdu) ->
    ShortMessage = operation:get_param(short_message, Pdu),
    {[?UDHL,?IEI,?IEIDL, _, TotalSegments|_], _Rest} = chop_udh(ShortMessage),
    TotalSegments.


%% @spec udhi(Pdu) -> bool()
%%    Pdu = pdu()
%%
%% @doc Returns <tt>true</tt> if UDHI is set, <tt>false</tt> otherwise.
%% @end 
udhi(Pdu) ->
    case operation:get_param(esm_class, Pdu) of
        EsmClass when (EsmClass band ?ESM_CLASS_GSM_UDHI)==?ESM_CLASS_GSM_UDHI,
                      EsmClass /= undefined ->
            true;
        _Otherwise ->
            false
    end.


%% @spec udhi(ParamList, Udhi) -> NewParamList
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Udhi = bool()
%%
%% @doc Sets <tt>Udhi</tt> in the <i>esm_class</i> parameter of given list.  
%% If <tt>Udhi</tt> is <tt>true</tt> UDHI is set to 1, when <tt>false</tt> sets
%% the UDHI to 0.  
%%
%% If <i>esm_class</i> parameter was not defined, returned list includes this
%% parameter with a value of <tt>ESM_CLASS_GSM_UDHI</tt> if and only
%% <tt>Udhi</tt> is <tt>true</tt>, if <tt>Udhi</tt> is <tt>false</tt>
%% <i>esm_class</i> will be left undefined.
%% @end 
udhi(ParamList, Udhi) ->
    case lists:keysearch(esm_class, 1, ParamList) of
        {value, {esm_class, EsmClass}} when Udhi == true ->
            NewEsmClass = EsmClass bor ?ESM_CLASS_GSM_UDHI,
            lists:keyreplace(esm_class, 1, ParamList, {esm_class,NewEsmClass});
        {value, {esm_class, EsmClass}} ->
            NewEsmClass = EsmClass band (?ESM_CLASS_GSM_UDHI bxor 2#11111111),
            lists:keyreplace(esm_class, 1, ParamList, {esm_class,NewEsmClass});
        false when Udhi == true ->
            [{esm_class, ?ESM_CLASS_GSM_UDHI}|ParamList];
        false ->
            ParamList
    end.


%%%===================================================================
%%% Undocumented functions
%%%
%%% <p>For compatibility only.  Will be removed in future versions</p>
%%%===================================================================
reply_address(Pdu) ->
    [{dest_addr_ton, operation:get_param(source_addr_ton, Pdu)},
     {dest_addr_npi, operation:get_param(source_addr_npi, Pdu)},
     {destination_addr, operation:get_param(source_addr, Pdu)}].


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @spec append_udh(Segments, RefNum) -> UdhSegments
%%
%% @doc <tt>Segments</tt> are assumed to be in reversed order.
%%
%% @see split_user_data/2
%% @end 
append_udh(Segments, RefNum) ->
    TotalSegments = length(Segments),
    append_udh(Segments, RefNum, TotalSegments, TotalSegments, []).

%% @doc Auxiliary function for append_udh/2
%%
%% @see append_udh/2
%% @end 
append_udh([], _RefNum, _TotalSegments, _SeqNum, Acc) ->
    Acc;
append_udh([H|T], RefNum, TotalSegments, SeqNum, Acc) ->
    % UDH = [?UDHL, ?IEI, ?IEIDL, RefNum, TotalSegments, SeqNum]
    NewH = [?UDHL, ?IEI, ?IEIDL, RefNum, TotalSegments, SeqNum | H],
    append_udh(T, RefNum, TotalSegments, SeqNum - 1, [NewH|Acc]).
