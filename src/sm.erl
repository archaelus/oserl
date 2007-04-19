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
%%% <h2>Changes 1.2 -&gt; 1.3</h2>
%%%
%%% [20 Sep 2006]
%%%
%%% <p>Improve support for message concatenation and support for application
%%% port addressing.</p>
%%%
%%% <ul>
%%%   <li>Function <i>reply_address/1</i> removed.
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
-include("oserl.hrl").
-include("smpp_base.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([add_ie/2,
         chop_udh/1,
         destination_port/1,
         ie/2,
         join_user_data/1,
         message_user_data/1,
         originator_port/1,
         port_addressing_8/3,
         port_addressing_16/3,
         reference_number/1,
         reply_addresses/1,
         reply_destination_address/1,
         reply_source_address/1,
         split/3,
         split_user_data/2,
         total_segments/1,
         udhi/1,
         udhi/2]).

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
%% @spec add_ie(ParamList, Ie) -> NewParamList
%%    ParamList = [{ParamName, ParamValue}]
%%    Ie = string()
%%    NewParamList = ParamList
%%
%% @doc Adds an Information Element to the User Data Header.  If needed the
%% User Data Header Indicator is set.  A <tt>NewParamList</tt> is returned.
%% @end 
add_ie(ParamList, Ie) ->
    {Tag, Data} = message_user_data(ParamList),
    case udhi(ParamList) of
        true ->
            {[_Udhl|Ies], Rest} = chop_udh(Data),
            NewData = udh([Ie, Ies]) ++ Rest,
            [{Tag, NewData}|lists:keydelete(Tag, 1, ParamList)];
        false ->
            NewData = udh([Ie]) ++ Data,
            [{Tag, NewData}|udhi(lists:keydelete(Tag, 1, ParamList), true)]
    end.

%% @spec chop_udh(Data) -> {Udh, Rest}
%%     Data = string()
%%     Rest = string()
%%     Udh = string()
%%
%% @doc Chops the User Data Header from the User Data of a short message.
%% @end 
chop_udh([Udhl|_] = Data) ->
    lists:split(Udhl + 1, Data).

%% @spec destination_port(Pdu) -> DestinationPort
%%    Pdu = pdu()
%%    DestinationPort = int()
%%
%% @doc Returns the <tt>DestinationPort</tt> of an Application Port Addressed
%% Short Message.
%%
%% <p>The function will fail if <tt>Pdu</tt> does not correspond with a valid
%% Application Port Addressed message.</p>
%% @end 
destination_port(Pdu) ->
    {_Tag, Data} = message_user_data(Pdu),
    case catch ie(?IEI_PORT_16, Data) of
        [?IEI_PORT_16, _, DestPort1, DestPort2, _, _] ->
            <<DestPort:16>> = <<DestPort1:8, DestPort2:8>>,
            DestPort;
        _Error ->
            [?IEI_PORT_8, _, DestPort, _] = ie(?IEI_PORT_8, Data),
            DestPort
    end.

%% @spec ie(Iei, Udh) -> Ie
%%    Iei = int()
%%    Udh = string()
%%    Ie = string()
%%
%% @doc Returns the Information Element in the User Data Header identified by
%% <tt>Iei</tt>.  If no IE found for the given id the function crashes.
%% @end 
ie(Iei, [Udhl|T]) ->
    ie(Iei, T, Udhl).

%% @doc Auxiliary function for ie/2
%%
%% @see ie/2
%% @end 
ie(Iei, [Iei, Iedl|_] = Data, _Len) -> 
    lists:sublist(Data, Iedl + 1);
ie(Iei, [_Other, Iedl|_] = Data, Len) ->
    NewLen = Len - Iedl - 1,
    ie(Iei, lists:sublist(Data, Iedl + 1,  NewLen), NewLen).

%% @spec join_user_data(Segments) -> {Data, RefNum}
%%    Segments = [Segment]
%%    Segment = string()
%%    Data = string()
%%    RefNum = int()
%%
%% @doc Joins the <tt>Segments</tt> of a concatenated message.
%%
%% <p>This functions assumes that no other IE than concatenation IE is present 
%% in the segments UDH, otherwise resulting <tt>Data</tt> will be corrupted.</p>
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
message_user_data(ParamList) when list(ParamList) ->
    case lists:keysearch(short_message, 1, ParamList) of
        {value, Value} ->
            Value;
        false ->
            {value, Value} = lists:keysearch(message_payload, 1, ParamList),
            Value
    end;
message_user_data(Pdu) ->
    case operation:get_param(short_message, Pdu) of
        ShortMessage when ShortMessage == ""; ShortMessage == undefined ->
            {message_payload, operation:get_param(message_payload, Pdu)};
        ShortMessage ->
            {short_message, ShortMessage}
    end.

%% @spec originator_port(Pdu) -> OriginatorPort
%%    Pdu = pdu()
%%    OriginatorPort = int()
%%
%% @doc Returns the <tt>OriginatorPort</tt> of an Application Port Addressed
%% Short Message.
%%
%% <p>The function will fail if <tt>Pdu</tt> does not correspond with a valid
%% Application Port Addressed message.</p>
%% @end 
originator_port(Pdu) ->
    {_Tag, Data} = message_user_data(Pdu),
    case catch ie(?IEI_PORT_16, Data) of
        [?IEI_PORT_16, _, _, _, OrigPort1, OrigPort2] ->
            <<OrigPort:16>> = <<OrigPort1:8, OrigPort2:8>>,
            OrigPort;
        _Error ->
            [?IEI_PORT_8, _, _, OrigPort] = ie(?IEI_PORT_8, Data),
            OrigPort
    end.

%% @spec port_addressing_8(ParamList, DestPort, OrigPort) -> 
%%           NewParamList
%%    Data = string()
%%    DestPort = int()
%%    OrigPort = int()
%%    NewData = string()
%%
%% @doc Adds application port addressing 8 bit address header to the message
%% user data.
%% @end 
port_addressing_8(ParamList, DestPort, OrigPort) ->
    Ie = port_addressing_8_ie(DestPort, OrigPort),
    add_ie(ParamList, Ie).

%% @spec port_addressing_16(ParamList, DestPort, OrigPort) -> 
%%           NewParamList
%%    Data = string()
%%    DestPort = int()
%%    OrigPort = int()
%%    NewData = string()
%%
%% @doc Adds application port addressing 16 bit address header to the message
%% user data.
%% @end 
port_addressing_16(ParamList, DestPort, OrigPort) ->
    Ie = port_addressing_16_ie(DestPort, OrigPort),
    add_ie(ParamList, Ie).

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
    [?IEI_CONCAT, _, RefNum|_] = ie(?IEI_CONCAT, ShortMessage),
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

%% @spec split(ParamList, RefNum, ConcatenationMethod) -> ParamListList
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    RefNum = int()
%%    ConcatenationMethod = udh | tlv
%%    ParamListList = [ParamList]
%%
%% @doc Returns a list with a <tt>ParamList</tt> for every segment of
%% a concatenated short message.
%% @end 
split(ParamList, RefNum, udh) ->
    {value, {short_message, SM}} = lists:keysearch(short_message, 1, ParamList),
    % The short message may already include an User Data Header
    {Ies, Segments} = case udhi(ParamList) of
                          true ->
                              {[Udhl|Rest], Data} = chop_udh(SM),
                              {Rest, segments(Data, ?SM_MAX_SEGMENT_SIZE-Udhl)};
                          false ->
                              {[], segments(SM, ?SM_MAX_SEGMENT_SIZE)}
                      end,
    TotalSegments = length(Segments),
    % Remove the long SM and add the UDHI if not present
    NewParamList = udhi(lists:keydelete(short_message, 1, ParamList), true),
    % Add the Concat Information Element to the UDH of every Segment and
    % build a new Param List for each.
    F = fun(Segment, SeqNum) ->
                ConcatIe = concat_ie(RefNum, TotalSegments, SeqNum),
                ShortMessage = udh([ConcatIe|Ies]) ++ Segment,
                {[{short_message, ShortMessage}|NewParamList], SeqNum + 1}
        end,
    element(1, lists:mapfoldl(F, 1, Segments));
split(ParamList, RefNum, tlv) ->
    {value, {short_message, SM}} = lists:keysearch(short_message, 1, ParamList),
    % The short message may already include an User Data Header
    Segments = case udhi(ParamList) of
                   true ->
                       {[Udhl|_Rest] = Udh, Data} = chop_udh(SM),
                       L = segments(Data, ?SM_MAX_SEGMENT_SIZE - Udhl),
                       % Add the header to every segment
                       lists:map(fun(X) -> Udh ++ X end, L);
                   false ->
                       segments(SM, ?SM_MAX_SEGMENT_SIZE)
               end,
    TotalSegments = length(Segments),
    % Remove the long SM from the Param List
    NewParamList = lists:keydelete(short_message, 1, ParamList),
    % Create the sar TLVs for every Segment and build a new Param List for each
    F = fun(Segment, SeqNum) ->
                Params = [{short_message, Segment},
                          {sar_msg_ref_num, RefNum},
                          {sar_segment_seqnum, SeqNum},
                          {sar_total_segments, TotalSegments}|NewParamList],
                {Params, SeqNum + 1}
        end,
    element(1, lists:mapfoldl(F, 1, Segments)).


%% @spec split_user_data(Data, RefNum) -> Segments
%%    Data = string()
%%    RefNum = int()
%%    Segments = [Segment]
%%    Segment = string()
%%
%% @doc Splits the user <tt>Data</tt> into <tt>Segments</tt> for concatenation.
%% Returned <tt>Segments</tt> have the appropriated UDH.
%%
%% <p>This functions assumes that no UDH is present in <tt>Data</tt>, if so
%% resulting <tt>Segments</tt> will be corrupted.</p>
%% @end 
split_user_data(Data, RefNum) ->
    Segments = segments(Data, ?SM_MAX_SEGMENT_SIZE),
    TotalSegments = length(Segments),
    F = fun(Segment, SeqNum) ->
                ConcatIe = concat_ie(RefNum, TotalSegments, SeqNum),
                {udh([ConcatIe]) ++ Segment, SeqNum + 1}
        end,
    element(1, lists:mapfoldl(F, 1, Segments)).

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
    [?IEI_CONCAT, _, _, TotalSegments|_] = ie(?IEI_CONCAT, ShortMessage),
    TotalSegments.

%% @spec udhi(Pdu) -> bool()
%%    Pdu = pdu()
%%
%% @doc Returns <tt>true</tt> if UDHI is set, <tt>false</tt> otherwise.
%% @end
udhi(ParamList) when list(ParamList) ->
    case lists:keysearch(esm_class, 1, ParamList) of
        {value, {esm_class, EsmClass}} ->
            udhi_value(EsmClass);
        _Otherwise ->
            false
    end;
udhi(Pdu) ->
    case operation:get_param(esm_class, Pdu) of
        EsmClass when EsmClass /= undefined ->
            udhi_value(EsmClass);
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
%%% Internal functions
%%%===================================================================
%% @spec concat_ie(RefNum, TotalSegments, SeqNum) -> Ie
%%     RefNum = int()
%%     TotalSegments = int()
%%     Seqnum = int()
%%     Ie = string()
%%
%% @doc Returns the Information Element <tt>Ie</tt> for a concatenated message.
%% @end 
concat_ie(RefNum, TotalSegments, SeqNum) ->
    [?IEI_CONCAT, ?IEDL_CONCAT, RefNum, TotalSegments, SeqNum].

%% @spec port_addressing_8_ie(DestPort, OrigPort) -> Ie
%%     DestPort = int()
%%     OrigPort = int()
%%     Ie = string()
%%
%% @doc Returns the Information Element <tt>Ie</tt> for Application Port 
%% Addressing 16 bit address.  Only lower 8 bits of the port numbers are taken.
%% @end 
port_addressing_8_ie(DestPort, OrigPort) ->
    [DestPort1] = binary_to_list(<<DestPort:8>>),
    [OrigPort1] = binary_to_list(<<OrigPort:8>>),
    [?IEI_PORT_8, ?IEDL_PORT_8, DestPort1, OrigPort1].

%% @spec port_addressing_16_ie(DestPort, OrigPort) -> Ie
%%     DestPort = int()
%%     OrigPort = int()
%%     Ie = string()
%%
%% @doc Returns the Information Element <tt>Ie</tt> for Application Port 
%% Addressing 16 bit address.  Only lower 16 bits of the port numbers are taken.
%% @end 
port_addressing_16_ie(DestPort, OrigPort) ->
    [DestPort1, DestPort2] = binary_to_list(<<DestPort:16>>),
    [OrigPort1, OrigPort2] = binary_to_list(<<OrigPort:16>>),
    [?IEI_PORT_16, ?IEDL_PORT_16, DestPort1, DestPort2, OrigPort1, OrigPort2].

%% @spec segments(Data, Len) -> Segments
%%    Data = string()
%%    Len = int()
%%    Segments = [Segment]
%%    Segment = string()
%%
%% @doc Splits <tt>Data</tt> in <tt>Segments</tt> of at most <tt>Len</tt>
%% characters.
%% @end 
segments(Data, Len) ->
    segments(Data, Len, []).

%% @doc Auxiliary function for segments/2
%%
%% @see segments/2
%% @end 
segments(Data, Len, Acc) ->
    case catch lists:split(Len, Data) of
        {'EXIT', _Badarg} ->
            lists:reverse([Data|Acc]);
        {Segment, []} ->
            lists:reverse([Segment|Acc]);
        {Segment, Rest} ->
            segments(Rest, Len, [Segment|Acc])
    end.

%% @spec udh(IeList) -> Udh
%%    IeList = [Ie]
%%    Ie = string()
%%    Udh = string()
%%
%% @doc Creates the <tt>Udh</tt> given the list of Information Elements.
%% @end 
udh(IeList) ->
    Udh = lists:flatten(IeList),
    [length(Udh) | Udh].

%% @spec udhi_value(EsmClass) -> bool()
%%    EsmClass = int()
%%
%% @doc Returns <tt>true</tt> if the User Data Header Indicator bit is 1
%% in <tt>EsmClass</tt>, <tt>false</tt> otherwise.
%% @end 
udhi_value(EsmClass) 
  when (EsmClass band ?ESM_CLASS_GSM_UDHI) == ?ESM_CLASS_GSM_UDHI ->
    true;
udhi_value(_EsmClass) ->
    false.
