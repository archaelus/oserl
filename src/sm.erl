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
%%% <ul>
%%%   <li>Functions <a href="#set_udhi-1">set_udhi/1</a> and 
%%%     <a href="#split_user_data-2">split_user_data/2</a> added.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 0.2, {24 Jul 2003} {@time}.
%%% @end
-module(sm).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_base.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([message_user_data/1, 
         reply_addresses/1,
         reply_destination_address/1,
         reply_source_address/1,
         set_udhi/1,
         split_user_data/2]).

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
%% @spec message_user_data(Pdu) -> UserData
%%    Pdu        = pdu()
%%    UserData   = {ParamName, ParamValue}
%%    ParamName  = short_message | message_payload
%%    ParamValue = undefined | string()
%%
%% @doc Gets the message user data from a PDU.  The message user data may came 
%% in the short_message or in the message_payload parameter.
%% @end
message_user_data(Pdu) ->
    case operation:get_param(short_message, Pdu) of
        ShortMessage when ShortMessage == ""; ShortMessage == undefined ->
            {message_payload, operation:get_param(message_payload, Pdu)};
        ShortMessage ->
            {short_message, ShortMessage}
    end.


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


%% @spec set_udhi(ParamList) -> NewParamList
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc Sets UDHI in the <i>esm_class</i> parameter of given list.  If
%% <i>esm_class</i> was not defined, returned list includes it with a
%% value of <tt>ESM_CLASS_GSM_UDHI</tt>.
%% @end 
set_udhi(ParamList) ->
    case lists:keysearch(esm_class, 1, ParamList) of
        {value, {esm_class, EsmClass}} ->
            NewEsmClass = EsmClass bor ?ESM_CLASS_GSM_UDHI,
            lists:keyreplace(esm_class, 1, ParamList, {esm_class,NewEsmClass});
        false ->
            [{esm_class, ?ESM_CLASS_GSM_UDHI}|ParamList]
    end.


%% @spec split_user_data(Data, RefNum) -> Segments
%%    Data = string()
%%    RefNum = int()
%%    Segments = [Segment]
%%    Segment = string()
%%
%% @doc Splits the user data into several segments for message concatenation.
%%
%% <p><tt>Data</tt> is only splited if longer than SM_MAX_SIZE macro
%% (defaults to 160), otherwise the list <tt>[Data]</tt> is returned.</p>
%%
%% <p>Resulting <tt>Segments</tt> contain the appropriated UDH for message
%% concatenation, thus they are at most SM_SEGMENT_MAX_SIZE + UDH length (6).
%% </p>
%%
%% <p><tt>Segments</tt> are returned in the right order.</p>
%% @end 
split_user_data(Data, RefNum) when length(Data) > ?SM_MAX_SIZE ->
    split_user_data(Data, ?REF_NUM(RefNum), []);
split_user_data(Data, _RefNum) ->
    [Data].

%% @doc Auxiliary function for split_user_data/2
%%
%% @see split_user_data/2
%% @end 
split_user_data(Data, RefNum, Acc) when length(Data) > ?SM_SEGMENT_MAX_SIZE ->
    {Segment, Rest} = lists:split(?SM_SEGMENT_MAX_SIZE, Data),
    split_user_data(Rest, RefNum, [Segment|Acc]);
split_user_data(Data, RefNum, Acc) ->
    append_udh([Data|Acc], RefNum).


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
    % UDH: UDHL, IEI, IEIDL, RefNum, TotalSegments, SeqNum
    NewH = [?UDHL, ?IEI, ?IEIDL, RefNum, TotalSegments, SeqNum | H],
    append_udh(T, RefNum, TotalSegments, SeqNum - 1, [NewH|Acc]).


