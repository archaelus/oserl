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

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([message_user_data/1, 
         reply_addresses/1,
         reply_destination_address/1,
         reply_source_address/1]).

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
