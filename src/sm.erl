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
% @doc Short Message Library Module.
%
% <p>Utility functions for short messages on SMPP requests/responses.</p>
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {24 Jul 2003} {@time}.
% @end
%%
-module(sm).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([message_user_data/1, reply_address/1]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec message_user_data(Pdu) -> UserData
%    Pdu        = pdu()
%    UserData   = {ParamName, ParamValue}
%    ParamName  = short_message | message_payload
%    ParamValue = undefined | string()
%
% @doc Gets the message user data from a PDU.  The message user data may came 
% in the short_message or in the message_payload parameter.
% @end
%
% %@see
%
% %@equiv
%%
message_user_data(Pdu) ->
    case pdu_syntax:get_value(short_message, Pdu) of
        ShortMessage when ShortMessage == ""; ShortMessage == undefined ->
            {message_payload, pdu_syntax:get_value(message_payload, Pdu)};
        ShortMessage ->
            {short_message, ShortMessage}
    end.


%%%
% @spec reply_address(Pdu) -> ParamList
%    Pdu = pdu()
%    ParamList = [DestAddrTon | [DestAddrNpi | [DestinationAddr]]]
%    DestAddrTon = {dest_addr_ton, int()}
%    DestAddrNpi = {dest_addr_npi, int()}
%    DestinationAddr = {destination_addr, string()}
%
% @doc Creates destination address parameters from the source address 
% parameters of a given <tt>Pdu</tt>.
% @end
%
% %@see
%
% %@equiv
%%
reply_address(Pdu) ->
    [{dest_addr_ton,    pdu_syntax:get_value(source_addr_ton, Pdu)},
     {dest_addr_npi,    pdu_syntax:get_value(source_addr_npi, Pdu)},
     {destination_addr, pdu_syntax:get_value(source_addr,     Pdu)}].


%%%===================================================================
% Internal functions
%%====================================================================
