%%%
% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@udc.es>
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
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [10 Feb 2004]
%
% <ul>
%   <li>Calls to <tt>pdu_syntax:get_value/2</tt> replaced by 
%     <tt>operation:get_param/2</tt>.
%    </li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.2 alpha, {24 Jul 2003} {@time}.
% @end
%%
-module(sm).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Imports.  
%
% <p>Imports are explicitly made on the function calls.  These lines are
% commented out and only included for informative purposes.</p>
%%--------------------------------------------------------------------
% -import(operation, [get_param/2]).

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
    case operation:get_param(short_message, Pdu) of
        ShortMessage when ShortMessage == ""; ShortMessage == undefined ->
            {message_payload, operation:get_param(message_payload, Pdu)};
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
    [{dest_addr_ton,    operation:get_param(source_addr_ton, Pdu)},
     {dest_addr_npi,    operation:get_param(source_addr_npi, Pdu)},
     {destination_addr, operation:get_param(source_addr,     Pdu)}].


%%%===================================================================
% Internal functions
%%====================================================================
