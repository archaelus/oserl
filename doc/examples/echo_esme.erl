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
% @doc Echo ESME.
%
% <p>A complete echo ESME.</p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [18 Feb 2004]
% 
% <ul>
%   <li>Trailing $\0 removed from the c_octet_string values.</li>
% </ul>
%
% [26 Feb 2004]
%
% <ul>
%   <li>Last changes in <a href="gen_esme.html">gen_esme.erl</a> adopted.</li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique]
% @version 1.0, {18 Jul 2003} {@time}.
% @end
%%
-module(echo_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("gen_esme.hrl").

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([start/0, start_link/5, stop/0]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
% Internal gen_esme exports
%%--------------------------------------------------------------------
-export([deliver_sm/3, deliver_data_sm/3]).
-export([bound_receiver/3,
         bound_transmitter/3,
         receiver_cannot_bind/3,
         transmitter_cannot_bind/3,
         receiver_unbind/2,
         transmitter_unbind/2,
         receiver_mc_unavailable/2,
         transmitter_mc_unavailable/2]).

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
% @spec start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) -> ok
%    SystemId = string()
%    Password = string()
%    AddrRange = string()
%    SourceAddr = string()
%    McAddr = string() | atom() | ip_address()
%
% @doc Starts the echo ESME and binds as a receiver and transmitter to McAddr.
% @end
%
% %@see
%
% %@equiv
%%
start() ->
    start_link("banana", "secret", "1948", "1948", {193, 144, 50, 51}).
    

start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) ->
%    Setup = ?GEN_ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    Setup = #gen_esme_setup{
      system_id         = SystemId,
      password          = Password,
      addr_ton          = ?TON_INTERNATIONAL,
      addr_npi          = ?NPI_ISDN,
      address_range     = AddrRange,
      source_addr_ton   = ?TON_INTERNATIONAL,
      source_addr_npi   = ?NPI_ISDN,
      source_addr       = SourceAddr,
      service_type      = ?SERVICE_TYPE_NULL,
      system_type       = ?NULL_C_OCTET_STRING,
      session_init_time = ?SESSION_INIT_TIME, % 180000
      enquire_link_time = ?ENQUIRE_LINK_TIME,
      inactivity_time   = 240000,
      response_time     = ?RESPONSE_TIME,
      rebind_time       = ?REBIND_TIME},
    case gen_esme:start_link({local, echo_esme}, ?MODULE, Setup) of
        {ok, Eid} ->
            gen_esme:open_transmitter(echo_esme, McAddr),
            gen_esme:bind_transmitter(echo_esme),
%            receive after 120000 ->    ok end,
            gen_esme:open_receiver(echo_esme, McAddr),
            gen_esme:bind_receiver(echo_esme),
            process_flag(trap_exit, true),
            {ok, Eid};
        Error ->
            Error
    end.


%%%
% @spec stop() -> ok
%
% @doc Unbinds and stops the echo ESME.
% @end
%
% %@see
%
% %@equiv
%%
stop() ->
    gen_esme:unbind_receiver(echo_esme),
    gen_esme:close_receiver(echo_esme),
    gen_esme:unbind_transmitter(echo_esme),
    gen_esme:close_transmitter(echo_esme),
    gen_esme:stop(echo_esme).


%%%===================================================================
% ESME functions
%%====================================================================
%%%
% @spec bound_receiver(Pid, Sid, PduResp) -> ok
%    Pid = pid()
%    Eid = pid()
%    PduResp = pdu()
%
% @doc The ESME just bound as a receiver.
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
bound_receiver(Pid, Sid, PduResp) ->
    SystemId = operation:get_value(system_id, PduResp),
    io:format("Bound as receiver to ~p~n", [SystemId]).


%%%
% @spec bound_transmitter(Pid, Sid, PduResp) -> ok
%    Pid = pid()
%    Eid = pid()
%    PduResp = pdu()
%
% @doc The ESME just bound as a transmitter.
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
bound_transmitter(Pid, Sid, PduResp) ->
    SystemId = operation:get_value(system_id, PduResp),
    io:format("Bound as transmitter to ~p~n", [SystemId]).


%%%
% @spec receiver_unbind(Pid, Eid) -> ok | {error, Error}
%    Pid = pid()
%    Eid = pid()
%    Error = int()
%
% @doc This callback forwards an unbind request (issued by the MC) to the 
% receiver session of the ESME.  If <tt>ok</tt> returned an unbind_resp
% with a ESME_ROK command_status is sent to the MC and the session moves 
% into the unbound state.  When <tt>{error, Error}</tt> is returned by 
% the ESME, the response PDU sent by the session to the MC will have an 
% <tt>Error</tt> command_status and the session will remain on it's 
% current bound_rx state.
%
% <p><tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
receiver_unbind(Pid, Eid) -> 
    io:format("Unbind request received on receiver session~n", []).


%%%
% @spec transmitter_unbind(Pid, Eid) -> ok | {error, Error}
%    Pid = pid()
%    Eid = pid()
%    Error = int()
%
% @doc This callback forwards an unbind request (issued by the MC) to the 
% transmitter session of the ESME.  If <tt>ok</tt> returned an unbind_resp
% with a ESME_ROK command_status is sent to the MC and the session moves 
% into the unbound state.  When <tt>{error, Error}</tt> is returned by 
% the ESME, the response PDU sent by the session to the MC will have an 
% <tt>Error</tt> command_status and the session will remain on it's 
% current bound_tx state.
%
% <p><tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
transmitter_unbind(Pid, Eid) -> 
    io:format("Unbind request received on transmitter session~n", []).


%%%
% @spec receiver_mc_unavailable(Pid, Eid) -> ok
%    Pid = pid()
%    Eid = pid()
%
% @doc If the MC becomes unavailable on the receiver session, this circumstance
% is notified with a call to this function.
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
receiver_mc_unavailable(Pid, Eid) ->
    io:format("MC unavailable on receiver session~n", []).


%%%
% @spec transmitter_mc_unavailable(Pid, Eid) -> ok
%    Pid = pid()
%    Eid = pid()
%
% @doc If the MC becomes unavailable on the transmitter session, this
% circumstance is notified with a call to this function.
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
transmitter_mc_unavailable(Pid, Eid) ->
    io:format("MC unavailable on transmitter session~n", []).


%%%
% @spec receiver_cannot_bind(Pid, Eid, Error) -> ok
%    Pid = pid()
%    Eid = pid()
%    Error = {error, term()}
%
% @doc Cannot bind on the receiver session.
% 
% <p><tt>Error</tt> is the error returned by the bind operation.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
receiver_cannot_bind(Pid, Eid, Error) ->
    io:format("Can not bind on receiver session: ~p~n", [Error]).
     

%%%
% @spec transmitter_cannot_bind(Pid, Eid, Error) -> ok
%    Pid = pid()
%    Eid = pid()
%    Error = {error, term()}
%
% @doc Cannot bind on the transmitter session.
% 
% <p><tt>Error</tt> is the error returned by the bind operation.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
transmitter_cannot_bind(Pid, Eid, Error) ->
    io:format("Can not bind on transmitter session: ~p~n", [Error]).


%%%
% @spec deliver_sm(Pid, Eid, Pdu) -> Result
%    Pid        = pid()
%    Eid        = pid()
%    Pdu        = pdu()
%    Result     = {ok, ParamList} | {error, Error, ParamList}
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%
% @doc Short messages delivered by the ESME via this callback are enclosed
% inside a deliver_sm PDU.
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the deliver_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
deliver_sm(_Pid, Eid, Pdu) -> 
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    io:format("Echoing SM: ~p~n", [Mesg]),
    spawn_link(fun() -> gen_esme:submit_sm(Eid, [Mesg|Dest]) end), 
    {ok, []}.


%%%
% @spec deliver_data_sm(Pid, Eid, Pdu) -> Result
%    Pid        = pid()
%    Eid        = pid()
%    Pdu        = pdu()
%    Result     = {ok, ParamList} | {error, Error, ParamList}
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%
% @doc Short messages delivered by the ESME via this callback are enclosed
% inside a data_sm PDU.
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the data_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
deliver_data_sm(_Pid, Eid, Pdu) -> 
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    io:format("Echoing SM: ~p~n", [Mesg]),
    spawn(fun() -> gen_esme:data_sm(Eid, [Mesg|Dest]) end), 
    {ok, []}.

%%%===================================================================
% Internal functions
%%====================================================================

