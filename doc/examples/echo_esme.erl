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
% @doc Echo ESME.
%
% <p>A complete echo ESME.</p>
%
% @copyright 2003 Enrique Marcote Peña
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
         receiver_unbind/2,
         transmitter_unbind/2,
         receiver_mc_unavailable/4,
         transmitter_mc_unavailable/4,
         receiver_can_not_resume/5,
         transmitter_can_not_resume/5]).

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
    start_link("banana\0", "secret\0", "1948\0", "1948\0", {193, 144, 50, 51}).
    

start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) ->
    Setup = ?GEN_ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    case gen_esme:start_link({local, echo_esme}, ?MODULE, Setup) of
        {ok, Eid} ->
            gen_esme:bind_receiver(echo_esme, McAddr),
            gen_esme:bind_transmitter(echo_esme, McAddr),
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
    gen_esme:unbind_transmitter(echo_esme),
    gen_esme:stop(echo_esme).


%%%===================================================================
% ESME functions
%%====================================================================
%%%
% @spec bound_receiver(Pid, Sid, SystemId) -> ok
%    Pid = pid()
%    Eid = pid()
%    SystemId = string()
%
% @doc After an outbind request or a period during the MC was unreachable, if 
% the ESME gets to (re)bind as a receiver to the MC, this callback is triggered
% to notify.  Returning value is ignored by the ESME. The callback module
% may start some initialization on response to this callback.
%
% <p>The MC on the other peer is identified by <tt>SystemId</tt>.  
% <tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process
% id.</p>
% @end
%
% %@see
%
% %@equiv
%%
bound_receiver(Pid, Sid, SystemId) ->
    io:format("Bound as receiver to ~p~n", [SystemId]).


%%%
% @spec bound_transmitter(Pid, Sid, SystemId) -> ok
%    Pid = pid()
%    Eid = pid()
%    SystemId = string()
%
% @doc After a period during the MC was unreachable, if the ESME gets to 
% rebind as a transmitter to the MC, this callback is triggered to notify.  
% Returning value is ignored by the ESME.  The callback module may start some
% initialization on response to this callback.
%
% <p>The MC on the other peer is identified by <tt>SystemId</tt>.  <tt>Pid</tt>
% is the ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
bound_transmitter(Pid, Sid, SystemId) ->
    io:format("Bound as transmitter to ~p~n", [SystemId]).


%%%
% @spec bound_transceiver(Pid, Sid, SystemId) -> ok
%    Pid = pid()
%    Eid = pid()
%    SystemId = string()
%
% @doc After an outbind request or a period during the MC was unreachable, if 
% the ESME gets to (re)bind as a transceiver to the MC, this callback is 
% triggered to notify.  Returning value is ignored by the ESME.  The callback 
% module may start some initialization on response to this callback.
%
% <p>The MC on the other peer is identified by <tt>SystemId</tt>.  <tt>Pid</tt>
% is the ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
% @end
%
% %@see
%
% %@equiv
%%

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
% @spec transceiver_unbind(Pid, Eid) -> ok | {error, Error}
%    Pid = pid()
%    Eid = pid()
%    Error = int()
%
% @doc This callback forwards an unbind request (issued by the MC) to the 
% transceiver session of the ESME.  If <tt>ok</tt> returned an unbind_resp
% with a ESME_ROK command_status is sent to the MC and the session moves 
% into the unbound state.  When <tt>{error, Error}</tt> is returned by 
% the ESME, the response PDU sent by the session to the MC will have an 
% <tt>Error</tt> command_status and the session will remain on it's 
% current bound_trx state.
%
% <p><tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%


%%%
% @spec receiver_mc_unavailable(Pid, Eid, Address, Port) -> ok
%    Pid = pid()
%    Eid = pid()
%    Address = string() | atom() | ip_address()
%    Port = int()
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
receiver_mc_unavailable(Pid, Eid, Address, Port) ->
    io:format("MC unavailable on receiver session~n", []).


%%%
% @spec transmitter_mc_unavailable(Pid, Eid, Address, Port) -> ok
%    Pid = pid()
%    Eid = pid()
%    Address = string() | atom() | ip_address()
%    Port = int()
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
transmitter_mc_unavailable(Pid, Eid, Address, Port) ->
    io:format("MC unavailable on transmitter session~n", []).
     

%%%
% @spec transceiver_mc_unavailable(Pid, Eid, Address, Port) -> ok
%    Pid = pid()
%    Eid = pid()
%    Address = string() | atom() | ip_address()
%    Port = int()
%
% @doc If the MC becomes unavailable on the transceiver session, this
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


%%%
% @spec receiver_can_not_resume(Pid, Eid, Addr, Port, Err) -> ok
%    Pid = pid()
%    Eid = pid()
%    Addr = string() | atom() | ip_address()
%    Port = int()
%    Err  = term()
%
% @doc After a period of unavailability of the MC, once the underlying 
% connection was recover, the receiver session failed to rebind to the MC with
% error <tt>Err</tt>.  The callback module must take care of this 
% situation.
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
receiver_can_not_resume(Pid, Eid, Addr, Port, Err) ->
    io:format("Can not resume service on receiver session~n", []).
     

%%%
% @spec transmitter_can_not_resume(Pid, Eid, Addr, Port, Err) -> ok
%    Pid = pid()
%    Eid = pid()
%    Addr = string() | atom() | ip_address()
%    Port = int()
%    Err  = term()
%
% @doc After a period of unavailability of the MC, once the underlying 
% connection was recover, the transmitter session failed to rebind to the MC 
% with error <tt>Err</tt>.  The callback module must take care of this 
% situation.
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
transmitter_can_not_resume(Pid, Eid, Addr, Port, Err) ->
    io:format("Can not resume service on transmitter session~n", []).


%%%
% @spec transceiver_can_not_resume(Pid, Eid, Addr, Port, Err) -> ok
%    Pid = pid()
%    Eid = pid()
%    Addr = string() | atom() | ip_address()
%    Port = int()
%    Err  = term()
%
% @doc After a period of unavailability of the MC, once the underlying 
% connection was recover, the transceiver session failed to rebind to the MC 
% with error <tt>Err</tt>.  The callback module must take care of this 
% situation.
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%

%%%
% @spec smpp_listen_error(Pid, Eid, Port) -> ok
%    Pid = pid()
%    Eid = pid()
%    Port = int()
%
% @doc When an ESME on listening state looses the listen socket, uses
% this callback to notify the failure.
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%

%%%
% @spec smpp_listen_recovery(Pid, Eid, Port) -> ok
%    Pid = pid()
%    Eid = pid()
%    Port = int()
%
% @doc After a listen failure, a new socket could be set to listen again on
% <tt>Port</tt>.  The ESME uses this callback to notify the recovery.
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%


%%%
% @spec alert_notification(Pid, Eid, Pdu) -> ok
%    Pid = pid()
%    Eid = pid()
%    Pdu = pdu()
%
% @doc Alert notifications are forwarded by the ESME using this callback.  The
% alert_notification PDU is given along.
%
% <p>The callback module is responsible of initiating the appropriate actions
% associated to the alert notification.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME process id.
% </p>
% @end
%
% %@see
%
% %@equiv
%%


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
