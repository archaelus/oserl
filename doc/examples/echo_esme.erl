%%%
% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
% <p>This module implements the <a href="gen_esme.html">gen_esme</a> behavior. % Notice how the header <i>gen_esme.hrl</i> must be also included to import the
% <tt>gen_esme_setup</tt> record definition.</p>
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
% [27 Feb 2004]
%
% <ul>
%   <li>Uses new callbacks defined in <a href="gen_esme.html">gen_esme.erl</a>.
%   </li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
%
% <p>You shall implement the callbacks you need for your particular ESME.
% This module gives sample implementations for callbacks exported below.</p>
%%--------------------------------------------------------------------
-export([bind_receiver_resp/3,
         bind_transmitter_resp/3,
         deliver_sm/3, 
         deliver_data_sm/3,
         receiver_mc_unavailable/2,
         transmitter_mc_unavailable/2,
         receiver_mc_unbind/2,
         transmitter_mc_unbind/2,
         unbind_receiver_resp/3,
         unbind_transmitter_resp/3]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
-define(MC_ADDR, {193, 144, 50, 51}).
-define(ESME_NAME, echo_esme).

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec start() -> ok
%
% @doc Starts the echo ESME.
%
% <p>Every ESME needs a start and stop function.  You may call these functions
% as you wish.</p>
% @end
%
% %@see
%
% %@equiv
%%
start() ->
    start_link("echo_esme", "secret", "1948", "1948", ?MC_ADDR).


%%%
% @spec start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) -> ok
%    SystemId = string()
%    Password = string()
%    AddrRange = string()
%    SourceAddr = string()
%    McAddr = string() | atom() | ip_address()
%
% @doc Starts the echo ESME and binds as a receiver and transmitter to McAddr.
%
% <p>Let us see what this function does.</p>
%
% <p>Receives the identity of the ESME and the MC address as parameters.  
% Builds the <tt>gen_esme_setup</tt> record and starts a 
% <a href="gen_esme.html">gen_esme</a> with the function 
% <a href="gen_esme.html#start_link-3">gen_esme:start_link/3</a>.</p>
%
% <p>If the call to <a href="gen_esme.html#start_link-3">gen_esme:start_link/3
% </a> succeeds, the echo ESME binds to the SMSC as a receiver and transmitter.
% </p>
% @end
%
% %@see
%
% %@equiv
%%
start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) ->
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
      session_init_time = ?SESSION_INIT_TIME, % @see smpp_globals.hrl
      enquire_link_time = ?ENQUIRE_LINK_TIME, % @see smpp_globals.hrl
      inactivity_time   = ?INACTIVITY_TIME,   % @see smpp_globals.hrl
      response_time     = ?RESPONSE_TIME,     % @see smpp_globals.hrl
      rebind_time       = ?REBIND_TIME},      % @see smpp_globals.hrl

    % If default values are OK for you, you may use the macro GEN_ESME_SETUP
    % to define an equivalent Setup by a compact expression like

%     Setup = ?GEN_ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),

    case gen_esme:start_link({local, ?ESME_NAME}, ?MODULE, Setup) of
        {ok, Eid} ->
            gen_esme:open_transmitter(?ESME_NAME, McAddr),
            gen_esme:bind_transmitter(?ESME_NAME),
            gen_esme:open_receiver(?ESME_NAME, McAddr),
            gen_esme:bind_receiver(?ESME_NAME),
            process_flag(trap_exit, true),
            {ok, Eid};
        Error ->
            Error
    end.


%%%
% @spec stop() -> ok
%
% @doc Unbinds and stops the echo ESME.
%
% <p>No big deal.  The stop function does the opposite to 
% <a href="#start_link-5">start_link/5</a>.</p>
%
% <p>Once unbounded from the MC, the ESME must be cleanly stopped by calling
% <a href="gen_esme.html#stop-1">gen_esme:stop/1</a>.</p>
%
% <p>Should you require complex shutdown strategies, use callbacks
% <a href="gen_esme.html#unbind_receiver_resp-3">unbind_receiver_resp/3</a> and
% <a href="gen_esme.html#unbind_transmitter_resp-3">unbind_transmitter_resp/3
% </a> for that purpose.</p>
% @end
%
% %@see
%
% %@equiv
%%
stop() ->
    gen_esme:unbind_receiver(?ESME_NAME),
    gen_esme:close_receiver(?ESME_NAME),
    gen_esme:unbind_transmitter(?ESME_NAME),
    gen_esme:close_transmitter(?ESME_NAME),
    gen_esme:stop(?ESME_NAME).


%%%===================================================================
% ESME functions
%%====================================================================
%%%
% @spec bind_receiver_resp(Pid, Sid, Resp) -> ok
%    Pid = pid()
%    Sid = pid()
%    Resp = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error = int()
%
% @doc <a href="gen_esme.html#bind_receiver_resp-3">gen_esme - 
% bind_receiver_resp/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
bind_receiver_resp(Pid, Sid, {ok, PduResp}) ->
    SystemId = operation:get_param(system_id, PduResp),
    io:format("Bound as receiver to ~p~n", [SystemId]);

bind_receiver_resp(Pid, Sid, Error) ->
    io:format("Bind error on receiver session: ~p~n", [Error]).


%%%
% @spec bind_transmitter_resp(Pid, Sid, Resp) -> ok
%    Pid = pid()
%    Sid = pid()
%    Resp = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error = int()
%
% @doc <a href="gen_esme.html#bind_transmitter_resp-3">gen_esme - 
% bind_transmitter_resp/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
bind_transmitter_resp(Pid, Sid, {ok, PduResp}) ->
    SystemId = operation:get_param(system_id, PduResp),
    io:format("Bound as transmitter to ~p~n", [SystemId]);

bind_transmitter_resp(Pid, Sid, Error) ->
    io:format("Bind error on receiver session: ~p~n", [Error]).


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
% @doc <a href="gen_esme.html#deliver_sm-3">gen_esme - deliver_sm/3</a>
% callback implementation.
%
% <p>This callback handles <i>deliver_sm</i> operations.  The 
% <a href="gen_esme.html">gen_esme</a> behavior triggers this callback 
% whenever receives a <i>deliver_sm</i> PDU from the SMSC.  Developers must 
% implement this function if they want their ESME to handle this SMPP 
% operation.</p>
% 
% <p>Since our echo ESME responds to these PDUs, by sending back to the
% subscriber the incomming short message, that's what we implement in the body
% of this function.</p>
%
% <p>Notice how the <i>deliver_sm</i> PDU is available as the third argument 
% of the callback.  PDUs are internally represented as dictionaries.  Should 
% you need to get any parameter of a PDU, you may use the 
% <a href="operation.html#get_param-2">operation:get_param/2</a> function.</p>
%
% <p><b>Important.</b>  PDUs received by callbacks are *guarantee* to be 
% correct.  Malformed PDUs are discarded at the session layer (where the 
% appropriate response PDU is sent back to the MC, indicating encountered 
% error), thus we do not need to care about checking PDU parameters, we can 
% safely assume PDUs we handle are correct.</p>
%
% <p>See how the response is sent back issuing a 
% <a href="gen_esme.html#submit_sm-2">submit_sm/2</a> operation.</p>
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
% @doc <a href="gen_esme.html#deliver_data_sm-3">gen_esme - deliver_data_sm/3
% </a> callback implementation.
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


%%%
% @spec receiver_mc_unavailable(Pid, Eid) -> ok
%    Pid = pid()
%    Eid = pid()
%
% @doc <a href="gen_esme.html#receiver_mc_unavailable-2">gen_esme - 
% receiver_mc_unavailable/2</a> callback implementation.
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
% @doc <a href="gen_esme.html#transmitter_mc_unavailable-2">gen_esme - 
% transmitter_mc_unavailable/2</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
transmitter_mc_unavailable(Pid, Eid) ->
    io:format("MC unavailable on transmitter session~n", []).


%%%
% @spec receiver_mc_unbind(Pid, Eid) -> ok | {error, Error}
%    Pid = pid()
%    Eid = pid()
%    Error = int()
%
% @doc <a href="gen_esme.html#receiver_mc_unbind-2">gen_esme - 
% receiver_mc_unbind/2</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
receiver_mc_unbind(Pid, Eid) -> 
    io:format("Unbind request received on receiver session~n", []),
    ok.


%%%
% @spec transmitter_mc_unbind(Pid, Eid) -> ok | {error, Error}
%    Pid = pid()
%    Eid = pid()
%    Error = int()
%
% @doc <a href="gen_esme.html#transmitter_mc_unbind-2">gen_esme - 
% transmitter_mc_unbind/2</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
transmitter_mc_unbind(Pid, Eid) -> 
    io:format("Unbind request received on transmitter session~n", []),
    ok.


%%%
% @spec unbind_receiver_resp(Pid, Sid, Resp) -> ok
%    Pid = pid()
%    Sid = pid()
%    Resp = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error = int()
%
% @doc <a href="gen_esme.html#unbind_receiver_resp-3">gen_esme - 
% unbind_receiver_resp/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
unbind_receiver_resp(Pid, Sid, {ok, _PduResp}) ->
    io:format("Unbound as receiver.~n", []);

unbind_receiver_resp(Pid, Sid, Error) ->
    io:format("Unbind error on receiver session: ~p~n", [Error]).


%%%
% @spec unbind_transmitter_resp(Pid, Sid, Resp) -> ok
%    Pid = pid()
%    Sid = pid()
%    Resp = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error = int()
%
% @doc <a href="gen_esme.html#unbind_transmitter_resp-3">gen_esme - 
% unbind_transmitter_resp/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
unbind_transmitter_resp(Pid, Sid, {ok, _PduResp}) ->
    io:format("Unbound as transmitter.~n", []);

unbind_transmitter_resp(Pid, Sid, Error) ->
    io:format("Unbind error on transmitter session: ~p~n", [Error]).


%%%===================================================================
% Internal functions
%%====================================================================

