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
% @doc Generic ESME.
%
% <p>A generic ESME implemented as a gen_server.</p>
%
%
% <h2>Callback Function Index</h2>
%
%
% <p>A module implementing this behaviour may export these functions.  Leaving 
% a function undefined preserves the default behaviour.</p>
%
% <table width="100%" border="1">
%   <tr>
%     <td valign="top">
%       <a href="#bind_receiver_resp-3">bind_receiver_resp/3</a>
%     </td>
%     <td>Forwards bind_receiver responses.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#bind_transmitter_resp-3">bind_transmitter_resp/3</a>
%     </td>
%     <td>Forwards bind_transmitter responses.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#bind_transceiver_resp-3">bind_transceiver_resp/3</a>
%     </td>
%     <td>Forwards bind_transceiver responses.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#alert_notification-3">alert_notification/3</a>
%     </td>
%     <td>Handles received SMPP alert_notification PDUs.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#deliver_sm-3">deliver_sm/3</a></td>
%     <td>Handles received SMPP deliver_sm PDUs</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#deliver_data_sm-3">deliver_data_sm/3</a></td>
%     <td>Handles received SMPP data_sm PDUs</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#receiver_mc_unavailable-2">receiver_mc_unavailable/2</a>
%     </td>
%     <td>The MC is unavailable on the receiver session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%      <a href="#transmitter_mc_unavailable-2">transmitter_mc_unavailable/2</a>
%     </td>
%     <td>The MC becomes unavailable on the transmitter session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%      <a href="#transceiver_mc_unavailable-2">transceiver_mc_unavailable/2</a>
%     </td>
%     <td>The MC becomes unavailable on the transceiver session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#smpp_listen_error-3">smpp_listen_error/3</a>
%     </td>
%     <td>An error occur while listening for outbind.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#smpp_listen_recovery-3">smpp_listen_recovery/3</a>
%     </td>
%     <td>Listen socket is up again.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#receiver_mc_unbind-2">receiver_mc_unbind/2</a>
%     </td>
%     <td>Forwards unbind requests (issued by the MC) on the receiver session.
%     </td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#transmitter_mc_unbind-2">transmitter_mc_unbind/2</a>
%     </td>
%     <td>Forwards unbind requests (issued by the MC) on the transmitter 
%       session.
%     </td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#transceiver_mc_unbind-2">transceiver_mc_unbind/2</a>
%     </td>
%     <td>Forwards unbind requests (issued by the MC) on the transceiver 
%       session.
%     </td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#unbind_receiver_resp-3">unbind_receiver_resp/3</a>
%     </td>
%     <td>Forwards unbind_receiver responses.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#unbind_transmitter_resp-3">unbind_transmitter_resp/3</a>
%     </td>
%     <td>Forwards unbind_transmitter responses.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#unbind_transceiver_resp-3">unbind_transceiver_resp/3</a>
%     </td>
%     <td>Forwards unbind_transceiver responses.</td>
%   </tr>
% </table>
%
%
% <h2>Callback Function Details</h2>
%
% <h3><a name="bind_receiver_resp-3">bind_receiver_resp/3</a></h3>
%
% <tt>bind_receiver_resp(Pid, Sid, Resp) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%   <li><tt>PduResp = pdu()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>Forwards bind_receiver responses.</p>
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
% 
% <p><tt>Error</tt> is the SMPP error returned by the bind operation.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%
% <p><b>See also:</b> <tt>callback_bind_receiver_resp/2</tt></p>
%
%
% <h3><a name="bind_transmitter_resp-3">bind_transmitter_resp/3</a></h3>
%
% <tt>bind_transmitter_resp(Pid, Sid, PduResp) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%   <li><tt>PduResp = pdu()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>Forwards bind_transmitter responses.</p>
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
% 
% <p><tt>Error</tt> is the SMPP error returned by the bind operation.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%
% <p><b>See also:</b> <tt>callback_bind_transmitter_resp/2</tt></p>
%
%
% <h3><a name="bind_transceiver_resp-3">bind_transceiver_resp/3</a></h3>
%
% <tt>bind_transceiver_resp(Pid, Sid, PduResp) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%   <li><tt>PduResp = pdu()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>Forwards bind_transceiver responses.</p>
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
% 
% <p><tt>Error</tt> is the SMPP error returned by the bind operation.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%
% <p><b>See also:</b> <tt>callback_bind_transceiver_resp/2</tt></p>
%
%
% <h3><a name="alert_notification-3">alert_notification/3</a></h3>
%
% <tt>alert_notification(Pid, Eid, Pdu) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Pdu = pdu()</tt></li>
% </ul>
%
% <p>Alert notifications are forwarded by the ESME using this callback.  The
% alert_notification PDU is given along.</p>
%
% <p>The callback module is responsible of initiating the appropriate actions
% associated to the alert notification.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_alert_notification/2</tt></p>
%
%
% <h3><a name="deliver_sm-3">deliver_sm/3</a></h3>
%
% <tt>deliver_sm(Pid, Eid, Pdu) -> Result</tt>
% <ul>
%   <li><tt>Pid        = pid()</tt></li>
%   <li><tt>Eid        = pid()</tt></li>
%   <li><tt>Pdu        = pdu()</tt></li>
%   <li><tt>Result     = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%   <li><tt>ParamList  = [{ParamName, ParamValue}]</tt></li>
%   <li><tt>ParamName  = atom()</tt></li>
%   <li><tt>ParamValue = term()</tt></li>
% </ul>
%
% <p>Short messages delivered by the ESME via this callback are enclosed
% inside a deliver_sm PDU.</p>
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the deliver_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_deliver_sm/2</tt></p>
%
%
% <h3><a name="deliver_data_sm-3">deliver_data_sm/3</a></h3>
%
% <tt>deliver_data_sm(Pid, Eid, Pdu) -> Result</tt>
% <ul>
%   <li><tt>Pid        = pid()</tt></li>
%   <li><tt>Eid        = pid()</tt></li>
%   <li><tt>Pdu        = pdu()</tt></li>
%   <li><tt>Result     = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%   <li><tt>ParamList  = [{ParamName, ParamValue}]</tt></li>
%   <li><tt>ParamName  = atom()</tt></li>
%   <li><tt>ParamValue = term()</tt></li>
% </ul>
%
% <p>Short messages delivered by the ESME via this callback are enclosed
% inside a data_sm PDU.</p>
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the data_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_deliver_data_sm/2</tt></p>
%
%
% <h3><a name="receiver_mc_unavailable-2">receiver_mc_unavailable/2</a></h3>
%
% <tt>receiver_mc_unavailable(Pid, Eid) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable on the receiver session, this circumstance
% is notified with a call to this function.</p>
%
% <p>Notice that this callback may also be triggered after an unbind
% operation, when the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_receiver_mc_unavailable/1</tt></p>
%
%
% <h3>
%   <a name="transmitter_mc_unavailable-2">transmitter_mc_unavailable/2</a>
% </h3>
%
% <tt>transmitter_mc_unavailable(Pid, Eid) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable on the transmitter session, this
% circumstance is notified with a call to this function.</p>
%
% <p>Notice that this callback may also be triggered after an unbind
% operation, when the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transmitter_mc_unavailable/1</tt></p>
%
%
% <h3>
%   <a name="transceiver_mc_unavailable-2">transceiver_mc_unavailable/2</a>
% </h3>
%
% <tt>transceiver_mc_unavailable(Pid, Eid) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable on the transceiver session, this
% circumstance is notified with a call to this function.</p>
%
% <p>Notice that this callback may also be triggered after an unbind
% operation, when the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transceiver_mc_unavailable/1</tt></p>
%
%
% <h3><a name="smpp_listen_error-3">smpp_listen_error/3</a></h3>
%
% <tt>smpp_listen_error(Pid, Eid, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>When an ESME on listening state looses the listen socket, uses
% this callback to notify the failure.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_smpp_listen_error/2</tt></p>
%
%
% <h3><a name="smpp_listen_recovery-3">smpp_listen_recovery/3</a></h3>
%
% <tt>smpp_listen_recovery(Pid, Eid, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>After a listen failure, a new socket could be set to listen again on
% <tt>Port</tt>.  The ESME uses this callback to notify the recovery.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_smpp_listen_recovery/2</tt></p>
%
%
% <h3><a name="receiver_mc_unbind-2">receiver_mc_unbind/2</a></h3>
%
% <tt>receiver_mc_unbind(Pid, Eid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards unbind requests (issued by the MC) on the 
% receiver session of the ESME.  If <tt>ok</tt> returned an unbind_resp
% with a ESME_ROK command_status is sent to the MC and the session moves 
% into the unbound state.  When <tt>{error, Error}</tt> is returned by 
% the ESME, the response PDU sent by the session to the MC will have an 
% <tt>Error</tt> command_status and the session will remain on it's 
% current bound_rx state.</p>
%
% <p><tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_receiver_mc_unbind/1</tt></p>
%
%
% <h3><a name="transmitter_mc_unbind-2">transmitter_mc_unbind/2</a></h3>
%
% <tt>transmitter_mc_unbind(Pid, Eid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards unbind requests (issued by the MC) on the 
% transmitter session of the ESME.  If <tt>ok</tt> returned an unbind_resp
% with a ESME_ROK command_status is sent to the MC and the session moves 
% into the unbound state.  When <tt>{error, Error}</tt> is returned by 
% the ESME, the response PDU sent by the session to the MC will have an 
% <tt>Error</tt> command_status and the session will remain on it's 
% current bound_tx state.</p>
%
% <p><tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transmitter_mc_unbind/1</tt></p>
%
%
% <h3><a name="transceiver_mc_unbind-2">transceiver_mc_unbind/2</a></h3>
%
% <tt>transceiver_mc_unbind(Pid, Eid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards unbind requests (issued by the MC) on the 
% transceiver session of the ESME.  If <tt>ok</tt> returned an unbind_resp
% with a ESME_ROK command_status is sent to the MC and the session moves 
% into the unbound state.  When <tt>{error, Error}</tt> is returned by 
% the ESME, the response PDU sent by the session to the MC will have an 
% <tt>Error</tt> command_status and the session will remain on it's 
% current bound_trx state.</p>
%
% <p><tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transceiver_mc_unbind/1</tt></p>
%
%
% <h3><a name="unbind_receiver_resp-3">unbind_receiver_resp/3</a></h3>
%
% <tt>unbind_receiver_resp(Pid, Sid, Resp) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%   <li><tt>PduResp = pdu()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>Forwards unbind_receiver responses.</p>
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
% 
% <p><tt>Error</tt> is the SMPP error returned by the unbind operation.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%
% <p><b>See also:</b> <tt>callback_unbind_receiver_resp/2</tt></p>
%
%
% <h3><a name="unbind_transmitter_resp-3">unbind_transmitter_resp/3</a></h3>
%
% <tt>unbind_transmitter_resp(Pid, Sid, PduResp) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%   <li><tt>PduResp = pdu()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>Forwards unbind_transmitter responses.</p>
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
% 
% <p><tt>Error</tt> is the SMPP error returned by the unbind operation.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%
% <p><b>See also:</b> <tt>callback_unbind_transmitter_resp/2</tt></p>
%
%
% <h3><a name="unbind_transceiver_resp-3">unbind_transceiver_resp/3</a></h3>
%
% <tt>unbind_transceiver_resp(Pid, Sid, PduResp) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Resp = {ok, PduResp} | {error, Error}</tt></li>
%   <li><tt>PduResp = pdu()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>Forwards unbind_transceiver responses.</p>
%
% <p>Returning value is ignored by the ESME. The callback module may start
% some initialization on response to this callback.</p>
% 
% <p><tt>Error</tt> is the SMPP error returned by the unbind operation.</p>
%
% <p><tt>PduResp</tt> is the PDU response sent by the MC.  <tt>Pid</tt> is the 
% ESME parent id, <tt>Eid</tt> as the ESME process id.</p>
%
% <p><b>See also:</b> <tt>callback_unbind_transceiver_resp/2</tt></p>
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
% [26 Feb 2004]
%
% <ul>
%   <li>Completely redesigned.</li>
% </ul>
%
% [27 Feb 2004]
%
% <ul>
%   <li>Callback interface redefined.</li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.2 alpha, {10 Jun 2003} {@time}.
% @end
%%
-module(gen_esme).

-behaviour(gen_server).
-behaviour(gen_esme_session).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("gen_esme.hrl").

%%%-------------------------------------------------------------------
% Behaviour exports
%%--------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([start_link/2, 
         start_link/3, 
         listen_receiver/1,
         listen_receiver/2,
         listen_receiver/3,
         listen_transmitter/1,
         listen_transmitter/2,
         listen_transmitter/3,
         listen_transceiver/1,
         listen_transceiver/2,
         listen_transceiver/3,
         open_receiver/2,
         open_receiver/3,
         open_receiver/4,
         open_transmitter/2,
         open_transmitter/3,
         open_transmitter/4,
         open_transceiver/2,
         open_transceiver/3,
         open_transceiver/4,
         bind_receiver/1,
         bind_transmitter/1,
         bind_transceiver/1,
         broadcast_sm/2,
         cancel_broadcast_sm/2,
         cancel_sm/2,
         data_sm/2,
         query_broadcast_sm/2,
         query_sm/2,
         replace_sm/2,
         submit_multi/2,
         submit_sm/2,
         unbind_receiver/1,
         unbind_transmitter/1,
         unbind_transceiver/1,
         close_receiver/1,
         close_transmitter/1,
         close_transceiver/1,
         stop/1]).

%%%-------------------------------------------------------------------
% Internal gen_server exports
%%--------------------------------------------------------------------
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-------------------------------------------------------------------
% Internal gen_esme_session exports
%%--------------------------------------------------------------------
-export([outbind/3, 
         unbind/2,
         mc_unavailable/4, 
         resume_service/4,
         smpp_listen_error/3,
         smpp_listen_recovery/3,
         alert_notification/3,
         deliver_sm/3, 
         deliver_data_sm/3]).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(MC_LIST_ALLOW_ALL, [{'_', '_'}]).
-define(MC_LIST_DENY_ALL,  []).

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%
% %@spec {state, 
%         Parent,
%         Self,
%         CallbackModule,
%         SystemId,
%         Password,
%         AddrTon,
%         AddrNpi,
%         AddressRange,
%         SourceAddrTon,
%         SourceAddrNpi,
%         SourceAddr,
%         ServiceType,
%         SystemType,
%         SessionSetup, 
%         RxSession,
%         TxSession,
%         RxMcList,
%         TxMcList}
%    Parent           = pid()
%    Self             = pid()
%    CallbackModule   = atom()
%    SystemId         = string()
%    Password         = string()
%    AddrTon          = int()
%    AddrNpi          = int()
%    AddressRange     = string()
%    SourceAddrTon    = int()
%    SourceAddrNpi    = int()
%    SourceAddr       = string()
%    ServiceType      = string()
%    SystemType       = string()
%    SessionSetup     = gen_esme_session_setup()
%    RxSession        = pid()
%    TxSession        = pid()
%    RxMcList         = [McId]
%    TxMcList         = [McId]
%    McId             = {SystemId, Password}
%
% %@doc Representation of the server's state.
%
% <dl>
%   <dt>Parent: </dt><dd>Pid of the parent process.  Is passed in the
%     callback functions to help identify the owner of the ESME.
%   </dd>
%   <dt>Self: </dt><dd>The Pid of the gen_esme main process.  Is passed
%     along with the callback functions to help identify the ESME triggering
%     the callback.  Since a callback function might be called from a spawned
%     process, we want to keep a reference to our main process.
%   </dd>
%   <dt>CallbackModule: </dt><dd>Module where callbacks are defined.</dd>
%   <dt>SystemId: </dt><dd>ESME identifier.  C-octet string, null terminated 
%     string (default value is ?NULL_C_OCTET_STRING).
%   </dd>
%   <dt>Password: </dt><dd>ESME password.  C-octet string, null terminated 
%     string (default value is ?NULL_C_OCTET_STRING).
%   </dd>
%   <dt>AddrTon: </dt><dd>ESME address TON.  Integer (default value is
%     ?TON_INTERNATIONAL).
%   </dd>
%   <dt>AddrNpi: </dt><dd>ESME address NPI.  Integer (default value is
%      ?NPI_ISDN).
%   </dd>
%   <dt>AddressRange: </dt><dd>Address range the ESME serves.  C-Octet string
%     (default value is ?NULL_C_OCTET_STRING).
%   </dd>
%   <dt>SourceAddrTon: </dt><dd>ESME source address TON.  Integer (default
%     value is ?TON_INTERNATIONAL).
%   </dd>
%   <dt>SourceAddrNpi: </dt><dd>ESME source address NPI.  Integer (default 
%      value is ?NPI_ISDN).
%   </dd>
%   <dt>SourceAddr: </dt><dd>ESME source address.  C-Octet string
%     (default value is ?NULL_C_OCTET_STRING).
%   </dd>
%   <dt>ServiceType: </dt><dd>ESME service type.  C-Octet string (default
%     value is ?NULL_C_OCTET_STRING).
%   </dd>
%   <dt>SystemType: </dt><dd>ESME system type.  C-Octet string (default
%     value is ?NULL_C_OCTET_STRING).
%   </dd>
%   <dt>SessionSetup: </dt><dd>ESME session setup parameters.</dd>
%   <dt>RxSession: </dt><dd>PID of the receiver session.  If bound as a
%     transceiver this parameters has the same value as <tt>TxSession</tt>.
%   </dd>
%   <dt>TxSession: </dt><dd>PID of the transmitter session.  If bound as a
%     transceiver this parameters has the same value as <tt>RxSession</tt>.
%   </dd>
%   <dt>RxMcList: </dt><dd>List with the identities of the MCs from where an
%     outbind operation is accepted.  In response to the outbind, a 
%     bind_receiver/bind_transceiver operations is issued.  See 
%     <a href="#listen_receiver-2">listen_receiver/2</a> and
%     <a href="#listen_transceiver-2">listen_transceiver/2</a> for more
%     details.
%   </dd>
%   <dt>TxMcList: </dt><dd>List with the identities of the MCs from where an
%     outbind operation is accepted.  In response to the outbind, a 
%     bind_transmitter/bind_transceiver operations is issued.  See 
%     <a href="#listen_transmitter-2">listen_transmitter/2</a> and
%     <a href="#listen_transceiver-2">listen_transceiver/2</a> for more
%     details.
%   </dd>
% </dl>
% %@end
%%
-record(state, 
        {parent,
         self,
         callback_module,
         system_id,
         password,
         addr_ton,
         addr_npi,
         address_range,
         source_addr_ton,
         source_addr_npi,
         source_addr,
         service_type,
         system_type,
         session_setup,
         rx_session,
         tx_session,
         rx_mc_list = [],
         tx_mc_list = []}).

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec behaviour_info(Category) -> Info
%    Category      = callbacks | term()
%    Info          = CallbacksInfo | term()
%    CallbacksInfo = [{FunctionName, Arity}]
%    FunctionName  = atom()
%    Arity         = int()
%
% @doc Gives information about the behaviour.
% @end
%
% %@see
%
% %@equiv
%%
behaviour_info(callbacks) ->
     [{bind_receiver_resp, 3}, 
      {bind_transmitter_resp, 3}, 
      {bind_transceiver_resp, 3}, 
      {alert_notification, 3},
      {deliver_sm, 3}, 
      {deliver_data_sm, 3},
      {receiver_mc_unavailable, 2}, 
      {transmitter_mc_unavailable, 2}, 
      {transceiver_mc_unavailable, 2}, 
      {smpp_listen_error, 3},
      {smpp_listen_recovery, 3},
      {receiver_mc_unbind, 2},
      {transmitter_mc_unbind, 2},
      {transceiver_mc_unbind, 2},
      {unbind_receiver_resp, 3},
      {unbind_transmitter_resp, 3},
      {unbind_transceiver_resp, 3}];

behaviour_info(_Other) ->
    undefined.


%%%
% @spec start_link(Module, Setup) -> Result
%    Module = atom()
%    Setup  = gen_esme_setup()
%    Result = {ok, Pid} | ignore | {error, Error}
%    Pid    = pid()
%    Error  = {already_started, Pid} | term()
%
% @doc Starts the ESME server.
%
% <p><tt>Setup</tt> is a gen_esme_setup() record with all the ESME setup 
% parameters.  Use the macro ?DEFAULT_GEN_ESME_SETUP to set the default values.
% </p>
%
% <p>Refer to <b>gen_esme.hrl</b> for details on the gen_esme_setup()
% record definition.</p>
%
% <p>Started gen_esme is not registered.</p>
%
% @see gen_server:start_link/3
% @see start_link/3
% @end
%%
start_link(Module, Setup) ->
    gen_server:start_link(?MODULE, [self(), Module, Setup], []).


%%%
% @spec start_link(EName, Module, Setup) -> Result
%    EName  = {local, Name} | {global, Name}
%    Name   = atom()
%    Module = atom()
%    Setup  = gen_esme_setup()
%    Result = {ok, Pid} | ignore | {error, Error}
%    Pid    = pid()
%    Error  = {already_started, Pid} | term()
%
% @doc Starts the ESME server.
%
% <p><tt>Setup</tt> is a gen_esme_setup() record with all the ESME setup 
% parameters.  Use the macro ?DEFAULT_GEN_ESME_SETUP to set the default values.
% </p>
%
% <p>Refer to <b>gen_esme.hrl</b> for details on the gen_esme_setup()
% record definition.</p>
%
% <p>If <tt>EName = {local, Name}</tt>, the gen_esme is registered
% locally as <tt>Name</tt>.  If <tt>EName = {global, Name}</tt>,
% the gen_esme is registered globally as <tt>Name</tt>.</p>
%
% @see gen_server:start_link/4
% @see start_link/2
% @end
%%
start_link(EName, Module, Setup) ->
    gen_server:start_link(EName, ?MODULE, [self(), Module, Setup], []).


%%%
% @spec listen_receiver(Eid) -> ok
%    Eid = pid()
%
% @doc
% @see listen_receiver/3
%
% @equiv listen_receiver(Eid, MC_LIST_ALLOW_ALL)
% @end
%%
listen_receiver(Eid) ->
    listen_receiver(Eid, ?MC_LIST_ALLOW_ALL).
    

%%%
% @spec listen_receiver(Eid, McList) -> ok
%    Eid      = pid()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%
% @doc
% @see listen_receiver/3
%
% @equiv listen_receiver(Eid, DEFAULT_SMPP_PORT, McList)
% @end
%%
listen_receiver(Eid, McList) ->
    listen_receiver(Eid, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec listen_receiver(Eid, Port, McList) -> ok
%    Eid      = pid()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%
% @doc Puts the receiver session to listen on <tt>Port</tt>. In 
% <a href="#listen_receiver-1">listen_receiver/1</a> and
% <a href="#listen_receiver-2">listen_receiver/2</a> the default SMPP port is 
% assumed (defined by ?DEFAULT_SMPP_PORT).  If an authorized MC connects to 
% this session and issues an outbind, the  corresponding bind_receiver request 
% is sent to the calling MC (identified on the outbind by a <tt>SystemId</tt> 
% and <tt>Password</tt>).
%
% <p>The <tt>McList</tt> is searched in sequence.  If a tuple on 
% this list matches the system_id and password sent along with the outbind
% PDU, access is granted to the calling MC.  Notice that the atom
% <tt>'_'</tt> is a system_id and/or password wild-card.  The MC list 
% <tt>[{'_', '_'}]</tt> matches any MC identity, this is the default 
% <tt>McList</tt> assumed by <a href="#listen_receiver-1">listen_receiver/1</a>
% .</p>
%
% <p>If the MC authentication fails the underlying session is closed.</p>
%
% @see gen_server:call/3
% @see listen_receiver/1
% @see listen_receiver/2
% @end
%
% %@equiv
%%
listen_receiver(Eid, Port, McList) ->
    gen_server:call(Eid, {listen_receiver, Port, McList}, infinity).


%%%
% @spec listen_transmitter(Eid) -> ok
%    Eid = pid()
%
% @doc
% @see listen_transmitter/3
%
% @equiv listen_transmitter(Eid, MC_LIST_ALLOW_ALL)
% @end
%%
listen_transmitter(Eid) ->
    listen_transmitter(Eid, ?MC_LIST_ALLOW_ALL).
    

%%%
% @spec listen_transmitter(Eid, McList) -> ok
%    Eid      = pid()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%
% @doc
% @see listen_transmitter/3
%
% @equiv listen_transmitter(Eid, DEFAULT_SMPP_PORT, McList)
% @end
%%
listen_transmitter(Eid, McList) ->
    listen_transmitter(Eid, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec listen_transmitter(Eid, Port, McList) -> ok
%    Eid      = pid()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%
% @doc Puts the transmitter session to listen on <tt>Port</tt>. In 
% <a href="#listen_transmitter-1">listen_transmitter/1</a> and
% <a href="#listen_transmitter-2">listen_transmitter/2</a> the default SMPP 
% port is assumed (defined by ?DEFAULT_SMPP_PORT).  If an authorized MC 
% connects to this session and issues an outbind, the  corresponding 
% bind_transmitter request is sent to the calling MC (identified on the 
% outbind by a <tt>SystemId</tt> and <tt>Password</tt>).
%
% <p>The <tt>McList</tt> is searched in sequence.  If a tuple on 
% this list matches the system_id and password sent along with the outbind
% PDU, access is granted to the calling MC.  Notice that the atom
% <tt>'_'</tt> is a system_id and/or password wild-card.  The MC list 
% <tt>[{'_', '_'}]</tt> matches any MC identity, this is the default 
% <tt>McList</tt> assumed by 
% <a href="#listen_transmitter-1">listen_transmitter/1</a>.</p>
%
% <p>If the MC authentication fails the underlying session is closed.</p>
%
% @see gen_server:call/3
% @see listen_transmitter/1
% @see listen_transmitter/2
% @end
%
% %@equiv
%%
listen_transmitter(Eid, Port, McList) ->
    gen_server:call(Eid, {listen_transmitter, Port, McList}, infinity).


%%%
% @spec listen_transceiver(Eid) -> ok
%    Eid = pid()
%
% @doc
% @see listen_transceiver/3
%
% @equiv listen_transceiver(Eid, MC_LIST_ALLOW_ALL)
% @end
%%
listen_transceiver(Eid) ->
    listen_transceiver(Eid, ?MC_LIST_ALLOW_ALL).
    

%%%
% @spec listen_transceiver(Eid, McList) -> ok
%    Eid      = pid()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%
% @doc
% @see listen_transceiver/3
%
% @equiv listen_transceiver(Eid, DEFAULT_SMPP_PORT, McList)
% @end
%%
listen_transceiver(Eid, McList) ->
    listen_transceiver(Eid, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec listen_transceiver(Eid, Port, McList) -> ok
%    Eid      = pid()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%
% @doc Puts the transceiver session to listen on <tt>Port</tt>. In 
% <a href="#listen_transceiver-1">listen_transceiver/1</a> and
% <a href="#listen_transceiver-2">listen_transceiver/2</a> the default SMPP 
% port is assumed (defined by ?DEFAULT_SMPP_PORT).  If an authorized MC 
% connects to this session and issues an outbind, the  corresponding 
% bind_transceiver request is sent to the calling MC (identified on the 
% outbind by a <tt>SystemId</tt> and <tt>Password</tt>).
%
% <p>The <tt>McList</tt> is searched in sequence.  If a tuple on 
% this list matches the system_id and password sent along with the outbind
% PDU, access is granted to the calling MC.  Notice that the atom
% <tt>'_'</tt> is a system_id and/or password wild-card.  The MC list 
% <tt>[{'_', '_'}]</tt> matches any MC identity, this is the default 
% <tt>McList</tt> assumed by 
% <a href="#listen_transceiver-1">listen_transceiver/1</a>.</p>
%
% <p>If the MC authentication fails the underlying session is closed.</p>
%
% @see gen_server:call/3
% @see listen_transceiver/1
% @see listen_transceiver/2
% @end
%
% %@equiv
%%
listen_transceiver(Eid, Port, McList) ->
    gen_server:call(Eid, {listen_transceiver, Port, McList}, infinity).


%%%
% @spec open_receiver(Eid, Address) -> {ok, PduResp} | {error, Error}
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    PduResp  = pdu()
%    Error    = term()
%
% @doc
% @see open_receiver/4
%
% @equiv open_receiver(Eid, Address, MC_LIST_DENY_ALL)
% @end
%%
open_receiver(Eid, Address) ->
    open_receiver(Eid, Address, ?MC_LIST_DENY_ALL).


%%%
% @spec open_receiver(Eid, Address, McList) -> {ok, PduResp} | {error, Error}
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%    PduResp  = pdu()
%    Error    = term()
%
% @doc
% @see open_receiver/4
%
% @equiv open_receiver(Eid, Address, DEFAULT_SMPP_PORT, McList)
% @end
%%
open_receiver(Eid, Address, McList) ->
    open_receiver(Eid, Address, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec open_receiver(Eid, Address, Port, McList) -> Result
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%    Result   = {ok, PduResp} | {error, Error}
%    PduResp  = pdu()
%    Error    = term()
%
% @doc The ESME with pid <tt>Eid</tt> opens the receiver session.
%
% <p>The default SMPP port is used (?DEFAULT_SMPP_PORT) unless otherwise 
% stated.</p>
%
% @see gen_server:call/3
% @see open_receiver/2
% @see open_receiver/3
% @end
%
% %@equiv
%%
open_receiver(Eid, Address, Port, McList) ->
    gen_server:call(Eid, {open_receiver, Address, Port, McList}, infinity).


%%%
% @spec open_transmitter(Eid, Address) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    PduResp = pdu()
%    Error   = term()
%
% @doc
% @see open_transmitter/4
%
% @equiv open_transmitter(Eid, Address, MC_LIST_DENY_ALL)
% @end
%%
open_transmitter(Eid, Address) ->
    open_transmitter(Eid, Address, ?MC_LIST_DENY_ALL).


%%%
% @spec open_transmitter(Eid, Address, McList) -> Result
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%    Result   = {ok, PduResp} | {error, Error}
%    PduResp  = pdu()
%    Error    = term()
%
% @doc
% @see open_transmitter/4
%
% @equiv open_transmitter(Eid, Address, DEFAULT_SMPP_PORT, McList)
% @end
%%
open_transmitter(Eid, Address, McList) ->
    open_transmitter(Eid, Address, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec open_transmitter(Eid, Address, Port, McList) -> Result
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%    Result   = {ok, PduResp} | {error, Error}
%    PduResp  = pdu()
%    Error    = term()
%
% @doc The ESME with pid <tt>Eid</tt> opens the transmitter session.
%
% <p>The default SMPP port is used (?DEFAULT_SMPP_PORT) unless otherwise 
% stated.</p>
%
% @see gen_server:call/3
% @see open_transmitter/2
% @see open_transmitter/3
% @end
%
% %@equiv
%%
open_transmitter(Eid, Address, Port, McList) ->
    gen_server:call(Eid, {open_transmitter, Address, Port, McList}, infinity).


%%%
% @spec open_transceiver(Eid, Address) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    PduResp = pdu()
%    Error   = term()
%
% @doc
% @see open_transceiver/4
%
% @equiv open_transceiver(Eid, Address, MC_LIST_DENY_ALL)
% @end
%%
open_transceiver(Eid, Address) ->
    open_transceiver(Eid, Address, ?MC_LIST_DENY_ALL).


%%%
% @spec open_transceiver(Eid, Address, McList) -> Result
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%    Result   = {ok, PduResp} | {error, Error}
%    PduResp  = pdu()
%    Error    = term()
%
% @doc
% @see open_transceiver/4
%
% @equiv open_transceiver(Eid, Address, DEFAULT_SMPP_PORT, McList)
% @end
%%
open_transceiver(Eid, Address, McList) ->
    open_transceiver(Eid, Address, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec open_transceiver(Eid, Address, Port, McList) -> Result
%    Eid      = pid()
%    Address  = string() | atom() | ip_address()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password}
%    SystemId = string()
%    Password = string()
%    Result   = {ok, PduResp} | {error, Error}
%    PduResp  = pdu()
%    Error    = term()
%
% @doc The ESME with pid <tt>Eid</tt> opens the transceiver session.
%
% <p>The default SMPP port is used (?DEFAULT_SMPP_PORT) unless otherwise 
% stated.</p>
%
% @see gen_server:call/3
% @see open_transceiver/2
% @see open_transceiver/3
% @end
%
% %@equiv
%%
open_transceiver(Eid, Address, Port, McList) ->
    gen_server:call(Eid, {open_transceiver, Address, Port, McList}, infinity).


%%%
% @spec bind_receiver(Eid) -> ok
%    Eid = pid()
%
% @doc The ESME with pid <tt>Eid</tt>, issues a bind_receiver request.
%
% <p>The session must be already open.</p>
%
% @see open_receiver/4
% @see gen_server:cast/2
% @end
%
% %@equiv
%%
bind_receiver(Eid) ->
    gen_server:cast(Eid, bind_receiver).


%%%
% @spec bind_transmitter(Eid) -> ok
%    Eid = pid()
%
% @doc The ESME with pid <tt>Eid</tt>, issues a bind_transmitter request.
%
% <p>The session must be already open.</p>
%
% @see open_transmitter/4
% @see gen_server:cast/2
% @end
%
% %@equiv
%%
bind_transmitter(Eid) ->
    gen_server:cast(Eid, bind_transmitter).


%%%
% @spec bind_transceiver(Eid) -> ok
%    Eid = pid()
%
% @doc The ESME with pid <tt>Eid</tt>, issues a bind_transceiver request.
%
% <p>The session must be already open.</p>
%
% @see open_transceiver/4
% @see gen_server:call/3
% @end
%
% %@equiv
%%
bind_transceiver(Eid) ->
    gen_server:cast(Eid, bind_transceiver).


%%%
% @spec broadcast_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a broadcast_sm operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
broadcast_sm(Eid, ParamList) ->
    gen_server:call(Eid, {broadcast_sm, ParamList}, infinity).


%%%
% @spec cancel_broadcast_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a cancel_broadcast_sm operation on the ESME identified by
% <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
cancel_broadcast_sm(Eid, ParamList) ->
    gen_server:call(Eid, {cancel_broadcast_sm, ParamList}, infinity).


%%%
% @spec cancel_sm(Eid, ParamList) -> Result
%    Eid        = atom()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a cancel_sm operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
cancel_sm(Eid, ParamList) ->
    gen_server:call(Eid, {cancel_sm, ParamList}, infinity).


%%%
% @spec data_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a data_sm operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
data_sm(Eid, ParamList) ->
    gen_server:call(Eid, {data_sm, ParamList}, infinity).


%%%
% @spec query_broadcast_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a query_broadcast_sm operation on the ESME identified by
% <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
query_broadcast_sm(Eid, ParamList) ->
    gen_server:call(Eid, {query_broadcast_sm, ParamList}, infinity).


%%%
% @spec query_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a query_sm operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
query_sm(Eid, ParamList) ->
    gen_server:call(Eid, {query_sm, ParamList}, infinity).


%%%
% @spec replace_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a data_sm operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
replace_sm(Eid, ParamList) ->
    gen_server:call(Eid, {replace_sm, ParamList}, infinity).


%%%
% @spec submit_multi(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a submit_multi operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
submit_multi(Eid, ParamList) ->
    gen_server:call(Eid, {submit_multi, ParamList}, infinity).


%%%
% @spec submit_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int() | atom()
%
% @doc Issues a submit_sm operation on the ESME identified by <tt>Eid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
submit_sm(Eid, ParamList) ->
    gen_server:call(Eid, {submit_sm, ParamList}, infinity).


%%%
% @spec unbind_receiver(Eid) -> ok
%    Eid = pid()
%
% @doc The ESME with pid <tt>Eid</tt>, issues an unbind on the receiver 
% session.
% @end
%
% %@see
%
% %@equiv
%%
unbind_receiver(Eid) ->
    gen_server:cast(Eid, unbind_receiver).


%%%
% @spec unbind_transmitter(Eid) -> ok
%    Eid = pid()
%
% @doc The ESME with pid <tt>Eid</tt>, issues an unbind on the transmitter
% session.
% @end
%
% %@see
%
% %@equiv
%%
unbind_transmitter(Eid) ->
    gen_server:cast(Eid, unbind_transmitter).


%%%
% @spec unbind_transceiver(Eid) -> ok
%    Eid = pid()
%
% @doc The ESME with pid <tt>Eid</tt>, issues an unbind on the transceiver
% session.
% @end
%
% %@see
%
% %@equiv
%%
unbind_transceiver(Eid) ->
    gen_server:cast(Eid, unbind_transceiver).


%%%
% @spec close_receiver(Eid) -> ok | {error, Error}
%    Eid   = pid()
%    Error = term()
%
% @doc The ESME with pid <tt>Eid</tt> closes the receiver session.
% @end
%
% %@see
%
% %@equiv
%%
close_receiver(Eid) ->
    gen_server:call(Eid, close_receiver, infinity).


%%%
% @spec close_transmitter(Eid) -> ok | {error, Error}
%    Eid   = pid()
%    Error = term()
%
% @doc The ESME with pid <tt>Eid</tt> closes the transmitter session.
% @end
%
% %@see
%
% %@equiv
%%
close_transmitter(Eid) ->
    gen_server:call(Eid, close_transmitter, infinity).


%%%
% @spec close_transceiver(Eid) -> ok | {error, Error}
%    Eid   = pid()
%    Error = term()
%
% @doc The ESME with pid <tt>Eid</tt> closes the transceiver session.
% @end
%
% %@see
%
% %@equiv
%%
close_transceiver(Eid) ->
    gen_server:call(Eid, close_transceiver, infinity).


%%%
% @spec stop(Eid) -> ok
%    Eid = pid()
%
% @doc Stops the ESME with pid <tt>Eid</tt>.
%
% @see gen_server:cast/2
% @end
%
% %@equiv
%%
stop(Eid) ->
    gen_server:cast(Eid, die).


%%%===================================================================
% Server functions
%%====================================================================
%%%
% @spec init(Args) -> Result
%    Args    = term()
%    Result  = {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%    State   = term()
%    Timeout = int() | infinity
%    Reason  = term()
%
% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server - init/1</a> callback implementation.  Initiates the 
% server.
% @end
%%
init([Pid, Module, Setup]) ->
    State = #state{parent          = Pid, 
                   self            = self(),
                   callback_module = Module,
                   system_id       = Setup#gen_esme_setup.system_id,
                   password        = Setup#gen_esme_setup.password,
                   addr_ton        = Setup#gen_esme_setup.addr_ton,
                   addr_npi        = Setup#gen_esme_setup.addr_npi,
                   address_range   = Setup#gen_esme_setup.address_range,
                   source_addr_ton = Setup#gen_esme_setup.source_addr_ton,
                   source_addr_npi = Setup#gen_esme_setup.source_addr_npi,
                   source_addr     = Setup#gen_esme_setup.source_addr,
                   service_type    = Setup#gen_esme_setup.service_type,
                   system_type     = Setup#gen_esme_setup.system_type,
                   session_setup   = ?GET_GEN_ESME_SESSION_SETUP(Setup)},
    process_flag(trap_exit, true),
    {ok, State}.


%%%
% @spec handle_call(Request, From, State) -> Result
%    Request   = term()
%    From      = {pid(), Tag}
%    State     = term()
%    Result    = {reply, Reply, NewState}          |
%                {reply, Reply, NewState, Timeout} |
%                {noreply, NewState}               |
%                {noreply, NewState, Timeout}      |
%                {stop, Reason, Reply, NewState}   |
%                {stop, Reason, NewState}
%    Reply     = term()
%    NewState  = term()
%    Timeout   = int() | infinity
%    Reason    = term()
%
% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server - handle_call/3</a> callback implementation.  Handling 
% call messages.
%
% <ul>
%   <li>On <tt>{stop, Reason, Reply, NewState}</tt>
%   terminate/2 is called</li>
%   <li>On <tt>{stop, Reason, NewState}</tt>
%   terminate/2 is called</li>
% </ul>
%
% @see terminate/2
% @end
%%
handle_call({broadcast_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_broadcast_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({cancel_broadcast_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_cancel_broadcast_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({cancel_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_cancel_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({data_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_data_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({query_broadcast_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_query_broadcast_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({query_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_query_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({replace_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_replace_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({submit_multi, ParamList}, From, State) ->
    spawn_link(fun() -> do_submit_multi(ParamList, From, State) end),
    {noreply, State};

handle_call({submit_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_submit_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({deliver_sm, Pdu}, From, State) -> 
    spawn_link(fun() -> do_deliver_sm(Pdu, From, State) end),
    {noreply, State};

handle_call({deliver_data_sm, Pdu}, From, State) ->
    spawn_link(fun() -> do_deliver_data_sm(Pdu, From, State) end),
    {noreply, State};

handle_call({unbind, S}, _From, #state{rx_session=S, tx_session=S} = State) ->
    % The MC issues an unbind on the transceiver session.
    {reply, callback_transceiver_mc_unbind(State), State};

handle_call({unbind, S}, _From, #state{rx_session = S} = State) ->
    % The MC issues an unbind on the receiver session.
    {reply, callback_receiver_mc_unbind(State), State};

handle_call({unbind, S}, _From, #state{tx_session = S} = State) ->
    % The MC issues an unbind on the transmitter session.
    {reply, callback_transmitter_mc_unbind(State), State};

handle_call({open_receiver, Addr, Port, McList}, _From, State) ->
    case do_open(State#state.rx_session,Addr,Port,State#state.session_setup) of
        {ok, RxSession} ->
            {reply, ok, State#state{rx_session=RxSession, rx_mc_list=McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call({open_transmitter, Addr, Port, McList}, _From, State) ->
    case do_open(State#state.tx_session,Addr,Port,State#state.session_setup) of
        {ok, TxSession} ->
            {reply, ok, State#state{tx_session=TxSession, tx_mc_list=McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call({open_transceiver, Addr, Port, McList}, _From, State) ->
    case do_open(State#state.rx_session,Addr,Port,State#state.session_setup) of
        {ok, TrxSession} ->
            {reply, ok, State#state{rx_session = TrxSession,
                                    tx_session = TrxSession,
                                    rx_mc_list = McList,
                                    tx_mc_list = McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call({listen_receiver, Port, McList}, _From, State) ->
    case do_listen(State#state.rx_session, Port, State#state.session_setup) of
        {ok, RxSession} ->
            {reply, ok, State#state{rx_session=RxSession, rx_mc_list=McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call({listen_transmitter, Port, McList}, _From, State) ->
    case do_listen(State#state.tx_session, Port, State#state.session_setup) of
        {ok, TxSession} ->
            {reply, ok, State#state{tx_session=TxSession, tx_mc_list=McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call({listen_transceiver, Port, McList}, _From, State) ->
    case do_listen(State#state.rx_session, Port, State#state.session_setup) of
        {ok, TrxSession} ->
            {reply, ok, State#state{rx_session = TrxSession, 
                                    tx_session = TrxSession,
                                    rx_mc_list = McList,
                                    tx_mc_list = McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call(close_receiver, _From, #state{rx_session = S} = State) ->
    {reply, do_close(S), State};

handle_call(close_transmitter, _From, #state{tx_session = S} = State) ->
    {reply, do_close(S), State};

handle_call(close_transceiver, _From, #state{rx_session = S, 
                                             tx_session = S} = State) ->
    {reply, do_close(S), State}.


%%%
% @spec handle_cast(Request, State) -> Result
%    Request  = term()
%    Result   = {noreply, NewState}          |
%               {noreply, NewState, Timeout} |
%               {stop, Reason, NewState}
%    NewState = term()
%    Timeout  = int() | infinity
%    Reason   = normal | term()
%
% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server - handle_cast/2</a> callback implementation.  Handling cast
% messages.
%
% <ul>
%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called</li>
% </ul>
%
% @see terminate/2
% @end
%%
handle_cast(bind_receiver, State) ->
    do_bind_receiver(State), 
    {noreply, State};

handle_cast(bind_transmitter, State) ->
    do_bind_transmitter(State),
    {noreply, State};

handle_cast(bind_transceiver, State) ->
    do_bind_transceiver(State), 
    {noreply, State};

handle_cast({alert_notification, Pdu}, State) ->
    spawn_link(fun() -> callback_alert_notification(Pdu, State) end),
    {noreply, State};
    
handle_cast(die, #state{rx_session = S, tx_session = S} = State) ->
    stop_session(S), 
    {noreply, State};
    
handle_cast(die, #state{rx_session = R, tx_session = T} = State) ->
    stop_session(R), 
    stop_session(T), 
    {noreply, State};

handle_cast({outbind, S, SystemId, Password}, #state{rx_session = S,
                                                     tx_session = S} = State)->
    case authenticate_mc(SystemId, Password, State#state.rx_mc_list) of
        true ->
            do_bind_transceiver(State);
        false ->  % Permission denied
            do_close(S)
    end,
    {noreply, State};

handle_cast({outbind, S, SystemId, Password}, #state{rx_session = S} = State)->
    case authenticate_mc(SystemId, Password, State#state.rx_mc_list) of
        true ->
            do_bind_receiver(State);
        false ->  % Permission denied
            do_close(S)
    end,
    {noreply, State};

handle_cast({outbind, S, SystemId, Password}, #state{tx_session = S} = State)->
    case authenticate_mc(SystemId, Password, State#state.rx_mc_list) of
        true ->
            do_bind_transmitter(State);
        false ->  % Permission denied
            do_close(S)
    end,
    {noreply, State};

handle_cast({mc_unavailable, S}, #state{rx_session=S, tx_session=S} = State) ->
    callback_transceiver_mc_unavailable(State),
    {noreply, State};

handle_cast({mc_unavailable, S}, #state{rx_session = S} = State) ->
    callback_receiver_mc_unavailable(State),
    {noreply, State};

handle_cast({mc_unavailable, S}, #state{tx_session = S} = State) ->
    callback_transmitter_mc_unavailable(State),
    {noreply, State};

handle_cast({resume_service, S}, #state{rx_session=S, tx_session=S} = State) ->
    do_bind_transceiver(State),
    {noreply, State};

handle_cast({resume_service, S}, #state{rx_session = S} = State) ->
    do_bind_receiver(State),
    {noreply, State};

handle_cast({resume_service, S}, #state{tx_session = S} = State) ->
    do_bind_transmitter(State),
    {noreply, State};

handle_cast({smpp_listen_error, Port}, State) ->
    callback_smpp_listen_error(Port, State),
    {noreply, State};

handle_cast({smpp_listen_recovery, Port}, State) ->
    callback_smpp_listen_recovery(Port, State),
    {noreply, State};

handle_cast(unbind_receiver, State) ->
    do_unbind_receiver(State),
    {noreply, State};

handle_cast(unbind_transmitter, State) ->
    do_unbind_transmitter(State),
    {noreply, State};

handle_cast(unbind_transceiver, State) ->
    do_unbind_transceiver(State),
    {noreply, State}.


%%%
% @doc Auxiliary function for handle_cast/2
%
% <p>This function is only used on the outbind request.</p>
% @end
%
% %@see
%%
authenticate_mc(_, _, [])                -> false;
authenticate_mc(Id, Pwd, [{Id,  Pwd}|_]) -> true;
authenticate_mc(Id, Pwd, [{Id,  '_'}|_]) -> true;
authenticate_mc(Id, Pwd, [{'_', Pwd}|_]) -> true;
authenticate_mc(Id, Pwd, [{'_', '_'}|_]) -> true;
authenticate_mc(Id, Pwd, [_|T])          -> authenticate_mc(Id, Pwd, T).
                      

%%%
% @spec handle_info(Info, State) -> Result
%    Info     = timeout | term()
%    State    = term()
%    Result   = {noreply, NewState}          |
%               {noreply, NewState, Timeout} |
%               {stop, Reason, NewState}
%    NewState = term()
%    Timeout  = int() | infinity
%    Reason   = normal | term()
%
% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server - handle_info/2</a> callback implementation.  Handling all 
% non call/cast messages.
%
% <ul>
%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called</li>
% </ul>
%
% @see terminate/2
% @end
%%
handle_info({'EXIT', S, Reason}, #state{rx_session = S} = State) ->
    case State#state.tx_session of
        undefined ->
            {stop, normal, State#state{rx_session = undefined}};
        TxSession when TxSession == S ->
            {stop, normal, State#state{rx_session = undefined, 
                                       tx_session = undefined}};
        TxSession when Reason /= normal ->
            do_close(TxSession),
            stop_session(TxSession),
            {noreply, State#state{rx_session = undefined}};
        _TxSession ->
            {noreply, State#state{rx_session = undefined}}
    end;

handle_info({'EXIT', S, Reason}, #state{tx_session = S} = State) ->
    case State#state.rx_session of
        undefined ->
            {stop, normal, State#state{tx_session = undefined}};
        RxSession when RxSession == S ->
            {stop, normal, State#state{rx_session = undefined,
                                       tx_session = undefined}};
        RxSession when Reason /= normal ->
            do_close(RxSession),
            stop_session(RxSession),
            {noreply, State#state{tx_session = undefined}};
        _RxSession ->
            {noreply, State#state{tx_session = undefined}}
    end;

handle_info({'EXIT', _Child, Reason}, State) when Reason /= normal ->
    % A child process terminates with abnormal status.
    if
        State#state.rx_session == State#state.tx_session ->
            do_close(State#state.rx_session),
            stop_session(State#state.rx_session);
        true ->
            do_close(State#state.rx_session),
            stop_session(State#state.rx_session),
            do_close(State#state.tx_session),
            stop_session(State#state.tx_session)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%%
% @spec terminate(Reason, State) -> true
%    Reason = normal | shutdown | term()
%    State  = term()
%
% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server - terminate/2</a> callback implementation.  Shutdown the 
% server.
%
% <p>Return value is ignored by <tt>gen_server</tt>.</p>
% @end
%%
terminate(Reason, #state{self = S} = _State) ->
    case process_info(S, registered_name) of
        {registered_name, Name} ->
            unregister(Name);
        _NotRegistered ->
            true
    end.


%%%
% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%    OldVsn   = undefined | term()
%    State    = term()
%    Extra    = term
%    NewState = term()
%
% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_server.html">gen_server - code_change/3</a> callback implementation.  Convert 
% process state when code is changed
% @end
%%
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%%%===================================================================
% Session functions
%%====================================================================
%%%
% @spec outbind(Pid, Sid, Pdu) -> ok 
%    Pid = pid()
%    Sid = pid()
%    Pdu = pdu()
%
% @doc <a href="gen_esme_session.html#outbind-3">gen_esme_session - 
% outbind/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
outbind(Pid, Sid, Pdu) ->
    SystemId = operation:get_param(system_id, Pdu),
    Password = operation:get_param(system_id, Pdu),
    gen_server:cast(Pid, {outbind, Sid, SystemId, Password}).
      

%%%
% @spec unbind(Pid, Sid) -> ok | {error, Error}
%    Pid = pid()
%    Sid = pid()
%    Error = int()
%
% @doc <a href="gen_esme_session.html#unbind-2">gen_esme_session - unbind/2</a>
% callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
unbind(Pid, Sid) ->
    gen_server:call(Pid, {unbind, Sid}, infinity).


%%%
% @spec mc_unavailable(Pid, Sid, Address, Port) -> ok
%    Pid = pid()
%    Sid = pid()
%    Address = string() | atom() | ip_address()
%    Port = int()
%
% @doc <a href="gen_esme_session.html#mc_unavailable-4">gen_esme_session - 
% mc_unavailable-4</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
mc_unavailable(Pid, Sid, _Address, _Port) ->
    gen_server:cast(Pid, {mc_unavailable, Sid}).

    
%%%
% @spec resume_service(Pid, Sid, Address, Port) -> ok
%    Pid = pid()
%    Sid = pid()
%    Address = string() | atom() | ip_address()
%    Port = int()
%
% @doc <a href="gen_esme_session.html#resume_service-4">gen_esme_session -
% resume_service/4</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
resume_service(Pid, Sid, _Address, _Port) ->
    gen_server:cast(Pid, {resume_service, Sid}).


%%%
% @spec smpp_listen_error(Pid, Sid, Port) -> ok
%    Pid = pid()
%    Sid = pid()
%    Port = int()
%
% @doc <a href="gen_esme_session.html#smpp_listen_error-3">gen_esme_session -
% smpp_listen_error/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
smpp_listen_error(Pid, _Sid, Port) -> 
    gen_server:cast(Pid, {smpp_listen_error, Port}).


%%%
% @spec smpp_listen_recovery(Pid, Sid, Port) -> ok
%    Pid = pid()
%    Sid = pid()
%    Port = int()
%
% @doc <a href="gen_esme_session.html#smpp_listen_recovery-3">gen_esme_session 
% - smpp_listen_recovery-3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
smpp_listen_recovery(Pid, _Sid, Port) ->
    gen_server:cast(Pid, {smpp_listen_recovery, Port}).


%%%
% @spec alert_notification(Pid, Sid, Pdu) -> ok
%    Pid = pid()
%    Sid = pid()
%    Pdu = pdu()
%
% @doc <a href="gen_esme_session.html#alert_notification-3">gen_esme_session -
% alert_notification/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
alert_notification(Pid, _Sid, Pdu) ->
    gen_server:cast(Pid, {alert_notification, Pdu}).


%%%
% @spec deliver_sm(Pid, Sid, Pdu) -> Result
%    Pid        = pid()
%    Sid        = pid()
%    Pdu        = pdu()
%    Result     = {ok, ParamList} | {error, Error, ParamList}
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%
% @doc <a href="gen_esme_session.html#deliver_sm-3">gen_esme_session - 
% deliver_sm/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
deliver_sm(Pid, Sid, Pdu) ->
    gen_server:call(Pid, {deliver_sm, Pdu}, infinity).


%%%
% @spec deliver_data_sm(Pid, Sid, Pdu) -> Result
%    Pid        = pid()
%    Sid        = pid()
%    Pdu        = pdu()
%    Result     = {ok, ParamList} | {error, Error, ParamList}
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%
% @doc <a href="gen_esme_session.html#deliver_data_sm-3">gen_esme_session -
% deliver_data_sm/3</a> callback implementation.
% @end
%
% %@see
%
% %@equiv
%%
deliver_data_sm(Pid, _Sid, Pdu) ->
    gen_server:call(Pid, {deliver_data_sm, Pdu}, infinity).



%%%-------------------------------------------------------------------
% Internal functions
%%--------------------------------------------------------------------
%%%
% @spec do_open(Session, Address, Port, SessionSetup) -> Result
%    Session      = pid() | undefined
%    Address      = string() | atom() | ip_address()
%    Port         = int()
%    SessionSetup = session_setup()
%    Result       = {ok, OpenSession} | {error, Error}
%    OpenSession  = pid()
%    Error        = term()
%
% @doc Opens the given <tt>Session</tt>.  If <tt>Session</tt> is the atom 
% <tt>undefined</tt>, a new SMPP session will be started.
%
% <p>Issuing an open operation on bound sessions returns
% <tt>{error, ?ESME_RINVBNDSTS}</tt>.</p>
% @end
%
% %@see
%
% %@equiv
%%
do_open(undefined, Address, Port, SessionSetup) ->
    case gen_esme_session:start_link(?MODULE, SessionSetup) of
        {ok, Session} ->
            do_open(Session, Address, Port, SessionSetup);
        Error ->
            Error
    end;

do_open(Session, Address, Port, _SessionSetup) ->
    case gen_esme_session:do_open(Session, Address, Port) of
        ok ->
            {ok, Session};
        Error ->
            Error
    end.


%%%
% @spec do_listen(Session, Port, SessionSetup) -> Result
%    Session      = pid()
%    Port         = int()
%    SessionSetup = gen_esme_session_setup()
%    Result       = {ok, LsnSession} | {error, Error}
%    LsnSession   = pid()
%    Error        = term()
%
% @doc Sets a session to listen on <tt>Port</tt>.
%
% <p>If <tt>Session</tt> is undefined a new SMPP session will be started.
% </p>
%
% <p>Issuing a listen operation on bound sessions returns
% <tt>{error, ?ESME_RINVBNDSTS}</tt>.</p>
% @end
%
% %@see
%
% %@equiv
%%
do_listen(undefined, Port, SessionSetup)->
    case gen_esme_session:start_link(?MODULE, SessionSetup) of
        {ok, Session} ->
            do_listen(Session, Port, SessionSetup);
        Error ->
            Error
    end;

do_listen(Session, Port, _SessionSetup) ->
    case gen_esme_session:do_listen(Session, Port) of
        ok ->
            {ok, Session};
        Error ->
            Error
    end.


%%%
% @spec do_close(Session) -> Result
%    Session = pid() | undefined
%    Result  = ok | {error, Error}
%    Error   = term()
%
% @doc Closes the given <tt>Session</tt>.  If <tt>Session</tt> is the atom 
% <tt>undefined</tt> the function does nothing.
% @end
%
% %@see
%
% %@equiv
%%
do_close(undefined) -> 
    ok;

do_close(Session) ->
    gen_esme_session:do_close(Session).


%%%
% @spec do_bind_receiver(State) -> ok
%    State = state()
%
% @doc Issues a bind_receiver request on the receiver session of the ESME.
% @end
%
% %@see
%
% %@equiv
%%
do_bind_receiver(#state{rx_session = RxSession} = State) ->
    EsmeParams = [{system_id,     State#state.system_id},
                  {password,      State#state.password},
                  {system_type,   State#state.system_type},
                  {addr_ton,      State#state.addr_ton},
                  {addr_npi,      State#state.addr_npi},
                  {address_range, State#state.address_range}],
    Resp = gen_esme_session:bind_receiver(RxSession, EsmeParams),
    callback_bind_receiver_resp(Resp, State).


%%%
% @spec do_bind_transmitter(State) -> ok
%    State = state()
%
% @doc Issues a bind_transmitter request on transmitter session of the ESME.
% @end
%
% %@see
%
% %@equiv
%%
do_bind_transmitter(#state{tx_session = TxSession} = State) ->
    EsmeParams = [{system_id,     State#state.system_id},
                  {password,      State#state.password},
                  {system_type,   State#state.system_type},
                  {addr_ton,      State#state.addr_ton},
                  {addr_npi,      State#state.addr_npi},
                  {address_range, State#state.address_range}],
    Resp = gen_esme_session:bind_transmitter(TxSession, EsmeParams),
    callback_bind_transmitter_resp(Resp, State).


%%%
% @spec do_bind_transceiver(State) -> Result
%    State = state()
%
% @doc Issues a bind_transceiver request on the receiver session of the ESME.
% Assumes the receiver session acts also as the transmitter session.
% @end
%
% %@see
%
% %@equiv
%%
do_bind_transceiver(#state{rx_session = RxSession} = State) ->
    EsmeParams = [{system_id,     State#state.system_id},
                  {password,      State#state.password},
                  {system_type,   State#state.system_type},
                  {addr_ton,      State#state.addr_ton},
                  {addr_npi,      State#state.addr_npi},
                  {address_range, State#state.address_range}],
    Resp = gen_esme_session:bind_transceiver(RxSession, EsmeParams),
    callback_bind_transceiver_resp(Resp, State).


%%%
% @spec do_broadcast_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Send a broadcast_sm request over the transmitter session.  
% <tt>From</tt> represents the caller process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_broadcast_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{service_type,    State#state.service_type},
                  {source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:broadcast_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_cancel_broadcast_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Sends a cancel_broadcast_sm request over the transmitter session. 
% <tt>From</tt> represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_cancel_broadcast_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{service_type,    State#state.service_type},
                  {source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:cancel_broadcast_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_cancel_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Sends a cancel_sm request over the transmitter session. 
% <tt>From</tt> represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_cancel_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{service_type,    State#state.service_type},
                  {source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:cancel_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_data_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Sends a data_sm request over the transmitter session.  <tt>From</tt>
% represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_data_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{service_type,    State#state.service_type},
                  {source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:data_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_deliver_sm(Pdu, From, State) -> ok
%    Pdu   = pdu()
%    From  = {pid(), Tag}
%    Tag   = term()
%    State = state()
%
% @doc Handles a short message delivery (deliver_sm) on the ESME.
% @end
%
% %@see
%
% %@equiv
%%
do_deliver_sm(Pdu, From, State) ->
    gen_server:reply(From, callback_deliver_sm(Pdu, State)).
    

%%%
% @spec do_deliver_data_sm(Pdu, From, State) -> ok
%    Pdu   = pdu()
%    From  = {pid(), Tag}
%    Tag   = term()
%    State = state()
%
% @doc Handles a short message delivery (data_sm) on the ESME.
% @end
%
% %@see
%
% %@equiv
%%
do_deliver_data_sm(Pdu, From, State) ->
    gen_server:reply(From, callback_deliver_data_sm(Pdu, State)).


%%%
% @spec do_query_broadcast_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Sends a query_broadcast_sm request over the transmitter session. 
% <tt>From</tt> represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_query_broadcast_sm(ParamList, From, #state{tx_session = S} = State)->
    EsmeParams = [{source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:query_broadcast_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_query_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Sends a query_sm request over the transmitter session.  
% <tt>From</tt> represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_query_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:query_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_replace_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Sends a replace_sm request over the transmitter session.  
% <tt>From</tt> represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_replace_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:replace_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_submit_multi(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Send a submit_multi request over the transmitter session.  
% <tt>From</tt> represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_submit_multi(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{service_type,    State#state.service_type},
                  {source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:submit_multi(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_submit_sm(ParamList, From, State) -> ok
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    State        = state()
%
% @doc Send a submit_sm request over the transmitter session. <tt>From</tt>
% represents the process issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
do_submit_sm(ParamList, From, #state{tx_session = S} = State) ->
    EsmeParams = [{service_type,    State#state.service_type},
                  {source_addr_ton, State#state.source_addr_ton},
                  {source_addr_npi, State#state.source_addr_npi},
                  {source_addr,     State#state.source_addr}],
    Params = operation:merge_params(ParamList, EsmeParams),
    Reply  = gen_esme_session:submit_sm(S, Params),
    gen_server:reply(From, Reply).


%%%
% @spec do_unbind_receiver(State) -> ok
%    State = state()
%
% @doc Requests an unbind on the receiver session.
% @end
%
% %@see
%
% %@equiv
%%
do_unbind_receiver(State) ->
    Resp = gen_esme_session:unbind(State#state.rx_session),
    callback_unbind_receiver_resp(Resp, State).


%%%
% @spec do_unbind_transmitter(State) -> ok
%    State = state()
%
% @doc Requests an unbind on the transmitter session.
% @end
%
% %@see
%
% %@equiv
%%
do_unbind_transmitter(State) ->
    Resp = gen_esme_session:unbind(State#state.tx_session),
    callback_unbind_transmitter_resp(Resp, State).


%%%
% @spec do_unbind_transceiver(State) -> ok
%    State = state()
%
% @doc Requests an unbind on the transceiver session.
% @end
%
% %@see
%
% %@equiv
%%
do_unbind_transceiver(State) ->
    Resp = gen_esme_session:unbind(State#state.rx_session),
    callback_unbind_transceiver_resp(Resp, State).


%%%
% @spec stop_session(Session) -> ok
%    Session = pid() | undefined
%
% @doc Stops an underlying session. 
% @end
%
% %@see
%
% %@equiv
%%
stop_session(undefined) ->
    ok;

stop_session(Session) ->
    gen_esme_session:stop(Session).


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Callback wrappers
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% @spec callback_bind_receiver_resp(Resp, State) -> ok
%    Resp    = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:bind_receiver_resp/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_bind_receiver_resp(Resp, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:bind_receiver_resp(Pid, Eid, Resp) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_bind_transmitter_resp(Resp, State) -> ok
%    Resp    = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:bind_transmitter_resp/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_bind_transmitter_resp(Resp, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:bind_transmitter_resp(Pid, Eid, Resp) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_bind_transceiver_resp(Resp, State) -> ok
%    Resp    = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:bind_transceiver_resp/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_bind_transceiver_resp(Resp, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:bind_transceiver_resp(Pid, Eid, Resp) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_alert_notification(Pdu, State) -> ok
%    Pdu   = pdu()
%    State = state()
%
% @doc Wrapper for CallbackModule:alert_notification/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_alert_notification(Pdu, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:alert_notification(Pid, Eid, Pdu) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_deliver_sm(Pdu, State) -> Result
%    Pdu        = pdu()
%    State      = state()
%    Result     = {ok, ParamList} | {error, Error, ParamList}
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%
% @doc Wrapper for CallbackModule:deliver_sm/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_deliver_sm(Pdu, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:deliver_sm(Pid, Eid, Pdu) of
        {ok, ParamList} when list(ParamList) ->
            {ok, ParamList};
        {error, Error, ParamList} when integer(Error), list(ParamList) ->
            {error, Error, ParamList};
        {error, _Error, ParamList} when list(ParamList) ->
            {error, ?ESME_RUNKNOWNERR, ParamList};
        _Otherwise ->
            {error, ?ESME_RUNKNOWNERR, []}
    end.


%%%
% @spec callback_deliver_data_sm(Pdu, State) -> Result
%    Pdu        = pdu()
%    State      = state()
%    Result     = {ok, ParamList} | {error, Error, ParamList}
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%
% @doc Wrapper for CallbackModule:deliver_data_sm/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_deliver_data_sm(Pdu, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:deliver_data_sm(Pid, Eid, Pdu) of
        {ok, ParamList} when list(ParamList) ->
            {ok, ParamList};
        {error, Error, ParamList} when integer(Error), list(ParamList) ->
            {error, Error, ParamList};
        {error, _Error, ParamList} when list(ParamList) ->
            {error, ?ESME_RUNKNOWNERR, ParamList};
        _Otherwise ->
            {error, ?ESME_RUNKNOWNERR, []}
    end.


%%%
% @spec callback_receiver_mc_unbind(State) -> ok | {error, Error}
%    State = state()
%    Error = int()
%
% @doc Wrapper for CallbackModule:receiver_mc_unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_receiver_mc_unbind(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:receiver_mc_unbind(Pid, Eid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        {error, _Error} ->
            {error, ?ESME_RUNKNOWNERR};
        _Otherwise ->
            ok
    end.


%%%
% @spec callback_transmitter_mc_unbind(State) -> ok | {error, Error}
%    State = state()
%    Error = int()
%
% @doc Wrapper for CallbackModule:transmitter_mc_unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_transmitter_mc_unbind(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transmitter_mc_unbind(Pid, Eid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        {error, _Error} ->
            {error, ?ESME_RUNKNOWNERR};
        _Otherwise ->
            ok
    end.


%%%
% @spec callback_transceiver_mc_unbind(State) -> ok | {error, Error}
%    State = state()
%    Error = int()
%
% @doc Wrapper for CallbackModule:transceiver_mc_unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_transceiver_mc_unbind(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transceiver_mc_unbind(Pid, Eid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        {error, _Error} ->
            {error, ?ESME_RUNKNOWNERR};
        _Otherwise ->
            ok
    end.

    
%%%
% @spec callback_receiver_mc_unavailable(State) -> ok
%    State = state()
%
% @doc Wrapper for CallbackModule:receiver_mc_unavailable/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_receiver_mc_unavailable(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:receiver_mc_unavailable(Pid, Eid) of
        _Whatever ->
            ok
    end.

    
%%%
% @spec callback_transmitter_mc_unavailable(State) -> ok
%    State = state()
%
% @doc Wrapper for CallbackModule:transmitter_mc_unavailable/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_transmitter_mc_unavailable(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transmitter_mc_unavailable(Pid, Eid) of
        _Whatever ->
            ok
    end.

    
%%%
% @spec callback_transceiver_mc_unavailable(State) -> ok
%    State = state()
%
% @doc Wrapper for CallbackModule:transceiver_mc_unavailable/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_transceiver_mc_unavailable(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transceiver_mc_unavailable(Pid, Eid) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_smpp_listen_error(Port, State) -> ok
%    Port  = int()
%    State = state()
%
% @doc Wrapper for CallbackModule:smpp_listen_error/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_smpp_listen_error(Port, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:smpp_listen_error(Pid, Eid, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_smpp_listen_recovery(Port, State) -> ok
%    Port  = int()
%    State = state()
%
% @doc Wrapper for CallbackModule:smpp_listen_recovery/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_smpp_listen_recovery(Port, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:smpp_listen_recovery(Pid, Eid, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_unbind_receiver_resp(Resp, State) -> ok
%    Resp    = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:unbind_receiver_resp/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_unbind_receiver_resp(Resp, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:unbind_receiver_resp(Pid, Eid, Resp) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_unbind_transmitter_resp(Resp, State) -> ok
%    Resp    = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:unbind_transmitter_resp/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_unbind_transmitter_resp(Resp, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:unbind_transmitter_resp(Pid, Eid, Resp) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_unbind_transceiver_resp(Resp, State) -> ok
%    Resp    = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:unbind_transceiver_resp/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_unbind_transceiver_resp(Resp, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:unbind_transceiver_resp(Pid, Eid, Resp) of
        _Whatever ->
            ok
    end.
