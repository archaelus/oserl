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
%     <td valign="top"><a href="#bound_receiver-3">bound_receiver/3</a></td>
%     <td>The ESME binds as a receiver.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#bound_transmitter-3">bound_transmitter/3</a>
%     </td>
%     <td>The ESME binds as a transmitter.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#bound_transceiver-3">bound_transceiver/3</a>
%     </td>
%     <td>The ESME binds as a transceiver.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#receiver_unbind-2">receiver_unbind/2</a></td>
%     <td>Unbinds on the receiver session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#transmitter_unbind-2">transmitter_unbind/2</a>
%     </td>
%     <td>Unbinds on the transmitter session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#transceiver_unbind-2">transceiver_unbind/2</a>
%     </td>
%     <td>Unbinds on the transceiver session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#receiver_mc_unavailable-4">receiver_mc_unavailable/4</a>
%     </td>
%     <td>The MC is unavailable on the receiver session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%      <a href="#transmitter_mc_unavailable-4">transmitter_mc_unavailable/4</a>
%     </td>
%     <td>The MC becomes unavailable on the transmitter session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%      <a href="#transceiver_mc_unavailable-4">transceiver_mc_unavailable/4</a>
%     </td>
%     <td>The MC becomes unavailable on the transceiver session.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#receiver_can_not_resume-5">receiver_can_not_resume/5</a>
%     </td>
%     <td>
%       Cannot resume the receiver session, though the connection was
%       successfully reestablished.
%     </td>
%   </tr>
%   <tr>
%     <td valign="top">
%      <a href="#transmitter_can_not_resume-5">transmitter_can_not_resume/5</a>
%     </td>
%     <td>
%       Cannot resume the transmitter session, though the connection was
%       successfully reestablished.
%     </td>
%   </tr>
%   <tr>
%     <td valign="top">
%      <a href="#transceiver_can_not_resume-5">transceiver_can_not_resume/5</a>
%     </td>
%     <td>
%       Cannot resume the transceiver session, though the connection was
%       successfully reestablished.
%     </td>
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
% </table>
%
%
% <h2>Callback Function Details</h2>
%
% <h3><a name="bound_receiver-3">bound_receiver/3</a></h3>
%
% <tt>bound_receiver(Pid, Sid, SystemId) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>SystemId = string()</tt></li>
% </ul>
%
% <p>After an outbind request or a period during the MC was unreachable, if 
% the ESME gets to (re)bind as a receiver to the MC, this callback is triggered
% to notify.  Returning value is ignored by the ESME. The callback module
% may start some initialization on response to this callback.</p>
%
% <p>The MC on the other peer is identified by <tt>SystemId</tt>.  
% <tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process
% id.</p>
%
% <p><b>See also:</b> <tt>callback_bound_receiver/2</tt></p>
%
%
% <h3><a name="bound_transmitter-3">bound_transmitter/3</a></h3>
%
% <tt>bound_transmitter(Pid, Sid, SystemId) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>SystemId = string()</tt></li>
% </ul>
%
% <p>After a period during the MC was unreachable, if the ESME gets to 
% rebind as a transmitter to the MC, this callback is triggered to notify.  
% Returning value is ignored by the ESME.  The callback module may start some
% initialization on response to this callback.</p>
%
% <p>The MC on the other peer is identified by <tt>SystemId</tt>.  
% <tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process
% id.</p>
%
% <p><b>See also:</b> <tt>callback_bound_transmitter/2</tt></p>
%
%
% <h3><a name="bound_transceiver-3">bound_transceiver/3</a></h3>
%
% <tt>bound_transceiver(Pid, Sid, SystemId) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>SystemId = string()</tt></li>
% </ul>
%
% <p>After an outbind request or a period during the MC was unreachable, if 
% the ESME gets to (re)bind as a transceiver to the MC, this callback is 
% triggered to notify.  Returning value is ignored by the ESME.  The callback 
% module may start some initialization on response to this callback.</p>
%
% <p>The MC on the other peer is identified by <tt>SystemId</tt>.  
% <tt>Pid</tt> is the ESME parent id, <tt>Eid</tt> as the ESME process
% id.</p>
%
% <p><b>See also:</b> <tt>callback_bound_transceiver/2</tt></p>
%
%
% <h3><a name="receiver_unbind-2">receiver_unbind/2</a></h3>
%
% <tt>receiver_unbind(Pid, Eid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards an unbind request (issued by the MC) to the 
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
% <p><b>See also:</b> <tt>callback_receiver_unbind/1</tt></p>
%
%
% <h3><a name="transmitter_unbind-2">transmitter_unbind/2</a></h3>
%
% <tt>transmitter_unbind(Pid, Eid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards an unbind request (issued by the MC) to the 
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
% <p><b>See also:</b> <tt>callback_transmitter_unbind/1</tt></p>
%
%
% <h3><a name="transceiver_unbind-2">transceiver_unbind/2</a></h3>
%
% <tt>transceiver_unbind(Pid, Eid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards an unbind request (issued by the MC) to the 
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
% <p><b>See also:</b> <tt>callback_transceiver_unbind/1</tt></p>
%
%
% <h3><a name="receiver_mc_unavailable-4">receiver_mc_unavailable/4</a></h3>
%
% <tt>receiver_mc_unavailable(Pid, Eid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable on the receiver session, this circumstance
% is notified with a call to this function.</p>
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_receiver_mc_unavailable/3</tt></p>
%
%
% <h3>
%   <a name="transmitter_mc_unavailable-4">transmitter_mc_unavailable/4</a>
% </h3>
%
% <tt>transmitter_mc_unavailable(Pid, Eid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable on the transmitter session, this
% circumstance is notified with a call to this function.</p>
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transmitter_mc_unavailable/3</tt></p>
%
%
% <h3>
%   <a name="transceiver_mc_unavailable-4">transceiver_mc_unavailable/4</a>
% </h3>
%
% <tt>transceiver_mc_unavailable(Pid, Eid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable on the transceiver session, this
% circumstance is notified with a call to this function.</p>
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transceiver_mc_unavailable/3</tt></p>
%
%
% <h3><a name="receiver_can_not_resume-5">receiver_can_not_resume/5</a></h3>
%
% <tt>receiver_can_not_resume(Pid, Eid, Addr, Port, Err) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Addr = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Err  = term()</tt></li>
% </ul>
%
% <p>After a period of unavailability of the MC, once the underlying 
% connection was recover, the receiver session failed to rebind to the MC with
% error <tt>Err</tt>.  The callback module must take care of this 
% situation.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_receiver_can_not_resume/4</tt></p>
%
%
% <h3>
%   <a name="transmitter_can_not_resume-5">transmitter_can_not_resume/5</a>
% </h3>
%
% <tt>transmitter_can_not_resume(Pid, Eid, Addr, Port, Err) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Addr = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Err  = term()</tt></li>
% </ul>
%
% <p>After a period of unavailability of the MC, once the underlying 
% connection was recover, the transmitter session failed to rebind to the MC 
% with error <tt>Err</tt>.  The callback module must take care of this 
% situation.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transmitter_can_not_resume/4</tt></p>
%
%
% <h3>
%   <a name="transceiver_can_not_resume-5">transceiver_can_not_resume/5</a>
% </h3>
%
% <tt>transceiver_can_not_resume(Pid, Eid, Addr, Port, Err) -> ok</tt>
% <ul>
%   <li><tt>Pid = Eid = pid()</tt></li>
%   <li><tt>Addr = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
%   <li><tt>Err  = term()</tt></li>
% </ul>
%
% <p>After a period of unavailability of the MC, once the underlying 
% connection was recover, the transceiver session failed to rebind to the MC 
% with error <tt>Err</tt>.  The callback module must take care of this 
% situation.</p>
% 
% <p><tt>Pid</tt> is the ESME's parent id, <tt>Eid</tt> as the ESME
% process id.</p>
%
% <p><b>See also:</b> <tt>callback_transceiver_can_not_resume/4</tt></p>
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
         listen/1,
         listen/2,
         listen/3,
         bind_receiver/2,
         bind_receiver/3,
         bind_transmitter/2,
         bind_transmitter/3,
         bind_transceiver/2,
         bind_transceiver/3,
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
%         McList}
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
%    McList           = [McId]
%    McId             = {SystemId, Password, BindMode}
%    BindMode         = receiver | transceiver
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
%   <dt>McList: </dt><dd>List with the identities of the MCs from where an
%     outbind operation is accepted.  See listen/2 for more details.
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
         mc_list    = []}).

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
     [{bound_receiver, 3}, 
     {bound_transmitter, 3}, 
     {bound_transceiver, 3}, 
     {receiver_unbind, 2},
     {transmitter_unbind, 2},
     {transceiver_unbind, 2},
     {receiver_mc_unavailable, 4}, 
     {transmitter_mc_unavailable, 4}, 
     {transceiver_mc_unavailable, 4}, 
     {receiver_can_not_resume, 5}, 
     {transmitter_can_not_resume, 5}, 
     {transceiver_can_not_resume, 5}, 
     {smpp_listen_error, 3},
     {smpp_listen_recovery, 3},
     {alert_notification, 3},
     {deliver_sm, 3}, 
     {deliver_data_sm, 3}];

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
% @spec listen(Eid) -> ok
%    Eid      = pid()
%
% @doc
% @see listen/2
% @see listen/3
%
% @equiv listen(Eid, [{'_', '_', receiver}])
% @end
%%
listen(Eid) ->
    listen(Eid, [{'_', '_', receiver}]).
    

%%%
% @spec listen(Eid, McList) -> ok
%    Eid      = pid()
%    McList   = [McId]
%    McId     = {SystemId, Password, BindMode}
%    SystemId = string()
%    Password = string()
%    BindMode = receiver | transceiver
%
% @doc
% @see listen/1
% @see listen/3
%
% @equiv listen(Eid, DEFAULT_SMPP_PORT, McList)
% @end
%%
listen(Eid, McList) ->
    listen(Eid, ?DEFAULT_SMPP_PORT, McList).


%%%
% @spec listen(Eid, Port, McList) -> ok
%    Eid      = pid()
%    Port     = int()
%    McList   = [McId]
%    McId     = {SystemId, Password, BindMode}
%    SystemId = string()
%    Password = string()
%    BindMode = receiver | transceiver
%
% @doc Puts a session to listen on the <tt>Port</tt>, on listen/1 and
% listen/2 the default SMPP port is assumed (defined by ?DEFAULT_SMPP_PORT).
% If an MC connects to this session and issues an outbind, the ESME, whose 
% pid() is <tt>Eid</tt>, sends the bind request corresponding to the 
% <tt>BindMode</tt> associated to the calling MC (identified on the
% outbind by a <tt>SystemId</tt> and <tt>Password</tt>).
%
% <p>The <tt>McList</tt> is searched in sequence.  Whenever a tuple on 
% this list matches the system_id and password sent along with the outbind
% PDU, a bind request of <tt>BindMode</tt> is issued, if it fails the
% next entry on the <tt>McList</tt> is tried.  Notice that the atom
% <tt>'_'</tt> is a system_id and/or password wild-card.  The MC list 
% <tt>[{'_', '_', receiver}]</tt> matches any MC identity and triggers a
% a bind_receiver request from the ESME, in fact this is the default 
% <tt>McList</tt> assumed by listen/1.</p>
%
% <p>If the MC authentication fails, or the bind operations for given bind
% modes do not succeed, the session goes back to listening, until a successful
% outbind arrives or any other action is explicitly stated.</p>
%
% @see gen_server:call/3
% @see listen/1
% @see listen/2
% @end
%
% %@equiv
%%
listen(Eid, Port, McList) ->
    gen_server:call(Eid, {listen, Port, McList}, infinity).


%%%
% @spec bind_receiver(Eid, Address) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    PduResp = pdu()
%    Error   = session_not_started | {error_code, int()}
%
% @doc
% @see bind_receiver/3
%
% @equiv bind_receiver(Eid, Address, DEFAULT_SMPP_PORT)
% @end
%%
bind_receiver(Eid, Address) ->
    bind_receiver(Eid, Address, ?DEFAULT_SMPP_PORT).


%%%
% @spec bind_receiver(Eid, Address, Port) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    PduResp = pdu()
%    Error   = session_not_started | {error_code, int()}
%
% @doc The ESME with pid <tt>Eid</tt>, issues a bind_receiver request.
%
% <p>If the receiver session is bound, the bind request fails.</p>
%
% <p>The default SMPP port is used (?DEFAULT_SMPP_PORT) unless otherwise 
% stated.</p>
%
% @see gen_server:call/3
% @see bind_receiver/2
% @end
%
% %@equiv
%%
bind_receiver(Eid, Address, Port) ->
    gen_server:call(Eid, {bind_receiver, Address, Port}, infinity).


%%%
% @spec bind_transmitter(Eid, Address) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    PduResp = pdu()
%    Error   = session_not_started | {error_code, int()}
%
% @doc
% @see bind_transmitter/3
%
% @equiv bind_transmitter(Eid, Address, DEFAULT_SMPP_PORT)
% @end
%%
bind_transmitter(Eid, Address) ->
    bind_transmitter(Eid, Address, ?DEFAULT_SMPP_PORT).


%%%
% @spec bind_transmitter(Eid, Address, Port) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    PduResp = pdu()
%    Error   = session_not_started | {error_code, int()}
%
% @doc The ESME with pid <tt>Eid</tt>, issues a bind_transmitter request.
%
% <p>If the transmitter session is bound, the bind request fails.</p>
%
% <p>The default SMPP port is used (?DEFAULT_SMPP_PORT) unless otherwise 
% stated.</p>
%
% @see gen_server:call/3
% @see bind_transmitter/2
% @end
%
% %@equiv
%%
bind_transmitter(Eid, Address, Port) ->
    gen_server:call(Eid, {bind_transmitter, Address, Port}, infinity).


%%%
% @spec bind_transceiver(Eid, Address) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    PduResp = pdu()
%    Error   = session_not_started | {error_code, int()}
%
% @doc
% @see bind_transceiver/3
%
% @equiv bind_transceiver(Eid, Address, DEFAULT_SMPP_PORT)
% @end
%%
bind_transceiver(Eid, Address) ->
    bind_transceiver(Eid, Address, ?DEFAULT_SMPP_PORT).


%%%
% @spec bind_transceiver(Eid, Address, Port) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    PduResp = pdu()
%    Error   = session_not_started | {error_code, int()}
%
% @doc The ESME with pid <tt>Eid</tt>, issues a bind_transceiver request.
%
% <p>If the ESME has a bound session, as a receiver, transmitter or 
% transceiver, the bind request will fail.</p>
%
% <p>The default SMPP port is used (?DEFAULT_SMPP_PORT) unless otherwise 
% stated.</p>
%
% @see gen_server:call/3
% @see bind_transceiver/2
% @end
%
% %@equiv
%%
bind_transceiver(Eid, Address, Port) ->
    gen_server:call(Eid, {bind_transceiver, Address, Port}, infinity).


%%%
% @spec broadcast_sm(Eid, ParamList) -> Result
%    Eid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = {error_code, int()} | atom()
%
% @doc Issues a broadcast_sm operation on the ESME identified by <tt>Eid
% </tt>.
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
%    Error      = {error_code, int()} | atom()
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
%    Error      = {error_code, int()} | atom()
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
%    Error      = {error_code, int()} | atom()
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
%    Error      = {error_code, int()} | atom()
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
%    Error      = {error_code, int()} | atom()
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
%    Error      = {error_code, int()} | atom()
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
%    Error      = {error_code, int()} | atom()
%
% @doc Issues a submit_multi operation on the ESME identified by <tt>Eid
% </tt>.
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
%    Error      = {error_code, int()} | atom()
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
% @spec unbind_receiver(Eid) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    PduResp = pdu()
%    Error   = undefined_session | {error_code, int()}
%
% @doc The ESME with pid <tt>Eid</tt>, issues an unbind on the receiver
% session.
%
% <p>If the rx_session is not defined this function returns the term
% <tt>{error, undefined_session}</tt>.</p>
% @end
%
% %@see
%
% %@equiv
%%
unbind_receiver(Eid) ->
    gen_server:call(Eid, unbind_receiver, infinity).


%%%
% @spec unbind_transmitter(Eid) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    PduResp = pdu()
%    Error   = undefined_session | {error_code, int()}
%
% @doc The ESME with pid <tt>Eid</tt>, issues an unbind on the transmitter
% session.
%
% <p>If the tx_session is not defined this function returns the term
% <tt>{error, undefined_session}</tt>.</p>
% @end
%
% %@see
%
% %@equiv
%%
unbind_transmitter(Eid) ->
    gen_server:call(Eid, unbind_transmitter, infinity).


%%%
% @spec unbind_transceiver(Eid) -> {ok, PduResp} | {error, Error}
%    Eid     = pid()
%    PduResp = pdu()
%    Error   = undefined_session | {error_code, int()}
%
% @doc The ESME with pid <tt>Eid</tt>, issues an unbind on the transceiver
% session.
%
% <p>If the rx_session and the tx_session are not defined or not the same, 
% this function returns the term <tt>{error, undefined_session}</tt>.</p>
% @end
%
% %@see
%
% %@equiv
%%
unbind_transceiver(Eid) ->
    gen_server:call(Eid, unbind_transceiver, infinity).


%%%
% @spec stop(Eid) -> ok
%    Eid = pid()
%
% @doc Stops the server.
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
% @doc Initiates the server
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
% @doc Handling call messages.
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
handle_call({listen, Port, McList}, _From, State) ->
    case do_listen(State#state.rx_session, Port, State#state.session_setup) of
        {ok, RxSession} ->
            {reply, ok, State#state{rx_session = RxSession, mc_list = McList}};
        Error ->
            {reply, Error, State}
    end;

handle_call({bind_receiver, Address, Port}, _From, State) ->
    Session = State#state.rx_session,
    case do_open(Session, Address, Port, State#state.session_setup) of
        {ok, RxSession} ->
            case do_bind_receiver(State#state{rx_session = RxSession}) of
                {ok, PduResp, NewState} ->
                    {reply, {ok, PduResp}, NewState};
                BindError ->
                    {reply, BindError, State#state{rx_session = RxSession}}
            end;
        OpenError ->
            {reply, OpenError, State}
    end;

handle_call({bind_transmitter, Address, Port}, _From, State) ->
    Session = State#state.tx_session,
    case do_open(Session, Address, Port, State#state.session_setup) of
        {ok, TxSession} ->
            case do_bind_transmitter(State#state{tx_session = TxSession}) of
                {ok, PduResp, NewState} ->
                    {reply, {ok, PduResp}, NewState};
                BindError ->
                    {reply, BindError, State#state{tx_session = TxSession}}
            end;
        OpenError ->
            {reply, OpenError, State}
    end;

handle_call({bind_transceiver, Address, Port}, _From, State) ->
    Session = State#state.rx_session,
    case do_open(Session, Address, Port, State#state.session_setup) of
        {ok, TrxSession} ->
            case do_bind_transceiver(State#state{rx_session = TrxSession}) of
                {ok, PduResp, NewState} ->
                    {reply, {ok, PduResp}, NewState};
                BindError ->
                    {reply, BindError, State#state{rx_session = TrxSession}}
            end;
        OpenError ->
            {reply, OpenError, State}
    end;

handle_call({broadcast_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({broadcast_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_broadcast_sm(ParamList, From, State) end),
    {noreply, State};
        
handle_call({cancel_broadcast_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({cancel_broadcast_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_cancel_broadcast_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({cancel_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({cancel_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_cancel_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({data_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({data_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_data_sm(ParamList, From, State) end),
    {noreply, State};
         
handle_call({query_broadcast_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({query_broadcast_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_query_broadcast_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({query_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({query_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_query_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({replace_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({replace_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_replace_sm(ParamList, From, State) end),
    {noreply, State};

handle_call({submit_multi, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({submit_multi, ParamList}, From, State) ->
    spawn_link(fun() -> do_submit_multi(ParamList, From, State) end),
    {noreply, State};

handle_call({submit_sm, _ParamList}, _From, State) 
  when State#state.tx_session == undefined ->
    {reply, {error, undefined_session}, State};

handle_call({submit_sm, ParamList}, From, State) ->
    spawn_link(fun() -> do_submit_sm(ParamList, From, State) end),
    {noreply, State};

handle_call(unbind_receiver, From, #state{rx_session = S} = State) ->
    spawn_link(fun() -> do_unbind(From, S) end),
    {noreply, State};

handle_call(unbind_transmitter, From, #state{tx_session = S} = State) ->
    spawn_link(fun() -> do_unbind(From, S) end),
    {noreply, State};

handle_call(unbind_transceiver, From, #state{rx_session = S, 
                                             tx_session = S} = State) ->
    spawn_link(fun() -> do_unbind(From, S) end),
    {noreply, State};

handle_call(unbind_transceiver, _From, State) ->
    {reply, {error, undefined_session}, State};

handle_call({deliver_sm, Pdu}, From, State) -> 
    spawn_link(fun() -> do_deliver_sm(Pdu, From, State) end),
    {noreply, State};

handle_call({deliver_data_sm, Pdu}, From, State) ->
    spawn_link(fun() -> do_deliver_data_sm(Pdu, From, State) end),
    {noreply, State};

handle_call({unbind, S}, _From, #state{rx_session=S, tx_session=S} = State) ->
    % The MC issues an unbind on the transceiver session.
    {reply, callback_transceiver_unbind(State), State};

handle_call({unbind, S}, _From, #state{rx_session = S} = State) ->
    % The MC issues an unbind on the receiver session.
    {reply, callback_receiver_unbind(State), State};

handle_call({unbind, S}, _From, #state{tx_session = S} = State) ->
    % The MC issues an unbind on the transmitter session.
    {reply, callback_transmitter_unbind(State), State};

handle_call({unbind, _S}, _From, State) ->
    % The MC issues an unbind on a session not currently used.
    {reply, ok, State}.


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
% @doc Handling cast messages.
%
% <ul>
%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called</li>
% </ul>
%
% @see terminate/2
% @end
%%
handle_cast({alert_notification, Pdu}, State) ->
    callback_alert_notification(Pdu, State),
    {noreply, State};
    
handle_cast(die, #state{rx_session = S, tx_session = S} = State) ->
    shutdown_session(S), 
    {noreply, State};
    
handle_cast(die, #state{rx_session = R, tx_session = T} = State) ->
    shutdown_session(R), 
    shutdown_session(T), 
    {noreply, State};

handle_cast({outbind, S, SystemId, Password}, #state{rx_session = S} = State)->
    case authenticate_mc(SystemId, Password, State#state.mc_list) of
        {ok, BindModes} ->
            case bind_to_mc(State, BindModes) of
                {ok, receiver,NewState} ->
                    callback_bound_receiver(SystemId, State),
                    {noreply, NewState};
                {ok, transceiver, NewState} ->
                    callback_bound_transceiver(SystemId, State),
                    {noreply, NewState};
                _Error ->
                    {noreply, State}
            end;
        _Error ->  % Permission denied
            gen_esme_session:do_close(S),
            {noreply, State}
    end;

handle_cast({outbind, Session, _SystemId, _Password}, State) ->
    gen_esme_session:do_close(Session),
    {noreply, State};

handle_cast({mc_unavailable, S, Addr, Port}, #state{rx_session = S, 
                                                    tx_session = S} = State) ->
    callback_transceiver_mc_unavailable(Addr, Port, State),
    {noreply, State};

handle_cast({mc_unavailable, S, Addr, Port}, #state{rx_session = S} = State) ->
    callback_receiver_mc_unavailable(Addr, Port, State),
    {noreply, State};

handle_cast({mc_unavailable, S, Addr, Port}, #state{tx_session = S} = State) ->
    callback_transmitter_mc_unavailable(Addr, Port, State),
    {noreply, State};

handle_cast({mc_unavailable, _S, _Addr, _Port}, State) ->
    {noreply, State};

handle_cast({resume_service, S, Addr, Port}, #state{rx_session = S, 
                                                    tx_session = S} = State) ->
    case do_bind_transceiver(State) of
        {ok, PduResp, NewState} ->
            SystemId = operation:get_param(system_id, PduResp),
            callback_bound_transceiver(SystemId, NewState),
            {noreply, NewState};
        Error ->
            callback_transceiver_can_not_resume(Addr,Port,Error,State),
            {noreply, State}
    end;

handle_cast({resume_service, S, Addr, Port}, #state{rx_session = S} = State) ->
    case do_bind_receiver(State) of
        {ok, PduResp, NewState} ->
            SystemId = operation:get_param(system_id, PduResp),
            callback_bound_receiver(SystemId, NewState),
            {noreply, NewState};
        Error ->
            callback_receiver_can_not_resume(Addr, Port, Error, State),
            {noreply, State}
    end;

handle_cast({resume_service, S, Addr, Port}, #state{tx_session = S} = State) ->
    case do_bind_transmitter(State) of
        {ok, PduResp, NewState} ->
            SystemId = operation:get_param(system_id, PduResp),
            callback_bound_transmitter(SystemId, NewState),
            {noreply, NewState};
        Error ->
            callback_transmitter_can_not_resume(Addr,Port,Error,State),
            {noreply, State}
    end;

handle_cast({resume_service, _S, _Addr, _Port}, State) ->
    {noreply, State};

handle_cast({smpp_listen_error, Port}, State) ->
    callback_smpp_listen_error(Port, State),
    {noreply, State};

handle_cast({smpp_listen_recovery, Port}, State) ->
    callback_smpp_listen_recovery(Port, State),
    {noreply, State};

handle_cast(Request, State) ->
   {noreply, State}.

%%%
% @doc Auxiliary function for handle_cast/2
%
% <p>This function is only used on the outbind request.</p>
% @end
%
% %@see
%%
authenticate_mc(SystemId, Password, McList) ->
    authenticate_mc(SystemId, Password, McList, []).

%%%
% @doc Auxiliary function for authenticate_mc/3
% @end
%
% %@see
%%
authenticate_mc(_, _, [], []) ->
    {error, permission_denied};

authenticate_mc(_, _, [], Acc) ->
    {ok, lists:reverse(Acc)};

authenticate_mc(I, P, [{I, P, M}|L], Acc) when M == receiver; 
                                               M == transceiver -> 
    authenticate_mc(I, P, L, [M|Acc]);

authenticate_mc(I, P, [{I, '_', M}|L], Acc) when M == receiver; 
                                                 M == transceiver -> 
    authenticate_mc(I, P, L, [M|Acc]);

authenticate_mc(I, P, [{'_', P, M}|L], Acc) when M == receiver; 
                                                 M == transceiver ->  
    authenticate_mc(I, P, L, [M|Acc]);

authenticate_mc(I, P, [{'_', '_', M}|L], Acc) when M == receiver; 
                                                   M == transceiver ->  
    authenticate_mc(I, P, L, [M|Acc]);

authenticate_mc(I, P, [_|L], Acc) -> 
    authenticate_mc(I, P, L, Acc).

%%%
% @doc Auxiliary function for handle_cast/2
%
% <p>This function is only used on the outbind request.</p>
% @end
%
% %@see
%%
bind_to_mc(State, [receiver|BindModes]) ->
    case do_bind_receiver(State) of
        {ok, _PduResp, NewState} ->
            {ok, receiver, NewState};
        {error, {error_code, ?ESME_RALYBND}} ->
            {error, {error_code, ?ESME_RALYBND}};
        _Error ->
            bind_to_mc(State, BindModes)
    end;

bind_to_mc(State, [transceiver|BindModes]) ->
    case do_bind_transceiver(State) of
        {ok, _PduResp, NewState} ->
            {ok, transceiver, NewState};
        {error, {error_code, ?ESME_RALYBND}} ->
            {error, {error_code, ?ESME_RALYBND}};
        _Error ->
            bind_to_mc(State, BindModes)
    end;

bind_to_mc(_State, _BindModes) ->
    {error, bind_failed}.
                      

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
% @doc Handling all non call/cast messages
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
            shutdown_session(TxSession),
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
            shutdown_session(RxSession),
            {noreply, State#state{tx_session = undefined}};
        _RxSession ->
            {noreply, State#state{tx_session = undefined}}
    end;

handle_info({'EXIT', _Child, Reason}, State) when Reason /= normal ->
    % A child process terminates with abnormal status.
    if
        State#state.rx_session == State#state.tx_session ->
            shutdown_session(State#state.rx_session);
        true ->
            shutdown_session(State#state.rx_session),
            shutdown_session(State#state.tx_session)
    end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%%%
% @spec terminate(Reason, State) -> true
%    Reason = normal | shutdown | term()
%    State  = term()
%
% @doc Shutdown the server.
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
% @doc Convert process state when code is changed
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
% @doc Outbind callback.  When the session receives an outbind request this
% callback is triggered to notify the ESME.  Returning value is ignored by the
% session.
% 
% <p>On response to this function the ESME must start the appropriate actions
% in order to bind or turn the session back to an open or closed state.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @doc This callback forwards an unbind request (issued by the MC) to the 
% ESME.  If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
% command_status is sent to the MC and the session moves into the unbound
% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
% response PDU sent by the session to the MC will have an <tt>Error</tt>
% command_status and the session will remain on it's current bound state
% (bound_rx, bound_tx or bound_trx).
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @doc If the MC becomes unavailable the session notifies that circumstance to
% the ESME with a call to this function.
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
mc_unavailable(Pid, Sid, Address, Port) ->
    gen_server:cast(Pid, {mc_unavailable, Sid, Address, Port}).

    
%%%
% @spec resume_service(Pid, Sid, Address, Port) -> ok
%    Pid = pid()
%    Sid = pid()
%    Address = string() | atom() | ip_address()
%    Port = int()
%
% @doc After a period of unavailability of the MC, once the connection is
% recover by the session, this function is called in order to let the ESME
% resume the service.
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
% @end
%
% %@see
%
% %@equiv
%%
resume_service(Pid, Sid, Address, Port) ->
    gen_server:cast(Pid, {resume_service, Sid, Address, Port}).


%%%
% @spec smpp_listen_error(Pid, Sid, Port) -> ok
%    Pid = pid()
%    Sid = pid()
%    Port = int()
%
% @doc When a session on listening state looses the listen socket, uses
% this callback to notify the ESME.
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @doc After a listen failure, a new socket could be set to listen again on
% <tt>Port</tt>.  This callback notifies the ESME.
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @doc Alert notifications are forwarded to the ESME by this callback.  The
% dictionary representing the alert_notification PDU is given to the ESME.
%
% <p>The ESME is responsible of initiating the appropriate actions associated
% to the alert notification.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @doc Short messages delivered to the ESME via this callback are enclosed
% inside a deliver_sm PDU.
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the deliver_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @doc Short messages delivered to the ESME via this callback are enclosed
% inside a data_sm PDU.
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the data_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% @spec do_listen(Session, Port, SessionSetup) -> Result
%    Session      = pid()
%    Port         = int()
%    SessionSetup = gen_esme_session_setup()
%    Result       = {ok, LsnSession} | {error, Error}
%    LsnSession   = pid()
%    Error        = session_not_started | {error_code, int()}
%
% @doc Sets a session to listen on <tt>Port</tt>.
%
% <p>If <tt>Session</tt> is undefined a new SMPP session will be started.
% </p>
%
% <p>Bound sessions cause the error <tt>{error_code, ?ESME_RINVBNDSTS}
% </tt>.</p>
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
        _Error ->
            {error, session_not_started}
    end;

do_listen(Session, Port, _SessionSetup) ->
    case gen_esme_session:do_listen(Session, Port) of
        ok ->
            {ok, Session};
        {error, {already_listening, _}} ->
            {ok, Session};
        {error, Error} ->
            {error, {error_code, Error}}
    end.


%%%
% @spec do_open(Session, Address, Port, SessionSetup) -> Result
%    Session     = pid() | undefined
%    Address     = string() | atom() | ip_address()
%    Port        = int()
%    SessionSetup  = session_setup()
%    Result      = {ok, OpenSession} | {error, Error}
%    OpenSession = pid()
%    Error       = session_not_started | {error_code, int()}
%
% @doc Opens a session.  If <tt>Session</tt> is undefined a new SMPP
% session will be started.
%
% <p>Bound sessions cause the error <tt>{error_code, ?ESME_RINVBNDSTS}
% </tt>.</p>
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
        _Error ->
            {error, session_not_started}
    end;

do_open(Session, Address, Port, _SessionSetup) ->
    case gen_esme_session:do_open(Session, Address, Port) of
        ok ->
            {ok, Session};
        {error, {already_connected, _, _}} ->
            {ok, Session};
        {error, Error} ->
            {error, {error_code, Error}}
    end.


%%%
% @spec do_bind_receiver(State) -> Result
%    State    = state()
%    Result   = {ok, PduResp, NewState} | {error, {error_code, int()}}
%    PduResp  = pdu()
%    NewState = state()
%
% @doc Issues a bind_receiver request on the receiver session of the ESME.
% @end
%
% %@see
%
% %@equiv
%%
do_bind_receiver(#state{rx_session = RxSession} = State) ->
    ParamList = [{system_id,     State#state.system_id},
                 {password,      State#state.password},
                 {system_type,   State#state.system_type},
                 {addr_ton,      State#state.addr_ton},
                 {addr_npi,      State#state.addr_npi},
                 {address_range, State#state.address_range}],
    case gen_esme_session:bind_receiver(RxSession, ParamList) of
        {ok, PduResp} ->
            case State#state.tx_session of
                TxSession when TxSession == RxSession -> % Acting as Trx
                    {ok, PduResp, State#state{tx_session = undefined}};
                _TxSession ->                            % Different sessions
                    {ok, PduResp, State}
            end;
        {error, Error} ->
            {error, {error_code, Error}}
    end.


%%%
% @spec do_bind_transmitter(State) -> Result
%    State    = state()
%    Result   = {ok, PduResp, NewState} | {error, {error_code, int()}}
%    PduResp  = pdu()
%    NewState = state()
%
% @doc Issues a bind_transmitter request on transmitter session of the ESME.
% @end
%
% %@see
%
% %@equiv
%%
do_bind_transmitter(#state{tx_session = TxSession} = State) ->
    ParamList = [{system_id,     State#state.system_id},
                 {password,      State#state.password},
                 {system_type,   State#state.system_type},
                 {addr_ton,      State#state.addr_ton},
                 {addr_npi,      State#state.addr_npi},
                 {address_range, State#state.address_range}],
    case gen_esme_session:bind_transmitter(TxSession, ParamList) of
        {ok, PduResp} ->
            case State#state.rx_session of
                RxSession when RxSession == TxSession -> % Acting as Trx
                    {ok, PduResp, State#state{rx_session = undefined}};
                _RxSession ->                            % Different sessions
                    {ok, PduResp, State}
            end;
        {error, Error} ->
            {error, {error_code, Error}}
    end.


%%%
% @spec do_bind_transceiver(State) -> Result
%    State    = state()
%    Result   = {ok, PduResp, NewState} | {error, {error_code, int()}}
%    PduResp  = pdu()
%    NewState = state()
%
% @doc Issues a bind_transceiver request on the receiver session of the ESME.
%
% <p>If there is a transmitter session defined, and is not equal to the 
% receiver one, this function tries to close any previous transmitter 
% session in order to assign the same session on both sides (transceiver).  
% If the pre-existing transmitter session rejects to close, course it's bound,
% the closing error code is returned.  Previously, on the receiver session, 
% which was bound as transceiver, an unbound request is issued.</p>
% @end
%
% %@see
%
% %@equiv
%%
do_bind_transceiver(#state{rx_session = RxSession} = State) ->
    ParamList = [{system_id,     State#state.system_id},
                 {password,      State#state.password},
                 {system_type,   State#state.system_type},
                 {addr_ton,      State#state.addr_ton},
                 {addr_npi,      State#state.addr_npi},
                 {address_range, State#state.address_range}],
    case gen_esme_session:bind_transceiver(RxSession, ParamList) of
        {ok, PduResp} ->
            case State#state.tx_session of
                TxSession when TxSession == RxSession -> % As Trx already
                    {ok, PduResp, State};
                undefined ->                             % Acting as Rx only
                    {ok, PduResp, State#state{tx_session = RxSession}};
                TxSession ->                             % Different sessions
                    case gen_esme_session:do_close(TxSession) of
                        ok ->
                            gen_esme_session:stop(TxSession),
                            {ok, PduResp, State#state{tx_session = RxSession}};
                        {error, CloseError} ->
                            gen_esme_session:unbind(RxSession),
                            {error, {error_code, CloseError}}            
                    end
            end;
        {error, BindError} ->
            {error, {error_code, BindError}}
    end.


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
% @spec do_unbind(From, Session) -> {ok, PduResp} | {error, Error}
%    From    = {pid(), Tag}
%    Tag     = term()
%    Session = undefined | pid()
%    PduResp = pdu()
%    Error   = undefined_session | {error_code, int()}
%
% @doc Requests an unbind for a given session.
% @end
%
% %@see
%
% %@equiv
%%
do_unbind(From, undefined) ->
    gen_server:reply(From, {error, undefined_session});

do_unbind(From, Session) ->
    Reply = case gen_esme_session:unbind(Session) of
                {ok, PduResp} ->
                    {ok, PduResp};
                {error, Error} ->
                    {error, {error_code, Error}}
            end,
    gen_server:reply(From, Reply).


%%%
% @spec shutdown_session(Session) -> ok
%    Session = pid() | undefined
%
% @doc Stops an underlying session. 
% @end
%
% %@see
%
% %@equiv
%%
shutdown_session(undefined) ->
    ok;

shutdown_session(Session) ->
    gen_esme_session:do_close(Session),
    gen_esme_session:stop(Session).


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Callback wrappers
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% @spec callback_bound_receiver(SystemId, State) -> ok
%    SystemId = string()
%    State    = state()
%
% @doc Wrapper for CallbackModule:bound_receiver/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_bound_receiver(SystemId, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:bound_receiver(Pid, Eid, SystemId) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_bound_transmitter(SystemId, State) -> ok
%    SystemId = string()
%    State    = state()
%
% @doc Wrapper for CallbackModule:bound_transmitter/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_bound_transmitter(SystemId, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:bound_transmitter(Pid, Eid, SystemId) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_bound_transceiver(SystemId, State) -> ok
%    SystemId = string()
%    Password = string()
%    State    = state()
%
% @doc Wrapper for CallbackModule:bound_transceiver/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_bound_transceiver(SystemId, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:bound_transceiver(Pid, Eid, SystemId) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_receiver_unbind(State) -> ok | {error, Error}
%    State = state()
%    Error = int()
%
% @doc Wrapper for CallbackModule:receiver_unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_receiver_unbind(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:receiver_unbind(Pid, Eid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        {error, _Error} ->
            {error, ?ESME_RUNKNOWNERR};
        _Otherwise ->
            ok
    end.


%%%
% @spec callback_transmitter_unbind(State) -> ok | {error, Error}
%    State = state()
%    Error = int()
%
% @doc Wrapper for CallbackModule:transmitter_unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_transmitter_unbind(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transmitter_unbind(Pid, Eid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        {error, _Error} ->
            {error, ?ESME_RUNKNOWNERR};
        _Otherwise ->
            ok
    end.


%%%
% @spec callback_transceiver_unbind(State) -> ok | {error, Error}
%    State = state()
%    Error = int()
%
% @doc Wrapper for CallbackModule:transceiver_unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_transceiver_unbind(State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transceiver_unbind(Pid, Eid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        {error, _Error} ->
            {error, ?ESME_RUNKNOWNERR};
        _Otherwise ->
            ok
    end.

    
%%%
% @spec callback_receiver_mc_unavailable(Address, Port, State) -> ok
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:receiver_mc_unavailable/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_receiver_mc_unavailable(Address, Port, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:receiver_mc_unavailable(Pid, Eid, Address, Port) of
        _Whatever ->
            ok
    end.

    
%%%
% @spec callback_transmitter_mc_unavailable(Address, Port, State) -> ok
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:transmitter_mc_unavailable/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_transmitter_mc_unavailable(Address, Port, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transmitter_mc_unavailable(Pid, Eid, Address, Port) of
        _Whatever ->
            ok
    end.

    
%%%
% @spec callback_transceiver_mc_unavailable(Address, Port, State) -> ok
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    State   = state()
%
% @doc Wrapper for CallbackModule:transceiver_mc_unavailable/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_transceiver_mc_unavailable(Address, Port, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transceiver_mc_unavailable(Pid, Eid, Address, Port) of
        _Whatever ->
            ok
    end.
    

%%%
% @spec callback_receiver_can_not_resume(Addr, Port, Err, State) -> ok
%    Addr  = string() | atom() | ip_address()
%    Port  = int()
%    Err   = term()
%    State = state()
%
% @doc Wrapper for CallbackModule:receiver_can_not_resume/5.
% @end
%
% %@see
%
% %@equiv
%%
callback_receiver_can_not_resume(Addr, Port, Err, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:receiver_can_not_resume(Pid,Eid,Addr,Port,Err) of
        _Whatever ->
            ok
    end.
    

%%%
% @spec callback_transmitter_can_not_resume(Addr, Port, Err, State) -> ok
%    Addr  = string() | atom() | ip_address()
%    Port  = int()
%    Err   = term()
%    State = state()
%
% @doc Wrapper for CallbackModule:transmitter_can_not_resume/5.
% @end
%
% %@see
%
% %@equiv
%%
callback_transmitter_can_not_resume(Addr, Port, Err, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transmitter_can_not_resume(Pid,Eid,Addr,Port,Err) of
        _Whatever ->
            ok
    end.
   

%%%
% @spec callback_transceiver_can_not_resume(Addr, Port, Err, State) -> ok
%    Addr  = string() | atom() | ip_address()
%    Port  = int()
%    Err   = term()
%    State = state()
%
% @doc Wrapper for CallbackModule:transceiver_can_not_resume/5.
% @end
%
% %@see
%
% %@equiv
%%
callback_transceiver_can_not_resume(Addr, Port, Err, State) ->
    Mod = State#state.callback_module,
    Pid = State#state.parent,
    Eid = State#state.self,
    case catch Mod:transceiver_can_not_resume(Pid,Eid,Addr,Port,Err) of
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
