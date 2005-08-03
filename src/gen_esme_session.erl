%%% Copyright (C) 2004 - 2005 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc Generic ESME SMPP Session.
%%%
%%% <p>A generic ESME SMPP session modeled as a FSM.</p>
%%%
%%% <p>Every SMPP session works over a single TCP/IP connection.  If the 
%%% underlying connection exits, the session is also terminated.</p>
%%%
%%% <p>Session failures due to connection errors must be handled by the
%%% callback ESME.</p>
%%%
%%%
%%% <h2>State transitions table</h2>
%%%
%%% <p>Possible states for the ESME SMPP session are shown in the first row.
%%% Events are those in the first column.  This table shows the next state 
%%% given an event and the current state.</p>
%%%
%%% <p>Operations issued by the other peer (SMSC) are treated asynchronously
%%% by the ESME session, thus represented by async events.</p>
%%%
%%% <p>Empty cells mean that events are not possible for those states.</p>
%%%
%%% <table width="100%" border="1" cellpadding="5">
%%%   <tr> 
%%%     <th><small>&#160;</small></th>
%%%     <th><small>open</small></th>
%%%     <th><small>outbound</small></th>
%%%     <th><small>bound_rx</small></th>
%%%     <th><small>bound_tx</small></th>
%%%     <th><small>bound_trx</small></th>
%%%     <th><small>unbound</small></th>
%%%   </tr>
%%%   <tr> 
%%%     <th align="left"><small>alert_notification (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>deliver_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>die (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>enquire_link (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>input (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>outbind (async)</small></th>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>replace_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_multi</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_sm</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by MC (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>unbound/bound_rx</small></td>
%%%     <td valign="top" align="center"><small>unbound/bound_tx</small></td>
%%%     <td valign="top" align="center"><small>unbound/bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by ESME</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%     <td valign="top" align="center"><small>unbound</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Timers</h2>
%%%
%%% <p>Timers are implemented as shown in tables below.  There's is a table
%%% for each timer, indicating the action accomplished on timeout.</p>
%%%
%%% <h3>session_init_timer</h3>
%%%
%%% <table width="100%" border="1" cellpadding="5">
%%%   <tr> 
%%%     <th><small>open</small></th>
%%%     <th><small>outbound</small></th>
%%%     <th><small>bound_rx</small></th>
%%%     <th><small>bound_tx</small></th>
%%%     <th><small>bound_trx</small></th>
%%%     <th><small>unbound</small></th>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top" align="center"><small>close session</small></td>
%%%     <td valign="top" align="center"><small>close session</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h3>inactivity_timer</h3>
%%%
%%% <table width="100%" border="1" cellpadding="5">
%%%   <tr> 
%%%     <th><small>open</small></th>
%%%     <th><small>outbound</small></th>
%%%     <th><small>bound_rx</small></th>
%%%     <th><small>bound_tx</small></th>
%%%     <th><small>bound_trx</small></th>
%%%     <th><small>unbound</small></th>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>unbind</small></td>
%%%     <td valign="top" align="center"><small>unbind</small></td>
%%%     <td valign="top" align="center"><small>unbind</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h3>enquire_link_timer</h3>
%%%
%%% <table width="100%" border="1" cellpadding="5">
%%%   <tr> 
%%%     <th><small>open</small></th>
%%%     <th><small>outbound</small></th>
%%%     <th><small>bound_rx</small></th>
%%%     <th><small>bound_tx</small></th>
%%%     <th><small>bound_trx</small></th>
%%%     <th><small>unbound</small></th>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top" align="center"><small>enquire_link</small></td>
%%%     <td valign="top" align="center"><small>enquire_link</small></td>
%%%     <td valign="top" align="center"><small>enquire_link</small></td>
%%%     <td valign="top" align="center"><small>enquire_link</small></td>
%%%     <td valign="top" align="center"><small>enquire_link</small></td>
%%%     <td valign="top" align="center"><small>enquire_link</small></td>
%%%   </tr>
%%% </table>
%%%
%%% <h3>response_timer</h3>
%%%
%%% <p>One <quote>instance</quote> of this timer is started for each request.  
%%% On expiration, the request is considered unsuccessful.</p>  
%%%
%%% <p>The timer associated to a request is stopped when the corresponding 
%%% response arrives.</p>
%%%
%%%
%%% <h2>Callback Function Index</h2>
%%%
%%% <p>A module implementing this behaviour must export these functions.  
%%% Leaving a callback undefined crashes the entire session (when that
%%% particular function is called).</p>
%%%
%%% <table width="100%" border="1">
%%%   <tr>
%%%     <td valign="top"><a href="#handle_outbind-3">handle_outbind/3</a></td>
%%%     <td>Forwards <i>outbind</i> operations (from the peer SMSC) to the 
%%%       callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_alert_notification-3">
%%%       handle_alert_notification/3</a></td>
%%%     <td>Forwards <i>alert_notification</i> operations (from the peer SMSC) 
%%%       to the callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_enquire_link-3">
%%%       handle_enquire_link/3</a></td>
%%%     <td>Forwards <i>enquire_link</i> operations (from the peer SMSC) 
%%%       to the callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_enquire_link_failure-3">
%%%       handle_enquire_link_failure/3</a></td>
%%%     <td>Notifies <i>enquire_link</i> operation failures (not responded by
%%%       peer SMSC) to the callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_operation-3">handle_operation/3</a>
%%%     </td>
%%%     <td>Forwards <i>data_sm</i> and <i>deliver_sm</i> operations (from the
%%%       peer SMSC) to the callback ESME.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_unbind-3">handle_unbind/3</a></td>
%%%     <td>This callback forwards <i>unbind</i> requests (issued by a peer
%%%       SMSC) to the callback ESME.
%%%     </td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%% 
%%% <h3><a name="handle_outbind-3">handle_outbind/3</a></h3>
%%%
%%% <tt>handle_outbind(ESME, Session, Pdu) -&gt; ok</tt>
%%% <ul>
%%%   <li><tt>ESME = pid()</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>outbind</i> operations (from the peer SMSC) to the 
%%% callback ESME.</p>
%%%
%%% <p>Response is ignored by the session.</p>
%%%
%%% <p><tt>ESME</tt> is the ESME's process id, <tt>Session</tt> is the 
%%% session process id.</p>
%%%
%%%
%%% <h3><a name="handle_alert_notification-3">handle_alert_notification/3</a>
%%% </h3>
%%%
%%% <tt>handle_alert_notification(ESME, Session, Pdu) -&gt; ok</tt>
%%% <ul>
%%%   <li><tt>ESME = pid()</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>alert_notification</i> operations (from the peer SMSC) to 
%%% the callback ESME.</p>
%%%
%%% <p>Response is ignored by the session.</p>
%%%
%%% <p><tt>ESME</tt> is the ESME's process id, <tt>Session</tt> is the 
%%% session process id.</p>
%%%
%%%
%%% <h3><a name="handle_enquire_link-3">handle_enquire_link/3</a></h3>
%%%
%%% <tt>handle_enquire_link(ESME, Session, Pdu) -&gt; ok</tt>
%%% <ul>
%%%   <li><tt>ESME = pid()</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>enquire_link</i> operations (from the peer SMSC) to 
%%% the callback ESME.</p>
%%%
%%% <p>Response is ignored by the session.  This callback is issued to ensure
%%% that the callback module is not deadlocked.</p>
%%%
%%% <p><tt>ESME</tt> is the ESME's process id, <tt>Session</tt> is the 
%%% session process id.</p>
%%%
%%%
%%% <h3><a name="handle_enquire_link_failure-3">handle_enquire_link_failure/3
%%% </a></h3>
%%%
%%% <tt>handle_enquire_link_failure(ESME, Session, CommandStatus) -&gt; ok</tt>
%%% <ul>
%%%   <li><tt>ESME = pid()</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>CommandStatus = int()</tt></li>
%%% </ul>
%%%
%%% <p>Notifies <i>enquire_link</i> operation failures (not responded by
%%% peer SMSC) to the callback ESME.</p>
%%%
%%% <p>Response is ignored by the session.</p>
%%%
%%% <p><tt>ESME</tt> is the ESME's process id, <tt>Session</tt> is the 
%%% session process id.</p>
%%%
%%% 
%%% <h3><a name="handle_operation-3">handle_operation/3</a></h3>
%%%
%%% <tt>handle_operation(ESME, {CmdName, Pdu}) -&gt; Result</tt>
%%% <ul>
%%%   <li><tt>ESME = pid()</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>CmdName = data_sm | deliver_sm</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>Result = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>data_sm</i> and <i>deliver_sm</i> operations (from the
%%% peer SMSC) to the callback ESME.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the ESME in the response PDU, the callback should return the
%%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%%% desired command_status error code.</p>
%%%
%%% <p><tt>ESME</tt> is the ESME's process id, <tt>Session</tt> is the 
%%% session process id.</p>
%%%
%%% 
%%% <h3><a name="handle_unbind-3">handle_unbind/3</a></h3>
%%%
%%% <tt>handle_unbind(ESME, Session, Pdu) -&gt; ok | {error, Error}</tt>
%%% <ul>
%%%   <li><tt>ESME = pid()</tt></li>
%%%   <li><tt>Session = pid()</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>Error = int()</tt></li>
%%% </ul>
%%%
%%% <p>This callback forwards <i>unbind</i> requests (issued by a peer SMSC) to
%%% the callback ESME.</p>
%%%
%%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%%% command_status is sent to the MC and the session moves into the unbound
%%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%%% command_status and the session will remain on it's current bound state
%%% (bound_rx, bound_tx or bound_trx).</p>
%%%
%%% <p><tt>ESME</tt> is the ESME's process id, <tt>Session</tt> is the 
%%% process id.</p>
%%%
%%%
%%% <h2>Changes 1.1 -&gt; 1.2</h2>
%%%
%%% [7 Abr 2005]
%%%
%%% <ul>
%%%   <li>New callback <a href="#handle_enquire_link_failure-3">
%%%     handle_enquire_link_failure/3</a> added.
%%%     <br/>
%%%     <a href="http://sourceforge.net/forum/forum.php?thread_id=1206343&amp;forum_id=350015">More</a>
%%%   </li>
%%%   <li>Use <tt>proc_lib:spawn_link/1</tt> instead of <tt>spawn_link</tt>.
%%%   </li>
%%%   <li>New function <a href="#reference_number-1">reference_number/1</a>
%%%     implemented.
%%%   </li>
%%% </ul>
%%%
%%% [6 May 2005]
%%%
%%% <ul>
%%%   <li>PDUs are logged using the new <a href="smpp_log.hmtl">smpp_log</a>
%%%     event manager.
%%%   </li>
%%%   <li>Errors reported using the <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/error_logger.html">error_logger</a>.
%%%   </li>
%%% </ul>
%%%
%%% [28 Jun 2005]
%%%
%%% <ul>
%%%   <li>New callback <a href="#handle_enquire_link-3">
%%%     handle_enquire_link/3</a> added.
%%%     <br/>
%%%     <a href="http://sourceforge.net/forum/forum.php?thread_id=1206343&amp;forum_id=350015">More</a>
%%%   </li>
%%%   <li>Use built in <i>gen_fsm</i> timers.</li>
%%% </ul>
%%%
%%% [5 Jul 2005]
%%%
%%% <ul>
%%%   <li>Timers (finally :-) redefined.</li>
%%%   <li>Replace request brokers with timers.  Now instead of spawning a
%%%     process for each request, a timer is set.</li>
%%% </ul>
%%%
%%% [6 Jul 2005]
%%%
%%% <ul>
%%%   <li>Congestion control redefined. (Miguel Rodriguez Rubinos).</li>
%%% </ul>
%%%
%%% [11 Jul 2005]
%%%
%%% <ul>
%%%   <li><i>enquire_link</i> behavior redefined.  No false <i>enquire_link</i>
%%%     failures permitted.
%%%     <br/>
%%%     Please see discussions on this subject at the <a href="http://sourceforge.net/forum/forum.php?thread_id=1206343&amp;forum_id=350015">OSERL forum</a> 
%%%     and the <a href="http://smsforum.net/smf/index.php?topic=1980.0">SMPP 
%%%     forum.</a>
%%%   </li>
%%%   <li>Use <a href="smpp_error.html#format-1">smpp_error:format/1</a> in
%%%     error reports.
%%%   </li>
%%%   <li>Show PDUs in hex format in error reports.</li>
%%% </ul>
%%%
%%% [30 Jul 2005 Anders Nygren]
%%%
%%% <ul>
%%%   <li>Wrap #state.sequence_number to 1 when it reaches 16#7FFFFFFF.</li>
%%%   <li>Do not reset timers when a PDU is dropped because the peer is 
%%%       congested.
%%%   </li>
%%% </ul>
%%%
%%%
%%% @copyright 2004 - 2005 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.2, {07 June 2004} {@time}.
%%% @end
-module(gen_esme_session).

-behaviour(gen_fsm).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% Behaviour exports
%%%-------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start/3, 
         start/4, 
         start_link/3, 
         start_link/4, 
         bind_receiver/2,
         bind_transmitter/2,
         bind_transceiver/2,
         broadcast_sm/2,
         cancel_broadcast_sm/2,
         cancel_sm/2,
         data_sm/2,
         query_broadcast_sm/2,
         query_sm/2,
         reference_number/1,
         replace_sm/2,
         submit_multi/2,
         submit_sm/2,
         unbind/1,
         stop/1]).

%%%-------------------------------------------------------------------
%%% Internal gen_fsm exports
%%%-------------------------------------------------------------------
-export([init/1,
         open/2,
         outbound/2,
         bound_rx/2,
         bound_tx/2,
         bound_trx/2,
         unbound/2,
         open/3,
         outbound/3,
         bound_rx/3,
         bound_tx/3,
         bound_trx/3,
         unbound/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

-define(MAX_SEQUENCE_NUMBER,16#7FFFFFFF).  % PDU sequence number

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, 
%%         ESME,
%%         Mod,
%%         SequenceNumber,
%%         Socket,
%%         Requests,
%%         SelfCongestionState,
%%         PeerCongestionState,
%%         SessionInitTime,
%%         SessionInitTimer,
%%         EnquireLinkTime,
%%         EnquireLinkTimer,
%%         InactivityTime,
%%         InactivityTimer,
%%         ResponseTime}
%%    ESME                = pid()
%%    Mod                 = atom()
%%    SequenceNumber      = int()
%%    Socket              = socket()
%%    Requests            = ets:table()
%%    SelfCongestionState = int()
%%    PeerCongestionState = int()
%%    SessionInitTime     = int()
%%    SessionInitTimer    = pid()
%%    EnquireLinkTime     = int()
%%    EnquireLinkTimer    = pid()
%%    InactivityTime      = int()
%%    InactivityTimer     = pid()
%%    ResponseTime        = int()
%%
%% %@doc Representation of the fsm's state
%%
%% <dl>
%%   <dt>ESME: </dt><dd>Pid of the ESME parent process.  Is passed in the
%%     callback functions to help identify the owner of the session.
%%   </dd>
%%   <dt>Mod: </dt><dd>Callback Module.</dd>
%%   <dt>SequenceNumber: </dt><dd>PDU sequence number.</dd>
%%   <dt>ReferenceNumber: </dt><dd>Concatenation reference number.</dd>
%%   <dt>Socket: </dt><dd>The <tt>socket()</tt> of the underlying connection
%%   </dd>
%%   <dt>Requests: </dt><dd>An ets table with the requests which responses are
%%     pending.
%%   </dd>
%%   <dt>SelfCongestionState: </dt><dd>ESME congestion state (default is 0.0).
%%     This value is internally stored as a float due to the congestion control
%%     algorithm used.  Default value is 0.0.
%%   </dd>
%%   <dt>PeerCongestionState: </dt><dd>MC congestion state.  Might the atom
%%     <tt>undefined</tt> if the peer ESME doesn't support the 
%%     <i>congestion_state</i> parameter.  Stored as returned by the peer MC.
%%   </dd>
%%   <dt>SessionInitTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>SessionInitTimer: </dt><dd>PID of the process running the session
%%     init timeout.
%%
%%     <p>This timer is started when a new connection is accepted from the
%%     Mc and aborted when the session moves away from the open state (or if
%%     the connection is reopen).  
%%
%%     <p>On expiration the event <tt>{timeout, Ref, session_init_timer}
%%     </tt> is asynchronously signaled to the current state.
%%   </dd>
%%   <dt>EnquireLinkTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>EnquireLinkTimer: </dt><dd>PID of the process running the enquire
%%     link timeout.
%%
%%     <p>This timer is started when the session gets to a bound state; 
%%     bound_rx, bound_tx or bound_trx, and restarted every time a *valid*
%%     request (response) is sent (received).  The timer is aborted if the
%%     session gets unbound.  Erroneous requests/responses do *not* reset this
%%     timer.
%%
%%     <p>On expiration the event <tt>{timeout, Ref, enquire_link_timer}<tt>
%%     is asynchronously signaled to the current state.
%%   </dd>
%%   <dt>InactivityTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>InactivityTimer: </dt><dd>PID of the process running the inactivity
%%     timeout.
%%
%%     <p>This timer is started when the session gets to a bound state; 
%%     bound_rx, bound_tx or bound_trx, and restarted every time a *valid*
%%     request (response) is sent (received).  The timer is aborted if the
%%     session gets unbound.  Erroneous requests/responses do *not* reset this
%%     timer.
%%
%%     <p>On expiration the event <tt>{timeout, Ref, inactivity_timer}<tt>
%%     is asynchronously signaled to the current state.
%%   </dd>
%%   <dt>ResponseTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>Enquire: </dt><dd>A flag set to <tt>true</tt> if an <i>enquire_link
%%     </i> request was sent, <tt>false</tt> otherwise.  Default value is
%%     <tt>false</tt>.
%%   </dd>
%% </dl>
%% %@end
-record(state, 
        {esme,
         mod,
         sequence_number = 0,
         reference_number = 0,
         socket,
         requests,
         self_congestion_state = 0.0,
         peer_congestion_state,
         session_init_time,
         session_init_timer,
         enquire_link_time,
         enquire_link_timer,
         inactivity_time,
         inactivity_timer,
         response_time,
         enquire = false}).


%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec behaviour_info(Category) -> Info
%%    Category      = callbacks | term()
%%    Info          = CallbacksInfo | term()
%%    CallbacksInfo = [{FunctionName, Arity}]
%%    FunctionName  = atom()
%%    Arity         = int()
%%
%% @doc Gives information about the behaviour.
%% @end
behaviour_info(callbacks) ->
    [{handle_outbind, 3}, 
     {handle_alert_notification, 3}, 
     {handle_enquire_link, 3},
     {handle_enquire_link_failure, 3},
     {handle_operation, 3}, 
     {handle_unbind, 3}];
behaviour_info(_Other) ->
    undefined.


%% @spec start(Mod, Socket, Timers) -> Result
%%    Mod    = atom()
%%    Socket = socket()
%%    Timers = timers()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server setting <tt>self()</tt> as the session SMSC (owner).
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record.  Use the macro 
%% ?DEFAULT_TIMERS to set the default values.</p>
%%
%% <p>Refer to <b>oserl.hrl</b> for more details on the <tt>timers</tt> record 
%% definition.</p>
%%
%% <p>The gen_esme_session is not registered.</p>
%%
%% @see gen_fsm:start/3
%% @see start_link/3
%% @end
start(Mod, Socket, Timers) ->
    gen_fsm:start(?MODULE, [self(), Mod, Socket, Timers], []).


%% @spec start(Name, Mod, Socket, Timers) -> Result
%%    Name   = {local, Name} | {global, Name}
%%    Name   = atom()
%%    Mod    = atom()
%%    Socket = pid()
%%    Timers = timers()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server setting <tt>self()</tt> as the session SMSC (owner).
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record.  Use the macro 
%% ?DEFAULT_TIMERS to set the default values.</p>
%%
%% <p>Refer to <b>oserl.hrl</b> for more details on the <tt>timers</tt> record 
%% definition.</p>
%%
%% <p>If <tt>Name = {local, TheName}</tt>, the gen_esme_session is registered
%% locally as <tt>TheName</tt>.  If <tt>Name = {global, TheName}</tt>, the 
%% gen_esme_session is registered globally as <tt>TheName</tt>.</p>
%%
%% @see gen_fsm:start/4
%% @see start_link/4
%% @end
start(Name, Mod, Socket, Timers) ->
    gen_fsm:start(Name, ?MODULE, [self(), Mod, Socket, Timers],[]).


%% @spec start_link(Mod, Socket, Timers) -> Result
%%    Mod    = atom()
%%    Socket   = socket()
%%    Timers = timers()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server setting <tt>self()</tt> as the session ESME (owner).
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record.  Use the macro 
%% ?DEFAULT_TIMERS to set the default values.</p>
%%
%% <p>Refer to <b>oserl.hrl</b> for more details on the <tt>timers</tt> record 
%% definition.</p>
%%
%% <p>The gen_esme_session is not registered.</p>
%%
%% @see gen_fsm:start_link/3
%% @see start_link/3
%% @end
start_link(Mod, Socket, Timers) ->
    gen_fsm:start_link(?MODULE, [self(), Mod, Socket, Timers], []).


%% @spec start_link(Name, Mod, Socket, Timers) -> Result
%%    Name   = {local, Atom} | {global, Atom}
%%    Atom   = atom()
%%    Mod    = atom()
%%    Socket = socket()
%%    Timers = timers()
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the server setting <tt>self()</tt> as the session ESME (owner).
%%
%% <p><tt>Timers</tt> is a <tt>timers</tt> record.  Use the macro 
%% ?DEFAULT_TIMERS to set the default values.</p>
%%
%% <p>Refer to <b>oserl.hrl</b> for more details on the <tt>timers</tt> record 
%% definition.</p>
%%
%% <p>If <tt>Name = {local, TheName}</tt>, the gen_esme_session is registered
%% locally as <tt>Atom</tt>.  If <tt>Name = {global, TheName}</tt>, the 
%% gen_esme_session is registered globally as <tt>Atom</tt>.</p>
%%
%% @see gen_fsm:start_link/4
%% @see start_link/2
%% @end
start_link(Name, Mod, Socket, Timers) ->
    gen_fsm:start_link(Name, ?MODULE, [self(), Mod, Socket, Timers],[]).


%% @spec bind_receiver(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>bind_receiver</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
bind_receiver(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_BIND_RECEIVER,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec bind_transmitter(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>bind_transmitter</i> operation on the session identified by
%% <tt>FsmRef</tt>.
%% @end
bind_transmitter(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_BIND_TRANSMITTER,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec bind_transceiver(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>bind_transceiver</i> operation on the session identified by
%% <tt>FsmRef</tt>.
%% @end
bind_transceiver(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_BIND_TRANSCEIVER,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec broadcast_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>broadcast_sm</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
broadcast_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_BROADCAST_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec cancel_broadcast_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>cancel_broadcast_sm</i> operation on the session identified
%% by <tt>FsmRef</tt>.
%% @end
cancel_broadcast_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_CANCEL_BROADCAST_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec cancel_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>cancel_sm</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
cancel_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_CANCEL_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec data_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>data_sm</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
data_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_DATA_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec enquire_link(FsmRef) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    Result = ok | {error, Error}
%%    PduResp = pdu()
%%    Error = int()
%%
%% @doc Issues an <i>enquire_link</i> operation on the session identified by
%% <tt>FsmRef</tt>.
%%
%% <p>This function is not exported.</p>
%% @end
enquire_link(FsmRef) ->
    CmdId = ?COMMAND_ID_ENQUIRE_LINK,
    gen_fsm:sync_send_all_state_event(FsmRef, CmdId, infinity).


%% @spec query_broadcast_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>query_broadcast_sm</i> operation on the session identified 
%% by <tt>FsmRef</tt>.
%% @end
query_broadcast_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_QUERY_BROADCAST_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec query_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>query_sm</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
query_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_QUERY_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).

%% @spec reference_number(FsmRef) -> RefNum
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    RefNum = int()
%%
%% @doc Gets the value of the reference number counter.  This counter is
%% intended for messages concatenation.
%% @end
reference_number(FsmRef) ->
    gen_fsm:sync_send_all_state_event(FsmRef, reference_number, infinity).


%% @spec replace_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>replace_sm</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
replace_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_REPLACE_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec submit_multi(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>submit_multi</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
submit_multi(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_SUBMIT_MULTI,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec submit_sm(FsmRef, ParamList) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a <i>submit_sm</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
submit_sm(FsmRef, ParamList) ->
    CmdId = ?COMMAND_ID_SUBMIT_SM,
    gen_fsm:sync_send_event(FsmRef, {CmdId, ParamList}, infinity).


%% @spec unbind(FsmRef) -> Result
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%    Result  = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error   = int()
%%
%% @doc Issues an <i>unbind</i> operation on the session identified by 
%% <tt>FsmRef</tt>.
%% @end
unbind(FsmRef) ->
    gen_fsm:sync_send_event(FsmRef, ?COMMAND_ID_UNBIND, infinity).


%% @spec stop(FsmRef) -> ok
%%    FsmRef = Name | {Name, Node} | {global, Name} | pid()
%%
%% @doc Stops the fsm.  This function does *NOT* issue an unbind operation.
%% The unbind must have been previously sent using the unbind/1 function.
%%
%% @see gen_fsm:send_all_state_event/2
%%
%% @equiv gen_fsm:send_all_state_event(FsmRef, die)
%% @end
stop(FsmRef) ->
    gen_fsm:send_all_state_event(FsmRef, die).


%%%===================================================================
%%% Server gen_fsm functions
%%%===================================================================
%% @spec init(Args) -> Result
%%    Args       = term()
%%    Result     = {ok, StateName, StateData}          |
%%                 {ok, StateName, StateData, Timeout} |
%%                 ignore                              |
%%                 {stop, StopReason}                   
%%    StateName  = atom()
%%    StateData  = term()
%%    Timeout    = int()
%%    StopReason = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - init/1</a> callback implementation. Initializes the the fsm.
%% @end
init([Pid, Mod, Socket, T]) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> wait_recv(Self, Socket, <<>>) end),
    TE = start_timer(T#timers.enquire_link_time, enquire_link_timer),
    TS = start_timer(T#timers.session_init_time, session_init_timer),
    {ok, open, #state{esme               = Pid,
                      mod                = Mod,
                      socket             = Socket,
                      requests           = ets:new(esme_requests, []),
                      session_init_time  = T#timers.session_init_time,
                      session_init_timer = TS,
                      enquire_link_time  = T#timers.enquire_link_time,
                      enquire_link_timer = TE,
                      inactivity_time    = T#timers.inactivity_time,
                      response_time      = T#timers.response_time}}.


%% @spec open(Event, StateData) -> Result
%%    Event         = timeout | term()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/2</a> callback implementation.  Handles async events
%% for the state name open.
%%
%% <p>PDUs comming from the other peer (ESME) are received asynchronously.</p>
%% @end
open({?COMMAND_ID_OUTBIND, _Pdu} = R, S) ->
    cancel_timer(S#state.session_init_timer),
    cancel_timer(S#state.enquire_link_timer),
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_peer_outbind(R, Self, S) end),
    report:info(?MODULE, outbound, [{pid, Self}]),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    TS = start_timer(S#state.session_init_time, session_init_timer),
    {next_state, outbound, S#state{enquire_link_timer = TE,
                                   session_init_timer = TS}};
open(?COMMAND_ID_BIND_TRANSCEIVER_RESP, S) ->
    cancel_timer(S#state.session_init_timer),
    report:info(?MODULE, bound_trx, [{pid, self()}]),
    {next_state, bound_trx, S};
open(?COMMAND_ID_BIND_TRANSMITTER_RESP, S) ->
    cancel_timer(S#state.session_init_timer),
    report:info(?MODULE, bound_tx, [{pid, self()}]),
    {next_state, bound_tx, S};
open(?COMMAND_ID_BIND_RECEIVER_RESP, S) ->
    cancel_timer(S#state.session_init_timer),
    report:info(?MODULE, bound_rx, [{pid, self()}]),
    {next_state, bound_rx, S};
open({timeout, _Ref, Timer}, S) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_timeout(Timer, Self, S) end),
    {next_state, open, S};
open(R, S) ->    
    esme_rinvbndsts_resp(R, open, S#state.socket),
    {next_state, open, S}.


%% @spec outbound(Event, S) -> Result
%%    Event         = timeout | term()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/2</a> callback implementation.  Handles async events
%% for the state name outbound.
%%
%% <p>PDUs comming from the other peer (ESME) are received asynchronously.</p>
%% @end
outbound({timeout, _Ref, Timer}, S) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_timeout(Timer, Self, S) end),
    {next_state, outbound, S};
outbound(R, S) ->
    esme_rinvbndsts_resp(R, outbound, S#state.socket),
    {next_state, outbound, S}.
    

%% @spec bound_rx(Event, StateData) -> Result
%%    Event         = timeout | term()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/2</a> callback implementation.  Handles async events
%% for the state name bound_rx.  Bound against a receiver ESME.
%%
%% <p>PDUs comming from the other peer (ESME) are received asynchronously.</p>
%% @end
bound_rx({?COMMAND_ID_ALERT_NOTIFICATION, _Pdu} = R, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_peer_alert_notification(R,Self,S) end),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(S#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, S#state{enquire_link_timer=TE,inactivity_timer=TI}};
bound_rx({CmdId, _Pdu} = R, S) when CmdId == ?COMMAND_ID_DATA_SM;
                                    CmdId == ?COMMAND_ID_DELIVER_SM ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_peer_operation(R, Self, S) end),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(S#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, S#state{enquire_link_timer=TE,inactivity_timer=TI}};
bound_rx({?COMMAND_ID_UNBIND, _Pdu} = R, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    case handle_peer_unbind(R, self(), S) of  % Synchronous
        true ->
            report:info(?MODULE, unbound, [{pid, self()}]),
            {next_state, unbound, S#state{enquire_link_timer = TE}};
        false ->
            TI = start_timer(S#state.inactivity_time, inactivity_timer),
            {next_state, bound_rx, S#state{enquire_link_timer = TE,
                                           inactivity_timer = TI}}
    end;
bound_rx(?COMMAND_ID_UNBIND_RESP, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    report:info(?MODULE, unbound, [{pid, self()}]),
    T = start_timer(S#state.enquire_link_time, enquire_link_timer),
    {next_state, unbound, S#state{enquire_link_timer = T}};
bound_rx({timeout, _Ref, Timer}, S) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_timeout(Timer, Self, S) end),
    {next_state, bound_rx, S};
bound_rx(R, S) ->    
    esme_rinvbndsts_resp(R, bound_rx, S#state.socket),
    {next_state, bound_rx, S}.


%% @spec bound_tx(Event, StateData) -> Result
%%    Event         = timeout | term()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/2</a> callback implementation.  Handles async events
%% for the state name bound_tx.  Bound against a transmitter ESME.
%%
%% <p>PDUs comming from the other peer (ESME) are received asynchronously.</p>
%% @end
bound_tx({?COMMAND_ID_UNBIND, _Pdu} = R, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    case handle_peer_unbind(R, self(), S) of  % Synchronous
        true ->
            report:info(?MODULE, unbound, [{pid, self()}]),
            {next_state, unbound, S#state{enquire_link_timer = TE}};
        false ->
            TI = start_timer(S#state.inactivity_time, inactivity_timer),
            {next_state, bound_tx, S#state{enquire_link_timer = TE,
                                           inactivity_timer = TI}}
    end;
bound_tx(?COMMAND_ID_UNBIND_RESP, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    report:info(?MODULE, unbound, [{pid, self()}]),
    T = start_timer(S#state.enquire_link_time, enquire_link_timer),
    {next_state, unbound, S#state{enquire_link_timer = T}};
bound_tx({timeout, _Ref, Timer}, S) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_timeout(Timer, Self, S) end),
    {next_state, bound_tx, S};
bound_tx(R, S) ->    
    esme_rinvbndsts_resp(R, bound_tx, S#state.socket),
    {next_state, bound_tx, S}.


%% @spec bound_trx(Event, StateData) -> Result
%%    Event         = timeout | term()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/2</a> callback implementation.  Handles async events
%% for the state name bound_trx.  Bound against a transceiver ESME.
%%
%% <p>PDUs comming from the other peer (ESME) are received asynchronously.</p>
%% @end
bound_trx({?COMMAND_ID_ALERT_NOTIFICATION, _Pdu} = R, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_peer_alert_notification(R,Self,S) end),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(S#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, S#state{enquire_link_timer = TE,
                                    inactivity_timer = TI}};
bound_trx({CmdId, _Pdu} = R, S) when CmdId == ?COMMAND_ID_DATA_SM;
                                     CmdId == ?COMMAND_ID_DELIVER_SM ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_peer_operation(R, Self, S) end), %Async
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(S#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, S#state{enquire_link_timer = TE,
                                    inactivity_timer = TI}};
bound_trx({?COMMAND_ID_UNBIND, _Pdu} = R, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    TE = start_timer(S#state.enquire_link_time, enquire_link_timer),
    case handle_peer_unbind(R, self(), S) of  % Synchronous
        true ->
            report:info(?MODULE, unbound, [{pid, self()}]),
            {next_state, unbound, S#state{enquire_link_timer = TE}};
        false ->
            TI = start_timer(S#state.inactivity_time, inactivity_timer),
            {next_state, bound_trx, S#state{enquire_link_timer = TE,
                                           inactivity_timer = TI}}
    end;
bound_trx(?COMMAND_ID_UNBIND_RESP, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    report:info(?MODULE, unbound, [{pid, self()}]),
    T = start_timer(S#state.enquire_link_time, enquire_link_timer),
    {next_state, unbound, S#state{enquire_link_timer = T}};
bound_trx({timeout, _Ref, Timer}, S) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_timeout(Timer, Self, S) end),
    {next_state, bound_trx, S};
bound_trx(R, S) ->    
    esme_rinvbndsts_resp(R, bound_trx, S#state.socket),
    {next_state, bound_trx, S}.


%% @spec unbound(Event, StateData) -> Result
%%    Event         = timeout | term()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/2</a> callback implementation.  Handles async events
%% for the state name unbound.
%%
%% <p>PDUs comming from the other peer (ESME) are received asynchronously.</p>
%% @end
unbound({timeout, _Ref, Timer}, S) ->
    Self = self(),
    proc_lib:spawn_link(fun() -> handle_timeout(Timer, Self, S) end),
    {next_state, unbound, S};
unbound(R, S) ->    
    esme_rinvbndsts_resp(R, unbound, S#state.socket),
    {next_state, unbound, S}.

%% @doc Auxiliary function for Event/2 functions.
%%
%% <p>Sends the corresponding response with a <tt>?ESME_RINVBNDSTS</tt>
%% status.</p>
%%
%% @see open/2
%% @see outbound/2
%% @see bound_rx/2
%% @see bound_tx/2
%% @see bound_trx/2
%% @see unbound/2
%% @end 
esme_rinvbndsts_resp({CmdId, Pdu}, SName, Socket) ->
    Details = [{state,SName},{command_id,CmdId},{pdu,operation:to_list(Pdu)}],
    report:info(?MODULE, esme_rinvbndsts, Details),
    SeqNum = operation:get_param(sequence_number, Pdu),
    case ?VALID_COMMAND_ID(CmdId) of
        true ->
            RespId = ?RESPONSE(CmdId),
            send_response(RespId, ?ESME_RINVBNDSTS, SeqNum, [], Socket);
        false ->
            RespId = ?COMMAND_ID_GENERIC_NACK,
            send_response(RespId, ?ESME_RINVCMDID, SeqNum, [], Socket)
    end.


%% @spec open(Event, From, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/3</a> callback implementation.  Handles events for 
%% the state name open.
%% @end
open({CmdId, ParamList}, From, S) when CmdId == ?COMMAND_ID_BIND_RECEIVER;
                                       CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                       CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(CmdId, ParamList, From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, open, NewS#state{enquire_link_timer = TE,
                                  inactivity_timer = TI}};
open(_Event, _From, S) ->
    {reply, {error, ?ESME_RINVBNDSTS}, open, S}.


%% @spec outbound(Event, From, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/3</a> callback implementation.  Handles events for 
%% the state name outbound.
%% @end
outbound({CmdId,ParamList},From, S) when CmdId == ?COMMAND_ID_BIND_TRANSCEIVER;
                                         CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                         CmdId == ?COMMAND_ID_BIND_RECEIVER ->
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(CmdId, ParamList, From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, outbound, NewS#state{enquire_link_timer = TE,
                                      inactivity_timer = TI}};
outbound(_Event, _From, S) ->
    {reply, {error, ?ESME_RINVBNDSTS}, outbound, S}.


%% @spec bound_rx(Event, From, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/3</a> callback implementation.  Handles events for 
%% the state name bound_rx.  Bound against a receiver ESME.
%% @end
bound_rx({CmdId, _}, _From, S) when CmdId == ?COMMAND_ID_BIND_RECEIVER;
                                    CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                    CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    {reply, {error, ?ESME_RALYBND}, bound_rx, S};
bound_rx(?COMMAND_ID_UNBIND, From, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(?COMMAND_ID_UNBIND, [], From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, NewS#state{enquire_link_timer = TE,
                                      inactivity_timer = TI}};
bound_rx(_Event, _From, S) ->
    {reply, {error, ?ESME_RINVBNDSTS}, bound_rx, S}.


%% @spec bound_tx(Event, From, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/3</a> callback implementation.  Handles events for
%% the state name bound_tx.  Bound against a transmitter ESME.
%% @end
bound_tx({CmdId, _}, _From, S) when CmdId == ?COMMAND_ID_BIND_RECEIVER;
                                    CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                    CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    {reply, {error, ?ESME_RALYBND}, bound_tx, S};
bound_tx({CmdId, _ParamList}, _From, S) 
  when is_integer(S#state.peer_congestion_state) and 
       (S#state.peer_congestion_state > 90) and 
       ((CmdId == ?COMMAND_ID_DATA_SM) or
        (CmdId == ?COMMAND_ID_SUBMIT_SM) or
        (CmdId == ?COMMAND_ID_SUBMIT_MULTI) or
        (CmdId == ?COMMAND_ID_REPLACE_SM) or
        (CmdId == ?COMMAND_ID_BROADCAST_SM) or
        (CmdId == ?COMMAND_ID_QUERY_SM) or
        (CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM) or
        (CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM) or
        (CmdId == ?COMMAND_ID_CANCEL_SM)) ->
    % If the other peer is congested the request is not sent.  
    %
    % Notice that only current request is dropped, for the next one we put
    % the peer_congestion_state back to 90.
    Reply = {error, ?ESME_RTHROTTLED},
    {reply, Reply, bound_tx, S#state{peer_congestion_state = 90}};
bound_tx({CmdId, ParamList}, From, S) 
  when CmdId == ?COMMAND_ID_DATA_SM;
       CmdId == ?COMMAND_ID_SUBMIT_SM;
       CmdId == ?COMMAND_ID_SUBMIT_MULTI;
       CmdId == ?COMMAND_ID_REPLACE_SM;
       CmdId == ?COMMAND_ID_BROADCAST_SM;
       CmdId == ?COMMAND_ID_QUERY_SM;
       CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_SM ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(CmdId, ParamList, From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_tx, NewS#state{enquire_link_timer = TE,
                                      inactivity_timer = TI}};
bound_tx(?COMMAND_ID_UNBIND, From, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(?COMMAND_ID_UNBIND, [], From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_tx, NewS#state{enquire_link_timer = TE, 
                                      inactivity_timer = TI}};
bound_tx(_Event, _From, S) ->
    {reply, {error, ?ESME_RINVBNDSTS}, bound_tx, S}.


%% @spec bound_trx(Event, From, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/3</a> callback implementation.  Handles events for
%% the state name bound_trx.  Bound against a transceiver ESME.
%% @end
bound_trx({CmdId, _}, _From, S) when CmdId == ?COMMAND_ID_BIND_RECEIVER;
                                     CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                     CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    {reply, {error, ?ESME_RALYBND}, bound_trx, S};
bound_trx({CmdId, _ParamList}, _From, S) 
  when is_integer(S#state.peer_congestion_state) and 
       (S#state.peer_congestion_state > 90) and 
       ((CmdId == ?COMMAND_ID_DATA_SM) or
        (CmdId == ?COMMAND_ID_SUBMIT_SM) or
        (CmdId == ?COMMAND_ID_SUBMIT_MULTI) or
        (CmdId == ?COMMAND_ID_REPLACE_SM) or
        (CmdId == ?COMMAND_ID_BROADCAST_SM) or
        (CmdId == ?COMMAND_ID_QUERY_SM) or
        (CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM) or
        (CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM) or
        (CmdId == ?COMMAND_ID_CANCEL_SM)) ->
    % If the other peer is congested the request is not sent.  
    %
    % Notice that only current request is dropped, for the next one we put
    % the peer_congestion_state back to 90.
    Reply = {error, ?ESME_RTHROTTLED},
    {reply, Reply, bound_trx, S#state{peer_congestion_state = 90}};
bound_trx({CmdId, ParamList}, From, S) 
  when CmdId == ?COMMAND_ID_DATA_SM;
       CmdId == ?COMMAND_ID_SUBMIT_SM;
       CmdId == ?COMMAND_ID_SUBMIT_MULTI;
       CmdId == ?COMMAND_ID_REPLACE_SM;
       CmdId == ?COMMAND_ID_BROADCAST_SM;
       CmdId == ?COMMAND_ID_QUERY_SM;
       CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM;
       CmdId == ?COMMAND_ID_CANCEL_SM ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(CmdId, ParamList, From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, NewS#state{enquire_link_timer = TE,
                                       inactivity_timer = TI}};
bound_trx(?COMMAND_ID_UNBIND, From, S) ->
    cancel_timer(S#state.inactivity_timer),
    cancel_timer(S#state.enquire_link_timer),
    {ok, NewS} = send_request(?COMMAND_ID_UNBIND, [], From, S),
    TE = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    TI = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, NewS#state{enquire_link_timer = TE,
                                       inactivity_timer = TI}};
bound_trx(_Event, _From, S) ->
    {reply, {error, ?ESME_RINVBNDSTS}, bound_trx, S}.


%% @spec unbound(Event, From, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - StateName/3</a> callback implementation.  Handles events for
%% the state name unbound.
%% @end
unbound(_Event, _From, S) ->
    {reply, {error, ?ESME_RINVBNDSTS}, unbound, S}.


%% @spec handle_event(Event, StateName, StateData) -> Result
%%    Event         = die | term()
%%    StateName     = atom()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - handle_event/3</a> callback implementation.  Handles
%% events received by <tt>gen_fsm:send_all_state_event/2</tt>.
%% @end
handle_event({input, Pdu, Lapse, Timestamp}, SName, SData) ->
    case operation:get_param(command_id, Pdu) of
        CmdId when CmdId == ?COMMAND_ID_ENQUIRE_LINK_RESP ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            RqstId = ?COMMAND_ID_ENQUIRE_LINK,
            case ets:match(SData#state.requests,{SeqNum,RqstId,'$1','$2'},1) of
                {[[RTimer, From]], _Continuation} -> % Expected response
                    cancel_timer(RTimer),
                    ets:delete(SData#state.requests, SeqNum),
                    gen_fsm:reply(From, {ok, Pdu});
                _Otherwise -> % Unexpected response
                    % An enquire_link_resp received although not expected,
                    % probably because it was assumed to be acknowledged by
                    % other valid SMPP primitive, which was already on the way.
                    %
                    % The recommendation of the SMPP forum is to ignore these
                    % responses.
                    Details = [{sequence_number, SeqNum}],
                    report:info(?MODULE, unexpected_enquire_link_resp, Details)
            end,
            {next_state, SName, SData#state{enquire = false}};
        CmdId when CmdId > 16#80000000 ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            RqstId = ?REQUEST(CmdId),
            case ets:match(SData#state.requests,{SeqNum,RqstId,'$1','$2'},1) of
                {[[RTimer, From]], _Continuation} -> % Expected response
                    cancel_timer(RTimer),
                    ets:delete(SData#state.requests, SeqNum),
                    case operation:get_param(command_status, Pdu) of
                        ?ESME_ROK when
                              CmdId == ?COMMAND_ID_BIND_RECEIVER_RESP;
                              CmdId == ?COMMAND_ID_BIND_TRANSMITTER_RESP;
                              CmdId == ?COMMAND_ID_BIND_TRANSCEIVER_RESP;
                              CmdId == ?COMMAND_ID_UNBIND_RESP ->
                            % These responses require a send_event in order to 
                            % change the session FSM state (only when ESME_ROK)
                            gen_fsm:send_event(self(), CmdId),
                            gen_fsm:reply(From, {ok, Pdu});
                        ?ESME_ROK ->
                            gen_fsm:reply(From, {ok, Pdu});
                        Status ->
                            gen_fsm:reply(From, {error, Status})
                    end;
                _Otherwise -> % Unexpected response
                    Sock = SData#state.socket,
                    Nack = ?COMMAND_ID_GENERIC_NACK,
                    send_response(Nack, ?ESME_RINVCMDID, SeqNum, [], Sock)
            end,
            Pcs = operation:get_param(congestion_state, Pdu),
            {next_state, SName, SData#state{peer_congestion_state = Pcs}};
        CmdId when CmdId == ?COMMAND_ID_GENERIC_NACK ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            case ets:match(SData#state.requests, {SeqNum,'$1','$2','$3'}, 1) of
                {[[?COMMAND_ID_ENQUIRE_LINK, RTimer, From]], _Continuation} ->
                    % The other peer doesn't seem to support the enquire_link
                    % operation.  Even a generic_nack was issued, the link
                    % is working OK, thus the enquire_link succeeded
                    cancel_timer(RTimer),
                    ets:delete(SData#state.requests, SeqNum),
                    gen_fsm:reply(From, {ok, Pdu});
                {[[_RqstId, RTimer, From]], _Continuation} -> % Expected
                    cancel_timer(RTimer),
                    ets:delete(SData#state.requests, SeqNum),
                    Status = operation:get_param(command_status, Pdu),
                    gen_fsm:reply(From, {error, Status});
                _Otherwise -> % Unexpected response
                    % Do not send anything, might enter a request/response loop
                    true
            end,
            {next_state, SName, SData};
        CmdId when CmdId == ?COMMAND_ID_ENQUIRE_LINK ->
            cancel_timer(SData#state.enquire_link_timer),
            (SData#state.mod):handle_enquire_link(SData#state.esme,self(),Pdu),
            SeqNum = operation:get_param(sequence_number, Pdu),
            RespId = ?COMMAND_ID_ENQUIRE_LINK_RESP,
            send_response(RespId, ?ESME_ROK, SeqNum, [], SData#state.socket),
            T = start_timer(SData#state.enquire_link_time, enquire_link_timer),
            {next_state, SName, SData#state{enquire_link_timer = T}};
        CmdId when SData#state.enquire == true ->
            % On response to an enquire_link another valid SMPP primitive was
            % received.  The SMPP specification permits this, considering the
            % enquire_link as successful.
            %
            % Notice the enquire_link_resp may be received later on (see first
            % clause of this case).
            gen_fsm:send_event(self(), {CmdId, Pdu}),
            RqstId = ?COMMAND_ID_ENQUIRE_LINK,
            [{SeqNum, RqstId, RTimer, From}] = 
                ets:match_object(SData#state.requests, {'_', RqstId, '_','_'}),
            cancel_timer(RTimer),
            ets:delete(SData#state.requests, SeqNum),
            gen_fsm:reply(From, {ok, Pdu}),
            Details = [{sequence_number,SeqNum}, {pdu,binary:to_hexlist(Pdu)}],
            report:info(?MODULE, no_enquire_link_resp, Details),
            {next_state, SName, SData#state{self_congestion_state = 0.0, 
                                            enquire = false}};
        CmdId ->
            gen_fsm:send_event(self(), {CmdId, Pdu}),
            Ccs = SData#state.self_congestion_state, % Current
            Scs = congestion_state(Ccs, Lapse, Timestamp),
            {next_state, SName, SData#state{self_congestion_state = Scs}}
    end;
handle_event({error, CmdId, Status, SeqNum}, SName, SData) ->
    RespId = case ?VALID_COMMAND_ID(CmdId) of
                 true when CmdId /= ?COMMAND_ID_GENERIC_NACK ->
                     ?RESPONSE(CmdId);
                 _Otherwise ->
                     ?COMMAND_ID_GENERIC_NACK
             end,
    send_response(RespId, Status, SeqNum, [], SData#state.socket),
    {next_state, SName, SData};
handle_event({recv_error, _Error}, unbound, SData) ->
    {stop, normal, SData#state{socket = closed}};
handle_event({recv_error, _Error} = R, _SName, SData) ->
    {stop, R, SData#state{socket = closed}};
handle_event(die, _SName, SData) ->
    {stop, normal, SData}.

%% @doc Auxiliary function for handle_event/3
%%
%% <p>Computes self congestion state.</p>
%%
%% <dl>
%%   <dt>CongestionState: </dt><dd>Current <i>congestion_state</i> value.</dd>
%%   <dt>WaitTime: </dt><dd>Are the microseconds waiting for the PDU.</dd>
%%   <dt>Timestamp: </dt><dd>Represents the moment when the PDU was received.
%%   </dd>
%% </dl>
%%
%% <p>The time since <tt>Timestamp</tt> is the PDU dispatching time.  If
%% this value equals the <tt>WaitTime</tt> (i.e. DispatchTime/WaitTime = 1), 
%% then we shall assume optimum load (value 85).  Having this in mind the
%% instant congestion state value is calculated.  Notice this value cannot be
%% greater than 99.</p>
%% @end
congestion_state(CongestionState, WaitTime, Timestamp) ->
    case (my_calendar:time_since(Timestamp) / (WaitTime + 1)) * 85 of
        Value when Value < 1 ->
            0.0;
        Value when Value > 99 ->  % Out of bounds
            float(((19 * CongestionState) + 99) / 20);
        Value ->
            float(((19 * CongestionState) + Value) / 20)
    end.


%% @spec handle_sync_event(Event, From, StateName, StateData) -> Result
%%    Event         = term()
%%    From          = {pid(), Tag}
%%    StateName     = atom()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}            |
%%                    {next_state, NextStateName, NextStateData, Timeout}   |
%%                    {reply, Reply, NextStateName, NextStateData}          |
%%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                          |
%%                    {stop, Reason, Reply, NewStateData}                    
%%    Reply         = term()
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - handle_sync_event/4</a> callback implementation.  Handles
%% events received via <tt>gen_fsm:sync_send_all_state_event/2,3</tt>.
%% @end
handle_sync_event(?COMMAND_ID_ENQUIRE_LINK, From, SName, SData) ->
    cancel_timer(SData#state.enquire_link_timer),
    {ok, NewS} = send_request(?COMMAND_ID_ENQUIRE_LINK, [], From, SData),
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, SName, NewS#state{enquire_link_timer = T, enquire = true}};
handle_sync_event(reference_number, _From, SName, SData) ->
    RefNum = SData#state.reference_number,
    {reply, RefNum, SName, SData#state{reference_number = RefNum + 1}};
handle_sync_event(Event, _From, SName, SData) ->
    Details = [{event, Event}, {state, SName}],
    report:info(?MODULE, unexpected_sync_event, Details),
    {reply, ok, SName, SData}.


%% @spec handle_info(Info, StateName, StateData) -> Result
%%    Info          = term()
%%    StateName     = atom()
%%    StateData     = term()
%%    Result        = {next_state, NextStateName, NextStateData}          |
%%                    {next_state, NextStateName, NextStateData, Timeout} |
%%                    {stop, Reason, NewStateData}                         
%%    NextStateName = atom()
%%    NextStateData = term()
%%    Timeout       = int() | infinity
%%    Reason        = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - handle_info/3</a> callback implementation.  Call on reception 
%% of any other messages than a synchronous or asynchronous event.
%% @end
handle_info(Info, SName, SData) -> 
    report:info(?MODULE, unexpected_info, [{info, Info}, {state, SName}]),
    {next_state, SName, SData}.


%% @spec terminate(Reason, StateName, StateData) -> true
%%    Reason    = normal | shutdown | term()
%%    StateName = atom()
%%    StateData = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - terminate/3</a> callback implementation.  Shutdown the fsm.
%%
%% <p>Return value is ignored by the server.</p>
%% @end
terminate(R, N, S) when S#state.socket == closed; R == kill ->
    report:info(?MODULE, terminate, [{state, N}, {reason, R}]),
    case process_info(self(), registered_name) of
        {registered_name, Name} ->
            unregister(Name);
        _NotRegistered ->
            true
    end;
terminate(R, N, S) ->
    gen_tcp:close(S#state.socket),
    terminate(R, N, S#state{socket = closed}).


%% @spec code_change(OldVsn, StateName, StateData, Extra) -> Result
%%    OldVsn        = undefined | term()
%%    StateName     = term()
%%    StateData     = term()
%%    Extra         = term()
%%    Result        = {ok, NextStateName, NewStateData}
%%    NextStateName = atom()
%%    NewStateData  = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - code_change/4</a> callback implementation.  Convert process 
%% state when code is changed
%% @end
code_change(_OldVsn, SName, SData, _Extra) ->
    {ok, SName, SData}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
%% @spec handle_peer_outbind({CmdId, Pdu}, Self, State) -> true
%%    CmdId = int()
%%    Pdu   = pdu()
%%    Self  = pid()
%%    State = state()
%%
%% @doc Handles <i>outbind</i> requests from the peer SMSC.
%%
%% <p>This function issues the <a href="#handle_outbind-2">handle_outbind/2</a>
%% callback to the callback module.</p>
%%
%% <p>Returns <tt>true</tt> if the bind is accepted by the callback module,
%% <tt>false</tt> otherwise.</p>
%% @end 
handle_peer_outbind({?COMMAND_ID_OUTBIND, Pdu}, Self, S) ->
    (S#state.mod):handle_outbind(S#state.esme, Self, Pdu).


%% @spec handle_peer_alert_notification({CmdId, Pdu}, Self, State) -> true
%%    CmdId = int()
%%    Pdu   = pdu()
%%    Self  = pid()
%%    State = state()
%%
%% @doc Handles <i>alert_notification</i> requests from the peer SMSC.  
%%
%% <p>This function issues the <a href="#handle_alert_notification-3">
%% handle_alert_notification/3</a> callback to the callback module.</p>
%%
%% <p>Returns <tt>ok</tt>.</p>
%% @end 
handle_peer_alert_notification({?COMMAND_ID_ALERT_NOTIFICATION,Pdu}, Self, S)->
    (S#state.mod):handle_alert_notification(S#state.esme, Self, Pdu).


%% @spec handle_peer_operation({CmdId, Pdu}, Self, State) -> bool()
%%    CmdId = int()
%%    Pdu   = pdu()
%%    Self  = pid()
%%    State = state()
%%
%% @doc Handles SMPP operations from the peer SMSC.
%%
%% <p>This function issues the <a href="#handle_operation-3">handle_operation/3
%% </a> callback to the callback module.</p>
%%
%% <p>Returns <tt>true</tt> if the unbind is accepted by the callback module,
%% <tt>false</tt> otherwise.</p>
%% @end 
handle_peer_operation({CmdId, Pdu}, Self, S) ->
    CmdName = ?COMMAND_NAME(CmdId),
    SeqNum  = operation:get_param(sequence_number, Pdu),
    RespId  = ?RESPONSE(CmdId),
    PList2  = [{congestion_state, round(S#state.self_congestion_state)}],
    case (S#state.mod):handle_operation(S#state.esme, Self, {CmdName, Pdu}) of
        {ok, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_response(RespId, ?ESME_ROK, SeqNum, ParamList,S#state.socket),
            true;
        {error, Error, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_response(RespId, Error, SeqNum, ParamList, S#state.socket),
            false
    end.


%% @spec handle_peer_unbind({CmdId, Pdu}, Self, State) -> bool()
%%    CmdId = int()
%%    Pdu   = pdu()
%%    Self  = pid()
%%    State = state()
%%
%%
%% @doc Handles <i>unbind</i> requests from the peer SMSC.
%%
%% <p>This function issues the <a href="#handle_unbind-3">handle_unbind/3</a>
%% callback to the callback module.</p>
%%
%% <p>Returns <tt>true</tt> if the unbind is accepted by the callback module,
%% <tt>false</tt> otherwise.</p>
%% @end 
handle_peer_unbind({?COMMAND_ID_UNBIND, Pdu}, Self, S) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    RespId = ?COMMAND_ID_UNBIND_RESP,
    case (S#state.mod):handle_unbind(S#state.esme, Self, Pdu) of
        ok ->
            send_response(RespId, ?ESME_ROK, SeqNum, [], S#state.socket),
            true;
        {error, Error} ->
            send_response(RespId, Error, SeqNum, [],  S#state.socket),
            false
    end.


%% @spec handle_timeout(Timeout, FsmRef, State) -> bool()
%%    Timeout = response_timer | 
%%              enquire_link_timer | 
%%              session_init_timer |
%%              inactivity_timer
%%    FsmRef = pid()
%%    State = state()
%%
%% @doc Handles session timeouts.
%%
%% <p>This function issues the <a href="#handle_enquire_link_failure-3">
%% handle_enquire_link_failure/3</a> callback to the callback module if
%% the <i>enquire_link</i> operation is not responded by the peer SMPP
%% entity.</p>
%% @end 
handle_timeout({response_timer, From, CmdId}, _Self, _S) ->
    Error = operation:request_failure_code(CmdId),
    gen_fsm:reply(From, {error, Error});
handle_timeout(enquire_link_timer, Self, S) ->
    case enquire_link(Self) of
        {ok, _Pdu} ->
            ok;
        {error, Status} ->
            (S#state.mod):handle_enquire_link_failure(S#state.esme,Self,Status)
    end;
handle_timeout(session_init_timer, Self, _S) ->
    report:info(?MODULE, timeout, [{timer, session_init_timer}, {pid, Self}]),
    exit(Self, {timeout, session_init_timer});
handle_timeout(inactivity_timer, Self, _S) ->
    report:info(?MODULE, timeout, [{timer, inactivity_timer}, {pid, Self}]),
    unbind(Self),
    exit(Self, {timeout, inactivity_timer}).


%% @spec send_request(CmdId, ParamList, From, StateData) -> Result
%%    CmdId        = int()
%%    ParamList    = [{ParamName, ParamValue}]
%%    ParamName    = atom()
%%    ParamValue   = term()
%%    From         = {pid(), Tag}
%%    Tag          = term()
%%    StateData    = state()
%%    Result       = {ok, NewStateData} | {error, Error}
%%    NewStateData = state()
%%    Error        = int()
%%
%% @doc Send a SMPP request given the command PDU.  <tt>From</tt> represents
%% the caller issuing the request.
%%
%% <p>This function spawns a request broker that waits for the response.</p>
%%
%% <p>If PDU is not successfully sent, the functions exits on error.</p>
%%
%% @see send_request/3
%% @end
send_request(CmdId, ParamList, From, S) ->
    SeqNum = case S#state.sequence_number of
		 ?MAX_SEQUENCE_NUMBER ->
		     1;
		 OldSeqNum ->
		     OldSeqNum +1
	     end,
    send_pdu(S#state.socket, operation:new(CmdId, SeqNum, ParamList)),
    RTimer = start_timer(S#state.response_time, {response_timer, From, CmdId}),
    ets:insert(S#state.requests, {SeqNum, CmdId, RTimer, From}),
    {ok, S#state{sequence_number = SeqNum}}.


%% @spec send_response(CmdId, Status, SeqNum, ParamList, Socket) -> Result
%%    CmdId      = int()
%%    Status     = int()
%%    SeqNum     = int()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Socket     = socket()
%%    Result     = ok | {error, Error}
%%    Error      = int()
%%
%% @doc Send a SMPP response over a <tt>Socket</tt>.
%% <tt>Status</tt> is the command status and <tt>SeqNun</tt> the sequence
%% number of the PDU.
%% @end
send_response(CmdId, Status, SeqNum, ParamList, Socket) ->
    send_pdu(Socket, operation:new(CmdId, Status, SeqNum, ParamList)).


%% @spec send_pdu(Socket, Pdu) -> ok | {error, Reason}
%%    Socket = socket()
%%    Pdu  = pdu()
%%
%% @doc Send the PDU <tt>Pdu</tt> over a <tt>Socket</tt>.
%% @end
send_pdu(Socket, Pdu) ->
    case operation:esme_pack(Pdu) of
        {ok, BinaryPdu} ->
            case gen_tcp:send(Socket, BinaryPdu) of
                ok ->
                    smpp_log:notify_self_operation(BinaryPdu);
                SendError ->
                    Details = [{socket, Socket}, 
                               {pdu, binary:to_hexlist(BinaryPdu)}],
                    report:error(?MODULE, send_error, SendError, Details),
                    exit(SendError)
            end;
        {error, _CmdId, Status, _SeqNum} ->
            Reason = smpp_error:format(Status),
            report:error(?MODULE, pack_error, Reason, operation:to_list(Pdu)),
            exit({error, Status})
    end.


%% @spec wait_recv(FsmRef, Socket, Buffer) -> void()
%%    FsmRef = pid()
%%    Socket = socket()
%%    Buffer = binary()
%%
%% @doc Waits until new data is received on <tt>Socket</tt> and starts a 
%% receive loop to get bulk input.  All data received on the same loop triggers
%% the event <i>recv</i> with the same timestamp (with a 0 time lapse).
%%
%% <p>If the <tt>Socket</tt> is closed a failure is reported.</p>
%% @end
wait_recv(FsmRef, Socket, Buffer) ->
    Timestamp = now(),
    case gen_tcp:recv(Socket, 0) of
        {ok, Input} ->
            L = my_calendar:time_since(Timestamp),
            B = handle_input(FsmRef, concat_binary([Buffer, Input]), L, 1),
            recv_loop(FsmRef, Socket, B);
        Error ->
            gen_fsm:send_all_state_event(FsmRef, {recv_error, Error})
    end.

%% @doc Auxiliary function for wait_recv/2
%%
%% @see wait_recv/2
%% @end
recv_loop(FsmRef, Socket, Buffer) ->
    case gen_tcp:recv(Socket, 0, 0) of
        {ok, Input} ->                    % Some input waiting already 
            B = handle_input(FsmRef, concat_binary([Buffer, Input]), 0, 1),
            recv_loop(FsmRef, Socket, B);
        {error, timeout} ->               % No data inmediately available
            wait_recv(FsmRef, Socket, Buffer);
        Error ->
            gen_fsm:send_all_state_event(FsmRef, {recv_error, Error})
    end.

%% @doc Auxiliary function for wait_recv/3 and recv_loop/3.  Splits input
%% into PDUs and returns unused data.
%%
%% <p><tt>N</tt> counts the PDUs in Buffer.</p>
%% @end
handle_input(FsmRef, <<CommandLength:32, Rest/binary>> = Buffer, Lapse, N) ->
    Now = now(), % PDU received.  PDU handling starts now!
    Len = CommandLength - 4,
    case Rest of
        <<PduRest:Len/binary-unit:8, NextPdus/binary>> -> 
            BinaryPdu = <<CommandLength:32, PduRest/binary>>,
            case catch operation:esme_unpack(BinaryPdu) of
                {ok, Pdu} ->
                    smpp_log:notify_peer_operation(BinaryPdu),
                    T = Lapse/N,
                    gen_fsm:send_all_state_event(FsmRef, {input, Pdu, T, Now});
                {error, CmdId, Status, SeqNum} = Event ->
                    Details = [{pdu, binary:to_hexlist(BinaryPdu)},
                               {command_id, CmdId},
                               {command_status, Status},
                               {sequence_number, SeqNum}],
                    report:info(?MODULE, unpack_error, Details),
                    gen_fsm:send_all_state_event(FsmRef, Event);
                {'EXIT', What} ->
                    Details = [{pdu, binary:to_hexlist(BinaryPdu)}],
                    report:error(?MODULE, unpack_error, What, Details),
                    Event = {error, 0, ?ESME_RUNKNOWNERR, 0},
                    gen_fsm:send_all_state_event(FsmRef, Event)
            end,
            % The buffer may carry more than one SMPP PDU.
            handle_input(FsmRef, NextPdus, Lapse, N + 1);
        _IncompletePdu ->
            Buffer
    end;
handle_input(_FsmRef, Buffer, _Lapse, _N) ->
    Buffer.


%% @spec start_timer(Time, Msg) -> Timer
%%    Time = int() | infinity
%%    Msg = term()
%%    Timer = Ref | undefined
%%    Ref = reference()
%%
%% @doc Wraps <tt>gen_fsm:start_timer/2</tt> to support <tt>infinity</tt> time.
%%
%% @see gen_fsm:start_timer/2
%% @end 
start_timer(infinity, _Msg) ->
    undefined;
start_timer(Time, Msg) ->
    gen_fsm:start_timer(Time, Msg).


%% @spec cancel_timer(Timer) -> RemainingTime | false
%%    Timer = Ref | undefined
%%    Ref = reference()
%%    RemainingTime = int()
%%
%% @doc Wraps <tt>gen_fsm:cancel_timer/1</tt> to support <tt>undefined</tt> 
%% timers.
%%
%% @see gen_fsm:cancel_timer/1
%% @end 
cancel_timer(undefined) ->
    false;
cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).
