%%% Copyright (C) 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
%%%

%%% @doc Generic SMSC SMPP Session.
%%%
%%% <p>A generic SMSC SMPP session modeled as a FSM.  It also implements the
%%% <a href="gen_connection.html">gen_connection</a> behaviour.</p>
%%%
%%% <p>Every SMPP session works over a single TCP/IP connection.  If the 
%%% underlying connection exits, the session is also terminated.</p>
%%%
%%% <p>Connection errors must be handled by the SMSC.</p>
%%%
%%%
%%% <h2>State transitions table</h2>
%%%
%%% <p>To a better understanding of this behaviour, should notice that the
%%% state name on a SMSC session references that state of the ESME session on 
%%% the other peer.  Thus:</p>
%%%
%%% <dl>
%%%   <dt>bound_rx: </dt><dd>A SMSC session has this state whenever there's a
%%%     receiver ESME on the other peer.
%%%   </dd>
%%%   <dt>bound_tx: </dt><dd>If there is a transmitter ESME on the other peer.
%%%   </dd>
%%%   <dt>bound_trx: </dt><dd>Bound to a transceiver.
%%%   </dd>
%%% </dl>
%%%
%%% <p>Possible states for the SMSC SMPP session are shown in the first row.
%%% Events are those in the first column.  This table shows the next state 
%%% given an event and the current state.</p>
%%%
%%% <p>Operations issued by the other peer (ESME) are treated asynchronously
%%% by the SMSC session, thus represented by async events.</p>
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
%%%     <th align="left"><small>accept (async)</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
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
%%%     <th align="left"><small>deliver_data_sm (async)</small></th>
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
%%%     <th align="left"><small>open</small></th>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>open</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>outbind</small></th>
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
%%%   <tr>
%%%     <th align="left"><small>timeout (async)</small></th>
%%%     <td valign="top" align="center"><small>closed</small></td>
%%%     <td valign="top" align="center"><small>outbound</small></td>
%%%     <td valign="top" align="center"><small>bound_rx</small></td>
%%%     <td valign="top" align="center"><small>bound_tx</small></td>
%%%     <td valign="top" align="center"><small>bound_trx</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%%
%%%
%%% <h2>Timers</h2>
%%%
%%% <p>Timers are implemented as shown in tables below.  There's is a table
%%% for each timer, indicating which events on every state force timers to be 
%%% started, restarted or stopped.</p>
%%%
%%% <h3>session_init_timer</h3>
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
%%%     <th align="left"><small>accept (async)</small></th>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%   </tr>
%%%   <tr> 
%%%     <th align="left"><small>alert_notification (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>close</small></th>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>deliver_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>die (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>enquire_link (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>input (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>open</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>outbind</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>replace_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_multi</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by MC (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by ESME</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%% <p>Actions on timeout.</p>
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
%%%     <td valign="top" align="center"><small>close connection</small></td>
%%%     <td valign="top" align="center"><small>close connection</small></td>
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
%%%     <th><small>&#160;</small></th>
%%%     <th><small>open</small></th>
%%%     <th><small>outbound</small></th>
%%%     <th><small>bound_rx</small></th>
%%%     <th><small>bound_tx</small></th>
%%%     <th><small>bound_trx</small></th>
%%%     <th><small>unbound</small></th>
%%%     <th><small>closed</small></th>
%%%   </tr>
%%%   <tr> 
%%%     <th align="left"><small>accept (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr> 
%%%     <th align="left"><small>alert_notification (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>close</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>deliver_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>die (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>enquire_link (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>input (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>open</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>outbind</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>replace_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_multi</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by MC (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by ESME</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%% <p>Actions on timeout.</p>
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
%%%     <th><small>&#160;</small></th>
%%%     <th><small>open</small></th>
%%%     <th><small>outbound</small></th>
%%%     <th><small>bound_rx</small></th>
%%%     <th><small>bound_tx</small></th>
%%%     <th><small>bound_trx</small></th>
%%%     <th><small>unbound</small></th>
%%%   </tr>
%%%   <tr> 
%%%     <th align="left"><small>accept (async)</small></th>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%   </tr>
%%%   <tr> 
%%%     <th align="left"><small>alert_notification (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>cancel_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>close</small></th>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>cancel</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>data_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>deliver_sm (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>die (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>enquire_link (async)</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>input (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>open</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>start</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>outbind</small></th>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_broadcast_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>query_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>replace_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_multi</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>submit_sm</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by MC (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind issued by ESME</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%%   <tr>
%%%     <th align="left"><small>unbind_resp (async)</small></th>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>reset</small></td>
%%%     <td valign="top" align="center"><small>&#160;</small></td>
%%%   </tr>
%%% </table>
%%%
%%% <p>Actions on timeout.</p>
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
%%%     <td valign="top"><a href="#handle_bind-3">handle_bind/3</a></td>
%%%     <td>Forwards <i>bind_receiver</i>, <i>bind_transmitter</i> and 
%%%       <i>bind_transceiver</i> operations (from the peer ESME) to the 
%%%       callback SMSC.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_operation-3">handle_operation/3</a>
%%%     </td>
%%%     <td>Forwards <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%%%       <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%%%       <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%%%       <i>data_sm</i> operations (from the peer ESME) to the callback 
%%%       SMSC.
%%%     </td>
%%%   </tr>
%%%   <tr>
%%%     <td valign="top"><a href="#handle_unbind-3">handle_unbind/3</a></td>
%%%     <td>This callback forwards an unbind request (issued by a peer ESME) 
%%%       to the SMSC.
%%%     </td>
%%%   </tr>
%%% </table>
%%%
%%%
%%% <h2>Callback Function Details</h2>
%%% 
%%% <h3><a name="handle_bind-3">handle_bind/3</a></h3>
%%%
%%% <tt>handle_bind(SMSC, Session, Bind) -> Result</tt>
%%% <ul>
%%%   <li><tt>SMSC = Session = pid()</tt></li>
%%%   <li><tt>Bind = {bind_receiver, Pdu} |
%%%                  {bind_transmitter, Pdu} |
%%%                  {bind_transceiver, Pdu}</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>Result = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>bind_receiver</i>, <i>bind_transmitter</i> and 
%%% <i>bind_transceiver</i> operations (from the peer ESME) to the 
%%% callback SMSC.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the bind response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the ESME in the response PDU, the callback should return the
%%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%%% desired command_status error code.</p>
%%%
%%% <p><tt>SMSC</tt> is the SMSC's process id, <tt>Session</tt> is the 
%%% process id.</p>
%%%
%%% 
%%% <h3><a name="handle_operation-3">handle_operation/3</a></h3>
%%%
%%% <tt>handle_operation(SMSC, Session, Operation) -> Result</tt>
%%% <ul>
%%%   <li><tt>SMSC = Session = pid()</tt></li>
%%%   <li><tt>Operation = {broadcast_sm, Pdu} |
%%%                       {cancel_broadcast_sm, Pdu} |
%%%                       {cancel_sm, Pdu} |
%%%                       {query_broadcast_sm, Pdu} |
%%%                       {query_sm, Pdu} |
%%%                       {replace_sm, Pdu} |
%%%                       {submit_multi, Pdu} |
%%%                       {submit_sm, Pdu} |
%%%                       {data_sm, Pdu}</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>Result = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%%%   <li><tt>ParamList = [{ParamName, ParamValue}]</tt></li>
%%%   <li><tt>ParamName = atom()</tt></li>
%%%   <li><tt>ParamValue = term()</tt></li>
%%% </ul>
%%%
%%% <p>Forwards <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%%% <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%%% <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%%% <i>data_sm</i> operations (from the peer ESME) to the callback SMSC.</p>
%%%
%%% <p>The <tt>ParamList</tt> included in the response is used to construct
%%% the response PDU.  If a command_status other than ESME_ROK is to
%%% be returned by the ESME in the response PDU, the callback should return the
%%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%%% desired command_status error code.</p>
%%%
%%% <p><tt>SMSC</tt> is the SMSC's process id, <tt>Session</tt> is the 
%%% process id.</p>
%%%
%%% 
%%% <h3><a name="handle_unbind-3">handle_unbind/3</a></h3>
%%%
%%% <tt>handle_unbind(SMSC, Session, Unbind) -> ok | {error, Error}</tt>
%%% <ul>
%%%   <li><tt>SMSC = Session = pid()</tt></li>
%%%   <li><tt>Unbind = {unbind, Pdu}</tt></li>
%%%   <li><tt>Pdu = pdu()</tt></li>
%%%   <li><tt>Error = int()</tt></li>
%%% </ul>
%%%
%%% <p>This callback forwards an unbind request (issued by a peer ESME) to the 
%%% SMSC.</p>
%%%
%%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%%% command_status is sent to the MC and the session moves into the unbound
%%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%%% command_status and the session will remain on it's current bound state
%%% (bound_rx, bound_tx or bound_trx).</p>
%%%
%%% <p><tt>SMSC</tt> is the SMSC's process id, <tt>Session</tt> is the 
%%% process id.</p>
%%%
%%%
%%% <h2>Used modules</h2>
%%%
%%% <ul>
%%%   <li>gen_fsm</li>
%%%   <li>ets</li>
%%%   <li>gen_connection</li>
%%%   <li>my_calendar</li>
%%%   <li>operation</li>
%%% <ul>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 1.0 beta, {07 June 2004} {@time}.
%%% @end
%%%
%%% %@TODO New fsm implementation (on stdlib 1.12) includes now timers, use 
%%% this new feature and remove custom timers.  Built in timers do not include 
%%% reset feature :-( a little bit of extra work will be required (cancel + 
%%% start and updating state data.  Consider creating the functions 
%%% reset_active_timers and cancel_active_timers, both take StateData as an 
%%% argument and return {ok, NewStateData} | error.  Put canceled timers to
%%% undefined)
%%%
%%% %@TODO On submit_sm operations, split into several short messages those 
%%% with a content larger than 160 characters.
-module(gen_smsc_session).

% ARREGLAR EL ALERT NOTIFICAITON

-behaviour(gen_fsm).
-behaviour(gen_tcp_connection).

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
-export([start_link/3, 
         start_link/4, 
         alert_notification/2,
         outbind/2,
         deliver_sm/2,
         data_sm/2,
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
%%% Internal gen_connection exports
%%%-------------------------------------------------------------------
-export([handle_accept/3, handle_input/4]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).

% Bound state for a given command_id
% ***COMMENT OUT*** this line to regerate edocs. Don't know why ???
-define(BOUND(B), if 
                      B == ?COMMAND_ID_BIND_RECEIVER    -> bound_rx;
                      B == ?COMMAND_ID_BIND_TRANSMITTER -> bound_tx;
                      B == ?COMMAND_ID_BIND_TRANSCEIVER -> bound_trx
                  end).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, 
%%         SMSC,
%%         Mod,
%%         SequenceNumber,
%%         Conn,
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
%%    SMSC                = pid()
%%    Mod                 = atom()
%%    SequenceNumber      = int()
%%    Conn                = pid()
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
%%   <dt>SMSC: </dt><dd>Pid of the smsc process.  Is passed in the
%%     callback functions to help identify the owner of the session.
%%   </dd>
%%   <dt>Mod: </dt><dd>Callback Module.</dd>
%%   <dt>SequenceNumber: </dt><dd>PDU sequence number.</dd>
%%   <dt>Conn: </dt><dd>The <tt>pid()</tt> of the underlying 
%%     connection
%%   </dd>
%%   <dt>Requests: </dt><dd>An ets table with the requests which responses are
%%     pending.
%%   </dd>
%%   <dt>SelfCongestionState: </dt><dd>ESME congestion state (default is 0).
%%   </dd>
%%   <dt>PeerCongestionState: </dt><dd>MC congestion state.  Might be always
%%     0 if the peer ESME doesn't support the congestion_state parameter.
%%   </dd>
%%   <dt>SessionInitTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>SessionInitTimer: </dt><dd>PID of the process running the session
%%     init timeout (might be the atom <tt>undefined</tt>).
%%
%%     <p>This timer is started when a new connection is accepted from the
%%     Mc and aborted when the session moves away from the open state (or if
%%     the connection is reopen).  
%%
%%     <p>On expiration the event <tt>{timeout, Ref, session_init_timer}
%%     </tt> is asynchronously signaled to the current state.
%%
%%     <p>Refer to the function start_timer/2 below.
%%   </dd>
%%   <dt>EnquireLinkTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>EnquireLinkTimer: </dt><dd>PID of the process running the enquire
%%     link timeout (might be the atom <tt>undefined</tt>).
%%
%%     <p>This timer is started when the session gets to a bound state; 
%%     bound_rx, bound_tx or bound_trx, and restarted every time a *valid*
%%     request (response) is sent (received).  The timer is aborted if the
%%     session gets unbound.  Erroneous requests/responses do *not* reset this
%%     timer.
%%
%%     <p>On expiration the event <tt>{timeout, Ref, enquire_link_timer}<tt>
%%     is asynchronously signaled to the current state.
%%
%%     <p>Refer to the function start_timer/2 below.
%%   </dd>
%%   <dt>InactivityTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%%   <dt>InactivityTimer: </dt><dd>PID of the process running the inactivity
%%     timeout (might be the atom <tt>undefined</tt>).
%%
%%     <p>This timer is started when the session gets to a bound state; 
%%     bound_rx, bound_tx or bound_trx, and restarted every time a *valid*
%%     request (response) is sent (received).  The timer is aborted if the
%%     session gets unbound.  Erroneous requests/responses do *not* reset this
%%     timer.
%%
%%     <p>On expiration the event <tt>{timeout, Ref, inactivity_timer}<tt>
%%     is asynchronously signaled to the current state.
%%
%%     <p>Refer to the function start_timer/1 below.
%%   </dd>
%%   <dt>ResponseTime: </dt><dd>A value in milliseconds or the atom
%%     <tt>infinity</tt> (the later disables the timer).
%%   </dd>
%% </dl>
%% %@end
-record(state, 
        {smsc,
         mod,
         sequence_number = 0,
         conn,
         requests,
         self_congestion_state = 0,
         peer_congestion_state = 0,
         session_init_time,
         session_init_timer,
         enquire_link_time,
         enquire_link_timer,
         inactivity_time,
         inactivity_timer,
         response_time}).


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
    [{handle_bind, 3}, {handle_operation, 3}, {handle_unbind, 3}];
behaviour_info(_Other) ->
    undefined.


%% @spec start_link(Mod, Conn, Timers) -> Result
%%    Mod    = atom()
%%    Conn   = pid()
%%    Timers = #timers()
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
%% <p>The gen_smsc_session is not registered.</p>
%%
%% @see gen_fsm:start_link/3
%% @see start_link/3
%% @end
start_link(Mod, Conn, Timers) ->
    gen_fsm:start_link(?MODULE, [self(), Mod, Conn, Timers], []).


%% @spec start_link(Name, Mod, Conn, Timers) -> Result
%%    Name   = {local, Name} | {global, Name}
%%    Name   = atom()
%%    Mod    = atom()
%%    Conn   = pid()
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
%% <p>If <tt>Name = {local, TheName}</tt>, the gen_smsc_session is registered
%% locally as <tt>TheName</tt>.  If <tt>Name = {global, TheName}</tt>, the 
%% gen_smsc_session is registered globally as <tt>TheName</tt>.</p>
%%
%% @see gen_fsm:start_link/4
%% @see start_link/2
%% @end
start_link(Name, Mod, Conn, Timers) ->
    gen_fsm:start_link(Name, ?MODULE, [self(), Mod, Conn, Timers],[]).


%% @spec alert_notification(Sid, ParamList) -> Result
%%    Sid        = atom()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues an alert notification operation on the session identified by 
%% <tt>Sid</tt>.
%% @end
alert_notification(Sid, ParamList) ->
    CmdId = ?COMMAND_ID_ALERT_NOTIFICATION,
    gen_fsm:sync_send_event(Sid, {CmdId, ParamList}, infinity).


%% @spec outbind(Sid, ParamList) -> Result
%%    Sid        = atom()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues an outbind operation on the session identified by <tt>Sid</tt>.
%% @end
outbind(Sid, ParamList) ->
    CmdId = ?COMMAND_ID_OUTBIND,
    gen_fsm:sync_send_event(Sid, {CmdId, ParamList}, infinity).


%% @spec data_sm(Sid, ParamList) -> Result
%%    Sid        = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a data_sm operation on the session identified by <tt>Sid
%% </tt>.
%% @end
data_sm(Sid, ParamList) ->
    CmdId = ?COMMAND_ID_DATA_SM,
    gen_fsm:sync_send_event(Sid, {CmdId, ParamList}, infinity).


%% @spec deliver_sm(Sid, ParamList) -> Result
%%    Sid        = pid()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Result     = {ok, PduResp} | {error, Error}
%%    PduResp    = pdu()
%%    Error      = int()
%%
%% @doc Issues a deliver_sm operation on the session identified by <tt>Sid
%% </tt>.
%% @end
deliver_sm(Sid, ParamList) ->
    CmdId = ?COMMAND_ID_DELIVER_SM,
    gen_fsm:sync_send_event(Sid, {CmdId, ParamList}, infinity).


%% @spec unbind(Sid) -> Result
%%    Sid     = atom()
%%    Result  = {ok, PduResp} | {error, Error}
%%    PduResp = pdu()
%%    Error   = int()
%%
%% @doc Issues an unbind operation on the session identified by <tt>Sid
%% </tt>.
%% @end
unbind(Sid) ->
    gen_fsm:sync_send_event(Sid, ?COMMAND_ID_UNBIND, infinity).


%% @spec stop(Sid) -> ok
%%
%% @doc Stops the fsm.  This function does *NOT* issue an unbind operation.
%% The unbind must have been previously sent using the unbind/1 function.
%%
%% @see gen_fsm:send_all_state_event/2
%%
%% @equiv gen_fsm:send_all_state_event(Sid, die)
%% @end
stop(Sid) ->
    gen_fsm:send_all_state_event(Sid, die).


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
init([Pid, Mod, Conn, Timers]) ->
    process_flag(trap_exit, true),
    State = #state{smsc              = Pid,
                   mod               = Mod,
                   conn              = Conn,
                   requests          = ets:new(smsc_requests, []),
                   session_init_time = Timers#timers.session_init_time,
                   enquire_link_time = Timers#timers.enquire_link_time,
                   inactivity_time   = Timers#timers.inactivity_time,
                   response_time     = Timers#timers.response_time},
    {ok, open, State}.


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
open({CmdId, _Pdu} = R, S) when CmdId == ?COMMAND_ID_BIND_RECEIVER;
                                CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    reset_timer(S#state.enquire_link_timer),
    case handle_peer_bind(R, self(), S) of  % Synchronous
        true ->
            cancel_timer(S#state.session_init_timer),
            Timer = start_timer(S#state.inactivity_time, inactivity_timer),
            {next_state, ?BOUND(CmdId), S#state{inactivity_timer = Timer}};
        false ->
            {next_state, open, S}
    end;
open({timeout, _Ref, enquire_link_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_ENQUIRE_LINK, [], undefined, S) of
               {ok, NewData} ->
                   NewData;
               _Error ->
                   S
           end,
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, open, NewS#state{enquire_link_timer = T}};
open({timeout, _Ref, session_init_timer}, S) ->
    process_flag(trap_exit, false),
    gen_tcp_connection:stop(S#state.conn),
    {stop, {timeout, session_init_timer}, S};
open({timeout, _Ref, _Timer}, S) ->
    % Ignore false timeouts
    {next_state, open, S};
open(R, S) ->    
    esme_rinvbndsts_resp(R, S#state.conn),
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
outbound({CmdId, _Pdu} = R, S) when CmdId == ?COMMAND_ID_BIND_RECEIVER;
                                    CmdId == ?COMMAND_ID_BIND_TRANSMITTER;
                                    CmdId == ?COMMAND_ID_BIND_TRANSCEIVER ->
    reset_timer(S#state.enquire_link_timer),
    case handle_peer_bind(R, self(), S) of  % Synchronous
        true ->
            cancel_timer(S#state.session_init_timer),
            Timer = start_timer(S#state.inactivity_time, inactivity_timer),
            {next_state, ?BOUND(CmdId), S#state{inactivity_timer = Timer}};
        false ->
            {next_state, open, S}
    end;
outbound({timeout, _Ref, enquire_link_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_ENQUIRE_LINK, [], undefined, S) of
               {ok, NewData} ->
                   NewData;
               _Error ->
                   S
           end,
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, outbound, NewS#state{enquire_link_timer = T}};
outbound({timeout, _Ref, session_init_timer}, S) ->
    process_flag(trap_exit, false),
    gen_tcp_connection:stop(S#state.conn),
    {stop, {timeout, session_init_timer}, S};
outbound({timeout, _Ref, _Timer}, S) ->
    % Ignore false timeouts
    {next_state, outbound, S};
outbound(R, S) ->
    esme_rinvbndsts_resp(R, S#state.conn),
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
bound_rx({?COMMAND_ID_UNBIND, _Pdu} = R, S) ->
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    case handle_peer_unbind(R, self(), S) of  % Synchronous
        true ->
            cancel_timer(S#state.inactivity_timer),
            {next_state, unbound, S};
        false ->
            {next_state, bound_rx, S}
    end;
bound_rx(?COMMAND_ID_UNBIND_RESP, S) ->
    cancel_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    {next_state, unbound, S};
bound_rx({timeout, _Ref, enquire_link_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_ENQUIRE_LINK, [], undefined, S) of
               {ok, NewData} ->
                   NewData;
               _Error ->
                   S
           end,
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, bound_rx, NewS#state{enquire_link_timer = T}};
bound_rx({timeout, _Ref, inactivity_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_UNBIND, [], undefined, S) of
               {ok, NewData} ->
                   reset_timer(NewData#state.enquire_link_timer),
                   NewData;
               _Error ->
                   %%@TODO: trigger a new callback here? exit?
                   S
           end,
    T = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, NewS#state{inactivity_timer = T}};
bound_rx({timeout, _Ref, _Timer}, S) ->
    % Ignore false timeouts
    {next_state, bound_rx, S};
bound_rx(R, S) ->    
    esme_rinvbndsts_resp(R, S#state.conn),
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
bound_tx({CmdId, _Pdu} = R, S) when CmdId == ?COMMAND_ID_DATA_SM;
                                    CmdId == ?COMMAND_ID_SUBMIT_SM;
                                    CmdId == ?COMMAND_ID_SUBMIT_MULTI;
                                    CmdId == ?COMMAND_ID_REPLACE_SM;
                                    CmdId == ?COMMAND_ID_BROADCAST_SM;
                                    CmdId == ?COMMAND_ID_QUERY_SM;
                                    CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM;
                                    CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM;
                                    CmdId == ?COMMAND_ID_CANCEL_SM ->
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    Self = self(),
    spawn_link(fun() -> handle_peer_operation(R, Self, S) end),  % Asynchronous
    {next_state, bound_tx, S};
bound_tx({?COMMAND_ID_UNBIND, _Pdu} = R, S) ->
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    case handle_peer_unbind(R, self(), S) of  % Synchronous
        true ->
            cancel_timer(S#state.inactivity_timer),
            {next_state, unbound, S};
        false ->
            {next_state, bound_tx, S}
    end;
bound_tx(?COMMAND_ID_UNBIND_RESP, S) ->
    cancel_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    {next_state, unbound, S};
bound_tx({timeout, _Ref, enquire_link_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_ENQUIRE_LINK, [], undefined, S) of
               {ok, NewData} ->
                   NewData;
               _Error ->
                   S
           end,
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, bound_tx, NewS#state{enquire_link_timer = T}};
bound_tx({timeout, _Ref, inactivity_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_UNBIND, [], undefined, S) of
               {ok, NewData} ->
                   reset_timer(NewData#state.enquire_link_timer),
                   NewData;
               _Error ->
                   %%@TODO: trigger a new callback here? exit?
                   S
           end,
    T = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_tx, NewS#state{inactivity_timer = T}};
bound_tx({timeout, _Ref, _Timer}, S) ->
    % Ignore false timeouts
    {next_state, bound_tx, S};
bound_tx(R, S) ->    
    esme_rinvbndsts_resp(R, S#state.conn),
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
bound_trx({CmdId, _Pdu} = R, S) when CmdId == ?COMMAND_ID_DATA_SM;
                                     CmdId == ?COMMAND_ID_SUBMIT_SM;
                                     CmdId == ?COMMAND_ID_SUBMIT_MULTI;
                                     CmdId == ?COMMAND_ID_REPLACE_SM;
                                     CmdId == ?COMMAND_ID_BROADCAST_SM;
                                     CmdId == ?COMMAND_ID_QUERY_SM;
                                     CmdId == ?COMMAND_ID_QUERY_BROADCAST_SM;
                                     CmdId == ?COMMAND_ID_CANCEL_BROADCAST_SM;
                                     CmdId == ?COMMAND_ID_CANCEL_SM ->
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    Self = self(),
    spawn_link(fun() -> handle_peer_operation(R, Self, S) end),  % Asynchronous
    {next_state, bound_trx, S};
bound_trx({?COMMAND_ID_UNBIND, _Pdu} = R, S) ->
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    case handle_peer_unbind(R, self(), S) of  % Synchronous
        true ->
            cancel_timer(S#state.inactivity_timer),
            {next_state, unbound, S};
        false ->
            {next_state, bound_trx, S}
    end;
bound_trx(?COMMAND_ID_UNBIND_RESP, S) ->
    cancel_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    {next_state, unbound, S};
bound_trx({timeout, _Ref, enquire_link_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_ENQUIRE_LINK, [], undefined, S) of
               {ok, NewData} ->
                   NewData;
               _Error ->
                   S
           end,
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, bound_trx, NewS#state{enquire_link_timer = T}};
bound_trx({timeout, _Ref, inactivity_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_UNBIND, [], undefined, S) of
               {ok, NewData} ->
                   reset_timer(NewData#state.enquire_link_timer),
                   NewData;
               _Error ->
                   %%@TODO: trigger a new callback here? exit?
                   S
           end,
    T = start_timer(NewS#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, NewS#state{inactivity_timer = T}};
bound_trx({timeout, _Ref, _Timer}, S) ->
    % Ignore false timeouts
    {next_state, bound_trx, S};
bound_trx(R, S) ->    
    esme_rinvbndsts_resp(R, S#state.conn),
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
unbound({timeout, _Ref, enquire_link_timer}, S) ->
    NewS = case send_request(?COMMAND_ID_ENQUIRE_LINK, [], undefined, S) of
               {ok, NewData} ->
                   NewData;
               _Error ->
                      S
           end,
    T = start_timer(NewS#state.enquire_link_time, enquire_link_timer),
    {next_state, unbound, NewS#state{enquire_link_timer = T}};
unbound({timeout, _Ref, _Timer}, S) ->
    % Ignore false timeouts
    {next_state, unbound, S};
unbound(R, S) ->    
    esme_rinvbndsts_resp(R, S#state.conn),
    {next_state, unbound, S}.

%% @doc Auxiliary function for Event/2 functions.
%%
%% <p>Sends the corresponding response with a <tt>?ESME_RINVBNDSTS</tt>
%% status.</p>
%%
%% @see open/2, outbound/2, bound_rx/2, bound_tx/2, bound_trx/2 and unbound/2
%% @end 
esme_rinvbndsts_resp({CmdId, Pdu}, Conn) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    case ?VALID_COMMAND_ID(CmdId) of
        true ->
            RespId = ?RESPONSE(CmdId),
            send_response(RespId, ?ESME_RINVBNDSTS, SeqNum, [], Conn);
        false ->
            RespId = ?COMMAND_ID_GENERIC_NACK,
            send_response(RespId, ?ESME_RINVCMDID, SeqNum, [], Conn)
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
open({?COMMAND_ID_OUTBIND, ParamList}, _From, S) ->
    case send_request(?COMMAND_ID_OUTBIND, ParamList, undefined, S) of
        {ok, NewS} ->
            reset_timer(S#state.session_init_timer),
            reset_timer(S#state.enquire_link_timer),
            {reply, ok, outbound, NewS};
        Error ->
            {reply, Error, open, S}
    end;
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
bound_rx({CmdId, _}, _From, S) 
  when (S#state.peer_congestion_state > 90) and
       ((CmdId == ?COMMAND_ID_DATA_SM) or
        (CmdId == ?COMMAND_ID_DELIVER_SM) or
        (CmdId == ?COMMAND_ID_ALERT_NOTIFICATION)) ->
    % If the other peer is congested do not send the request.  
    %
    % Notice that only current request is dropped, for the next one, we put
    % the peer_congestion_state back to 90.
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    Reply = {error, ?ESME_RTHROTTLED},
    {reply, Reply, bound_rx, S#state{peer_congestion_state = 90}};
bound_rx({CmdId, ParamList}, From, S) 
  when CmdId == ?COMMAND_ID_DATA_SM;
       CmdId == ?COMMAND_ID_DELIVER_SM;
       CmdId == ?COMMAND_ID_ALERT_NOTIFICATION ->
    case send_request(CmdId, ParamList, From, S) of
        {ok, NewS} ->
            reset_timer(S#state.inactivity_timer),
            reset_timer(S#state.enquire_link_timer),
            {next_state, bound_rx, NewS};
        Error ->
            {reply, Error, bound_rx, S}
    end;
bound_rx(?COMMAND_ID_UNBIND, From, S) ->
    case send_request(?COMMAND_ID_UNBIND, [], From, S) of
        {ok, NewS} ->
            reset_timer(S#state.inactivity_timer),
            reset_timer(S#state.enquire_link_timer),
            {next_state, bound_rx, NewS};
        Error ->
            {reply, Error, bound_rx, S}
    end;
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
bound_tx(?COMMAND_ID_UNBIND, From, S) ->
    case send_request(?COMMAND_ID_UNBIND, [], From, S) of
        {ok, NewS} ->
            reset_timer(S#state.inactivity_timer),
            reset_timer(S#state.enquire_link_timer),
            {next_state, bound_tx, NewS};
        Error ->
            {reply, Error, bound_tx, S}
    end;
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
bound_trx({CmdId, _}, _From, S) 
  when (S#state.peer_congestion_state > 90) and 
       ((CmdId == ?COMMAND_ID_DATA_SM) or
        (CmdId == ?COMMAND_ID_DELIVER_SM) or
        (CmdId == ?COMMAND_ID_ALERT_NOTIFICATION)) ->
    % If the other peer is congested the request is not sent.  
    %
    % Notice that only current request is dropped, for the next one we put
    % the peer_congestion_state back to 90.
    reset_timer(S#state.inactivity_timer),
    reset_timer(S#state.enquire_link_timer),
    Reply = {error, ?ESME_RTHROTTLED},
    {reply, Reply, bound_trx, S#state{peer_congestion_state = 90}};
bound_trx({CmdId, ParamList}, From, S) 
  when CmdId == ?COMMAND_ID_DATA_SM;
       CmdId == ?COMMAND_ID_DELIVER_SM;
       CmdId == ?COMMAND_ID_ALERT_NOTIFICATION ->
    case send_request(CmdId, ParamList, From, S) of
        {ok, NewS} ->
            reset_timer(S#state.inactivity_timer),
            reset_timer(S#state.enquire_link_timer),
            {next_state, bound_trx, NewS};
        Error ->
            {reply, Error, bound_trx, S}
    end;
bound_trx(?COMMAND_ID_UNBIND, From, S) ->
    case send_request(?COMMAND_ID_UNBIND, [], From, S) of
        {ok, NewS} ->
            reset_timer(S#state.inactivity_timer),
            reset_timer(S#state.enquire_link_timer),
            {next_state, bound_trx, NewS};
        Error ->
            {reply, Error, bound_trx, S}
    end;
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
handle_event({input, BinaryPdu, Lapse, Index}, StateName, StateData) ->
    Timestamp = now(),
    case catch operation:smsc_unpack(BinaryPdu) of
        {ok, Pdu} ->
            handle_input_correct_pdu(Pdu, StateData),
            NewStateData = 
                case StateName of
                    bound_rx ->
                        % Bound against a receiver, check ESME congestion
                        Pcs  = operation:get_param(congestion_state, Pdu),
                        Time = 2 * my_calendar:time_since(Timestamp),
                        StateData#state{peer_congestion_state = Pcs};
                    bound_tx ->
                        % Bound against a transmitter, check our congestion
                        Time = 2 * my_calendar:time_since(Timestamp),
                        Scs  = congestion_state(Lapse, Index, Time),
                        StateData#state{self_congestion_state = Scs};
                    bound_trx ->
                        % Against transceivers care about both sides congestion
                        Pcs  = operation:get_param(congestion_state, Pdu),
                        Time = 2 * my_calendar:time_since(Timestamp),
                        Scs  = congestion_state(Lapse, Index, Time),
                        StateData#state{self_congestion_state = Scs,
                                        peer_congestion_state = Pcs};
                    _Other ->
                        % Not bound, congestion state and dispatch time
                        % are meaningfulness for us.
                        StateData
                end,
            {next_state, StateName, NewStateData};
        {error, CmdId, Status, SeqNum} ->
            handle_input_corrupt_pdu(CmdId, Status, SeqNum, StateData),
            {next_state, StateName, StateData};
        {'EXIT', _What} ->
%             io:format("What's going on? ~p~n", [What]),
            handle_input_corrupt_pdu(undefined,?ESME_RUNKNOWNERR,0,StateData),
            {next_state, StateName, StateData}
    end;
handle_event(die, StateName, StateData) ->
    gen_tcp_connection:stop(StateData#state.conn),
    {next_state, StateName, StateData}.


%% @doc Auxiliary function for handle_event/3
%%
%% <p>This function handles an input PDU when the unpacking operation was
%% successful.  Used only on the <tt>input</tt> event.</p>
%%
%% @see handle_input_corrupt_pdu/3
%% @end
handle_input_correct_pdu(Pdu, StateData) ->
%    io:format("Correct input. PDU = ~p~n", [Pdu]),
    case operation:get_param(command_id, Pdu) of
        CmdId when CmdId > 16#80000000 ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            RqstId = ?REQUEST(CmdId),
			case ets:match(StateData#state.requests, {SeqNum,RqstId,'$1'},1) of
				{[[Broker]], _Continuation} ->  % Expected response
                    Broker ! {self(), {response, CmdId, Pdu}},
                    ets:delete(StateData#state.requests, SeqNum);
                _Otherwise ->                   % Unexpected response
                    Conn = StateData#state.conn,
                    Nack = ?COMMAND_ID_GENERIC_NACK,
                    send_response(Nack, ?ESME_RINVCMDID, SeqNum, [], Conn)
            end;
        CmdId when CmdId == ?COMMAND_ID_GENERIC_NACK ->
            SeqNum = operation:get_param(sequence_number, Pdu),
			case ets:match(StateData#state.requests, {SeqNum, '_', '$1'}, 1) of
				{[[Broker]], _Continuation} ->  % Expected response
                    Broker ! {self(), {response, CmdId, Pdu}},
                    ets:delete(StateData#state.requests, SeqNum);
                _Otherwise ->                   % Unexpected response
                    % Do not send anything, might enter a request/response loop
                    true
            end;
        CmdId when CmdId == ?COMMAND_ID_ENQUIRE_LINK ->
            reset_timer(StateData#state.enquire_link_timer),
            SeqNum = operation:get_param(sequence_number, Pdu),
			RespId = ?RESPONSE(CmdId),
            send_response(RespId, ?ESME_ROK, SeqNum, [], StateData#state.conn);
        CmdId ->
            gen_fsm:send_event(self(), {CmdId, Pdu})
    end.

%% @doc Auxiliary function for handle_event/3
%%
%% <p>This function handles an input PDU when the unpacking operation was
%% unsuccessful.  Used only on the <tt>input</tt> event.</p>
%%
%% @see handle_input_correct_pdu/2
%% @end
%    io:format("Corrupt input.~nCmdId = ~p~nStatus = ~p~nSeqNum = ~p~n", [CmdId, Status, SeqNum]),
handle_input_corrupt_pdu(?COMMAND_ID_GENERIC_NACK, _Status, _SeqNum, _S) ->
    % Do not send anything, might enter a request/response loop
    true;
handle_input_corrupt_pdu(CmdId, Status, SeqNum, S) ->
    case ?VALID_COMMAND_ID(CmdId) of
        true ->
            RespId = ?RESPONSE(CmdId),
            send_response(RespId, Status, SeqNum, [], S#state.conn);
        false ->
            RespId = ?COMMAND_ID_GENERIC_NACK,
            send_response(RespId, Status, SeqNum, [], S#state.conn)
    end.

%% @doc Auxiliary function for handle_event/3
%%
%% <p>Computes the congestion state.  Used only on the <tt>input</tt> event.
%% <tt>Time</tt> indicates the microseconds to dispatch a PDU.  <tt>Lapse
%% </tt></p>
%%
%% <dl>
%%   <dt>Lapse: </dt><dd>Are the microseconds waiting for the input buffer.</dd>
%%   <dt>Time: </dt><dd>Microseconds to dispatch a PDU.</dd>
%%   <dt>Index: </dt><dd>An input buffer may contain more than 1 PDU.</dd>
%% </dl>
%% @end
congestion_state(Lapse, Index, Time) ->
    case catch ((99 * Time) / ((Lapse / Index) + Time)) of 
        Result when float(Result) -> 
            trunc(Result);
        _DivisionByZero -> 
            0
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
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.


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
handle_info({'EXIT', C, R}, unbound, #state{conn = C} = StateData) ->
    % If the underlying connection terminates, the session must be stopped.
%    io:format("Underlying connection terminated with reason: ~p~n", [R]),
    {stop, normal, StateData#state{conn = closed}};
handle_info({'EXIT', C, R}, _StateName, #state{conn = C} = StateData) ->
    % If the underlying connection terminates, the session must be stopped.
%    io:format("Underlying connection terminated with reason: ~p~n", [R]),
    {stop, R, StateData#state{conn = closed}};
handle_info({'EXIT', _Child, R}, StateName, StateData) when R /= normal ->
    % A child process (request broker) terminates abnormally.
%    io:format("Request broker terminated with reason: ~p~n", [R]),
    {stop, R, StateData};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%% @spec terminate(Reason, StateName, StateData) -> true
%%    Reason    = normal | shutdown | term()
%%    StateName = atom()
%%    StateData = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - terminate/3</a> callback implementation.  Shutdown the fsm.
%%
%% <p>Return value is ignored by the server.</p>
%% @end
terminate(Reason, StateName, S) when S#state.conn == closed; Reason == kill ->
	io:format("*** gen_smsc_session terminating: ~p - ~p ***~n", [self(), Reason]),
    process_flag(trap_exit, false),
    case process_info(self(), registered_name) of
        {registered_name, Name} ->
            unregister(Name);
        _NotRegistered ->
            true
    end;
terminate(Reason, StateName, StateData) ->
    WaitExit = fun(Pid, Why) ->
                       Wait = fun(P, W, F) ->
                                      receive
                                          {'EXIT', P, W} -> ok;
                                          _Otherwise     -> F(P, W, F)
                                      end
                              end,
                       exit(Pid, Why),
                       Wait(Pid, Why, Wait)
               end,
    WaitExit(StateData#state.conn, Reason),
    terminate(Reason, StateName, StateData#state{conn = closed}).


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
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%===================================================================
%%% Server gen_connection functions
%%%===================================================================
%% @spec handle_accept(Owner, Conn, Socket) -> {ok, NewOwner} | error
%%    Owner = NewOwner = Conn = pid()
%%    Socket = socket()
%%
%% @doc <a href="gen_connection.html#handle_accept-2">gen_connection 
%% - handle_accept/2</a> callback implementation.
%% @end
handle_accept(_Owner, _Conn, _Socket) -> error.
     

%% @spec handle_input(Owner, Conn, Input, Lapse) -> {ok, RestInput}
%%    Owner = Conn = pid()
%%    Input = RestInput = binary()
%%    Lapse = int()
%%
%% @doc <a href="gen_connection.html#handle_input-4">gen_connection
%% - handle_input/4</a> callback implementation.
%% @end
handle_input(Pid, Conn, Buffer, Lapse) ->
    handle_input(Pid, Conn, Buffer, Lapse, 1).
handle_input(Pid, Conn, <<CommandLength:32,Rest/binary>> = Buffer, Lapse, N) ->
    Len = CommandLength - 4,
    case Rest of
        <<PduRest:Len/binary-unit:8, NextPdus/binary>> -> 
            BinaryPdu = <<CommandLength:32, PduRest/binary>>,
            gen_fsm:send_all_state_event(Pid, {input, BinaryPdu, Lapse, N}),
            % The buffer may carry more than one SMPP PDU.
            handle_input(Pid, Conn, NextPdus, Lapse, N + 1);
        _IncompletePdu ->
            {ok, Buffer}
    end;
handle_input(_Pid, _Conn, Buffer, _Lapse, _N) ->
    {ok, Buffer}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
%% @spec handle_peer_bind(Bind, Self, State) -> bool()
%%    Bind  = {bind_receiver, Pdu}    |
%%            {bind_transmitter, Pdu} |
%%            {bind_transceiver, Pdu}
%%    Self  = pid()
%%    State = #state()
%%
%% @doc Handles bind requests from the peer ESME.  <tt>Self</tt> is pid of
%% the SMSC session.
%%
%% <p>This function issues the <a href="#handle_bind-3">handle_bind/3</a>
%% callback to the callback module.</p>
%%
%% <p>Returns <tt>true</tt> if the bind is accepted by the callback module,
%% <tt>false</tt> otherwise.</p>
%% @end 
handle_peer_bind({CmdId, Pdu}, Self, S) ->
    CmdName = ?COMMAND_NAME(CmdId),
    SeqNum  = operation:get_param(sequence_number, Pdu),
    RespId  = ?RESPONSE(CmdId),
    case (S#state.mod):handle_bind(S#state.smsc, Self, {CmdName, Pdu}) of
        {ok, ParamList} ->
            send_response(RespId, ?ESME_ROK, SeqNum, ParamList, S#state.conn),
            true;
        {error, Error, ParamList} ->
            send_response(RespId, Error, SeqNum, ParamList, S#state.conn),
            false
    end.


%% @spec handle_peer_operation(Operation, Self, State) -> bool()
%%    Operation = {CmdName, Pdu}
%%    CmdName   = atom()
%%    Self      = pid()
%%    State     = #state()
%%
%% @doc Handles SMPP operations from the peer ESME.  <tt>Self</tt> is pid of 
%% the SMSC session.
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
    PList2  = [{congestion_state, S#state.self_congestion_state}],
    case (S#state.mod):handle_operation(S#state.smsc, Self, {CmdName, Pdu}) of
        {ok, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_response(RespId, ?ESME_ROK, SeqNum, ParamList, S#state.conn),
            true;
        {error, Error, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_response(RespId, Error, SeqNum, ParamList, S#state.conn),
            false
    end.


%% @spec handle_peer_unbind(Unbind, Self, State) -> bool()
%%    Unbind = {bind_receiver, Pdu}    |
%%             {bind_transmitter, Pdu} |
%%             {bind_transceiver, Pdu}
%%    Self   = pid()
%%    State  = #state()
%%
%%
%% @doc Handles unbind requests from the peer ESME.  <tt>Self</tt> is pid of
%% the SMSC session.
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
    case (S#state.mod):handle_unbind(S#state.smsc, self(), {unbind, Pdu}) of
        ok ->
            send_response(RespId, ?ESME_ROK, SeqNum, [], S#state.conn),
            true;
        {error, Error} ->
            send_response(RespId, Error, SeqNum, [],  S#state.conn),
            false
    end.


%% @spec send_request(CmdId, ParamList, From, StateData) -> Result
%%    CmdId        = atom()
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
%% the caller issuing the request (might be the atom <tt>undefined</tt>).
%% </p>
%%
%% <p>Refer to <a href="operation.html#command_name-3">operation:command_name/3
%% </a> for a complete list of valid values for <tt>CommandName</tt>.</p>
%% @end
send_request(CmdId, ParamList, From, StateData) ->
    SeqNum = StateData#state.sequence_number + 1,
    Pdu    = operation:new(CmdId, SeqNum, ParamList),
    case send_pdu(StateData#state.conn, Pdu) of
        ok ->
            CmdId  = operation:get_param(command_id, Pdu),
            Err    = operation:request_failure_code(CmdId),
            Time   = StateData#state.response_time,
            Broker = spawn_link(fun() -> request_broker(From, Err, Time) end),
            ets:insert(StateData#state.requests, {SeqNum, CmdId, Broker}),
            {ok, StateData#state{sequence_number = SeqNum}};
        Error ->
            Error
    end.


%% @spec send_response(CmdId, Status, SeqNum, ParamList, Conn) -> Result
%%    CmdId      = int()
%%    Status     = int()
%%    SeqNum     = int()
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%    Conn       = pid()
%%    Result     = ok | {error, Error}
%%    Error      = int()
%%
%% @doc Send a SMPP response over the connection identified by <tt>Conn</tt>.
%% <tt>Status</tt> is the command status and <tt>SeqNun</tt> the sequence
%% number of the PDU.
%% @end
send_response(CmdId, Status, SeqNum, ParamList, Conn) ->
    send_pdu(Conn, operation:new(CmdId, Status, SeqNum, ParamList)).


%% @spec send_pdu(Conn, Pdu) -> ok | {error, Reason}
%%    Conn = pid()
%%    Pdu  = pdu()
%%
%% @doc Send the PDU <tt>Pdu</tt> over the connection <tt>Conn</tt>.
%% @end
send_pdu(Conn, Pdu) ->
%     io:format("Sending PDU: ~p~n", [Pdu]),
    case catch operation:smsc_pack(Pdu) of
        {ok, BinaryPdu} ->
            case gen_tcp_connection:send(Conn, BinaryPdu) of
                ok ->
%                     io:format("OK~n", []),
                    ok;
                _SendError ->
%                     io:format("Error ~p~n", [SendError]),
                    {error, ?ESME_RUNKNOWNERR}
            end;
        {error, _CmdId, Status, _SeqNum} ->
            {error, Status};
        {'EXIT', _What} ->
            {error, ?ESME_RUNKNOWNERR}
    end.


%% @spec request_broker(Caller, TimeoutError, Time) -> true
%%    Caller       = {pid(), Tag}
%%    TimeoutError = int()
%%    Time         = int()
%%
%% @doc The request broker waits for <tt>Time</tt> milliseconds until a
%% response for the request arrives.  The response is forwarded to the 
%% <tt>Caller</tt> of the request, if the <tt>Time</tt> expires
%% before any response is received, the term <tt>{error, TimeoutError}</tt> 
%% is reported to the <tt>Caller</tt>.
%% @end
request_broker(undefined, TimeoutError, Time) ->
    receive 
        {Sid, {response, CmdId, Pdu}} ->
            case operation:get_param(command_status, Pdu) of
                ?ESME_ROK when CmdId == ?COMMAND_ID_UNBIND_RESP ->
					gen_fsm:send_event(Sid, ?COMMAND_ID_UNBIND_RESP);
                ?ESME_ROK ->
					ok;
                Error ->
                    {error, Error}
            end
    after Time ->
            {error, TimeoutError}
    end;
request_broker(Caller, TimeoutError, Time) ->
    receive
        {Sid, {response, CmdId, Pdu}} ->
            case operation:get_param(command_status, Pdu) of
                ?ESME_ROK when CmdId == ?COMMAND_ID_UNBIND_RESP ->
					gen_fsm:reply(Caller, {ok, Pdu}),
					gen_fsm:send_event(Sid, ?COMMAND_ID_UNBIND_RESP);
				?ESME_ROK when CmdId == ?COMMAND_ID_GENERIC_NACK ->
					gen_fsm:reply(Caller, {error, ?ESME_RUNKNOWNERR});
                ?ESME_ROK ->
					gen_fsm:reply(Caller, {ok, Pdu});
                Error ->
                    gen_fsm:reply(Caller, {error, Error})
            end
    after Time ->
            gen_fsm:reply(Caller, {error, TimeoutError})
    end.


%% @spec start_timer(Time, Msg) -> Ref
%%    Time = int()
%%    Msg  = term()
%%    Ref  = pid()
%%
%% @doc Starts a timer, setting the current process as its parent, and returns 
%% the PID of the timeout process.
%% 
%% <p><tt>Time</tt> is the value of the time in milliseconds or the
%% atom <tt>infinity</tt>, the later doesn't start any timer.</p>
%%
%% <p>A timer can only be stopped from the same process who started it.</p>
%%
%% <p>When the timer expires, the event <tt>Msg</tt> is sent to current
%% process via gen_fsm:send_event/2.</p>
%% @end
%%
%% %@TODO See todo tags at the begining of this file.
start_timer(Time, Msg) ->
    SMSC = self(),
    spawn(fun() -> timer_loop(SMSC, Time, Msg) end).

%% @doc Auxiliary function for start_timer/2.
%% @end
timer_loop(_Parent, infinity, _Event) ->
    ok;
timer_loop(Parent, Time, Event) ->
    receive 
        {Parent, cancel_timer} ->
            ok;
        Reset ->
            timer_loop(Parent, Time, Event)
    after Time ->
            gen_fsm:send_event(Parent, {timeout, self(), Event})
    end.


%% @spec cancel_timer(Timer) -> ok
%%    Timer = pid()
%%
%% @doc Aborts the timer process whose PID is <tt>Timer</tt>.  Only 
%% the timer's parent is allowed to stop it.
%% @end
cancel_timer(Timer) when pid(Timer) ->
    Timer ! {self(), cancel_timer};
cancel_timer(_Timer) ->  % might be the atom undefined
    ok.


%% @spec reset_timer(Timer) -> ok
%%    Timer = pid()
%%
%% @doc Restarts the timer process whose PID is <tt>Timer</tt>.  Only 
%% the timer's parent is allowed to restart it.
%% @end
reset_timer(Timer) when pid(Timer) ->
    Timer ! {self(), reset};
reset_timer(_Timer) ->  % might be the atom undefined
    ok.
