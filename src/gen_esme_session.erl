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
% @doc Generic ESME SMPP Session.
%
% <p>A generic ESME SMPP session modeled as a FSM.  It also implements the
% gen_connection behaviour.</p>
%
% <p>Every SMPP session works over a single TCP/IP connection.  Connection 
% errors/recovery are automatically handled by the session and reported to the
% ESME through mc_unavailable/2 and resume_service/2 functions.  Please notice
% that if a harmful error occurs, causing the process of the underlying
% connection to terminate, the session is also terminated.</p>
%
%
% <h2>State transitions table</h2>
%
% <p>Possible states for the ESME SMPP session are shown in the first row.
% Events are those in the first column.  This table shows the next state given
% an event and the current state.</p>
%
% <p>Operations issued by the other peer (the MC) are treated asynchronously
% by the ESME session, thus represented by async events.</p>
%
% <p>Empty cells mean that events are not possible for those states.</p>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>&#160;</small></th>
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr> 
%     <th align="left"><small>accept (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%   </tr>
%   <tr> 
%     <th align="left"><small>alert_notification (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>broadcast_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>close</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_error (async)</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_recovery (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>data_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_data_sm (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_sm (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>die (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>enquire_link (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>input (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen</small></th>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_error (async)</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_recovery (async)</small></th>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>open</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>open</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>outbind (async)</small></th>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>replace_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_multi</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_sm</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by MC (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>unbound/bound_rx</small></td>
%     <td valign="top" align="center"><small>unbound/bound_tx</small></td>
%     <td valign="top" align="center"><small>unbound/bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by ESME</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>listening</small></td>
%     <td valign="top" align="center"><small>closed</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind_resp (async)</small></th>
%     <td valign="top" align="center"><small>open</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>unbound</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>timeout (async)</small></th>
%     <td valign="top" align="center"><small>closed</small></td>
%     <td valign="top" align="center"><small>outbound</small></td>
%     <td valign="top" align="center"><small>bound_rx</small></td>
%     <td valign="top" align="center"><small>bound_tx</small></td>
%     <td valign="top" align="center"><small>bound_trx</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
%
%
% <h2>Timers</h2>
%
% <p>Timers are implemented as shown in tables below.  There's is a table
% for each timer, indicating what events on every state force timers to be 
% started, restarted or stopped.</p>
%
% <h3>session_init_timer</h3>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>&#160;</small></th>
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr> 
%     <th align="left"><small>accept (async)</small></th>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%   </tr>
%   <tr> 
%     <th align="left"><small>alert_notification (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>close</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_error (async)</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_recovery (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>data_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_data_sm (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_sm (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>die (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>enquire_link (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>input (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_error (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_recovery (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>open</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>outbind (async)</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>replace_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_multi</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by MC (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by ESME</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind_resp (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
% <p>Actions on timeout.</p>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr>
%     <td valign="top" align="center"><small>close connection</small></td>
%     <td valign="top" align="center"><small>close connection</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
%
% <h3>inactivity_timer</h3>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>&#160;</small></th>
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr> 
%     <th align="left"><small>accept (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr> 
%     <th align="left"><small>alert_notification (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>close</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_error (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_recovery (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>data_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_data_sm (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_sm (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>die (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>enquire_link (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>input (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_error (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_recovery (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>open</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>outbind (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>replace_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_multi</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by MC (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by ESME</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind_resp (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
% <p>Actions on timeout.</p>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>unbind</small></td>
%     <td valign="top" align="center"><small>unbind</small></td>
%     <td valign="top" align="center"><small>unbind</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
%
% <h3>enquire_link_timer</h3>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>&#160;</small></th>
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr> 
%     <th align="left"><small>accept (async)</small></th>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%   </tr>
%   <tr> 
%     <th align="left"><small>alert_notification (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_receiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transmitter_resp (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>bind_transceiver_resp (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>cancel_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>close</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_error (async)</small></th>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>cancel</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>connect_recovery (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>data_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_data_sm (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>deliver_sm (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>die (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>enquire_link (async)</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>input (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_error (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>listen_recovery (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>open</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%     <td valign="top" align="center"><small>start</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>outbind (async)</small></th>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_broadcast_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>query_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>replace_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_multi</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>submit_sm</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by MC (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind issued by ESME</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
%   <tr>
%     <th align="left"><small>unbind_resp (async)</small></th>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>reset</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
% <p>Actions on timeout.</p>
%
% <table width="100%" border="1" cellpadding="5">
%   <tr> 
%     <th><small>open</small></th>
%     <th><small>outbound</small></th>
%     <th><small>bound_rx</small></th>
%     <th><small>bound_tx</small></th>
%     <th><small>bound_trx</small></th>
%     <th><small>unbound</small></th>
%     <th><small>listening</small></th>
%     <th><small>closed</small></th>
%   </tr>
%   <tr>
%     <td valign="top" align="center"><small>enquire_link</small></td>
%     <td valign="top" align="center"><small>enquire_link</small></td>
%     <td valign="top" align="center"><small>enquire_link</small></td>
%     <td valign="top" align="center"><small>enquire_link</small></td>
%     <td valign="top" align="center"><small>enquire_link</small></td>
%     <td valign="top" align="center"><small>enquire_link</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%     <td valign="top" align="center"><small>&#160;</small></td>
%   </tr>
% </table>
%
%
% <h3>response_timer</h3>
%
% <p>One <quote>instance</quote> of this timer is started for each request.  
% On expiration, the request is considered unsuccessful.</p>  
%
% <p>The timer associated to a request is stopped when the corresponding 
% response arrives.</p>
%
%
% <h2>Callback Function Index</h2>
%
% <p>A module implementing this behaviour may export these functions.  Leaving 
% a function undefined preserves the default behaviour.</p>
%
% <table width="100%" border="1">
%   <tr>
%     <td valign="top"><a href="#outbind-3">outbind/3</a></td>
%     <td>Notifies outbind operations.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#unbind-2">unbind/2</a></td>
%     <td>Forwards unbind requests.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#mc_unavailable-4">mc_unavailable/4</a></td>
%     <td>Triggered when the MC becomes unavailable.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#resume_service-4">resume_service/4</a></td>
%     <td>The MC is back.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#smpp_listen_error-3">smpp_listen_error/3</a>
%     </td>
%     <td>Listen socket lost.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#smpp_listen_recovery-3">smpp_listen_recovery/3</a>
%     </td>
%     <td>A new socket is now listening.</td>
%   </tr>
%   <tr>
%     <td valign="top">
%       <a href="#alert_notification-3">alert_notification/3</a>
%     </td>
%     <td>Forwards alert_notification operations to the ESME.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#deliver_sm-3">deliver_sm/3</a></td>
%     <td>Forwards deliver_sm operations to the ESME.</td>
%   </tr>
%   <tr>
%     <td valign="top"><a href="#deliver_data_sm-3">deliver_data_sm/3</a></td>
%     <td>Forwards data_sm operations to the ESME.</td>
%   </tr>
% </table>
%
%
% <h2>Callback Function Details</h2>
% 
% <h3><a name="outbind-3">outbind/3</a></h3>
%
% <tt>outbind(Pid, Sid, Pdu) -> ok</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Pdu = pdu()</tt></li>
% </ul>
%
% <p>Outbind callback.  When the session receives an outbind request this
% callback is triggered to notify the ESME.  Returning value is ignored by the
% session.</p>
% 
% <p>On response to this function the ESME must start the appropriate actions
% in order to bind or turn the session back to an open or closed state.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_outbind/2</tt></p>
%
% <h3><a name="unbind-2">unbind/2</a></h3>
%
% <tt>unbind(Pid, Sid) -> ok | {error, Error}</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Error = int()</tt></li>
% </ul>
%
% <p>This callback forwards an unbind request (issued by the MC) to the 
% ESME.  If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
% command_status is sent to the MC and the session moves into the unbound
% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
% response PDU sent by the session to the MC will have an <tt>Error</tt>
% command_status and the session will remain on it's current bound state
% (bound_rx, bound_tx or bound_trx).</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_unbind/1</tt></p>
%
%
% <h3><a name="mc_unavailable-4">mc_unavailable/4</a></h3>
%
% <tt>mc_unavailable(Pid, Sid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>If the MC becomes unavailable the session notifies that circumstance to
% the ESME with a call to this function.</p>
%
% <p>Notice that this callback could also be triggered after an unbind
% operation, if the MC closes the underlying connection first.  The ESME must
% handle these <i>undesired</i> callbacks appropriately.</p>
% 
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_mc_unavailable/3</tt></p>
%
%
% <h3><a name="resume_service-4">resume_service/4</a></h3>
%
% <tt>resume_service(Pid, Sid, Address, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Address = string() | atom() | ip_address()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>After a period of unavailability of the MC, once the connection is
% recover by the session, this function is called in order to let the ESME
% resume the service.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_resume_service/3</tt></p>
%
%
% <h3><a name="smpp_listen_error-3">smpp_listen_error/3</a></h3>
%
% <tt>smpp_listen_error(Pid, Sid, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>When a session on listening state looses the listen socket, uses
% this callback to notify the ESME.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_smpp_listen_error/2</tt></p>
%
%
% <h3><a name="smpp_listen_recovery-3">smpp_listen_recovery/3</a></h3>
%
% <tt>smpp_listen_recovery(Pid, Sid, Port) -> ok</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Port = int()</tt></li>
% </ul>
%
% <p>After a listen failure, a new socket could be set to listen again on
% <tt>Port</tt>.  This callback notifies the ESME.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_smpp_listen_recovery/2</tt></p>
%
%
% <h3><a name="alert_notification-3">alert_notification/3</a></h3>
%
% <tt>alert_notification(Pid, Sid, Pdu) -> ok</tt>
% <ul>
%   <li><tt>Pid = Sid = pid()</tt></li>
%   <li><tt>Pdu = pdu()</tt></li>
% </ul>
%
% <p>Alert notifications are forwarded to the ESME by this callback.  The
% alert_notification PDU is given to the ESME.</p>
%
% <p>The ESME is responsible of initiating the appropriate actions associated
% to the alert notification.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_alert_notification/2</tt></p>
%
%
% <h3><a name="deliver_sm-3">deliver_sm/3</a></h3>
%
% <tt>deliver_sm(Pid, Sid, Pdu) -> Result</tt>
% <ul>
%   <li><tt>Pid        = pid()</tt></li>
%   <li><tt>Sid        = pid()</tt></li>
%   <li><tt>Pdu        = pdu()</tt></li>
%   <li><tt>Result     = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%   <li><tt>ParamList  = [{ParamName, ParamValue}]</tt></li>
%   <li><tt>ParamName  = atom()</tt></li>
%   <li><tt>ParamValue = term()</tt></li>
% </ul>
%
% <p>Short messages delivered to the ESME via this callback are enclosed
% inside a deliver_sm PDU.</p>
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the deliver_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
%
% <p><b>See also:</b> <tt>callback_deliver_sm/2</tt></p>
%
%
% <h3><a name="deliver_data_sm-3">deliver_data_sm/3</a></h3>
%
% <tt>deliver_data_sm(Pid, Sid, Pdu) -> Result</tt>
% <ul>
%   <li><tt>Pid        = pid()</tt></li>
%   <li><tt>Sid        = pid()</tt></li>
%   <li><tt>Pdu        = pdu()</tt></li>
%   <li><tt>Result     = {ok, ParamList} | {error, Error, ParamList}</tt></li>
%   <li><tt>ParamList  = [{ParamName, ParamValue}]</tt></li>
%   <li><tt>ParamName  = atom()</tt></li>
%   <li><tt>ParamValue = term()</tt></li>
% </ul>
%
% <p>Short messages delivered to the ESME via this callback are enclosed
% inside a data_sm PDU.</p>
%
% <p>The <tt>ParamList</tt> included in the response is used to construct
% the data_sm_resp PDU.  If a command_status other than ESME_ROK is to
% be returned by the ESME in the response PDU, the callback should return the
% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
% desired command_status error code.</p>
%
% <p><tt>Pid</tt> is the session's parent id, <tt>Sid</tt> own 
% session's process id.</p>
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
% [24 Feb 2004]
%
% <ul>
%   <li>All timers reviewed (fixed).</li>
%   <li><tt>request_broker</tt> fixed when <tt>Caller</tt> is
%     <tt>undefined</tt>.
%   </li>
% </ul>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.2 alpha, {26 May 2003} {@time}.
% @end
%
% %@TODO New fsm implementation (on stdlib 1.12) includes now timers, use this 
% new feature and remove custom timers.  Built in timers do not include reset 
% feature :-( a little bit of extra work will be required (cancel + start and
% updating state data.  Consider creating the functions reset_active_timers
% and cancel_active_timers, both take StateData as an argument and return 
% {ok, NewStateData} | error.  Put canceled timers to undefined)
%
% <p>Current timers implementation may cause false timer events.</p>
%
% %@TODO On submit_sm operations, split into several short messages those 
% with a content larger than 255 character.
%%
-module(gen_esme_session).

-behaviour(gen_fsm).
-behaviour(gen_connection).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("gen_esme_session.hrl").

%%%-------------------------------------------------------------------
% Imports.  
%
% <p>Imports are explicitly made on the function calls.  These lines are
% commented out and only included for informative purposes.</p>
%%--------------------------------------------------------------------
% -import(ets, [insert/2, delete/2, lookup/2, new/1]).
% -import(gen_connection, [close/1, connect/4, disable_retry/1, enable_retry/1, listen/2, start_link/2, send/2]).
% -import(my_calendar, [time_since/1]).
% -import(operation, [get_param/2, esme_pack/1, esme_unpack/1, merge_params/2, request_command_id/1, request_failure_code/1, new_bind_receiver/2, new_bind_transmitter/2, new_bind_transceiver/2, new_broadcast_sm/2, new_cancel_broadcast_sm/2, new_cancel_sm/2, new_query_broadcast_sm/2, new_generic_nack/3, new_data_sm/2, new_data_sm_resp/3, new_deliver_sm_resp/3, new_enquire_link/2, new_enquire_link_resp/3, new_unbind_resp/3, new_query_sm/2, new_replace_sm/2, new_submit_multi/2, new_submit_sm/2, new_unbind/2]).

%%%-------------------------------------------------------------------
% Behaviour exports
%%--------------------------------------------------------------------
-export([behaviour_info/1]).

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([start_link/2, 
         start_link/3, 
         bind_receiver/2,
         bind_transmitter/2,
         bind_transceiver/2,
         broadcast_sm/2,
         cancel_broadcast_sm/2,
         cancel_sm/2,
         data_sm/2,
         query_broadcast_sm/2,
         query_sm/2,
         replace_sm/2,
         submit_multi/2,
         submit_sm/2,
         unbind/1,
         do_open/3,
         do_listen/2,
         do_close/1,
         stop/1]).

%%%-------------------------------------------------------------------
% Internal gen_fsm exports
%%--------------------------------------------------------------------
-export([init/1,
         open/2,
         outbound/2,
         bound_rx/2,
         bound_tx/2,
         bound_trx/2,
         unbound/2,
         listening/2,
         closed/2,
         open/3,
         outbound/3,
         bound_rx/3,
         bound_tx/3,
         bound_trx/3,
         unbound/3,
         listening/3,
         closed/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%%-------------------------------------------------------------------
% Internal gen_connection exports
%%--------------------------------------------------------------------
-export([handle_accept/2,
         handle_input/4,
         handle_listen_error/3,
         handle_connect_error/4,
         handle_listen_recovery/3,
         handle_connect_recovery/4]).

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
%         SequenceNumber,
%         Connection,
%         Requests,
%         SelfCongestionState,
%         PeerCongestionState,
%         SessionInitTime,
%         SessionInitTimer,
%         EnquireLinkTime,
%         EnquireLinkTimer,
%         InactivityTime,
%         InactivityTimer,
%         ResponseTime}
%    Parent              = pid()
%    Self                = pid()
%    CallbackModule      = atom()
%    SequenceNumber      = int()
%    Connection          = pid()
%    Requests            = ets:table()
%    SelfCongestionState = int()
%    PeerCongestionState = int()
%    SessionInitTime     = int()
%    SessionInitTimer    = pid()
%    EnquireLinkTime     = int()
%    EnquireLinkTimer    = pid()
%    InactivityTime      = int()
%    InactivityTimer     = pid()
%    ResponseTime        = int()
%
% %@doc Representation of the fsm's state
%
% <dl>
%   <dt>Parent: </dt><dd>Pid of the parent process.  Is passed in the
%     callback functions to help identify the owner of the session.
%   </dd>
%   <dt>Self: </dt><dd>The Pid of the gen_esme_session main process.  Is passed
%     along with the callback functions to help identify the session triggering
%     the callback.  Since a callback function might be called from a spawned
%     process, we want to keep a reference to our main process.
%   </dd>
%   <dt>CallbackModule: </dt><dd>Module where callbacks are defined.</dd>
%   <dt>SequenceNumber: </dt><dd>PDU sequence number.</dd>
%   <dt>Connection: </dt><dd>The <tt>pid()</tt> of the underlying 
%     connection
%   </dd>
%   <dt>Requests: </dt><dd>An ets table with the requests which responses are
%     pending.
%   </dd>
%   <dt>SelfCongestionState: </dt><dd>ESME congestion state (default is 0).
%   </dd>
%   <dt>PeerCongestionState: </dt><dd>MC congestion state.  Might be atom
%     <tt>undefined</tt> if the MC doesn't support the congestion_state 
%     parameter.
%   </dd>
%   <dt>SessionInitTime: </dt><dd>A value in milliseconds or the atom
%     <tt>infinity</tt> (the later disables the timer).
%   </dd>
%   <dt>SessionInitTimer: </dt><dd>PID of the process running the session
%     init timeout (might be the atom <tt>undefined</tt>).
%
%     <p>This timer is started when a new connection is accepted from the
%     Mc and aborted when the session moves away from the open state (or if
%     the connection is reopen).  
%
%     <p>On expiration the event <tt>{timeout, Ref, session_init_timer}
%     </tt> is asynchronously signaled to the current state.
%
%     <p>Refer to the function start_timer/2 below.
%   </dd>
%   <dt>EnquireLinkTime: </dt><dd>A value in milliseconds or the atom
%     <tt>infinity</tt> (the later disables the timer).
%   </dd>
%   <dt>EnquireLinkTimer: </dt><dd>PID of the process running the enquire
%     link timeout (might be the atom <tt>undefined</tt>).
%
%     <p>This timer is started when the session gets to a bound state; 
%     bound_rx, bound_tx or bound_trx, and restarted every time a *valid*
%     request (response) is sent (received).  The timer is aborted if the
%     session gets unbound.  Erroneous requests/responses do *not* reset this
%     timer.
%
%     <p>On expiration the event <tt>{timeout, Ref, enquire_link_timer}<tt>
%     is asynchronously signaled to the current state.
%
%     <p>Refer to the function start_timer/2 below.
%   </dd>
%   <dt>InactivityTime: </dt><dd>A value in milliseconds or the atom
%     <tt>infinity</tt> (the later disables the timer).
%   </dd>
%   <dt>InactivityTimer: </dt><dd>PID of the process running the inactivity
%     timeout (might be the atom <tt>undefined</tt>).
%
%     <p>This timer is started when the session gets to a bound state; 
%     bound_rx, bound_tx or bound_trx, and restarted every time a *valid*
%     request (response) is sent (received).  The timer is aborted if the
%     session gets unbound.  Erroneous requests/responses do *not* reset this
%     timer.
%
%     <p>On expiration the event <tt>{timeout, Ref, inactivity_timer}<tt>
%     is asynchronously signaled to the current state.
%
%     <p>Refer to the function start_timer/1 below.
%   </dd>
%   <dt>ResponseTime: </dt><dd>A value in milliseconds or the atom
%     <tt>infinity</tt> (the later disables the timer).
%   </dd>
% </dl>
% %@end
%%
-record(state, 
        {parent,
         self,
         callback_module,
         sequence_number       = 0,
         connection,
         requests,
         self_congestion_state = 0,
         peer_congestion_state,
         session_init_time,
         session_init_timer,
         enquire_link_time,
         enquire_link_timer,
         inactivity_time,
         inactivity_timer,
         response_time}).

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
    [{outbind, 3}, 
     {unbind, 2},
     {mc_unavailable, 4}, 
     {resume_service, 4}, 
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
%    Setup  = gen_esme_session_setup()
%    Result = {ok, Pid} | ignore | {error, Error}
%    Pid    = pid()
%    Error  = {already_started, Pid} | term()
%
% @doc Starts the server.
%
% <p><tt>Setup</tt> is a gen_esme_session_setup() record with all the
% session setup parameters.  Use the macro ?DEFAULT_GEN_ESME_SESSION_SETUP to 
% set the default values.</p>
%
% <p>Refer to <b>gen_esme_session.hrl</b> for more details on the
% gen_esme_session_setup() record definition.</p>
%
% <p>The gen_esme_session is not registered.</p>
%
% @see gen_fsm:start_link/3
% @see start_link/3
% @end
%%
start_link(Module, Setup) ->
    gen_fsm:start_link(?MODULE, [self(), Module, Setup], []).


%%%
% @spec start_link(SName, Module, Setup) -> Result
%    SName  = {local, Name} | {global, Name}
%    Name   = atom()
%    Module = atom()
%    Setup  = gen_esme_session_setup()
%    Result = {ok, Pid} | ignore | {error, Error}
%    Pid    = pid()
%    Error  = {already_started, Pid} | term()
%
% @doc Starts the server.
%
% <p><tt>Setup</tt> is a gen_esme_session_setup() record with all the
% session setup parameters.  Use the macro ?DEFAULT_GEN_ESME_SESSION_SETUP to 
% set the default values.</p>
%
% <p>Refer to <b>gen_esme_session.hrl</b> for more details on the
% gen_esme_session_setup() record definition.</p>
%
% <p>If <tt>SName = {local, Name}</tt>, the gen_esme_session is registered
% locally as <tt>Name</tt>.  If <tt>SName = {global, Name}</tt>, the 
% gen_esme_session is registered globally as <tt>Name</tt>.</p>
%
% @see gen_fsm:start_link/4
% @see start_link/2
% @end
%%
start_link(SName, Module, Setup) ->
    gen_fsm:start_link(SName, ?MODULE, [self(), Module, Setup], []).


%%%
% @spec bind_receiver(Sid, ParamList) -> Result
%    Sid        = atom()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a bind receiver operation on the session identified by 
% <tt>Sid</tt>.
% @end
% 
% %@see
%
% %@equiv
%%
bind_receiver(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {bind_receiver, ParamList}, infinity).


%%%
% @spec bind_transmitter(Sid, ParamList) -> Result
%    Sid        = atom()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a bind transmitter operation on the session identified by 
% <tt>Sid</tt>.
% @end
% 
% %@see
%
% %@equiv
%%
bind_transmitter(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {bind_transmitter, ParamList}, infinity).


%%%
% @spec bind_transceiver(Sid, ParamList) -> Result
%    Sid        = atom()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a bind transceiver operation on the session identified by
% <tt>Sid</tt>.
% @end
% 
% %@see
%
% %@equiv
%%
bind_transceiver(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {bind_transceiver, ParamList}, infinity).


%%%
% @spec broadcast_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a broadcast_sm operation on the session identified by <tt>Sid
% </tt>.
% @end
%
% %@see
%
% %@equiv
%%
broadcast_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {broadcast_sm, ParamList}, infinity).


%%%
% @spec cancel_broadcast_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a cancel_broadcast_sm operation on the session identified by
% <tt>Sid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
cancel_broadcast_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {cancel_broadcast_sm, ParamList}, infinity).


%%%
% @spec cancel_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a cancel_sm operation on the session identified by
% <tt>Sid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
cancel_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {cancel_sm, ParamList}, infinity).


%%%
% @spec data_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a data_sm operation on the session identified by <tt>Sid
% </tt>.
% @end
%
% %@see
%
% %@equiv
%%
data_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {data_sm, ParamList}, infinity).


%%%
% @spec query_broadcast_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a query_broadcast_sm operation on the session identified by
% <tt>Sid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
query_broadcast_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {query_broadcast_sm, ParamList}, infinity).


%%%
% @spec query_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a query_sm operation on the session identified by <tt>Sid
% </tt>.
% @end
%
% %@see
%
% %@equiv
%%
query_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {query_sm, ParamList}, infinity).


%%%
% @spec replace_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a data_sm operation on the session identified by <tt>Sid
% </tt>.
% @end
%
% %@see
%
% %@equiv
%%
replace_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {replace_sm, ParamList}, infinity).


%%%
% @spec submit_multi(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a submit_multi operation on the session identified by <tt>Sid
% </tt>.
% @end
%
% %@see
%
% %@equiv
%%
submit_multi(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {submit_multi, ParamList}, infinity).


%%%
% @spec submit_sm(Sid, ParamList) -> Result
%    Sid        = pid()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Result     = {ok, PduResp} | {error, Error}
%    PduResp    = pdu()
%    Error      = int()
%
% @doc Issues a submit_sm operation on the session identified by <tt>Sid
% </tt>.
% @end
%
% %@see
%
% %@equiv
%%
submit_sm(Sid, ParamList) ->
    gen_fsm:sync_send_event(Sid, {submit_sm, ParamList}, infinity).


%%%
% @spec unbind(Sid) -> Result
%    Sid     = atom()
%    Result  = {ok, PduResp} | {error, Error}
%    PduResp = pdu()
%    Error   = int()
%
% @doc Issues an unbind operation on the session identified by <tt>Sid
% </tt>.
% @end
% 
% %@see
%
% %@equiv
%%
unbind(Sid) ->
    gen_fsm:sync_send_event(Sid, unbind, infinity).


%%%
% @spec do_open(Sid, Address, Port) -> ok | {error, Error}
%    Sid     = atom()
%    Address = string() | atom() | ip_address()
%    Port    = int()
%    Error   = int()
%
% @doc Opens the session identified by <tt>Sid</tt> with the MC whose 
% address is <tt>Address</tt>.
% @end
% 
% %@see
%
% %@equiv
%%
do_open(Sid, Address, Port) ->
    gen_fsm:sync_send_event(Sid, {open, Address, Port}, infinity).


%%%
% @spec do_listen(Sid, Port) -> ok | {error, Error}
%    Sid   = atom()
%    Port  = int()
%    Error = int()
%
% @doc Puts the session identified by <tt>Sid</tt> to listen on port
% <tt>Port</tt>.
% @end
% 
% %@see
%
% %@equiv
%%
do_listen(Sid, Port) ->
    gen_fsm:sync_send_event(Sid, {listen, Port}, infinity).


%%%
% @spec do_close(Sid) -> ok | {error, Error}
%    Sid   = pid()
%    Error = int()
%
% @doc Closes the session identified by <tt>Sid</tt>.
% @end
% 
% %@see
%
% %@equiv
%%
do_close(Sid) ->
    gen_fsm:sync_send_event(Sid, close, infinity).


%%%
% @spec stop(Sid) -> ok
%
% @doc Stops the fsm.  This function does *NOT* issue an unbind operation.
% The unbind must have been previously sent using the unbind/1 function.
%
% @see gen_fsm:send_all_state_event/2
%
% @equiv gen_fsm:send_all_state_event(Sid, die)
% @end
%%
stop(Sid) ->
    gen_fsm:send_all_state_event(Sid, die).


%%%===================================================================
% Server gen_fsm functions
%%====================================================================
%%%
% @spec init(Args) -> Result
%    Args       = term()
%    Result     = {ok, StateName, StateData}          |
%                 {ok, StateName, StateData, Timeout} |
%                 ignore                              |
%                 {stop, StopReason}                   
%    StateName  = atom()
%    StateData  = term()
%    Timeout    = int()
%    StopReason = term()
%
% @doc Initializes the the fsm.
% @end
%%
init([Pid, Module, #gen_esme_session_setup{retry_time = T} = Setup]) ->
    case gen_connection:start_link(?MODULE, T) of
        {ok, Cid} ->
%       {error, {already_started, Cid}} ->
            State = #state{parent            = Pid,
                           self              = self(),
                           callback_module   = Module,
                           connection        = Cid,
                           requests          = ets:new(esme_requests, []),
                           session_init_time = 
                               Setup#gen_esme_session_setup.session_init_time,
                           enquire_link_time = 
                               Setup#gen_esme_session_setup.enquire_link_time,
                           inactivity_time   = 
                               Setup#gen_esme_session_setup.inactivity_time,
                           response_time     =
                               Setup#gen_esme_session_setup.response_time},
            process_flag(trap_exit, true),
            {ok, closed, State};
        {error, Reason} ->
            {stop, Reason};
        ignore ->
            ignore
    end.


%%%
% @spec open(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name open.
% @end
%%
open({alert_notification, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, open, StateData};

open(bind_receiver_resp, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    Timer = start_timer(StateData#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, StateData#state{inactivity_timer = Timer}};

open(bind_transmitter_resp, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    Timer = start_timer(StateData#state.inactivity_time, inactivity_timer),
    {next_state, bound_tx, StateData#state{inactivity_timer = Timer}};

open(bind_transceiver_resp, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    Timer = start_timer(StateData#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, StateData#state{inactivity_timer = Timer}};

open({deliver_data_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_data_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, open, StateData};

open({deliver_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, open, StateData};

open({outbind, Pdu}, StateData) ->
    reset_timer(StateData#state.session_init_timer),
    reset_timer(StateData#state.enquire_link_timer),
    spawn(fun() -> callback_outbind(Pdu, StateData) end),
    {next_state, outbound, StateData};

open({unbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_unbind_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, open, StateData};

open({timeout, _Ref, enquire_link_timer}, StateData) ->
    NewData = case send_enquire_link([], undefined, StateData) of
                  {ok, NewStateData} ->
                      NewStateData;
                  _Error ->
                      StateData
              end,
    Timer = start_timer(NewData#state.enquire_link_time, enquire_link_timer),
    {next_state, open, NewData#state{enquire_link_timer = Timer}};

open({timeout, _Ref, session_init_timer}, #state{connection=C} = StateData) ->
    gen_connection:close(C),
    {next_state, closed, StateData};

open(_Event, StateData) ->
    {next_state, open, StateData}.


%%%
% @spec outbound(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name outbound.
% @end
%%
outbound({alert_notification, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, outbound, StateData};

outbound(bind_receiver_resp, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    Timer = start_timer(StateData#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, StateData#state{inactivity_timer = Timer}};

outbound(bind_transmitter_resp, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    Timer = start_timer(StateData#state.inactivity_time, inactivity_timer),
    {next_state, bound_tx, StateData#state{inactivity_timer = Timer}};

outbound(bind_transceiver_resp, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    Timer = start_timer(StateData#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, StateData#state{inactivity_timer = Timer}};

outbound({deliver_data_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_data_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, outbound, StateData};

outbound({deliver_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, outbound, StateData};

outbound({outbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, outbound, StateData};

outbound({unbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_unbind_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, outbound, StateData};

outbound({timeout, _Ref, enquire_link_timer}, StateData) ->
    NewData = case send_enquire_link([], undefined, StateData) of
                  {ok, NewStateData} ->
                      NewStateData;
                  _Error ->
                      StateData
              end,
    Timer = start_timer(NewData#state.enquire_link_time, enquire_link_timer),
    {next_state, outbound, NewData#state{enquire_link_timer = Timer}};

outbound({timeout, _Ref, session_init_timer}, #state{connection=C}=StateData)->
    gen_connection:close(C),
    {next_state, closed, StateData};

outbound(_Event, StateData) ->
    {next_state, outbound, StateData}.
    

%%%
% @spec bound_rx(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name bound_rx.
% @end
%%
bound_rx({alert_notification, Pdu}, StateData) ->
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    callback_alert_notification(Pdu, StateData),
    {next_state, bound_rx, StateData};

bound_rx({deliver_data_sm, Pdu}, #state{connection = C} = StateData) ->
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    SeqNum = operation:get_param(sequence_number, Pdu),
    PList2 = [{congestion_state, StateData#state.self_congestion_state}],
    case callback_deliver_data_sm(Pdu, StateData) of
        {ok, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_deliver_data_sm_resp(?ESME_ROK, SeqNum, ParamList, C);
        {error, Error, PList1} ->
            ParamList =    operation:merge_params(PList1, PList2),
            send_deliver_data_sm_resp(Error, SeqNum, ParamList, C)
    end,
    {next_state, bound_rx, StateData};

bound_rx({deliver_sm, Pdu}, #state{connection = C} = StateData) ->
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    SeqNum = operation:get_param(sequence_number, Pdu),
    PList2 = [{congestion_state, StateData#state.self_congestion_state}],
    case callback_deliver_sm(Pdu, StateData) of
        {ok, PList1} ->
            ParamList =    operation:merge_params(PList1, PList2),
            send_deliver_sm_resp(?ESME_ROK, SeqNum, ParamList, C);
        {error, Error, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_deliver_sm_resp(Error, SeqNum, ParamList, C)
    end,
    {next_state, bound_rx, StateData};

bound_rx({outbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, bound_rx, StateData};

bound_rx({unbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    case callback_unbind(StateData) of
        ok ->
            cancel_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            gen_connection:disable_retry(C),
            send_unbind_resp(?ESME_ROK, SeqNum, [], C),
            {next_state, unbound, StateData};
        {error, Error} ->
            send_unbind_resp(Error, SeqNum, [], C),
            {next_state, bound_rx, StateData}
    end;

bound_rx(unbind_resp, StateData) ->
    gen_connection:disable_retry(StateData#state.connection),
    cancel_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    {next_state, unbound, StateData};

bound_rx({timeout, _Ref, enquire_link_timer}, StateData) ->
    NewData = case send_enquire_link([], undefined, StateData) of
                  {ok, NewStateData} ->
                      NewStateData;
                  _Error ->
                      StateData
              end,
    Timer = start_timer(NewData#state.enquire_link_time, enquire_link_timer),
    {next_state, bound_rx, NewData#state{enquire_link_timer = Timer}};

bound_rx({timeout, _Ref, inactivity_timer}, StateData) ->
    NewData = case send_unbind([], undefined, StateData) of
                  {ok, NewStateData} ->
                      reset_timer(NewStateData#state.enquire_link_timer),
                      NewStateData;
                  _Error ->
                      % %@TODO: trigger a new callback here? exit?
                      StateData
              end,
    Timer = start_timer(NewData#state.inactivity_time, inactivity_timer),
    {next_state, bound_rx, NewData#state{inactivity_timer = Timer}};

bound_rx(_Event, StateData) ->    
    {next_state, bound_rx, StateData}.


%%%
% @spec bound_tx(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name bound_tx.
% @end
%%
bound_tx({alert_notification, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, bound_tx, StateData};

bound_tx({deliver_data_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_data_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, bound_tx, StateData};

bound_tx({deliver_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, bound_tx, StateData};

bound_tx({outbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, bound_tx, StateData};

bound_tx({unbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    case callback_unbind(StateData) of
        ok ->
            cancel_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            gen_connection:disable_retry(C),
            send_unbind_resp(?ESME_ROK, SeqNum, [], C),
            {next_state, unbound, StateData};
        {error, Error} ->
            send_unbind_resp(Error, SeqNum, [], C),
            {next_state, bound_tx, StateData}
    end;

bound_tx(unbind_resp, StateData) ->
    gen_connection:disable_retry(StateData#state.connection),
    cancel_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    {next_state, unbound, StateData};

bound_tx({timeout, _Ref, enquire_link_timer}, StateData) ->
    NewData = case send_enquire_link([], undefined, StateData) of
                  {ok, NewStateData} ->
                      NewStateData;
                  _Error ->
                      StateData
              end,
    Timer = start_timer(NewData#state.enquire_link_time, enquire_link_timer),
    {next_state, bound_tx, NewData#state{enquire_link_timer = Timer}};

bound_tx({timeout, _Ref, inactivity_timer}, StateData) ->
    NewData = case send_unbind([], undefined, StateData) of
                  {ok, NewStateData} ->
                      reset_timer(NewStateData#state.enquire_link_timer),
                      NewStateData;
                  _Error ->
                      % %@TODO: trigger a new callback here? exit?
                      StateData
              end,
    Timer = start_timer(NewData#state.inactivity_time, inactivity_timer),
    {next_state, bound_tx, NewData#state{inactivity_timer = Timer}};

bound_tx(_Event, StateData) ->    
    {next_state, bound_tx, StateData}.


%%%
% @spec bound_trx(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name bound_trx.
% @end
%%
bound_trx({alert_notification, Pdu}, StateData) ->
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    callback_alert_notification(Pdu, StateData),
    {next_state, bound_trx, StateData};


bound_trx({deliver_data_sm, Pdu}, #state{connection = C} = StateData) ->
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    SeqNum = operation:get_param(sequence_number, Pdu),
    PList2 = [{congestion_state, StateData#state.self_congestion_state}],
    case callback_deliver_data_sm(Pdu, StateData) of
        {ok, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_deliver_data_sm_resp(?ESME_ROK, SeqNum, ParamList, C);
        {error, Error, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_deliver_data_sm_resp(Error, SeqNum, ParamList, C)
    end,
    {next_state, bound_trx, StateData};

bound_trx({deliver_sm, Pdu}, #state{connection = C} = StateData) ->
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    SeqNum = operation:get_param(sequence_number, Pdu),
    PList2 = [{congestion_state, StateData#state.self_congestion_state}],
    case callback_deliver_sm(Pdu, StateData) of
        {ok, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_deliver_sm_resp(?ESME_ROK, SeqNum, ParamList, C);
        {error, Error, PList1} ->
            ParamList = operation:merge_params(PList1, PList2),
            send_deliver_sm_resp(Error, SeqNum, ParamList, C)
    end,
    {next_state, bound_trx, StateData};

bound_trx({outbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, bound_trx, StateData};

bound_trx({unbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    case callback_unbind(StateData) of
        ok ->
            cancel_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            gen_connection:disable_retry(C),
            send_unbind_resp(?ESME_ROK, SeqNum, [], C),
            {next_state, unbound, StateData};
        {error, Error} ->
            send_unbind_resp(Error, SeqNum, [], C),
            {next_state, bound_trx, StateData}
    end;

bound_trx(unbind_resp, StateData) ->
    gen_connection:disable_retry(StateData#state.connection),
    cancel_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    {next_state, unbound, StateData};

bound_trx({timeout, _Ref, enquire_link_timer}, StateData) ->
    NewData = case send_enquire_link([], undefined, StateData) of
                  {ok, NewStateData} ->
                      NewStateData;
                  _Error ->
                      StateData
              end,
    Timer = start_timer(NewData#state.enquire_link_time, enquire_link_timer),
    {next_state, bound_trx, NewData#state{enquire_link_timer = Timer}};

bound_trx({timeout, _Ref, inactivity_timer}, StateData) ->
    NewData = case send_unbind([], undefined, StateData) of
                  {ok, NewStateData} ->
                      reset_timer(NewStateData#state.enquire_link_timer),
                      NewStateData;
                  _Error ->
                      % %@TODO: trigger a new callback here? exit?
                      StateData
              end,
    Timer = start_timer(NewData#state.inactivity_time, inactivity_timer),
    {next_state, bound_trx, NewData#state{inactivity_timer = Timer}};

bound_trx(_Event, StateData) ->
    {next_state, bound_trx, StateData}.


%%%
% @spec unbound(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name unbound.
% @end
%%
unbound({alert_notification, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, unbound, StateData};

unbound({deliver_data_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_data_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, unbound, StateData};

unbound({deliver_sm, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_deliver_sm_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, unbound, StateData};

unbound({outbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_generic_nack(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, unbound, StateData};

unbound({unbind, Pdu}, #state{connection = C} = StateData) ->
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_unbind_resp(?ESME_RINVBNDSTS, SeqNum, [], C),
    {next_state, unbound, StateData};

unbound({timeout, _Ref, enquire_link_timer}, StateData) ->
    NewData = case send_enquire_link([], undefined, StateData) of
                  {ok, NewStateData} ->
                      NewStateData;
                  _Error ->
                      StateData
              end,
    Timer = start_timer(NewData#state.enquire_link_time, enquire_link_timer),
    {next_state, unbound, NewData#state{enquire_link_timer = Timer}};

unbound(_Event, StateData) ->
    {next_state, unbound, StateData}.


%%%
% @spec listening(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name listening.
% @end
%%
listening(_Event, StateData) ->
    {next_state, listening, StateData}.


%%%
% @spec closed(Event, StateData) -> Result
%    Event         = timeout | term()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles async events for the state name closed.
% @end
%%
closed(_Event, StateData) ->
    {next_state, closed, StateData}.


%%%
% @spec open(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name open.
% @end
%%
open({bind_receiver, ParamList}, From, StateData) ->
    case send_bind_receiver(ParamList, From, StateData) of
        {ok, NewStateData} ->
			reset_timer(StateData#state.enquire_link_timer),
            {next_state, open, NewStateData};
        Error ->
            {reply, Error, open, StateData}
    end;

open({bind_transmitter, ParamList}, From, StateData) ->
    case send_bind_transmitter(ParamList, From, StateData) of
        {ok, NewStateData} ->
			reset_timer(StateData#state.enquire_link_timer),
            {next_state, open, NewStateData};
        Error ->
            {reply, Error, open, StateData}
    end;

open({bind_transceiver, ParamList}, From, StateData) ->
    case send_bind_transceiver(ParamList, From, StateData) of
        {ok, NewStateData} ->
			reset_timer(StateData#state.enquire_link_timer),
            {next_state, open, NewStateData};
        Error ->
            {reply, Error, open, StateData}
    end;

open(close, _From, #state{connection = C} = StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    cancel_timer(StateData#state.enquire_link_timer),
    {reply, gen_connection:close(C), closed, StateData};

open({listen, Port}, _From, #state{connection = C} = StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    cancel_timer(StateData#state.enquire_link_timer),
    {reply, gen_connection:listen(C, Port), listening, StateData};

open({open, A, P}, _From, #state{connection=C, response_time=T} = StateData) ->
    case gen_connection:connect(C, A, P, T) of
        ok ->
            reset_timer(StateData#state.session_init_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {reply, ok, open, StateData};
        Error ->
            {reply, Error, open, StateData}
    end;

open(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, open, StateData}.


%%%
% @spec outbound(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name outbound.
% @end
%%
outbound({bind_receiver, ParamList}, From, StateData) ->
    case send_bind_receiver(ParamList, From, StateData) of
        {ok, NewStateData} ->
			reset_timer(StateData#state.enquire_link_timer),
            {next_state, outbound, NewStateData};
        Error ->
            {reply, Error, outbound, StateData}
    end;

outbound({bind_transmitter, ParamList}, From, StateData) ->
    case send_bind_transmitter(ParamList, From, StateData) of
        {ok, NewStateData} ->
			reset_timer(StateData#state.enquire_link_timer),
            {next_state, outbound, NewStateData};
        Error ->
            {reply, Error, outbound, StateData}
    end;

outbound({bind_transceiver, ParamList}, From, StateData) ->
    case send_bind_transceiver(ParamList, From, StateData) of
        {ok, NewStateData} ->
			reset_timer(StateData#state.enquire_link_timer),
            {next_state, outbound, NewStateData};
        Error ->
            {reply, Error, outbound, StateData}
    end;

outbound(close, _From, #state{connection = C} = StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    cancel_timer(StateData#state.enquire_link_timer),
    {reply, gen_connection:close(C), closed, StateData};

outbound({listen, Port}, _From, #state{connection = C} = StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    cancel_timer(StateData#state.enquire_link_timer),
    {reply, gen_connection:listen(C, Port), listening, StateData};

outbound({open, A, P}, _From, #state{connection=C,response_time=T}=StateData)->
    reset_timer(StateData#state.session_init_timer),
    reset_timer(StateData#state.enquire_link_timer),
    {reply, gen_connection:connect(C, A, P, T), open, StateData};

outbound(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, outbound, StateData}.


%%%
% @spec bound_rx(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name bound_rx.
% @end
%%
bound_rx({Bind, _ParamList}, _From, StateData) when Bind == bind_receiver;
                                                    Bind == bind_transmitter;
                                                    Bind == bind_transceiver ->
    {reply, {error, ?ESME_RALYBND}, bound_rx, StateData};

bound_rx(unbind, From, StateData) ->
    case send_unbind([], From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_rx, NewStateData};
        Error ->
            {reply, Error, bound_rx, StateData}
    end;

bound_rx(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, bound_rx, StateData}.


%%%
% @spec bound_tx(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name bound_tx.
% @end
%%
bound_tx({Bind, _ParamList}, _From, StateData) when Bind == bind_receiver;
                                                    Bind == bind_transmitter;
                                                    Bind == bind_transceiver ->
    {reply, {error, ?ESME_RALYBND}, bound_tx, StateData};

bound_tx({Request, _}, _From, #state{peer_congestion_state = P} = StateData)
  when is_integer(P) and (P > 90) and ((Request == broadcast_sm)       or 
                                       (Request == data_sm)            or
                                       (Request == query_broadcast_sm) or 
                                       (Request == query_sm)           or
                                       (Request == replace_sm)         or
                                       (Request == submit_multi)       or
                                       (Request == submit_sm)) ->
    % If the other peer is congested the request is not sent.  
    %
    % Notice that following requests won't be dropped unless the other
    % peer notifies that it is still congested.
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    Reply = {error, ?ESME_RTHROTTLED},
    {reply, Reply, bound_tx, StateData#state{peer_congestion_state = 90}};

bound_tx({broadcast_sm, ParamList}, From, StateData) ->
    case send_broadcast_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({cancel_broadcast_sm, ParamList}, From, StateData) ->
    case send_cancel_broadcast_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({cancel_sm, ParamList}, From, StateData) ->
    case send_cancel_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({data_sm, ParamList}, From, StateData) ->
    case send_data_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({query_broadcast_sm, ParamList}, From, StateData) ->
    case send_query_broadcast_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({query_sm, ParamList}, From, StateData) ->
    case send_query_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({replace_sm, ParamList}, From, StateData) ->
    case send_replace_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({submit_multi, ParamList}, From, StateData) ->
    case send_submit_multi(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx({submit_sm, ParamList}, From, StateData) ->
    case send_submit_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx(unbind, From, StateData) ->
    case send_unbind([], From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_tx, NewStateData};
        Error ->
            {reply, Error, bound_tx, StateData}
    end;

bound_tx(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, bound_tx, StateData}.


%%%
% @spec bound_trx(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name bound_trx.
% @end
%%
bound_trx({Bind,_ParamList}, _From, StateData) when Bind == bind_receiver;
                                                    Bind == bind_transmitter;
                                                    Bind == bind_transceiver ->
    {reply, {error, ?ESME_RALYBND}, bound_trx, StateData};

bound_trx({Request, _}, _From, #state{peer_congestion_state = P} = StateData)
  when is_integer(P) and (P > 90) and ((Request == broadcast_sm)       or 
                                       (Request == data_sm)            or
                                       (Request == query_broadcast_sm) or 
                                       (Request == query_sm)           or
                                       (Request == replace_sm)         or
                                       (Request == submit_multi)       or
                                       (Request == submit_sm)) ->
    % If the other peer is congested the request is not sent.  
    %
    % Notice that following requests won't be dropped unless the other
    % peer notifies that it is still congested.
    reset_timer(StateData#state.inactivity_timer),
    reset_timer(StateData#state.enquire_link_timer),
    Reply = {error, ?ESME_RTHROTTLED},
    {reply, Reply, bound_trx, StateData#state{peer_congestion_state = 90}};

bound_trx({broadcast_sm, ParamList}, From, StateData) ->
    case send_broadcast_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({cancel_broadcast_sm, ParamList}, From, StateData) ->
    case send_cancel_broadcast_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({cancel_sm, ParamList}, From, StateData) ->
    case send_cancel_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({data_sm, ParamList}, From, StateData) ->
    case send_data_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({query_broadcast_sm, ParamList}, From, StateData) ->
    case send_query_broadcast_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({query_sm, ParamList}, From, StateData) ->
    case send_query_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({replace_sm, ParamList}, From, StateData) ->
    case send_replace_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({submit_multi, ParamList}, From, StateData) ->
    case send_submit_multi(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx({submit_sm, ParamList}, From, StateData) ->
    case send_submit_sm(ParamList, From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx(unbind, From, StateData) ->
    case send_unbind([], From, StateData) of
        {ok, NewStateData} ->
            reset_timer(StateData#state.inactivity_timer),
            reset_timer(StateData#state.enquire_link_timer),
            {next_state, bound_trx, NewStateData};
        Error ->
            {reply, Error, bound_trx, StateData}
    end;

bound_trx(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, bound_trx, StateData}.


%%%
% @spec unbound(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name unbound.
% @end
%%
unbound(close, _From, #state{connection = C} = StateData) ->
    cancel_timer(StateData#state.enquire_link_timer),
    {reply, gen_connection:close(C), closed, StateData};

unbound({listen, Port}, _From, #state{connection = C} = StateData) ->
    cancel_timer(StateData#state.enquire_link_timer),
    gen_connection:enable_retry(C),
    {reply, gen_connection:listen(C, Port), listening, StateData};

unbound({open, A, P}, _From, #state{connection = C} = StateData) ->
    reset_timer(StateData#state.enquire_link_timer),
    gen_connection:enable_retry(C),
    Reply = gen_connection:connect(C, A, P, StateData#state.response_time),
    Timer = start_timer(StateData#state.session_init_time, session_init_timer),
    {reply, Reply, open, StateData#state{session_init_timer = Timer}};

unbound(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, unbound, StateData}.


%%%
% @spec listening(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name listening.
% @end
%%
listening(close, _From, #state{connection = C} = StateData) ->
    {reply, gen_connection:close(C), closed, StateData};

listening({listen, Port}, _From, #state{connection = C} = StateData) ->
    {reply, gen_connection:listen(C, Port), listening, StateData};

listening({open, A, P}, _From, #state{connection = C} = StateData) ->
    Reply  = gen_connection:connect(C, A, P, StateData#state.response_time),
    ETimer = start_timer(StateData#state.enquire_link_time,enquire_link_timer),
    STimer = start_timer(StateData#state.session_init_time,session_init_timer),
    {reply, Reply, open, StateData#state{enquire_link_timer = ETimer,
                                         session_init_timer = STimer}};

listening(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, listening, StateData}.


%%%
% @spec closed(Event, From, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events for the state name closed.
% @end
%%
closed(close, _From, StateData) ->
    {reply, ok, closed, StateData};

closed({listen, Port}, _From, #state{connection = C} = StateData) ->
    gen_connection:enable_retry(C),
    {reply, gen_connection:listen(C, Port), listening, StateData};

closed({open, A, P}, _From, #state{connection = C} = StateData) ->
    gen_connection:enable_retry(C),
    Reply  = gen_connection:connect(C, A, P, StateData#state.response_time),
    ETimer = start_timer(StateData#state.enquire_link_time,enquire_link_timer),
    STimer = start_timer(StateData#state.session_init_time,session_init_timer),
    {reply, Reply, open, StateData#state{enquire_link_timer = ETimer,
                                         session_init_timer = STimer}};

closed(_Event, _From, StateData) ->
    {reply, {error, ?ESME_RINVBNDSTS}, closed, StateData}.


%%%
% @spec handle_event(Event, StateName, StateData) -> Result
%    Event         = die | term()
%    StateName     = atom()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                         
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events received by <tt>gen_fsm:send_all_state_event/2</tt>.
% @end
%%
handle_event({input, BinaryPdu, Lapse, Index}, StateName, StateData) ->
    Timestamp = now(),
    case catch operation:esme_unpack(BinaryPdu) of
        {ok, Pdu} ->
            handle_input_correct_pdu(Pdu, StateData),
            NewStateData = 
                case StateName of
                    bound_rx ->
                        % As a receiver, we care about our own congestion state
                        Time = 2 * my_calendar:time_since(Timestamp),
                        Scs  = congestion_state(Lapse, Index, Time),
                        StateData#state{self_congestion_state = Scs};
                    bound_tx ->
                        % As a transmitter, we care about MC's congestion state
                        Pcs  = operation:get_param(congestion_state, Pdu),
                        Time = 2 * my_calendar:time_since(Timestamp),
                        StateData#state{peer_congestion_state = Pcs};
                    bound_trx ->
                        % Transceivers care about both congestion states
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
            handle_input_corrupt_pdu(undefined,?ESME_RUNKNOWNERR,0,StateData),
            {next_state, StateName, StateData}
    end;

handle_event(accept, StateName, StateData) ->
    STimer = start_timer(StateData#state.session_init_time,session_init_timer),
    ETimer = start_timer(StateData#state.enquire_link_time,enquire_link_timer),
    {next_state, open, StateData#state{session_init_timer = STimer,
                                       enquire_link_timer = ETimer}};

handle_event({connect_error, Addr, Port}, _StateName, StateData) ->
    cancel_timer(StateData#state.session_init_timer),
    cancel_timer(StateData#state.inactivity_timer),
    cancel_timer(StateData#state.enquire_link_timer),
    spawn(fun() -> callback_mc_unavailable(Addr, Port, StateData) end),
    {next_state, closed, StateData#state{self_congestion_state = 0}};

handle_event({connect_recovery, Addr, Port}, closed, StateData) ->
    STimer = start_timer(StateData#state.session_init_time,session_init_timer),
    ETimer = start_timer(StateData#state.enquire_link_time,enquire_link_timer),
    spawn(fun() -> callback_resume_service(Addr, Port, StateData) end),
    {next_state, open, StateData#state{session_init_timer = STimer,
                                       enquire_link_timer = ETimer}};

handle_event({connect_recovery, Addr, Port}, StateName, StateData) ->
    {next_state, StateName, StateData};

handle_event(die, StateName, #state{connection = C} = StateData) ->
    stop_connection(C),
    {next_state, StateName, StateData};

handle_event({enquire_link, Pdu}, StateName, StateData) ->
    reset_timer(StateData#state.enquire_link_timer),
    SeqNum = operation:get_param(sequence_number, Pdu),
    send_enquire_link_resp(?ESME_ROK, SeqNum, [], StateData#state.connection),
    {next_state, StateName, StateData};

handle_event({listen_error, Port}, _StateName, StateData) ->
    spawn(fun() -> callback_smpp_listen_error(Port, StateData) end),
    {next_state, closed, StateData};

handle_event({listen_recovery, Port}, closed, StateData) ->
    spawn(fun() -> callback_smpp_listen_recovery(Port, StateData) end),
    {next_state, listening, StateData};

handle_event({listen_recovery, Port}, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%%
% @doc Auxiliary function for handle_event/3
%
% <p>This function handles an input PDU when the unpacking operation was
% successful.  Used only on the <tt>input</tt> event.</p>
%
% @see handle_input_corrupt_pdu/3
% @end
%%
handle_input_correct_pdu(Pdu, StateData) ->
%    io:format("Correct input. PDU = ~p~n", [Pdu]),
    case operation:get_param(command_id, Pdu) of
        RespCmdId when RespCmdId > 16#80000000 ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            CmdId  = operation:request_command_id(RespCmdId),
            case ets:lookup(StateData#state.requests, SeqNum) of
                [{SeqNum, CmdId, Broker}] ->   % Expected response
                    Broker ! {self(), {response, Pdu}},
                    ets:delete(StateData#state.requests, SeqNum);
                _Otherwise ->                  % Unexpected response
                    Cid = StateData#state.connection,
                    send_generic_nack(?ESME_RINVCMDID, SeqNum, [], Cid)
            end;
        ?COMMAND_ID_GENERIC_NACK ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            case ets:lookup(StateData#state.requests, SeqNum) of
                [{SeqNum, _CmdId, Broker}] ->  % Expected response
                    Broker ! {self(), {response, Pdu}},
                    ets:delete(StateData#state.requests, SeqNum);
                _Otherwise ->                  % Unexpected response
                    % Do not send anything, might enter a request/response loop
                    true
            end;
        ?COMMAND_ID_ALERT_NOTIFICATION ->
            alert_notification(self(), Pdu);
        ?COMMAND_ID_DATA_SM ->
            deliver_data_sm(self(), Pdu);
        ?COMMAND_ID_DELIVER_SM ->
            deliver_sm(self(), Pdu);
        ?COMMAND_ID_ENQUIRE_LINK ->
            enquire_link(self(), Pdu);
        ?COMMAND_ID_OUTBIND ->
            outbind(self(), Pdu);
        ?COMMAND_ID_UNBIND ->
            unbind(self(), Pdu);
        _OtherRequest ->
            SeqNum = operation:get_param(sequence_number, Pdu),
            Cid    = StateData#state.connection,
            send_generic_nack(?ESME_RINVCMDID, SeqNum, [], Cid)
    end.

%%%
% @doc Auxiliary function for handle_event/3
%
% <p>This function handles an input PDU when the unpacking operation was
% unsuccessful.  Used only on the <tt>input</tt> event.</p>
%
% @see handle_input_correct_pdu/2
% @end
%%
handle_input_corrupt_pdu(CmdId, Status, SeqNum, StateData) ->
%    io:format("Corrupt input.~nCmdId = ~p~nStatus = ~p~nSeqNum = ~p~n", [CmdId, Status, SeqNum]),
    Cid = StateData#state.connection,
    if
        CmdId == ?COMMAND_ID_GENERIC_NACK ->
            % Do not send anything, might enter a request/response loop
            true;
        CmdId == ?COMMAND_ID_DATA_SM ->
            send_deliver_data_sm_resp(Status, SeqNum, [], Cid);
        CmdId == ?COMMAND_ID_DELIVER_SM ->
            send_deliver_sm_resp(Status, SeqNum, [], Cid);
        true ->
            send_generic_nack(Status, SeqNum, [], Cid)
    end.

%%%
% @doc Auxiliary function for handle_event/3
%
% <p>Computes the congestion state.  Used only on the <tt>input</tt> event.
% <tt>Time</tt> indicates the microseconds to dispatch a PDU.  <tt>Lapse
% </tt></p>
%
% <dl>
%   <dt>Lapse: </dt><dd>Are the microseconds waiting for the input buffer.</dd>
%   <dt>Time: </dt><dd>Microseconds to dispatch a PDU.</dd>
%   <dt>Index: </dt><dd>An input buffer may contain more than 1 PDU.</dd>
% </dl>
% @end
%
% %@see
%%
congestion_state(Lapse, Index, Time) ->
    case catch ((99 * Time) / ((Lapse / Index) + Time)) of 
        Result when float(Result) -> 
            trunc(Result);
        _DivisionByZero -> 
            0
    end.


%%%
% @spec handle_sync_event(Event, From, StateName, StateData) -> Result
%    Event         = term()
%    From          = {pid(), Tag}
%    StateName     = atom()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}            |
%                    {next_state, NextStateName, NextStateData, Timeout}   |
%                    {reply, Reply, NextStateName, NextStateData}          |
%                    {reply, Reply, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                          |
%                    {stop, Reason, Reply, NewStateData}                    
%    Reply         = term()
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Handles events received via 
% <tt>gen_fsm:sync_send_all_state_event/2,3</tt>.
% @end
%%
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.


%%%
% @spec handle_info(Info, StateName, StateData) -> Result
%    Info          = term()
%    StateName     = atom()
%    StateData     = term()
%    Result        = {next_state, NextStateName, NextStateData}          |
%                    {next_state, NextStateName, NextStateData, Timeout} |
%                    {stop, Reason, NewStateData}                         
%    NextStateName = atom()
%    NextStateData = term()
%    Timeout       = int() | infinity
%    Reason        = term()
%
% @doc Call on reception of any other messages than a synchronous or
% asynchronous event.
% @end
%%
handle_info({'EXIT', C, R}, _StateName, #state{connection = C} = StateData) ->
    % If the underlying connection terminates, the session must be stopped.
%    io:format("Underlying connection was stopped~n", []),
    {stop, R, StateData#state{connection = undefined}};

handle_info({'EXIT', _Child, R}, StateName, StateData) when R /= normal ->
    % A child process (request broker) terminates abnormally.
    stop_connection(StateData#state.connection),
    {next_state, StateName, StateData};

handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%%
% @spec terminate(Reason, StateName, StateData) -> true
%    Reason    = normal | shutdown | term()
%    StateName = atom()
%    StateData = term()
%
% @doc Shutdown the fsm.
%
% <p>Return value is ignored by the server.</p>
% @end
%%
terminate(_Reason, _StateName, #state{self = S} = _StateData) ->
    case process_info(S, registered_name) of
        {registered_name, Name} ->
            unregister(Name);
        _NotRegistered ->
            true
    end.


%%%
% @spec code_change(OldVsn, StateName, StateData, Extra) -> Result
%    OldVsn        = undefined | term()
%    StateName     = term()
%    StateData     = term()
%    Extra         = term()
%    Result        = {ok, NextStateName, NewStateData}
%    NextStateName = atom()
%    NewStateData  = term()
%
% @doc Convert process state when code is changed
% @end
%%
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.


%%%===================================================================
% Server gen_connection functions
%%====================================================================
%%%
% @spec handle_accept(Pid, Cid) -> ok
%    Pid = pid()
%    Cid = pid()
%
% @doc Whenever an incoming connection is accepted under a listening state, 
% the gen_connection turns into connect state and triggers this callback to 
% notify such an event to the parent of the connection. <tt>Cid</tt> is
% the pid of the connection, <tt>Pid</tt> is the connection's 
% parent pid.
% @end
%
% %@see
%
% %@equiv
%%
handle_accept(Pid, Cid) ->
    gen_fsm:send_all_state_event(Pid, accept).
     

%%%
% @spec handle_input(Pid, Cid, Input, Lapse) -> {ok, RestBuffer}
%    Pid = pid()
%    Cid = pid()
%    Input = binary()
%    RestBuffer = binary()
%    Lapse = int()
%
% @doc Every time the connection receives new data, this function is called to 
% let the callback function consume data from the <tt>Input</tt> and from
% the stored <tt>Buffer</tt>.  This function should return the unused data.
% <tt>Cid</tt> is the pid of the connection, <tt>Pid</tt> is the
% connection's parent pid.
%
% <p><tt>Lapse</tt> are the microseconds waited until input came.</p>
% @end
%
% %@see
%
% %@equiv
%%
handle_input(Pid, Cid, Buffer, Lapse) ->
    handle_input(Pid, Cid, Buffer, Lapse, 1).


handle_input(Pid, Cid, <<CommandLength:32, Rest/binary>> = Buffer, Lapse, N) ->
    Len = CommandLength - 4,
    case Rest of
        <<PduRest:Len/binary-unit:8, NextPdus/binary>> -> 
            BinaryPdu = <<CommandLength:32, PduRest/binary>>,
            gen_fsm:send_all_state_event(Pid, {input, BinaryPdu, Lapse, N}),
            % The buffer may carry more than one SMPP PDU.
            handle_input(Pid, Cid, NextPdus, Lapse, N + 1);
        _IncompletePdu ->
            {ok, Buffer}
    end;

handle_input(_Pid, _Cid, Buffer, _Lapse, _N) ->
    {ok, Buffer}.


%%%
% @spec handle_listen_error(Pid, Cid, Port) -> ok
%    Port = int()
%    Pid = pid()
%    Cid = pid()
%
% @doc If the connection is listening on port <tt>Port</tt> and an error
% occurs, this function reports the failure to the callback module so it can
% take the appropriate actions.  The connection may try to recover itself
% every <tt>#state.retry_timer</tt> milliseconds, if this timeout wasn't
% set to the atom <tt>infinity</tt>, in such a case the callback module
% should take care of this situation. <tt>Cid</tt> is the pid of the
% connection, <tt>Pid</tt> is the connection's parent pid.
%
% <p>Return value is ignored by the connection.</p>
% @end
%
% %@see
%
% %@equiv
%%
handle_listen_error(Pid, _Cid, Port) ->
    gen_fsm:send_all_state_event(Pid, {listen_error, Port}).


%%%
% @spec handle_connect_error(Pid, Cid, Address, Port) -> ok
%    Address = string() | atom() | ip_address()
%    Port = int()
%    Pid = pid()
%    Cid = pid()
%
% @doc If the connection is active (sending/receiving data to/from address
% <tt>Address</tt> on port <tt>Port</tt>) and an error occurs, this 
% function reports the failure to the callback module so it can take the
% appropriate actions.  The connection may try to recover itself every
% <tt>#state.retry_timer</tt> milliseconds, if this timeout wasn't set
% to the atom <tt>infinity</tt>, in such a case the callback module should
% take care of this situation.  <tt>Cid</tt> is the pid of the connection,
% <tt>Pid</tt> is the connection's parent pid.
%
% <p>Return value is ignored by the connection.</p>
% @end
%
% %@see
%
% %@equiv
%%
handle_connect_error(Pid, _Cid, Address, Port) ->
    gen_fsm:send_all_state_event(Pid, {connect_error, Address, Port}).


%%%
% @spec handle_listen_recovery(Pid, Cid, Port) -> ok
%    Port = int()
%    Pid = pid()
%    Cid = pid()
%
% @doc Notifies the callback module than the connection is again, after a 
% failure, up and listening on port <tt>Port</tt>. <tt>Cid</tt> is the
% pid of the connection, <tt>Pid</tt> is the connection's parent pid.
%
% <p>Return value is ignored by the connection.</p>
% @end
%
% %@see
%
% %@equiv
%%
handle_listen_recovery(Pid, _Cid, Port) ->
    gen_fsm:send_all_state_event(Pid, {listen_recovery, Port}).


%%%
% @spec handle_connect_recovery(Pid, Cid, Address, Port) -> ok
%    Address = string() | atom() | ip_address()
%    Port = int()
%    Pid = pid()
%    Cid = pid()
%
% @doc Notifies the callback module than the connection is again, after a 
% failure, up and ready to send/receive data to address <tt>Address</tt>
% on port <tt>Port</tt> . <tt>Cid</tt> is the pid of the connection. 
% <tt>Pid</tt> is the connection's parent pid.
%
% <p>Return value is ignored by the connection.</p>
% @end
%
% %@see
%
% %@equiv
%%
handle_connect_recovery(Pid, _Cid, Address, Port) ->
    gen_fsm:send_all_state_event(Pid, {connect_recovery, Address, Port}).


%%%-------------------------------------------------------------------
% Internal server calls
%%--------------------------------------------------------------------
%%%
% @spec alert_notification(Sid, Pdu) -> ok
%    Sid = pid()
%    Pdu = pdu()
%
% @doc The session identified by <tt>Sid</tt> receives an alert 
% notification.
% @end
% 
% %@see
%
% %@equiv
%%
alert_notification(Sid, Pdu) ->
    gen_fsm:send_event(Sid, {alert_notification, Pdu}).


%%%
% @spec bind_receiver_resp(Sid) -> ok
%    Sid = pid()
%
% @doc The session identified by <tt>Sid</tt> receives a bind receiver
% response.
% @end
% 
% %@see
%
% %@equiv
%%
bind_receiver_resp(Sid) ->
    gen_fsm:send_event(Sid, bind_receiver_resp).


%%%
% @spec bind_transmitter_resp(Sid) -> ok
%    Sid = pid()
%
% @doc The session identified by <tt>Sid</tt> receives a bind transmitter
% response.
% @end
% 
% %@see
%
% %@equiv
%%
bind_transmitter_resp(Sid) ->
    gen_fsm:send_event(Sid, bind_transmitter_resp).


%%%
% @spec bind_transceiver_resp(Sid) -> ok
%    Sid = pid()
%
% @doc The session identified by <tt>Sid</tt> receives a bind transceiver 
% response.
% @end
% 
% %@see
%
% %@equiv
%%
bind_transceiver_resp(Sid) ->
    gen_fsm:send_event(Sid, bind_transceiver_resp).


%%%
% @spec deliver_data_sm(Sid, Pdu) -> ok
%    Sid = pid()
%    Pdu = pdu()
%
% @doc The session identified by <tt>Sid</tt> receives a delivery data_sm 
% request.
% @end
% 
% %@see
%
% %@equiv
%%
deliver_data_sm(Sid, Pdu) ->
    gen_fsm:send_event(Sid, {deliver_data_sm, Pdu}).


%%%
% @spec deliver_sm(Sid, Pdu) -> ok
%    Sid = pid()
%    Pdu = pdu()
%
% @doc The session identified by <tt>Sid</tt> receives a deliver_sm 
% request.
% @end
% 
% %@see
%
% %@equiv
%%
deliver_sm(Sid, Pdu) ->
    gen_fsm:send_event(Sid, {deliver_sm, Pdu}).


%%%
% @spec enquire_link(Sid, Pdu) -> ok
%    Sid = pid()
%    Pdu = pdu()
%
% @doc The session identified by <tt>Sid</tt> receives an enquire_link
% request.
% @end
% 
% %@see
%
% %@equiv
%%
enquire_link(Sid, Pdu) ->
    gen_fsm:send_all_state_event(Sid, {enquire_link, Pdu}).


%%%
% @spec outbind(Sid, Pdu) ->  ok
%    Sid = pid()
%    Pdu = pdu()
%
% @doc The session identified by <tt>Sid</tt> receives an outbind request.
% @end
% 
% %@see
%
% %@equiv
%%
outbind(Sid, Pdu) ->
    gen_fsm:send_event(Sid, {outbind, Pdu}).


%%%
% @spec unbind(Sid, Pdu) ->  ok
%    Sid = pid()
%    Pdu = pdu()
%
% @doc The session identified by <tt>Sid</tt> receives an unbind request.
% @end
% 
% %@see
%
% %@equiv
%%
unbind(Sid, Pdu) ->
    gen_fsm:send_event(Sid, {unbind, Pdu}).


%%%
% @spec unbind_resp(Sid) -> ok
%    Sid = pid()
%
% @doc The session identified by <tt>Sid</tt> receives an unbind response.
% @end
% 
% %@see
%
% %@equiv
%%
unbind_resp(Sid) ->
    gen_fsm:send_event(Sid, unbind_resp).


%%%-------------------------------------------------------------------
% Internal functions
%%--------------------------------------------------------------------
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% PDU send functions
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% @spec send_bind_receiver(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a bind receiver request for the current session. <tt>From</tt>
% represents the caller (ESME) issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
send_bind_receiver(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_bind_receiver(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_bind_transmitter(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a bind transmitter request for the current session.  
% <tt>From</tt> represents the caller (ESME) issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
send_bind_transmitter(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_bind_transmitter(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_bind_transceiver(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a bind transceiver request for the current session.  
% <tt>From</tt> represents the caller (ESME) issuing the request.
% @end
%
% %@see
%
% %@equiv
%%
send_bind_transceiver(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_bind_transceiver(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_broadcast_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Send a broadcast_sm request over the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_broadcast_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_broadcast_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_cancel_broadcast_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a cancel_broadcast_sm request over the current session. 
% <tt>From</tt> represents the caller (ESME) issuing the request (might be
% the atom <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_cancel_broadcast_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_cancel_broadcast_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_cancel_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a cancel_sm request over the current session. <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_cancel_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_cancel_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_data_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a data_sm request over the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_data_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_data_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_deliver_data_sm_resp(Status, SeqNum, ParamList, Cid) -> Result
%    Status     = int()
%    SeqNum     = int()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Cid        = pid()
%    Result     = ok | {error, Error}
%    Error      = int()
%
% @doc Sends a data_sm response over the connection identified by <tt>
% Cid</tt>.  <tt>Status</tt> is the command status and <tt>SeqNum
% </tt> the sequence number of the PDU.
%
% <p>Initial parameter values might be given to the PDU by the 
% <tt>ParamList</tt></p>
% @end
%
% %@see
%
% %@equiv
%%
send_deliver_data_sm_resp(Status, SeqNum, ParamList, Cid) ->
    Pdu = operation:new_data_sm_resp(Status, SeqNum, ParamList),
    send_pdu(Cid, Pdu).


%%%
% @spec send_deliver_sm_resp(Status, SeqNum, ParamList, Cid) -> Result
%    Status     = int()
%    SeqNum     = int()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Cid        = pid()
%    Result     = ok | {error, Error}
%    Error      = int()
%
% @doc Sends a deliver_sm response over the connection identified by <tt>
% Cid</tt>.  <tt>Status</tt> is the command status and <tt>SeqNum
% </tt> the sequence number of the PDU.
%
% <p>Initial parameter values might be given to the PDU by the 
% <tt>ParamList</tt></p>
% @end
%
% %@see
%
% %@equiv
%%
send_deliver_sm_resp(Status, SeqNum, ParamList, Cid) ->
    Pdu = operation:new_deliver_sm_resp(Status, SeqNum, ParamList),
    send_pdu(Cid, Pdu).


%%%
% @spec send_enquire_link(ParamList, From, StateData) -> Result
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Send an enquire link request for the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
%
% <p>This function ignores the <tt>ParamList</tt> argument.</p>
% @end
%
% %@see
%
% %@equiv
%%
send_enquire_link(_ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_enquire_link(N, []) end,
    send_request(Function, From, StateData).


%%%
% @spec send_enquire_link_resp(Status, SeqNum, ParamList, Cid) -> Result
%    Status     = int()
%    SeqNum     = int()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Cid        = pid()
%    Result     = ok | {error, Error}
%    Error      = int()
%
% @doc Send an enquire link response over the connection identified by <tt>
% Cid</tt>.  <tt>Status</tt> is the command status and <tt>SeqNum
% </tt> the sequence number of the PDU.
%
% <p>The argument <tt>ParamList</tt> is ignored by this function.</p>
% @end
%
% %@see
%
% %@equiv
%%
send_enquire_link_resp(Status, SeqNum, _ParamList, Cid) ->
    Pdu = operation:new_enquire_link_resp(Status, SeqNum, []),
    send_pdu(Cid, Pdu).


%%%
% @spec send_generic_nack(Status, SeqNum, ParamList, Cid) -> Result
%    Status     = int()
%    SeqNum     = int()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Cid        = pid()
%    Result     = ok | {error, Error}
%    Error      = int()
%
% @doc Sends a generic nack response.
%
% <p>The argument <tt>ParamList</tt> is ignored by this function.</p>
% @end
%
% %@see
%
% %@equiv
%%
send_generic_nack(Status, SeqNum, _ParamList, Cid) ->
    Pdu = operation:new_generic_nack(Status, SeqNum, []),
    send_pdu(Cid, Pdu).


%%%
% @spec send_query_broadcast_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a query_broadcast_sm request over the current session. 
% <tt>From</tt> represents the caller (ESME) issuing the request (might be
% the atom <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_query_broadcast_sm(ParamList, From, StateData)->
    Function = fun(N) -> operation:new_query_broadcast_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_query_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a query_sm request over the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_query_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_query_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_replace_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Sends a replace_sm request over the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_replace_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_replace_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_submit_multi(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Send a submit_multi request over the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_submit_multi(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_submit_multi(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_submit_sm(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Send a submit_sm request over the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request (might be the atom 
% <tt>undefined</tt>).
% @end
%
% %@see
%
% %@equiv
%%
send_submit_sm(ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_submit_sm(N, ParamList) end,
    send_request(Function, From, StateData).


%%%
% @spec send_unbind(ParamList, From, StateData) -> Result
%    ParamList    = [{ParamName, ParamValue}]
%    ParamName    = atom()
%    ParamValue   = term()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc Send an unbind request for the current session.  <tt>From</tt>
% represents the caller (ESME) issuing the request.
%
% <p>The argument <tt>ParamList</tt> is ignored by this function.</p>
% @end
%
% %@see
%
% %@equiv
%%
send_unbind(_ParamList, From, StateData) ->
    Function = fun(N) -> operation:new_unbind(N, []) end,
    send_request(Function, From, StateData).


%%%
% @spec send_unbind_resp(Status, SeqNum, ParamList, Cid) -> Result
%    Status     = int()
%    SeqNum     = int()
%    ParamList  = [{ParamName, ParamValue}]
%    ParamName  = atom()
%    ParamValue = term()
%    Cid        = pid()
%    Result     = ok | {error, Error}
%    Error      = int()
%
% @doc Send an unbind response over the connection identified by <tt>
% Cid</tt>.  <tt>Status</tt> is the command status and <tt>SeqNum
% </tt> the sequence number of the PDU.
%
% <p>The argument <tt>ParamList</tt> is ignored by this function.</p>
% @end
%
% %@see
%
% %@equiv
%%
send_unbind_resp(Status, SeqNum, _ParamList, Cid) ->
    Pdu = operation:new_unbind_resp(Status, SeqNum, []),
    send_pdu(Cid, Pdu).


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Utility functions
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% @spec send_request(Function, From, StateData) -> Result
%    SeqNum       = int()
%    From         = {pid(), Tag}
%    Tag          = term()
%    StateData    = state()
%    Result       = {ok, NewStateData} | {error, Error}
%    NewStateData = state()
%    Error        = int()
%
% @doc where
% <ul>
%   <li><tt>Function = fun(SeqNum) -> pdu()</tt></li>
% </ul>
%
% <p>Send a SMPP request given the command PDU.  <tt>From</tt> represents
% the caller issuing the request (might be the atom <tt>undefined</tt>).
% </p>
% @end
%
% %@see
%
% %@equiv
%%
send_request(Function, From, StateData) ->
    SeqNum = StateData#state.sequence_number + 1,
    Pdu    = Function(SeqNum),
    case send_pdu(StateData#state.connection, Pdu) of
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


%%%
% @spec send_pdu(Cid, Pdu) -> ok | {error, Reason}
%    Cid = pid()
%    Pdu = pdu()
%
% @doc Send the PDU <tt>Pdu</tt> over the connection <tt>Cid</tt>.
% @end
%
% %@see
%
% %@equiv
%%
send_pdu(Cid, Pdu) ->
%     io:format("Sending PDU: ~p~n", [Pdu]),
    case catch operation:esme_pack(Pdu) of
        {ok, BinaryPdu} ->
            case gen_connection:send(Cid, BinaryPdu) of
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


%%%
% @spec request_broker(Caller, TimeoutError, Time) -> true
%    Caller       = {pid(), Tag}
%    TimeoutError = int()
%    Time         = int()
%
% @doc The request broker waits for <tt>Time</tt> milliseconds until a
% response for the request arrives.  The response is forwarded to the 
% <tt>Caller</tt> of the request, if the <tt>Time</tt> expires
% before any response is received, the term <tt>{error, TimeoutError}</tt> 
% is reported to the <tt>Caller</tt>.
% @end
%
% %@see
%
% %@equiv
%%
request_broker(undefined, TimeoutError, Time) ->
    receive 
        {Sid, {response, Pdu}} ->
            case operation:get_param(command_status, Pdu) of
                ?ESME_ROK ->
                    case operation:get_param(command_id, Pdu) of
                        ?COMMAND_ID_BIND_RECEIVER_RESP ->
                            bind_receiver_resp(Sid);
                        ?COMMAND_ID_BIND_TRANSMITTER_RESP ->
                            bind_transmitter_resp(Sid);
                        ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
                            bind_transceiver_resp(Sid);
                        ?COMMAND_ID_UNBIND_RESP ->
                            unbind_resp(Sid);
                        _OtherResponse ->
                            ok
                    end;
                Error ->
                    {error, Error}
            end
    after Time ->
            {error, TimeoutError}
    end;

request_broker(Caller, TimeoutError, Time) ->
    receive
        {Sid, {response, Pdu}} ->
            case operation:get_param(command_status, Pdu) of
                ?ESME_ROK ->
                    case operation:get_param(command_id, Pdu) of
                        ?COMMAND_ID_BIND_RECEIVER_RESP ->
                            gen_fsm:reply(Caller, {ok, Pdu}),
                            bind_receiver_resp(Sid);
                        ?COMMAND_ID_BIND_TRANSMITTER_RESP ->
                            gen_fsm:reply(Caller, {ok, Pdu}),
                            bind_transmitter_resp(Sid);
                        ?COMMAND_ID_BIND_TRANSCEIVER_RESP ->
                            gen_fsm:reply(Caller, {ok, Pdu}),
                            bind_transceiver_resp(Sid);
                        ?COMMAND_ID_UNBIND_RESP ->
                            gen_fsm:reply(Caller, {ok, Pdu}),
                            unbind_resp(Sid);
                        ?COMMAND_ID_GENERIC_NACK ->
                            gen_fsm:reply(Caller, {error, ?ESME_RUNKNOWNERR});
                        _OtherResponse ->
                            gen_fsm:reply(Caller, {ok, Pdu})
                    end;
                Error ->
                    gen_fsm:reply(Caller, {error, Error})
            end
    after Time ->
            gen_fsm:reply(Caller, {error, TimeoutError})
    end.


%%%
% @spec stop_connection(Connection) -> ok
%    Connection = pid() | undefined
%
% @doc Stops the underlying connection. 
% @end
%
% %@see
%
% %@equiv
%%
stop_connection(undefined) ->
    ok;

stop_connection(Connection) ->
    gen_connection:stop(Connection).    


%%%
% @spec start_timer(Time, Msg) -> Ref
%    Time = int()
%    Msg  = term()
%    Ref  = pid()
%
% @doc Starts a timer, setting the current process as its parent, and returns 
% the PID of the timeout process.
% 
% <p><tt>Time</tt> is the value of the time in milliseconds or the
% atom <tt>infinity</tt>, the later doesn't start any timer.</p>
%
% <p>A timer can only be stopped from the same process who started it.</p>
%
% <p>When the timer expires, the event <tt>Msg</tt> is sent to current
% process via gen_fsm:send_event/2.</p>
% @end
%
% %@see
%
% %@equiv
%
% %@TODO See todo tags at the begining of this file.
%%
start_timer(Time, Msg) ->
    Parent = self(),
    spawn(fun() -> timer_loop(Parent, Time, Msg) end).

%%%
% @doc Auxiliary function for start_timer/2.
% @end
%
% %@see
%%
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


%%%
% @spec cancel_timer(Timer) -> ok
%    Timer = pid()
%
% @doc Aborts the timer process whose PID is <tt>Timer</tt>.  Only 
% the timer's parent is allowed to stop it.
% @end
%
% %@see
%
% %@equiv
%%
cancel_timer(Timer) when pid(Timer) ->
    Timer ! {self(), cancel_timer};

cancel_timer(_Timer) ->  % might be the atom undefined
    ok.


%%%
% @spec reset_timer(Timer) -> ok
%    Timer = pid()
%
% @doc Restarts the timer process whose PID is <tt>Timer</tt>.  Only 
% the timer's parent is allowed to restart it.
% @end
%
% %@see
%
% %@equiv
%%
reset_timer(Timer) when pid(Timer) ->
    Timer ! {self(), reset};

reset_timer(_Timer) ->  % might be the atom undefined
    ok.


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
% Callback wrappers
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% @spec callback_outbind(Pdu, StateData) -> ok
%    Pdu       = pdu()
%    StateData = state()
%
% @doc Wrapper for CallbackModule:outbind/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_outbind(Pdu, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:outbind(Pid, Sid, Pdu) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_unbind(StateData) -> ok | {error, Error}
%    StateData = state()
%    Error     = int()
%
% @doc Wrapper for CallbackModule:unbind/2.
% @end
%
% %@see
%
% %@equiv
%%
callback_unbind(StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:unbind(Pid, Sid) of
        {error, Error} when integer(Error) ->
            {error, Error};
        _Otherwise ->
            ok
    end.

    
%%%
% @spec callback_mc_unavailable(Address, Port, StateData) -> ok
%    Address   = string() | atom() | ip_address()
%    Port      = int()
%    StateData = state()
%
% @doc Wrapper for CallbackModule:mc_unavailable/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_mc_unavailable(Address, Port, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:mc_unavailable(Pid, Sid, Address, Port) of
        _Whatever ->
            ok
    end.
    

%%%
% @spec callback_resume_service(Address, Port, StateData) -> ok
%    Address   = string() | atom() | ip_address()
%    Port      = int()
%    StateData = state()
%
% @doc Wrapper for CallbackModule:resume_service/4.
% @end
%
% %@see
%
% %@equiv
%%
callback_resume_service(Address, Port, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:resume_service(Pid, Sid, Address, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_smpp_listen_error(Port, StateData) -> ok
%    Port      = int()
%    StateData = state()
%
% @doc Wrapper for CallbackModule:smpp_listen_error/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_smpp_listen_error(Port, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:smpp_listen_error(Pid, Sid, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_smpp_listen_recovery(Port, StateData) -> ok
%    Port      = int()
%    StateData = state()
%
% @doc Wrapper for CallbackModule:smpp_listen_recovery/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_smpp_listen_recovery(Port, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:smpp_listen_recovery(Pid, Sid, Port) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_alert_notification(Pdu, StateData) -> ok
%    Pdu       = pdu()
%    StateData = state()
%
% @doc Wrapper for CallbackModule:alert_notification/3.
% @end
%
% %@see
%
% %@equiv
%%
callback_alert_notification(Pdu, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:alert_notification(Pid, Sid, Pdu) of
        _Whatever ->
            ok
    end.


%%%
% @spec callback_deliver_sm(Pdu, StateData) -> Result
%    Pdu        = pdu()
%    StateData  = state()
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
callback_deliver_sm(Pdu, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:deliver_sm(Pid, Sid, Pdu) of
        {ok, ParamList} when list(ParamList) ->
            {ok, ParamList};
        {error, Error, ParamList} when integer(Error), list(ParamList) ->
            {error, Error, ParamList};
        _Otherwise ->
            {error, ?ESME_RUNKNOWNERR, []}
    end.


%%%
% @spec callback_deliver_data_sm(Pdu, StateData) -> Result
%    Pdu        = pdu()
%    StateData  = state()
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
callback_deliver_data_sm(Pdu, StateData) ->
    Mod = StateData#state.callback_module,
    Pid = StateData#state.parent,
    Sid = StateData#state.self,
    case catch Mod:deliver_data_sm(Pid, Sid, Pdu) of
        {ok, ParamList} when list(ParamList) ->
            {ok, ParamList};
        {error, Error, ParamList} when integer(Error), list(ParamList) ->
            {error, Error, ParamList};
        _Otherwise ->
            {error, ?ESME_RUNKNOWNERR, []}
    end.
