%%%
% Copyright (C) 2003 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
% @doc Generic ESME SMPP Session Header.
%
% <p>Header for the gen_esme_session behaviour.  Includes required files
% and defines the behaviour setup record.</p>
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {17 Jul 2003} {@time}.
% @end
%%

-ifndef(gen_esme_session).
-define(gen_esme_session, true).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("smpp_globals.hrl").

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
-define(DEFAULT_GEN_ESME_SESSION_SETUP, #gen_esme_session_setup{}).

-define(GEN_ESME_SESSION_SETUP(STime, ETime, ITime, RTime, RetryT),
        #gen_esme_session_setup{session_init_time = STime,
                                enquire_link_time = ETime,
                                inactivity_time   = ITime,
                                response_time     = RTime,
                                retry_time        = RetryT}).

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%
% %@spec {gen_esme_session_setup,
%         SessionInitTime,
%         EnquireLinkTime,
%         InactivityTime,
%         ResponseTime,
%         RetryTime}
%    SessionInitTime = int() | infinity
%    EnquireLinkTime = int() | infinity
%    InactivityTime  = int() | infinity
%    ResponseTime    = int() | infinity
%    RetryTime       = int() | infinity
%
% %@doc The times are expressed in milliseconds.  The atom <tt>infinity
% </tt> disables the timer.
%
% <dl>
%   <dt>SessionInitTime: </dt><dd>Session init timer.  An ESME will close the 
%     MC-initiated connection if the MC fails to issue an outbind within the
%     defined period of time.
%
%     <p>On expiration, close the connection  (default value is 
%     ?SESSION_INIT_TIME).</p>
%   </dd>
%   <dt>EnquireLinkTime: </dt><dd>Enquire link timer. Time lapse allowed 
%     between operations after which a SMPP entity should interrogate whether 
%     its peer still has an active session.
%
%     <p>On expiration an enquire_link request should be initiated (default
%     value is ?ENQUIRE_LINK_TIME).</p>
%   </dd>
%   <dt>InactivityTime: </dt><dd>Inactivity timer.  Maximum time lapse allowed
%     between transactions.
%
%     <p>On expiration, close the session or issue an unbind request (default
%     value is ?INACTIVITY_TIME).</p>
%   </dd>
%   <dt>ResponseTime: </dt><dd>Response timer. Time lapse allowed between a 
%     SMPP request and the corresponding SMPP response.
%
%     <p>On expiration assume the operation have failed  (default value is
%     ?RESPONSE_TIME).</p>
%   </dd>
%   <dt>RetryTime: </dt><dd>This timer sets the time lapse in which the ESME
%     should try to reconnect once the MC becomes unavailable.
%
%     <p>On expiration a new connection attempt is issued.  (default value is
%     ?REBIND_TIME).</p>
%   </dd>
% </dl>
% %@end
%%
-record(gen_esme_session_setup, 
        {session_init_time = ?SESSION_INIT_TIME,
         enquire_link_time = ?ENQUIRE_LINK_TIME,
         inactivity_time   = ?INACTIVITY_TIME,
         response_time     = ?RESPONSE_TIME,
         retry_time        = ?REBIND_TIME}).


-endif.  % -ifndef(gen_esme_session)
