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

%%% @doc OSERL global definitions.
%%%
%%% <p>Some global OSERL macros.  Default values may be changed to fit
%%% your particular needs.</p>
%%%
%%% <p>As a guideline, some comments reference the section number of the
%%% document [SMPP 5.0].</p>
%%%
%%%
%%% <h2>References</h2>
%%% <dl>
%%%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification.
%%%     Version 5.0. SMS Forum.
%%%   </dd>
%%% </dl>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique/]
%%% @version 1.0 beta, {17 May 2004} {@time}.
%%% @end
-ifndef(oserl).
-define(oserl, true).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_globals.hrl").

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%% Default Port Number (Standardised by IANA)
%%
%% %@see section 2.2 on [SMPP 5.0]
-define(DEFAULT_SMPP_PORT, 2775).


%% Timers default values
%%
%% %@doc Besides the timers declared on [SMPP 5.0], a rebind timer default is
%% defined.  This timer sets the time lapse in which the ESME should try to
%% rebind once the MC becomes unavailable.</p>
%%
%% %@TODO Review these default values.
%%
%% %@see section 2.7 on [SMPP 5.0]
-define(SESSION_INIT_TIME, 180000).  % 3 minutes
-define(ENQUIRE_LINK_TIME,  60000).  % 1 minute
-define(INACTIVITY_TIME, infinity).  % No timeout, never drop the session.
-define(RESPONSE_TIME,      60000).  % 1 minute


%% Timers record
-define(DEFAULT_SMPP_TIMERS, #timers{}).

-define(TIMERS(STime, ETime, ITime, RTime),
        #timers{session_init_time = STime,
                enquire_link_time = ETime,
                inactivity_time   = ITime,
                response_time     = RTime}).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {timers,
%%         SessionInitTime,
%%         EnquireLinkTime,
%%         InactivityTime,
%%         ResponseTime}
%%    SessionInitTime = int() | infinity
%%    EnquireLinkTime = int() | infinity
%%    InactivityTime  = int() | infinity
%%    ResponseTime    = int() | infinity
%%
%% %@doc The times are expressed in milliseconds.  The atom <tt>infinity
%% </tt> disables the timer.
%%
%% <dl>
%%   <dt>SessionInitTime: </dt><dd>Session init timer.  An ESME will close the 
%%     MC-initiated connection if the MC fails to issue an outbind within the
%%     defined period of time.
%%
%%     <p>On expiration, close the connection  (default value is 
%%     ?SESSION_INIT_TIME).</p>
%%   </dd>
%%   <dt>EnquireLinkTime: </dt><dd>Enquire link timer. Time lapse allowed 
%%     between operations after which a SMPP entity should interrogate whether 
%%     its peer still has an active session.
%%
%%     <p>On expiration an enquire_link request should be initiated (default
%%     value is ?ENQUIRE_LINK_TIME).</p>
%%   </dd>
%%   <dt>InactivityTime: </dt><dd>Inactivity timer.  Maximum time lapse allowed
%%     between transactions.
%%
%%     <p>On expiration, close the session or issue an unbind request (default
%%     value is ?INACTIVITY_TIME).</p>
%%   </dd>
%%   <dt>ResponseTime: </dt><dd>Response timer. Time lapse allowed between a 
%%     SMPP request and the corresponding SMPP response.
%%
%%     <p>On expiration assume the operation have failed  (default value is
%%     ?RESPONSE_TIME).</p>
%%   </dd>
%% </dl>
%% %@end
-record(timers, 
        {session_init_time = ?SESSION_INIT_TIME,
         enquire_link_time = ?ENQUIRE_LINK_TIME,
         inactivity_time   = ?INACTIVITY_TIME,
         response_time     = ?RESPONSE_TIME}).

-endif.  % -ifndef(oserl)
