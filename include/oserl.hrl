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

%%% @doc OSERL Setup records.
%%%
%%% <p>Header for oserl's behaviours setup records.  Includes required files 
%%% and defines behaviours setup records.</p>
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
-include("smpp_base.hrl").

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%% Timers
-define(DEFAULT_TIMERS, #timers{}).

-define(TIMERS(STime, ETime, ITime, RTime, RetryT),
        #timers{session_init_time = STime,
                enquire_link_time = ETime,
                inactivity_time   = ITime,
                response_time     = RTime,
                retry_time        = RetryT}).

%% Session
-define(DEFAULT_SESSION_SETUP, #session_setup{}).
-define(SESSION_SETUP(STime, ETime, ITime, RTime, RetryT),
        #session_setup{session_init_time = STime,
                       enquire_link_time = ETime,
                       inactivity_time   = ITime,
                       response_time     = RTime,
                       retry_time        = RetryT}).
-define(GET_ESME_SESSION_SETUP(ESMESetup),
        ?SESSION_SETUP(ESMESetup#esme_setup.session_init_time,
                       ESMESetup#esme_setup.enquire_link_time,
                       ESMESetup#esme_setup.inactivity_time,
                       ESMESetup#esme_setup.response_time,
                       ESMESetup#esme_setup.rebind_time)).
-define(GET_SMSC_SESSION_SETUP(SMSCSetup),
        ?SESSION_SETUP(SMSCSetup#mc_setup.session_init_time,
                       SMSCSetup#mc_setup.enquire_link_time,
                       SMSCSetup#mc_setup.inactivity_time,
                       SMSCSetup#mc_setup.response_time,
                       SMSCSetup#mc_setup.rebind_time)).

% ESME
-define(DEFAULT_ESME_SETUP, #esme_setup{}).
-define(ESME_SETUP(SystemId, Password, AddressRange, SourceAddr),
        #esme_setup{system_id     = SystemId,
                    password      = Password,
                    address_range = AddressRange,
                    source_addr   = SourceAddr}).

% SMSC
-define(DEFAULT_SMSC_SETUP, #smsc_setup{}).
-define(SMSC_SETUP(SystemId, Password),
        #smsc_setup{system_id = SystemId, password = Password}).


%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {timers,
%%         SessionInitTime,
%%         EnquireLinkTime,
%%         InactivityTime,
%%         ResponseTime,
%%         RetryTime}
%%    SessionInitTime = int() | infinity
%%    EnquireLinkTime = int() | infinity
%%    InactivityTime  = int() | infinity
%%    ResponseTime    = int() | infinity
%%    RetryTime       = int() | infinity
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
%%   <dt>RetryTime: </dt><dd>This timer sets the time lapse in which the ESME
%%     should try to reconnect once the MC becomes unavailable.
%%
%%     <p>On expiration a new connection attempt is issued.  (default value is
%%     ?REBIND_TIME).</p>
%%   </dd>
%% </dl>
%% %@end
-record(timers, 
        {session_init_time = ?SESSION_INIT_TIME,
         enquire_link_time = ?ENQUIRE_LINK_TIME,
         inactivity_time   = ?INACTIVITY_TIME,
         response_time     = ?RESPONSE_TIME,
         retry_time        = ?REBIND_TIME}).


%% %@spec {session_setup,
%%         SessionInitTime,
%%         EnquireLinkTime,
%%         InactivityTime,
%%         ResponseTime,
%%         RetryTime}
%%    SessionInitTime = int() | infinity
%%    EnquireLinkTime = int() | infinity
%%    InactivityTime  = int() | infinity
%%    ResponseTime    = int() | infinity
%%    RetryTime       = int() | infinity
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
%%   <dt>RetryTime: </dt><dd>This timer sets the time lapse in which the ESME
%%     should try to reconnect once the MC becomes unavailable.
%%
%%     <p>On expiration a new connection attempt is issued.  (default value is
%%     ?REBIND_TIME).</p>
%%   </dd>
%% </dl>
%% %@end
-record(session_setup, 
        {session_init_time = ?SESSION_INIT_TIME,
         enquire_link_time = ?ENQUIRE_LINK_TIME,
         inactivity_time   = ?INACTIVITY_TIME,
         response_time     = ?RESPONSE_TIME,
         retry_time        = ?REBIND_TIME}).


%% %@spec {esme_setup,
%%         SystemId,
%%         Password,
%%         AddrTon,
%%         AddrNpi,
%%         AddressRange,
%%         SourceAddrTon,
%%         SourceAddrNpi,
%%         SourceAddr,
%%         ServiceType,
%%         SystemType,
%%         SessionInitTime, 
%%         EnquireLinkTime, 
%%         InactivityTime, 
%%         ResponseTime,
%%         RebindTime}
%%    SystemId        = string()
%%    Password        = string()
%%    AddrTon         = int()
%%    AddrNpi         = int()
%%    AddressRange    = string()
%%    SourceAddrTon   = int()
%%    SourceAddrNpi   = int()
%%    SourceAddr      = string()
%%    ServiceType     = string()
%%    SystemType      = string()
%%    SessionInitTime = int() | infinity 
%%    EnquireLinkTime = int() | infinity 
%%    InactivityTime  = int() | infinity 
%%    ResponseTime    = int() | infinity 
%%    RebindTime      = int() | infinity 
%%
%% %@doc ESME setup record.
%%
%% <dl>
%%   <dt>SystemId: </dt><dd>ESME identifier.  C-octet string (default value is 
%%     ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>Password: </dt><dd>ESME password.  C-octet string (default value is 
%%     ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>AddrTon: </dt><dd>ESME address TON.  Integer (default value is
%%     ?TON_INTERNATIONAL).
%%   </dd>
%%   <dt>AddrNpi: </dt><dd>ESME address NPI.  Integer (default value is
%%      ?NPI_ISDN).
%%   </dd>
%%   <dt>AddressRange: </dt><dd>Address range the ESME serves.  C-Octet string
%%     (default value is ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>SourceAddrTon: </dt><dd>ESME source address TON.  Integer (default
%%     value is ?TON_INTERNATIONAL).
%%   </dd>
%%   <dt>SourceAddrNpi: </dt><dd>ESME source address NPI.  Integer (default 
%%      value is ?NPI_ISDN).
%%   </dd>
%%   <dt>SourceAddr: </dt><dd>ESME source address.  C-Octet string
%%     (default value is ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>ServiceType: </dt><dd>ESME service type.  C-Octet string (default
%%     value is ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>SystemType: </dt><dd>ESME system type.  C-Octet string (default
%%     value is ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>SessionInitTime: </dt><dd>On a MC, this timer specifies the time lapse
%%     between a network connection being established by an ESME (or an outbind
%%     request being sent to an ESME) and a bind request being sent to the MC. 
%%
%%     <p>An ESME will close the MC-initiated connection if the MC fails to 
%%     issue an outbind within the defined period of time.</p>
%%
%%     <p>On expiration, close the connection  (default value is 
%%     ?SESSION_INIT_TIME).</p>
%%   </dd>
%%   <dt>EnquireLinkTime: </dt><dd>Time lapse allowed between operations 
%%     after which a SMPP entity should interrogate whether its peer still has
%%     an active session.
%%
%%     <p>On expiration an enquire_link request should be initiated (default
%%     value is ?ENQUIRE_LINK_TIME).</p>
%%   </dd>
%%   <dt>InactivityTime: </dt><dd>Maximun time lapse allowed between 
%%     transactions.
%%
%%     <p>On expiration, close the session or issue an unbind request (default
%%     value is ?INACTIVITY_TIME).</p>
%%   </dd>
%%   <dt>ResponseTime: </dt><dd>Time lapse allowed between a SMPP request and 
%%     the corresponding SMPP response
%%
%%     <p>On expiration assume the operation have failed  (default value is
%%     ?RESPONSE_TIME).</p>
%%   </dd>
%%   <dt>RebindTime: </dt><dd>Time lapse allowed between a SMPP request and 
%%     the corresponding SMPP response
%%
%%     <p>On expiration assume the operation have failed  (default value is
%%     ?REBIND_TIME).</p>
%%   </dd>
%% </dl>
%% %@end
-record(esme_setup, 
        {system_id         = ?NULL_C_OCTET_STRING,
         password          = ?NULL_C_OCTET_STRING,
         addr_ton          = ?TON_INTERNATIONAL,
         addr_npi          = ?NPI_ISDN,
         address_range     = ?NULL_C_OCTET_STRING,
         source_addr_ton   = ?TON_INTERNATIONAL,
         source_addr_npi   = ?NPI_ISDN,
         source_addr       = ?NULL_C_OCTET_STRING,
         service_type      = ?SERVICE_TYPE_NULL,
         system_type       = ?NULL_C_OCTET_STRING,
         session_init_time = ?SESSION_INIT_TIME, 
         enquire_link_time = ?ENQUIRE_LINK_TIME,
         inactivity_time   = ?INACTIVITY_TIME,
         response_time     = ?RESPONSE_TIME,
         rebind_time       = ?REBIND_TIME}).


%% %@spec {smsc_setup, SystemId, Password, Port, Timers}
%%    SystemId = string()
%%    Password = string()
%%    Timers   = #timers()
%%
%% %@doc SMSC setup record.
%%
%% <dl>
%%   <dt>SystemId: </dt><dd>SMSC identifier.  C-octet string (default value is
%%     ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>Password: </dt><dd>SMSC password.  C-octet string (default value is
%%     ?NULL_C_OCTET_STRING).
%%   </dd>
%%   <dt>Timers: </dt><dd>SMPP Timers.</dd>
%% </dl>
%% %@end
-record(smsc_setup, 
        {system_id = ?NULL_C_OCTET_STRING,
         password  = ?NULL_C_OCTET_STRING,
         timers    = #timers()}).

-endif.  % -ifndef(oserl)
