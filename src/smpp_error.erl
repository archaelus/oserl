%%% Copyright (C) 2005 Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%
%%% This program is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation; either version 2 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA

%%% @doc SMPP Error
%%%
%%% <p>Standard SMPP Error description.</p>
%%%
%%% @copyright 2005 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.2, {28 Jun 2005} {@time}.
%%% @end
-module(smpp_error).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_globals.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([format/1]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
%-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec format(CommandStatus) -> Description
%%    CommandStatus = int()
%%    Description = string()
%%
%% @doc Given the <tt>CommandStatus</tt> returned by any SMPP operation, it
%% returns a descriptive string of the error in English (as described in the
%% SMPP specification V5.0).
%% @end 
format(?ESME_ROK) ->
    "No Error.";
format(?ESME_RINVMSGLEN) ->
    "Message Length is invalid.";
format(?ESME_RINVCMDLEN) ->
    "Command Length is invalid.";
format(?ESME_RINVCMDID) ->
    "Invalid Command ID.";
format(?ESME_RINVBNDSTS) ->
    "Incorrect BIND Status for given command.";
format(?ESME_RALYBND) ->
    "ESME Already in Bound State.";
format(?ESME_RINVPRTFLG) ->
    "Invalid Priority Flag.";
format(?ESME_RINVREGDLVFLG) ->
    "Invalid Registered Delivery Flag.";
format(?ESME_RSYSERR) ->
    "System Error.";
format(?ESME_RINVSRCADR) ->
    "Invalid Source Address.";
format(?ESME_RINVDSTADR) ->
    "Invalid Destination Address.";
format(?ESME_RINVMSGID) ->
    "Message ID is invalid.";
format(?ESME_RBINDFAIL) ->
    "Bind Failed.";
format(?ESME_RINVPASWD) ->
    "Invalid Password.";
format(?ESME_RINVSYSID) ->
    "Invalid System ID.";
format(?ESME_RCANCELFAIL) ->
    "Cancel SM Failed.";
format(?ESME_RREPLACEFAIL) ->
    "Replace SM Failed.";
format(?ESME_RMSGQFUL) ->
    "Message Queue Full.";
format(?ESME_RINVSERTYP) ->
    "Invalid Service Type.";
format(?ESME_RINVNUMDESTS) ->
    "Invalid number of destinations.";
format(?ESME_RINVDLNAME) ->
    "Invalid Distribution List name.";
format(?ESME_RINVDESTFLAG) ->
    "Destination flag is invalid (submit_multi).";
format(?ESME_RINVSUBREP) ->
    "Submit w/replace functionality has been requested where it is either unsupported or inappropriate for the particular MC.";
format(?ESME_RINVESMCLASS) ->
    "Invalid esm_class field data.";
format(?ESME_RCNTSUBDL) ->
    "Cannot Submit to Distribution List.";
format(?ESME_RSUBMITFAIL) ->
    "submit_sm, data_sm or submit_multi failed.";
format(?ESME_RINVSRCTON) ->
    "Invalid Source address TON.";
format(?ESME_RINVSRCNPI) ->
    "Invalid Source address NPI.";
format(?ESME_RINVDSTTON) ->
    "Invalid Destination address TON.";
format(?ESME_RINVDSTNPI) ->
    "Invalid Destination address NPI.";
format(?ESME_RINVSYSTYP) ->
    "Invalid system_type field.";
format(?ESME_RINVREPFLAG) ->
    "Invalid replace_if_present flag.";
format(?ESME_RINVNUMMSGS) ->
    "Invalid number of messages.";
format(?ESME_RTHROTTLED) ->
    "Throttling error (ESME has exceeded allowed message limits).";
format(?ESME_RINVSCHED) ->
    "Invalid Scheduled Delivery Time.";
format(?ESME_RINVEXPIRY) ->
    "Invalid message validity period (Expiry time).";
format(?ESME_RINVDFTMSGID) ->
    "Predefined Message ID is Invalid or specified predefined message was not found.";
format(?ESME_RX_T_APPN) ->
    "ESME Receiver Temporary App Error Code.";
format(?ESME_RX_P_APPN) ->
    "ESME Receiver Permanent App Error Code.";
format(?ESME_RX_R_APPN) ->
    "ESME Receiver Reject Message Error Code.";
format(?ESME_RQUERYFAIL) ->
    "query_sm request failed.";
format(?ESME_RINVTLVSTREAM) ->
    "Error in the optional part of the PDU Body.";
format(?ESME_RTLVNOTALLWD) ->
    "TLV not allowed.";
format(?ESME_RINVTLVLEN) ->
    "Invalid Parameter Length.";
format(?ESME_RMISSINGTLV) ->
    "Expected TLV missing.";
format(?ESME_RINVTLVVAL) ->
    "Invalid TLV Value.";
format(?ESME_RDELIVERYFAILURE) ->
    "Transaction Delivery Failure.";
format(?ESME_RUNKNOWNERR) ->
    "Unknown Error.";
format(?ESME_RSERTYPUNAUTH) ->
    "ESME Not authorised to use specified service_type.";
format(?ESME_RPROHIBITED) ->
    "ESME Prohibited from using specified operation.";
format(?ESME_RSERTYPUNAVAIL) ->
    "Specified service_type is unavailable.";
format(?ESME_RSERTYPDENIED) ->
    "Specified service_type is denied.";
format(?ESME_RINVDCS) ->
    "Invalid Data Coding Scheme.";
format(?ESME_RINVSRCADDRSUBUNIT) ->
    "Source Address Sub unit is Invalid.";
format(?ESME_RINVDSTADDRSUBUNIT) ->
    "Destination Address Sub unit is Invalid.";
format(?ESME_RINVBCASTFREQINT) ->
    "Broadcast Frequency Interval is invalid.";
format(?ESME_RINVBCASTALIAS_NAME) ->
    "Broadcast Alias Name is invalid.";
format(?ESME_RINVBCASTAREAFMT) ->
    "Broadcast Area Format is invalid.";
format(?ESME_RINVNUMBCAST_AREAS) ->
    "Number of Broadcast Areas is invalid.";
format(?ESME_RINVBCASTCNTTYPE) ->
    "Broadcast Content Type is invalid.";
format(?ESME_RINVBCASTMSGCLASS) ->
    "Broadcast Message Class is invalid.";
format(?ESME_RBCASTFAIL) ->
    "broadcast_sm operation failed."; 
format(?ESME_RBCASTQUERYFAIL) ->
    "query_broadcast_sm operation failed."; 
format(?ESME_RBCASTCANCELFAIL) ->
    "cancel_broadcast_sm operation failed."; 
format(?ESME_RINVBCAST_REP) ->
    "Number of Repeated Broadcasts is invalid.";
format(?ESME_RINVBCASTSRVGRP) ->
    "Broadcast Service Group is invalid.";
format(?ESME_RINVBCASTCHANIND) ->
    "Broadcast Channel Indicator is invalid.".

%%%===================================================================
%%% Internal functions
%%%===================================================================

