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

%%% @doc Test ESME.
%%%
%%% <p>A very simple test ESME.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique]
%%% @version 1.0, {16 Jun 2004} {@time}.
%%% @end
-module(test_esme).

-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start/0, start_link/5]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([deliver_sm/3, deliver_data_sm/3]).

%%%-------------------------------------------------------------------
%%% Internal gen_esme exports
%%%-------------------------------------------------------------------
-export([]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%-define(MC_ADDR, {193, 144, 50, 51}).
-define(SMSC_ADDR, {192, 168, 1, 4}).
-define(ESME_NAME, submit_esme).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start() -> ok
%%
%% @doc Starts the submit ESME.
%% @end
start() ->
    start_link("test_esme", "secret", "1950", "1950", ?SMSC_ADDR).

start_link(SystemId, Password, AddrRange, SourceAddr, McAddr) ->
    Setup = ?ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    case gen_esme:start_link({local, ?ESME_NAME}, ?MODULE, Setup) of
        {ok, Eid} ->
            loop(?SMSC_ADDR, unbound),
            gen_esme:stop(?ESME_NAME);
        Error ->
            Error
    end.


loop(SMSCAddr, unbound) ->
    io:format("1-. Bind receiver~n", []),
    io:format("2-. Bind transmitter~n", []),
    io:format("3-. Bind transceiver~n", []),
    io:format("4-. Exit~n", []),
    case read_decimal("Enter your choice: ") of
        1 -> 
            bind_receiver(SMSCAddr, unbound);
        2 -> 
            bind_transmitter(SMSCAddr, unbound);
        3 -> 
            bind_transceiver(SMSCAddr, unbound);
        4 -> 
            io:format("Bye~n", []);
        Other ->
            io:format("Unknown option: ~p~n", [Other]),
            loop(SMSCAddr, unbound)
    end;
loop(SMSCAddr, State) when State == bound_tx; State == bound_trx ->
    io:format(" 1-. Broadcast SM~n", []),
    io:format(" 2-. Cancel Broadcast SM~n", []),
    io:format(" 3-. Cancel SM~n", []),
    io:format(" 4-. Data SM~n", []),
    io:format(" 5-. Query Broadcast SM~n", []),
    io:format(" 6-. Query SM~n", []),
    io:format(" 7-. Replace SM~n", []),
    io:format(" 8-. Submit Multi~n", []),
    io:format(" 9-. Submit SM~n", []),
    io:format("10-. Unbind~n", []),
    io:format("11-. Exit~n", []),
    case read_decimal("Enter your choice: ") of
        1  -> 
            broadcast_sm(SMSCAddr, State);
        2  -> 
            cancel_broadcast_sm(SMSCAddr, State);
        3  -> 
            cancel_sm(SMSCAddr, State);
        4  -> 
            data_sm(SMSCAddr, State);
        5  -> 
            query_broadcast_sm(SMSCAddr, State);
        6  -> 
            query_sm(SMSCAddr, State);
        7  ->
            replace_sm(SMSCAddr, State);
        8  ->
            submit_multi(SMSCAddr, State);
        9  -> 
            submit_sm(SMSCAddr, State);
        10 -> 
            unbind(SMSCAddr, State);
        11 ->
            io:format("Bye~n", []);
        Other ->
            io:format("Unknown option: ~p~n", [Other]),
            loop(SMSCAddr, unbound)
    end;
loop(SMSCAddr, bound_rx) ->
    io:format("1-. Unbind~n", []),
    io:format("2-. Exit~n", []),
    case read_decimal("Enter your choice: ") of
        1 -> 
            unbind(SMSCAddr, bound_rx);
        2 ->
            io:format("Bye~n", []);
        Other ->
            io:format("Unknown option: ~p~n", [Other]),
            loop(SMSCAddr, unbound)
    end.


%%%===================================================================
%%% ESME functions
%%%===================================================================
%% @spec deliver_sm(Pid, Eid, Pdu) -> Result
%%    Pid        = pid()
%%    Eid        = pid()
%%    Pdu        = pdu()
%%    Result     = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_esme.html#deliver_sm-3">gen_esme - deliver_sm/3
%% </a> callback implementation.
%% @end
deliver_sm(_Pid, Eid, Pdu) -> 
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    io:format("Echoing SM: ~p~n", [Mesg]),
    spawn_link(fun() -> gen_esme:submit_sm(Eid, [Mesg|Dest]) end), 
    {ok, []}.


%% @spec deliver_data_sm(Pid, Eid, Pdu) -> Result
%%    Pid        = pid()
%%    Eid        = pid()
%%    Pdu        = pdu()
%%    Result     = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList  = [{ParamName, ParamValue}]
%%    ParamName  = atom()
%%    ParamValue = term()
%%
%% @doc <a href="gen_esme.html#deliver_data_sm-3">gen_esme - deliver_data_sm/3
%% </a> callback implementation.
%% @end
deliver_data_sm(_Pid, Eid, Pdu) -> 
    Mesg = sm:message_user_data(Pdu),   % gets incoming short message
    Dest = sm:reply_address(Pdu),       % source address as response address
    io:format("Echoing SM: ~p~n", [Mesg]),
    spawn(fun() -> gen_esme:data_sm(Eid, [Mesg|Dest]) end), 
    {ok, []}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
bind_receiver(SMSCAddr, State) ->
    gen_esme:open_receiver(?ESME_NAME, SMSCAddr),
    case gen_esme:bind_receiver(?ESME_NAME) of
        ok ->
            io:format("Response OK~n", []),
            loop(SMSCAddr, bound_rx);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

bind_transmitter(SMSCAddr, State) ->
    gen_esme:open_transmitter(?ESME_NAME, SMSCAddr),
    case gen_esme:bind_transmitter(?ESME_NAME) of
        ok ->
            io:format("Response OK~n", []),
            loop(SMSCAddr, bound_tx);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

bind_transceiver(SMSCAddr, State) ->
    gen_esme:open_transceiver(?ESME_NAME, SMSCAddr),
    case gen_esme:bind_transceiver(?ESME_NAME) of
        ok ->
            io:format("Response OK~n", []),
            loop(SMSCAddr, bound_trx);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

broadcast_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    DestinationAddress = read_string("Destination Address> "),
    ShortMessage = read_string("Short Message> "),
    ParamList = [{source_addr, SourceAddress},
                 {destination_addr, DestinationAddress},
                 {short_message, ShortMessage}],                 
    case gen_esme:broadcast_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

cancel_broadcast_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    MessageId = read_string("Message Id> "),
    ParamList = [{source_addr, SourceAddress}, {message_id, MessageId}],
    case gen_esme:cancel_broadcast_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

cancel_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    DestinationAddress = read_string("Destination Address> "),
    MessageId = read_string("Message Id> "),
    ParamList = [{source_addr, SourceAddress}, 
                 {destination_addr, DestinationAddress},
                 {message_id, MessageId}],
    case gen_esme:cancel_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

data_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    DestinationAddress = read_string("Destination Address> "),
    MessagePayload = read_string("Message Payload> "),
    ParamList = [{source_addr, SourceAddress},
                 {destination_addr, DestinationAddress},
                 {message_payload, MessagePayload}],                 
    case gen_esme:data_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

query_broadcast_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    MessageId = read_string("Message Id> "),
    ParamList = [{source_addr, SourceAddress}, {message_id, MessageId}],
    case gen_esme:query_broadcast_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

query_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    MessageId = read_string("Message Id> "),
    ParamList = [{source_addr, SourceAddress}, {message_id, MessageId}],
    case gen_esme:query_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.

replace_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    DestinationAddress = read_string("Destination Address> "),
    ShortMessage = read_string("Short Message> "),
    ParamList = [{source_addr, SourceAddress},
                 {destination_addr, DestinationAddress},
                 {short_message, ShortMessage}],                 
    case gen_esme:replace_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.
                 
submit_multi(SMSCAddr, State) ->
    io:format("Not implemented~n", []),
    loop(SMSCAddr, State).

submit_sm(SMSCAddr, State) ->
    SourceAddress = read_string("Source Address> "),
    DestinationAddress = read_string("Destination Address> "),
    ShortMessage = read_string("Short Message> "),
    ParamList = [{source_addr, SourceAddress},
                 {destination_addr, DestinationAddress},
                 {short_message, ShortMessage}],                 
    case gen_esme:submit_sm(?ESME_NAME, ParamList) of
        {ok, Pdu} ->
            io:format("Response OK: ~p~n", [dict:to_list(Pdu)]),
            loop(SMSCAddr, State);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, State)
    end.                 

unbind(SMSCAddr, bound_rx) ->
    case gen_esme:unbind_receiver(?ESME_NAME) of
        ok ->
            io:format("Response OK~n", []),
            gen_esme:close_receiver(?ESME_NAME),
            loop(SMSCAddr, unbound);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, bound_rx)
    end;
unbind(SMSCAddr, bound_tx) ->
    case gen_esme:unbind_transmitter(?ESME_NAME) of
        ok ->
            io:format("Response OK~n", []),
            gen_esme:close_transmitter(?ESME_NAME),
            loop(SMSCAddr, unbound);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, bound_tx)
    end;
unbind(SMSCAddr, bound_trx) ->
    case gen_esme:unbind_transceiver(?ESME_NAME) of
        ok ->
            io:format("Response OK~n", []),
            gen_esme:close_transceiver(?ESME_NAME),
            loop(SMSCAddr, unbound);
        {error, Error} ->
            io:format("Error code : ~p~n", [Error]),
            loop(SMSCAddr, bound_trx)
    end.


%% @spec read_string(Prompt) -> Result
%%     Prompt = string()
%%     Result = string | {error, What} | eof
%%     What   = term()
%%
%% @doc Reads a string from the standard input.
%% @end 
read_string(Prompt) ->
    case io:fread(Prompt, "~s") of
        {ok, InputList} ->
            hd(InputList);
        Error ->
            Error
    end.


%% @spec read_decimal(Prompt) -> Result
%%     Prompt = string()
%%     Result = int() | {error, What} | eof
%%     What   = term()
%%
%% @doc Reads a decimal number from the standard input.
%% @end 
read_decimal(Prompt) ->
    case io:fread(Prompt, "~d") of
        {ok, InputList} ->
            hd(InputList);
        Error ->
            Error
    end.
