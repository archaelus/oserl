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

%%% @doc Code lock ESME.
%%%
%%% <p>FSM-ESME sample.</p>
%%%
%%%
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [18 Feb 2004]
%%% 
%%% <ul>
%%%   <li>Trailing $\0 removed from the c_octet_string values.</li>
%%% </ul>
%%%
%%% [26 Feb 2004]
%%%
%%% <ul>
%%%   <li>Last changes in <a href="gen_esme.html">gen_esme.erl</a> adopted.
%%%   </li>
%%% </ul>
%%%
%%%
%%% <h2>Changes 0.2 -&gt; 1.0</h2>
%%%
%%% [17 May 2004]
%%% 
%%% <ul>
%%%   <li>New header <i>oserl.hrl</i> imported.</li>
%%% </ul>
%%%
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%%%         [http://www.des.udc.es/~mpquique]
%%% @version 1.0, {05 Feb 2004} {@time}.
%%% @end
-module(code_lock_esme).

-behaviour(gen_fsm).
-behaviour(gen_esme).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start/0, start_link/6, stop/0]).

%%%-------------------------------------------------------------------
%%% Internal gen_esme exports
%%%-------------------------------------------------------------------
-export([deliver_sm/3]).

%%%-------------------------------------------------------------------
%%% Internal gen_fsm exports
%%%-------------------------------------------------------------------
-export([init/1, closed/2, open/2, handle_event/3, terminate/3]).

%%%-------------------------------------------------------------------
% Macros
%%%-------------------------------------------------------------------
-define(MC_ADDR, {193, 144, 50, 51}).
-define(ESME_NAME, code_lock_esme).
-define(FSM_NAME, code_lock_fsm).


%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start() -> ok
%%
%% @doc Starts the echo ESME.
%% @end
start() ->
    start_link("code_lock_esme", "secret", "1949", "1949", ?MC_ADDR, [1,2]).


%% @spec start_link(SystemId,Password,AddrRange,SourceAddr,McAddr,Code) -> ok
%%    SystemId = string()
%%    Password = string()
%%    AddrRange = string()
%%    SourceAddr = string()
%%    McAddr = string() | atom() | ip_address()
%%    Code = [int()]
%%
%% @doc Starts the code lock ESME and binds as a receiver and transmitter to
%% McAddr.
%% @end
start_link(SystemId, Password, AddrRange, SourceAddr, McAddr, Code) ->
    Setup = ?ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    gen_fsm:start_link({local, ?FSM_NAME}, ?MODULE, [Code, Setup, McAddr], []).


%% @spec stop() -> ok
%%
%% @doc Unbinds and stops the code lock ESME.
%% @end
stop() ->
    gen_fsm:send_all_state_event(?FSM_NAME, stop).


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
%% @doc <a href="gen_esme.html#deliver_sm-3">gen_esme - deliver_sm/3</a>
%% callback implementation.
%% @end
deliver_sm(_Pid, _Eid, Pdu) -> 
    Caller = sm:reply_address(Pdu),
    Digit  = list_to_integer(element(2, sm:message_user_data(Pdu))),
    io:format("Digit: ~p~nCaller: ~p~n", [Digit, Caller]),
    gen_fsm:send_event(?FSM_NAME, {button, Digit, Caller}),
    {ok, []}.


%%%===================================================================
%%% FSM functions
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
init([Code, Setup, McAddr]) ->
    io:format("Starting the code lock ESME...", []),
    case gen_esme:start_link({local, ?ESME_NAME}, ?MODULE, Setup) of
        {ok, _Eid} ->
            gen_esme:open_transmitter(?ESME_NAME, McAddr),
            gen_esme:bind_transmitter(?ESME_NAME),
            gen_esme:open_receiver(?ESME_NAME, McAddr),
            gen_esme:bind_receiver(?ESME_NAME),
            io:format("bound as receiver/transceiver~n", []),
            process_flag(trap_exit, true),
            {ok, closed, {[], Code, undefined}};
        Error ->
            {stop, Error}
    end.


%% @spec closed(Event, StateData) -> Result
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
%% for the state name closed.
%% @end
closed({button, Digit, Caller}, {SoFar, Code, Who}) when Who == undefined;
                                                         Who == Caller ->
    case [Digit|SoFar] of
        Code ->
            do_open(Caller),
            {next_state, open, {[], Code, Caller}, 3000};
        Incomplete when length(Incomplete) < length(Code) ->
            {next_state, closed, {Incomplete, Code, Caller}};
        _Wrong ->
            {next_state, closed, {[], Code, undefined}}
    end;
closed({button, Digit, Caller}, {SoFar, Code, Who}) ->
    submit_sm("Please, wait your turn", Caller),
    {next_state, closed, {SoFar, Code, Who}}.
    

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
%% @end
open(timeout, {SoFar, Code, Who}) ->
    do_close(Who),
    {next_state, closed, {SoFar, Code, undefined}};
open({button, _Digit, Caller}, State) ->
    submit_sm("Another user is now entering. Please, wait your turn", Caller).


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
handle_event(stop, _StateName, StateData) ->
    io:format("Stopping the code lock ESME...", []),
    gen_esme:unbind_receiver(?ESME_NAME),
    gen_esme:close_receiver(?ESME_NAME),
    gen_esme:unbind_transmitter(?ESME_NAME),
    gen_esme:close_transmitter(?ESME_NAME),
    gen_esme:stop(?ESME_NAME),
    io:format("done~n", []),
    {stop, normal, StateData}.


%% @spec terminate(Reason, StateName, StateData) -> true
%%    Reason    = normal | shutdown | term()
%%    StateName = atom()
%%    StateData = term()
%%
%% @doc <a href="http://www.erlang.org/doc/r9c/lib/stdlib-1.12/doc/html/gen_fsm.html">gen_fsm - terminate/3</a> callback implementation.  Shutdown the fsm.
%%
%% <p>Return value is ignored by the server.</p>
%% @end
terminate(Reason, StateName, StateData) ->
    ok.

 
%%%===================================================================
%%% Internal functions
%%%===================================================================
do_open(Who) ->
    submit_sm("Come in please.  The door is open", Who).


do_close(Who) ->
    submit_sm("Closing the door. Please, hurry up.", Who).


submit_sm(Mesg, Dest) ->
    io:format("Submitted SM: ~p~nDestination: ~p~n", [Mesg, Dest]),
    ParamList = [{short_message, Mesg}|Dest],
    spawn(fun() -> gen_esme:submit_sm(?ESME_NAME, ParamList) end).
    

