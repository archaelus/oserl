%%%
% Copyright (C) 2004 Enrique Marcote Peña <mpquique@udc.es>
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
% @doc Code lock ESME.
%
% <p>FSM-ESME sample.</p>
%
%
% <h2>Changes 0.1 -&gt; 0.2</h2>
%
% [18 Feb 2004]
% 
% <ul>
%   <li>Trailing $\0 removed from the c_octet_string values.</li>
% </ul>
%
% [26 Feb 2004]
%
% <ul>
%   <li>Last changes in <a href="gen_esme.html">gen_esme.erl</a> adopted.</li>
% </ul>
%
%
% @copyright 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique]
% @version 1.0, {05 Feb 2004} {@time}.
% @end
%%
-module(code_lock_esme).

-behaviour(gen_fsm).
-behaviour(gen_esme).

-include("gen_esme.hrl").

-export([start/0, start_link/6, stop/0]).
-export([deliver_sm/3]).
-export([init/1, closed/2, open/2, handle_event/3, terminate/3]).

start() ->
    start_link("banana", "secret", "1949", "1949", {193, 144, 50, 51}, [1,2]).

start_link(SystemId, Password, AddrRange, SourceAddr, McAddr, Code) ->
    io:format("Starting the code lock ESME...", []),
    Setup = ?GEN_ESME_SETUP(SystemId, Password, AddrRange, SourceAddr),
    case gen_esme:start_link({local, code_lock_esme}, ?MODULE, Setup) of
        {ok, Eid} ->
            gen_esme:open_transmitter(code_lock_esme, McAddr),
            gen_esme:bind_transmitter(code_lock_esme),
            gen_esme:open_receiver(code_lock_esme, McAddr),
            gen_esme:bind_receiver(code_lock_esme),
            io:format("bound as receiver/transceiver~n", []),
            gen_fsm:start_link({local, code_lock_fsm}, ?MODULE, Code, []),
            {ok, Eid};
        Error ->
            Error
    end.


stop() ->
    io:format("Stopping the code lock ESME...", []),
    gen_esme:unbind_receiver(code_lock_esme),
    gen_esme:close_receiver(code_lock_esme),
    gen_esme:unbind_transmitter(code_lock_esme),
    gen_esme:close_transmitter(code_lock_esme),
    gen_esme:stop(code_lock_esme),
    gen_fsm:send_all_state_event(code_lock_fsm, stop),
    io:format("done~n", []).


deliver_sm(_Pid, _Eid, Pdu) -> 
    Caller = sm:reply_address(Pdu),
    Digit  = list_to_integer(element(2, sm:message_user_data(Pdu))),
    io:format("Digit: ~p~nCaller: ~p~n", [Digit, Caller]),
    gen_fsm:send_event(code_lock_fsm, {button, Digit, Caller}),
    {ok, []}.


init(Code) ->
    process_flag(trap_exit, true),
    {ok, closed, {[], Code, undefined}}.

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
    

open(timeout, {SoFar, Code, Who}) ->
    do_close(Who),
    {next_state, closed, {SoFar, Code, undefined}};
open({button, _Digit, Caller}, State) ->
    submit_sm("Another user is now entering. Please, wait your turn", Caller).


handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData}.

terminate(Reason, StateName, StateData) ->
    ok.

 
do_open(Who) ->
    submit_sm("Came in please.  The door is open", Who).


do_close(Who) ->
    submit_sm("Closing the door. Please, hurry up.", Who).


submit_sm(Mesg, Dest) ->
    io:format("Submitted SM: ~p~nDestination: ~p~n", [Mesg, Dest]),
    ParamList = [{short_message, Mesg}|Dest],
    spawn(fun() -> gen_esme:submit_sm(code_lock_esme, ParamList) end).
    
