%%% Copyright (C) 2005 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc SMPP Log
%%%
%%% <p>A SMPP <tt>gen_event</tt> handler for SMPP operations logging.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net]
%%% @version 1.2, { 3 May 2005} {@time}.
%%% @end
-module(smpp_log).

-behaviour(gen_event).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
%%-compile(export_all).
-export([start/0, 
         start_link/0, 
         add_handler/0, 
         add_handler/1,
         notify_peer_operation/1,
         notify_self_operation/1]).
%          swap_handler/0,
%          swap_handler/1]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(LOG, ?MODULE).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state}
%%
%% %@doc Representation of the server's state
%%
%% <dl>
%%   <dt>: </dt><dd>
%%   </dd>
%% </dl>
%% %@end
-record(state, {pred}).

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start() -> Result
%%    Result = {ok, Pid} | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid}
%%
%% @doc Starts the server.
%%
%% @see gen_event:start/1
%% @end
start() ->
    gen_event:start({local, ?SERVER}). 


%% @spec start_link() -> Result
%%    Result = {ok, Pid} | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid}
%%
%% @doc Starts the server.
%%
%% @see gen_event:start_link/1
%% @end
start_link() ->
    gen_event:start_link({local, ?SERVER}). 


%% @spec add_handler() -> Result
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds an event handler
%%
%% @equiv add_handler(fun(_) -> true end).
%% @end
add_handler() ->
    add_handler(fun(_) -> true end).


%% @spec add_handler(Pred) -> Result
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%    Pred = fun(Event) -> boolean()
%%
%% @doc Adds an event handler
%%
%% @equiv gen_event:add_handler(?SERVER, ?MODULE, [Pred]).
%% @end
add_handler(Pred) ->
    gen_event:add_handler(?SERVER, ?MODULE, [Pred]).


%% @spec notify_peer_operation(Pdu) -> ok
%%    Pdu = binary()
%%
%% @doc Notifies peer SMPP operations to the log.
%%
%% @equiv gen_event:notify(?LOG, {peer, Pdu}).
%% @end
notify_peer_operation(Pdu) ->
    gen_event:notify(?LOG, {peer, Pdu}).


%% @spec notify_self_operation(Pdu) -> ok
%%    Pdu = binary()
%%
%% @doc Notifies self SMPP operations to the log.
%%
%% @equiv gen_event:notify(?LOG, {self, Pdu}).
%% @end
notify_self_operation(Pdu) ->
    gen_event:notify(?LOG, {self, Pdu}).


%% @spec swap_handler() -> Result
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds an event handler
%%
%% @equiv add_handler(fun(_) -> true end).
%% @end
% swap_handler() ->
%     swap_handler(fun(_) -> true end).


%% @spec swap_handler(Pred) -> Result
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%    Pred = fun(Event) -> boolean()
%%
%% @doc Adds an event handler
%%
%% @equiv gen_event:add_handler(?SERVER, ?MODULE, [Pred]).
%% @end
% swap_handler(Pred) ->
%     gen_event:add_handler(?SERVER, ?MODULE, [Pred]).

%%%===================================================================
%%% Server functions
%%%===================================================================
%% @spec init(Args) -> {ok, State}
%%    Args  = term()
%%    State = term()
%%
%% @doc Initialize the event handler
%% @end
init([Pred]) ->
    {ok, ?LOG} = dets:open_file(?LOG, []),
    {ok, #state{pred = Pred}}.


%% @spec handle_event(Event, State) -> Result
%%    Event    = term()
%%    State    = term()
%%    Result   = {ok, NewState}                                   |
%%               {swap_handler, Args1, NewState, Handler2, Args2} |
%%               remove_handler 
%%    NewState = term()
%%    Args1    = term()
%%    Args2    = term()
%%    Handler2 = Module2 | {Module2, Id}
%%    Module2  = atom()
%%    Id       = term()
%% @doc
%% @end
handle_event({Who, Pdu}, #state{pred = Pred} = State) ->
    case catch Pred(Pdu) of
        true ->
            dets:insert(?LOG, {now(), Pdu, Who});
        _Otherwise ->
            ok
    end,
    {ok, State}.


%% @spec handle_call(Request, State) -> Result
%%    Request  = term()
%%    State    = term()
%%    Result   = {ok, Reply, State}                                      |
%%               {swap_handler, Reply, Args1, NewState, Handler2, Args2} |
%%               {remove_handler, Reply}
%%    Reply    = term()
%%    NewState = term()
%%    Args1    = term()
%%    Args2    = term()
%%    Handler2 = Module2 | {Module2, Id}
%%    Module2  = atom()
%%    Id       = term()
%% @doc
%% @end
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%% @spec handle_info(Info, State) -> Result
%%    Info     = term()
%%    State    = term()
%%    Result   = {ok, NewState}                                  |
%%               {swap_handler, Args1, NewState, Handler, Args2} |
%%               remove_handler
%%    NewState = term()
%%    Args1    = term()
%%    Args2    = term()
%%    Handler2 = Module2 | {Module2, Id}
%%    Module2  = atom()
%%    Id       = term()
%% @doc
%% @end
handle_info(_Info, State) ->
    {ok, State}.

%% @spec terminate(Arg, State) -> ok
%%    Arg    = Args | {stop, Reason} | stop | remove_handler |
%%             {error, {'EXIT', Reason}} | {error, Reason}
%%    Reason = term()
%%    State  = term()
%%
%% @doc Shutdown the server.
%%
%% <p>Return value is ignored by the server.</p>
%% @end
terminate(_Reason, _State) ->
    dets:close(?LOG).

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc Convert process state when code is changed
%% @end
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------
