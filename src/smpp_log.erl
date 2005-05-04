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
         add_disk_log_handler/1, 
         add_error_logger_handler/1,
         delete_disk_log_handler/0, 
         delete_error_logger_handler/0,
         notify_peer_operation/1,
         notify_self_operation/1,
         stop/0]).
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
-record(state, {pred, type}).

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


%% @spec add_disk_log_handler(Args) -> Result
%%    Args = [Arg]
%%    Arg = {file, File} | {type, Type} | {pred, Pred}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds the disk log event handler
%%
%% @see gen_event:add_handler/3
%% @end
add_disk_log_handler(Args) ->
    NewArgs = [{type, disk_log}|Args],
    gen_event:add_handler(?SERVER, {?MODULE, disk_log}, NewArgs).


%% @spec add_error_logger_handler(Args) -> Result
%%    Args = [Arg]
%%    Arg = {file, File} | {type, Type} | {pred, Pred}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds the error logger event handler
%%
%% @see gen_event:add_handler/3
%% @end
add_error_logger_handler(Args) ->
    NewArgs = [{type, error_logger}|Args],
    gen_event:add_handler(?SERVER, {?MODULE, error_logger}, NewArgs).


%% @spec delete_disk_log_handler() -> Result
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Deletes the disk log event handler
%%
%% @see delete_handler/1
%% @end
delete_disk_log_handler() ->
    gen_event:delete_handler(?SERVER, {?MODULE, disk_log}, stop).


%% @spec delete_error_logger_handler() -> Result
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Deletes the disk log event handler
%%
%% @see add_handler/2
%% @end
delete_error_logger_handler() ->
    gen_event:delete_handler(?SERVER, {?MODULE, error_logger}, stop).


%% @spec notify_peer_operation(Pdu) -> ok
%%    Pdu = binary()
%%
%% @doc Notifies peer SMPP operations to the log.
%%
%% @equiv gen_event:notify(?LOG, {peer, Pdu}).
%% @end
notify_peer_operation(Pdu) ->
    gen_event:notify(?SERVER, {peer, Pdu}).


%% @spec notify_self_operation(Pdu) -> ok
%%    Pdu = binary()
%%
%% @doc Notifies self SMPP operations to the log.
%%
%% @equiv gen_event:notify(?LOG, {self, Pdu}).
%% @end
notify_self_operation(Pdu) ->
    gen_event:notify(?SERVER, {self, Pdu}).


%% @spec stop() -> Result
%%    Result = {ok, Pid} | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid}
%%
%% @doc Starts the server.
%%
%% @see gen_event:start/1
%% @end
stop() ->
    gen_event:stop(?SERVER). 

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
init(Args) ->
    Pred = get_arg(pred, Args, fun(_) -> true end),
    Type = get_arg(type, Args, disk_log),
    Result = if
                 Type == disk_log ->
                     File = get_arg(file, Args, atom_to_list(?LOG)),
                     disk_log:open([{name, ?LOG}, {file, File}, {type, wrap}]);
                 true ->
                     ok
             end,
    report:info(smpp_log, add_handler, [{result, Result}|Args]),
    {ok, #state{pred = Pred, type = Type}}.


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
handle_event({From, BinaryPdu}, #state{pred = Pred, type = Type} = State) ->
    case catch Pred(BinaryPdu) of
        true when Type == disk_log ->
            disk_log:alog(?LOG, {now(), BinaryPdu, From});
        true when Type == error_logger ->
            Params = case operation:unpack(BinaryPdu) of
                         {ok, PduDict} ->
                             operation:to_list(PduDict);
                         Error ->
                             Error
                     end,
            Pdu = binary:to_hexlist(BinaryPdu),
            report:info(smpp_log, operation, [{from,From},{pdu,Pdu}|Params]);
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
terminate(Reason, #state{type = Type}) ->
    Result = if
                 Type == disk_log ->
                     disk_log:close(?LOG);
                 true ->
                     ok
             end,
    Details = [{reason, Reason}, {type, Type}, {result, Result}],
    report:info(smpp_log, terminate, Details).


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
%% @spec get_arg(Key, List, Default) -> Value
%%    Key = term()
%%    List = [{Key, Value}]
%%    Value = term()
%%    Default = term()
%%
%% @doc Gets the <tt>Value</tt> for a given <tt>Key</tt>, if not found
%% <tt>Default</tt> is returned.
%% @end 
get_arg(Key, List, Default) ->
    case lists:keysearch(Key, 1, List) of
        {value, {_Key, Val}} -> 
            Val;
        _ -> 
            Default
    end.
