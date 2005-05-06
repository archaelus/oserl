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
-include("oserl.hrl").

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
         match/3,
         match/4,
         notify_peer_operation/1,
         notify_self_operation/1,
         stop/0]).

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
-define(NAME, ?MODULE).    % Default disk_log name.

% Default logging predicate.
%
% Logs every operation PDU other than an enquire_link or an enquire_link_resp.
-define(PRED, fun(BinaryPdu) ->
                      case pdu_syntax:command_id(BinaryPdu) of
                          ?COMMAND_ID_ENQUIRE_LINK ->
                              false;
                          ?COMMAND_ID_ENQUIRE_LINK_RESP ->
                              false;
                          _CommandId ->
                              true
                      end
              end).

% Default log size.
%
% Assuming 256 bytes for each PDU.  Individual log files of the wrap disk log
% can store up to 40960 PDUs.  Having 20 files on the wrap log, the total
% PDU capacity, before wrapping to the first log, is about 819200.
-define(SIZE, {10485760, 20}). % 10M x 20 = 200M

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
-record(state, {type, pred, file}).

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
%%    Arg = {file, File} | {pred, Pred}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds the disk log event handler.
%%
%% <ul>
%%   <li><b>File:</b> disk log file name.  Default is <i>smpp_log</i>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be logged.  </tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%%   <li><b>Size:</b> a pair <tt>{MaxNoBytes, MaxNoFiles}</tt> as given by the 
%%     <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/disk_log.html">disk_log:open/1</a> option <tt>size</tt>.  The default size is
%%     <tt>{10485760, 20}</tt> (10M x 20).  Assuming 256 bytes for each PDU.  
%%     Individual log files of the wrap disk log can store up to 40960 PDUs 
%%     for the default value.  Having 20 files on the wrap log, the total PDU 
%%     capacity, before wrapping to the first log, is about 819200.
%%   </li>
%% </ul>
%%
%% @see gen_event:add_handler/3
%% @end
add_disk_log_handler(Args) ->
    NewArgs = [{type, disk_log}|Args],
    gen_event:add_handler(?SERVER, {?MODULE, disk_log}, NewArgs).


%% @spec add_error_logger_handler(Args) -> Result
%%    Args = [Arg]
%%    Arg = {file, File} | {pred, Pred}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds the error logger event handler.
%%
%% <ul>
%%   <li><b>File:</b> error_logger logfile.  If not defined no logfile is 
%%     used</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be logged.  </tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
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

%% @spec match(Date, From, Pred, Continuation) -> Result
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun(BinaryPdu) -> boolean()
%%    Continuation = start | cont()
%%    Result = {Continuation2, List} |
%%             {Continuation2, List, Badbytes} |
%%             eof |
%%             {error, Reason}
%%    List = [{Now, BinaryPdu, From}]
%%    Now = {MegaSecs, Secs, Microsecs}
%%    BinaryPdu = binary()
%%
%% @doc PDUs are logged in the disk log as <tt>{Now, BinaryPdu, From}</tt> 
%% terms.  This function gets the list of logged PDUs matching the given 
%% <tt>Date</tt>, <tt>From</tt> and <tt>Pred</tt>.
%%
%% <ul>
%%   <li><b>Date:</b> <tt>any</tt>, <tt>{from, Time}</tt>, <tt>{until, Time}
%%     </tt> or <tt>{lapse, FromTime,ToTime}</tt>.</li>
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or </tt>both</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  </tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% 
%% <p>This function internally calls <a href="#match-4">match/4</a> with 
%% <tt>start</tt> as the <tt>Continuation</tt> parameters.</p>
%%
%% @see match/4
%%
%% @equiv match(Date, From, Pred, start)
%% @end 
match(Date, From, Pred) ->
    match(Date, From, Pred, start).


%% @spec match(Date, From, Pred, Continuation) -> Result
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun(BinaryPdu) -> boolean()
%%    Continuation = start | cont()
%%    Result = {Continuation2, List} |
%%             {Continuation2, List, Badbytes} |
%%             eof |
%%             {error, Reason}
%%    List = [{Now, BinaryPdu, From}]
%%    Now = {MegaSecs, Secs, Microsecs}
%%    BinaryPdu = binary()
%%
%% @doc PDUs are logged in the disk log as <tt>{Now, BinaryPdu, From}</tt> 
%% terms.  This function gets the list of logged PDUs matching the given 
%% <tt>Date</tt>, <tt>From</tt> and <tt>Pred</tt>.
%%
%% <ul>
%%   <li><b>Date:</b> <tt>any</tt>, <tt>{from, Time}</tt>, <tt>{until, Time}
%%     </tt> or <tt>{lapse, FromTime,ToTime}</tt>.</li>
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or </tt>both</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  </tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% 
%% <p>This function uses <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/disk_log.html">disk_log:chunk/2</a> to read the terms 
%% appended to the disk log.  <tt>Continuation</tt> parameter and result
%% meanings are those documented in that function.</p>
%%
%% @see disk_log:chunk/2
%% @end 
match(Date, From, Pred, Continuation) ->
    F = fun(X) ->
                DatePred = fun({_Now, _Pdu, _From}) when Date == any ->
                                   true;
                              ({Now, _Pdu, _From}) ->
                                   case{Date,calendar:now_to_local_time(Now)}of
                                       {{from, T}, N} when N >= T ->
                                           true;
                                       {{until, T}, N} when N =< T ->
                                           true;
                                       {{lapse, T1, T2}, N} when N >= T1, 
                                                                 N =< T2 ->
                                           true;
                                       _Otherwise ->
                                           false
                                   end
                           end,
                FromPred = fun({_Now, _Pdu, _From}) when From == any ->
                                   true;
                              ({_Now, _Pdu, self}) when From == self ->
                                   true;
                              ({_Now, _Pdu, peer}) when From == peer ->
                                   true;
                              (_) ->
                                   false
                           end,
                DatePred(X) and FromPred(X) and Pred(X)
        end,
    case disk_log:chunk(?NAME, Continuation) of
        {Continuation2, List} ->
            {Continuation2, lists:filter(F, List)};
        {Continuation2, List, Badbytes} ->
            {Continuation2, lists:filter(F, List), Badbytes};
        Error ->
            Error
    end.


%% @spec notify_peer_operation(Pdu) -> ok
%%    Pdu = binary()
%%
%% @doc Notifies peer SMPP operations to the log.
%%
%% @equiv gen_event:notify(SERVER, {peer, Pdu}).
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
    Type = get_arg(type, Args, disk_log),
    Pred = get_arg(pred, Args, ?PRED),
    File = get_arg(file, Args, atom_to_list(?NAME)),
    Result = if
                 Type == disk_log ->
                     Size = get_arg(size, Args, ?SIZE),
                     Opts = [{name,?NAME},{file,File},{type,wrap},{size,Size}],
                     disk_log:open(Opts);
                 true ->
                     case lists:keymember(file, 1, Args) of
                         true ->
                             error_logger:logfile({open, File});
                         false ->
                             ok
                     end
             end,
    report:info(smpp_log, add_handler, [{result, Result}|Args]),
    {ok, #state{type = Type, pred = Pred, file = File}}.


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
handle_event({From, BinaryPdu}, #state{type = Type, pred = Pred} = State) ->
    case catch Pred(BinaryPdu) of
        true when Type == disk_log ->
            disk_log:alog(?NAME, {now(), BinaryPdu, From});
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
terminate(Reason, #state{type = Type, file = File}) ->
    Result = if
                 Type == disk_log ->
                     disk_log:close(?NAME);
                 true ->
                     case error_logger:logfile(filename) of
                         File ->
                             error_logger:logfile(close);
                         _Otherwise ->
                             ok
                     end
             end,
    Details = [{reason, Reason}, {type, Type}, {file, File}, {result, Result}],
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
