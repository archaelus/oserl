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

%%% @doc SMPP Log.
%%%
%%% <p>A SMPP <tt>gen_event</tt> manager for SMPP PDU operation logging.</p>
%%%
%%% <p>This module provides two independent event handlers for SMPP PDU 
%%% logging:</p>
%%%
%%% <ul>
%%%   <li>A <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/disk_log.html">disk_log</a> based handler.
%%%   </li>
%%%   <li>An <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/error_logger.html">error_logger</a> handler.
%%%   </li>
%%% </ul>
%%%
%%% <p>These handlers may be added and swapped dynamically.  By defining the
%%% appropriated logging predicate (see <a href="#add_disk_log-1">
%%% add_disk_log/1</a> and <a href="#add_error_logger-1">add_error_logger/1</a>
%%% for further details and default predicates) you can control which PDUs 
%%% should be logged by each handler.</p>
%%%
%%% <p>The <i>disk_log</i> stores by default PDUs in binary format within 
%%% tuples <tt>{Now,BinaryPdu,From}</tt>.  <tt>Now</tt> is the timestamp in the
%%% format returned by the BIF <tt>now()</tt>.  <tt>BinaryPdu</tt> is the SMPP 
%%% PDU in binary format and <tt>From</tt> indicates who issued the operation, 
%%% <tt>self</tt> if the operation was issued by our SMPP entity, <tt>peer</tt>
%%% in case the peer SMPP entity was the originator.</p>
%%%
%%% <p>The format of the term stored by the <i>disk_log</i> can be redefined 
%%% using a custom <tt>Format</tt> predicated.  In such a case, PDUs are 
%%% stored as <tt>{Now, Format(BinaryPdu), From}</tt>.</p>
%%%
%%% <p>In the same way, the format of the output dumped by the
%%% <i>error_logger</i> handler can be redefined by the user.  By default the
%%% complete parameters list of the selected SMPP PDUs is sent.</p>
%%%
%%% <p>This module provides also a set of functions for inspecting the
%%% <i>disk_log</i>:  <a href="#count-3">count/3</a>, <a href="#match-3">
%%% match/3</a> and <a href="#match-4">match/4</a>.  Again, by defining the 
%%% search predicate, any set of PDUs is easily extracted.</p>
%%%
%%%
%%% <h3>Usage</h3>
%%%
%%% <p>During <a href="gen_esme.html">gen_esme</a> and <a href="gen_smsc.html">
%%% gen_esme</a> startup the <i>smpp_log</i> manager is started, although no
%%% event handler is added at that point, thus no logging is done until a 
%%% handler is explicitly added from the ESME/SMSC implementation by calling 
%%% the functions <a href="#add_disk_log_handler-1">add_disk_log_handler/1</a>
%%% and/or <a href="#add_error_logger_handler-1">add_error_logger_handler/1
%%% </a>.</p>
%%%
%%% <p>Every time a PDU is packed at the session layer the function 
%%% <a href="#notify_self_operation-1">notify_self_operation/1</a> is
%%% called.  The same applies before unpacking for the
%%% <a href="#notify_peer_operation-1">notify_peer_operation/1</a> function.
%%% If no log handler was added to the <i>smpp_log</i> manager, these calls
%%% have no effect.</p>
%%%
%%% <p>The <i>error_logger</i> handler is very useful for debugging purposes.  
%%% The logging predicate can be set accordingly to filter the type of 
%%% information you need to inspect at runtime. The information sent to the 
%%% <i>error_logger</i> can also be formatted at your own wish.</p>
%%%
%%% <p>The <i>disk_log</i> handler is suitable for PDU storage.</p>
%%%
%%%
%%% <h4>Disk log example</h4>
%%%
%%% <p>The <i>disk_log</i> was designed to store SMPP operations on disk.  By
%%% default PDUs are efficiently stored in binary format.  Storage format can
%%% be easily customizable.  It is also possible to define which set of PDUs
%%% should be stored.  For example, to store only <i>submit_sm</i> and 
%%% <i>deliver_sm</i> PDUs on disk do:</p>
%%%
%%% <pre>
%%% File = "/home/otpuser/otp/log/smpp_log",
%%% Pred = fun(BinaryPdu) ->
%%%            case pdu_syntax:command_id(BinaryPdu) of
%%%                ?COMMAND_ID_SUBMIT_SM ->
%%%                    true;
%%%                ?COMMAND_ID_DELIVER_SM ->
%%%                    true;
%%%                _CommandId ->
%%%                    false
%%%            end
%%%        end,
%%% smpp_log:add_disk_log_handler([{file, File}, {pred, Pred}]),
%%% </pre>
%%%
%%% <p>By default every PDU operation other than <i>enquire_link</i> and
%%% <i>enquire_link_resp</i> are logged.</p>
%%%
%%%
%%% <h4>Error logger example</h4>
%%%
%%% <p>During runtime you may need to inspect <i>deliver_sm</i> operations
%%% originated by a given <i>source_addr</i>.  You can dump this information
%%% to the error_logger by adding or swapping the error_logger handler at any
%%% time:</p>
%%%
%%% <pre>
%%% SourceAddr = "1234",
%%% Pred = fun(BinaryPdu) ->
%%%            case pdu_syntax:command_id(BinaryPdu) of
%%%                ?COMMAND_ID_DELIVER_SM ->
%%%                    case operation:unpack(BinaryPdu) of
%%%                        {ok, Pdu} ->
%%%                             case operation:get_param(source_addr, Pdu) of
%%%                                 {ok, SourceAddr} ->
%%%                                     true;
%%%                                 _Otherwise ->
%%%                                     false
%%%                             end;
%%%                         Error ->
%%%                             false
%%%                    end;
%%%                _CommandId ->
%%%                    false
%%%            end
%%%        end,
%%% smpp_log:add_error_logger_handler([{pred, Pred}]),
%%% </pre>
%%%
%%% <p>By default the information sent to the error_logger is the complete PDU
%%% parameters list, you could for instance send the PDU in hex format by 
%%% simply redefining the format function:</p>
%%%
%%% <pre>
%%% Format = fun(BinaryPdu) -> binary:to_hexlist(BinaryPdu) end,
%%% smpp_log:swap_error_logger_handler([{format, Format}]),
%%% </pre>
%%%
%%% <p>Once finished with the debugging you can delete or swap the 
%%% <i>error_logger</i> handler back to the defaults.  Default logging
%%% predicate sends to the <i>error_logger</i> only those PDU responses with 
%%% an error code other than 0.</p>
%%%
%%% <h2>Changes 1.3 -&gt; 1.4</h2>
%%%
%%% [11 Jul 2007]
%%%
%%% <ul>
%%%   <li>Add name to disk_log.  Having a name let us starting several ESMEs
%%%      in the same node, each ESME with a different disk_log.
%%%   </li>
%%%   <li>Use proplists module.</li>
%%% </ul>
%%%
%%% @copyright 2005 Enrique Marcote Peña
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
         add_disk_log_handler/2, 
         add_error_logger_handler/2,
         count/4,
         delete_disk_log_handler/1, 
         delete_error_logger_handler/1,
         match/4,
         match/5,
         notify_peer_operation/2,
         notify_self_operation/2,
         swap_disk_log_handler/2,
         swap_error_logger_handler/2,
         stop/1]).

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
-define(NAME, ?MODULE).    % Default disk_log name.

% Default logging predicates.
%
% Logs every operation PDU other than an enquire_link or an enquire_link_resp.
-define(DISK_LOG_PRED, 
        fun(BinaryPdu) ->
                case pdu_syntax:command_id(BinaryPdu) of
                    ?COMMAND_ID_ENQUIRE_LINK ->
                        false;
                    ?COMMAND_ID_ENQUIRE_LINK_RESP ->
                        false;
                    _CommandId ->
                        true
                end
        end).
% Logs operation PDU responses with an error command_status
-define(ERROR_LOGGER_PRED, 
        fun(BinaryPdu) ->
                case pdu_syntax:command_status(BinaryPdu) of
                    0 ->
                        false;
                    _CommandStatus ->
                        true
                end
        end).

% Default disk_log format function
-define(DISK_LOG_FORMAT, fun(BinaryPdu) -> BinaryPdu end).

% Default error_logger format function
-define(ERROR_LOGGER_FORMAT, 
        fun(BinaryPdu) ->
                case operation:unpack(BinaryPdu) of
                    {ok, PduDict} ->
                        operation:to_list(PduDict);
                    Error ->
                        Error
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
%% %@spec {state, Name, Type, Pred, File, Size, Format}
%%
%% %@doc Representation of the server's state
%%
%% <dl>
%%   <dt>Name: </dt><dd>Name of the disk_log.  Default value is 
%%     <tt>smpp_log</tt>.</dd>
%%   <dt>Type: </dt><dd><tt>disk_log</tt> or <tt>error_logger</tt>.</dd>
%%   <dt>Pred: </dt><dd>the predicate binary PDUs must satisfy in order to
%%     be logged.
%%   </dd>
%%   <dt>File: </dt><dd>File name of the disk_log or the error_logger.</dd>
%%   <dt>Size: </dt><dd>Size of the disk_log.  Only relevant for disk_log
%%     handlers.
%%   </dd>
%%   <dt>Format: </dt><dd>function to format PDUs sent to the logs.</dd>
%% </dl>
%% %@end
-record(state, {name, type, pred, file, size, format}).

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
    gen_event:start(). 


%% @spec start_link() -> Result
%%    Result = {ok, Pid} | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid}
%%
%% @doc Starts the server and links to it.
%%
%% @see gen_event:start_link/1
%% @end
start_link() ->
    gen_event:start_link(). 


%% @spec add_disk_log_handler(Pid, Args) -> Result
%%    Pid = pid()
%%    Args = [Arg]
%%    Arg =  {name, Name} | 
%%           {file, File} | 
%%           {pred, Pred} | 
%%           {size, Size} | 
%%           {format, Format}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds the disk log event handler.
%%
%% <ul>
%%   <li><b>Name:</b> disk log name.  Default is <i>smpp_log</i>.</li>
%%   <li><b>File:</b> disk log file name.  Default is <i>smpp_log</i>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be logged.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.  Default predicate
%%     logs every PDU operation other than <i>enquire_link</i> and
%%     <i>enquire_link_resp</i>.
%%   </li>
%%   <li><b>Size:</b> a pair <tt>{MaxNoBytes, MaxNoFiles}</tt> as given by the 
%%     <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/disk_log.html">disk_log:open/1</a> option <tt>size</tt>.  The default size is
%%     <tt>{10485760, 20}</tt> (10M x 20).  Assuming 256 bytes for each PDU.  
%%     Individual log files of the wrap disk log can store up to 40960 PDUs 
%%     for the default value.  Having 20 files on the wrap log, the total PDU 
%%     capacity, before wrapping to the first log, is about 819200.
%%   </li>
%%   <li><b>Format:</b> a fun <tt>fun(BinaryPdu) -&gt; term()</tt> that gets
%%     the binary PDU as input and returns the term sent to the disk_log.  
%%     Default format function stores the binary PDU as is.
%%   </li>
%% </ul>
%%
%% @see gen_event:add_handler/3
%% @end
add_disk_log_handler(Pid, Args) ->
    NewArgs = [{type, disk_log}|Args],
    gen_event:add_handler(Pid, {?MODULE, disk_log}, NewArgs).


%% @spec add_error_logger_handler(Pid, Args) -> Result
%%    Pid = pid()
%%    Args = [Arg]
%%    Arg = {file, File} | {pred, Pred} | {format, Format}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Adds the error logger event handler.
%%
%% <ul>
%%   <li><b>File:</b> error_logger logfile.  If not defined or equals to the
%%     atom <tt>undefined</tt>, then no logfile is used.
%%   </li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be logged.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.  Default predicate
%%     logs only response PDUs with a <i>command_status</i> other than 0x0.
%%   </li>
%%   <li><b>Format:</b> a fun <tt>fun(BinaryPdu) -&gt; term()</tt> that gets
%%     the binary PDU as input and returns the term sent to the error_logger.  
%%     Default format function shows the complete parameters list of the PDU.
%%   </li>
%% </ul>
%%
%% @see gen_event:add_handler/3
%% @end
add_error_logger_handler(Pid, Args) ->
    NewArgs = [{type, error_logger}|Args],
    gen_event:add_handler(Pid, {?MODULE, error_logger}, NewArgs).


%% @spec count(Pid, Date, From, Pred) -> int()
%%    Pid = pid()
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun()
%%
%% @doc PDUs are logged in the disk log as <tt>{Now, BinaryPdu, From}</tt> 
%% terms.  This function counts logged PDUs matching the given 
%% <tt>Date</tt>, <tt>From</tt> and <tt>Pred</tt>.
%%
%% <ul>
%%   <li><b>Date:</b> <tt>any</tt>, <tt>{from, Time}</tt>, <tt>{until, Time}
%%     </tt> or <tt>{lapse, FromTime,ToTime}</tt>.</li>
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or <tt>any</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%%
%% @see match/3
%% @see match/4
%% @end 
count(Pid, Date, From, Pred) ->
    count(Pid, Date, From, Pred, start, 0).

%% @doc Auxiliary function for count/3
%%
%% @see count/3
%% @end 
count(Pid, Date, From, Pred, Continuation, Count) ->
    case match(Pid, Date, From, Pred, Continuation) of
        eof ->
            {ok, Count};
        {Continuation2, List} ->
            count(Pid, Date, From, Pred, Continuation2, Count + length(List));
        {Continuation2, List, _Badbytes} ->
            count(Pid, Date, From, Pred, Continuation2, Count + length(List));
        Error ->
            Error
    end.


%% @spec delete_disk_log_handler(Pid) -> Result
%%    Pid = pid()
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Deletes the disk log event handler
%%
%% @see gen_event:delete_handler/3
%% @end
delete_disk_log_handler(Pid) ->
    gen_event:delete_handler(Pid, {?MODULE, disk_log}, stop).


%% @spec delete_error_logger_handler(Pid) -> Result
%%    Pid = pid()
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Deletes the disk log event handler
%%
%% @see gen_event:delete_handler/3
%% @end
delete_error_logger_handler(Pid) ->
    gen_event:delete_handler(Pid, {?MODULE, error_logger}, stop).


%% @spec match(Pid, Date, From, Pred) -> Result
%%    Pid = pid()
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun()
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
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or <tt>any</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% 
%% <p>This function internally calls <a href="#match-4">match/4</a> with 
%% <tt>start</tt> as the <tt>Continuation</tt> parameters.</p>
%%
%% @see match/4
%%
%% @equiv match(Date, From, Pred, start)
%% @end 
match(Pid, Date, From, Pred) ->
    match(Pid, Date, From, Pred, start).


%% @spec match(Pid, Date, From, Pred, Continuation) -> Result
%%    Pid = pid()
%%    Date = any | {from, Time} | {until, Time} | {lapse, FromTime, ToTime}
%%    Time = {{Year, Month, Day},{Hour, Minutes, Seconds}}
%%    FromTime = Time
%%    ToTime = Time
%%    From = any | self | peer
%%    Pred = fun()
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
%%   <li><b>From:</b> <tt>self</tt>, <tt>peer</tt> or <tt>any</tt>.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be matched.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.</li>
%% </ul>
%% 
%% <p>This function uses <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/disk_log.html">disk_log:chunk/2</a> to read the terms 
%% appended to the disk log.  <tt>Continuation</tt> parameter and result
%% meanings are those documented in that function.</p>
%%
%% @see disk_log:chunk/2
%% @end 
match(Pid, Date, From, Pred, Continuation) ->
    Request = {match, Date, From, Pred, Continuation},
    gen_event:call(Pid, {?MODULE, disk_log}, Request).

%% @spec notify_peer_operation(Pid, Pdu) -> ok
%%    Pid = pid()
%%    Pdu = binary()
%%
%% @doc Notifies peer SMPP operations to the log.
%%
%% @equiv gen_event:notify(Pid, {peer, Pdu})
%% @end
notify_peer_operation(Pid, Pdu) ->
    gen_event:notify(Pid, {peer, Pdu}).


%% @spec notify_self_operation(Pid, Pdu) -> ok
%%    Pid = pid()
%%    Pdu = binary()
%%
%% @doc Notifies self SMPP operations to the log.
%%
%% @equiv gen_event:notify(Pid, {self, Pdu})
%% @end
notify_self_operation(Pid, Pdu) ->
    gen_event:notify(Pid, {self, Pdu}).


%% @spec swap_disk_log_handler(Pid, Args) -> Result
%%    Pid = pid()
%%    Args = [Arg]
%%    Arg = {file, File} | {pred, Pred} | {size, Size}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Swaps the disk log event handler.
%%
%% <ul>
%%   <li><b>File:</b> disk log file name.</li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be logged.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.
%%   </li>
%%   <li><b>Size:</b> a pair <tt>{MaxNoBytes, MaxNoFiles}</tt> as given by the 
%%     <a href="http://www.erlang.se/doc/doc-5.4.3/lib/kernel-2.10.3/doc/html/disk_log.html">disk_log:open/1</a> option <tt>size</tt>. 
%%   </li>
%%   <li><b>Format:</b> a fun <tt>fun(BinaryPdu) -&gt; term()</tt> that gets
%%     the binary PDU as input and returns the term sent to the disk_log.  
%%     Default format function stores the binary PDU as is.
%%   </li>
%% </ul>
%%
%% <p>If a parameter is unspecified, the value of the old disk_log handler 
%% is used.</p>
%%
%% @see gen_event:swap_handler/3
%% @end
swap_disk_log_handler(Pid, Args) ->
    Handler = {?MODULE, disk_log},
    NewArgs = [{type, disk_log}|Args],
    gen_event:swap_handler(Pid, {Handler,swap_handler}, {Handler,NewArgs}).


%% @spec swap_error_logger_handler(Pid, Args) -> Result
%%    Pid = pid()
%%    Args = [Arg]
%%    Arg = {file, File} | {pred, Pred} | {format, Format}
%%    Result = ok | {'EXIT', Reason} | term()
%%    Reason = term()
%%
%% @doc Swaps the error logger event handler.
%%
%% <ul>
%%   <li><b>File:</b> error_logger logfile.  If not defined or equals to the
%%     atom <tt>undefined</tt>, then no logfile is used.
%%   </li>
%%   <li><b>Pred:</b> a predicate the binary PDUs must satisfy in order to
%%     be logged.  <tt>fun(BinaryPdu) -&gt; bool()</tt>.  Default predicate
%%     logs only response PDUs with a <i>command_status</i> other than 0x0.
%%   </li>
%%   <li><b>Format:</b> a fun <tt>fun(BinaryPdu) -&gt; term()</tt> that gets
%%     the binary PDU as input and returns the term sent to the error_logger.  
%%     Default format function shows the complete parameters list of the PDU.
%%   </li>
%% </ul>
%%
%% <p>If a parameter is unspecified, the value of the old disk_log handler 
%% is used.</p>
%%
%% @see gen_event:swap_handler/3
%% @end
swap_error_logger_handler(Pid, Args) ->
    Handler = {?MODULE, error_logger},
    NewArgs = [{type, error_logger}|Args],
    gen_event:swap_handler(Pid, {Handler,swap_handler}, {Handler,NewArgs}).


%% @spec stop(Pid) -> ok
%%    Pid    = pid()
%%
%% @doc Stops the smpp_log event manager with the given <tt>Pid</tt>.
%% @end
stop(Pid) ->
    gen_event:stop(Pid). 

%%%===================================================================
%%% Server functions
%%%===================================================================
%% @spec init(Args) -> {ok, State}
%%    Args  = term()
%%    State = term()
%%
%% @doc Initialize the event handler
%% @end
init({NewArgs, OldArgs}) ->
    init(merge_args(NewArgs, OldArgs));
init(Args) ->
    case proplists:get_value(type, Args, disk_log) of
        disk_log ->
            init_disk_log(Args);
        error_logger ->
            init_error_logger(Args)
    end.


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
handle_event({From, IoList}, S) when list(IoList) ->
    handle_event({From, concat_binary(IoList)}, S);
handle_event({From, BinaryPdu}, #state{type=T, pred=P, format=F} = S) ->
    case catch P(BinaryPdu) of
        true when T == disk_log ->
            disk_log:alog(S#state.name, {now(), F(BinaryPdu), From});
        true when T == error_logger ->
            report:info(?MODULE, {smpp_operation, From}, F(BinaryPdu));
        _Otherwise ->
            ok
    end,
    {ok, S}.


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
handle_call({match, Date, From, Pred, Continuation}, S) ->
    F = fun({_, _, F}) when From /= any, From /= F ->
                false;
           ({_, P, _}) when Date == any ->
                Pred(P);
           ({N, P, _}) ->
                T = calendar:now_to_local_time(N),
                case Date of
                    {from, T1} when T >= T1 ->
                        Pred(P);
                    {until, T1} when T =< T1 ->
                        Pred(P);
                    {lapse, T1, T2} when T >= T1, T =< T2 ->
                        Pred(P);
                    _Otherwise ->
                        false
                end
        end,
    R = case disk_log:chunk(S#state.name, Continuation) of
            {Continuation2, List} ->
                {Continuation2, lists:filter(F, List)};
            {Continuation2, List, Badbytes} ->
                {Continuation2, lists:filter(F, List), Badbytes};
            Error ->
                Error
        end,
    {ok, R, S};
handle_call(Request, S) ->
    report:error(?MODULE, unexpected_call, Request, S),
    {ok, ok, S}.

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
handle_info(Info, S) ->
    report:error(?MODULE, unexpected_info, Info, S),
    {ok, S}.

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
terminate(Reason, #state{type = disk_log} = S) ->
    Result = disk_log:close(S#state.name),
    Args = setup_args(S),
    report:info(smpp_log, terminate, [{reason,Reason}, {result,Result}|Args]),
    Args;
terminate(Reason, #state{type = error_logger, file = File} = S) ->
    Result = case error_logger:logfile(filename) of
                 File when Reason /= swap_handler ->
                     error_logger:logfile(close);
                 _Otherwise ->
                     ok
             end,
    Args = setup_args(S),
    report:info(smpp_log, terminate, [{reason,Reason}, {result,Result}|Args]),
    Args.


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
%% @spec init_disk_log(Args) -> {ok, State}
%%    Args = term()
%%    State = term()
%%
%% @doc Inits the <i>disk_log</i> handler.
%%
%% @see init/1
%% @see init_error_logger/1
%% @end 
init_disk_log(Args) ->
    Name = proplists:get_value(name, Args, ?NAME),
    Pred = proplists:get_value(pred, Args, ?DISK_LOG_PRED),
    File = proplists:get_value(file, Args, atom_to_list(?NAME)),
    Size = proplists:get_value(size, Args, ?SIZE),
    Format = proplists:get_value(format, Args, ?DISK_LOG_FORMAT),
    Result = disk_log:open([{name,Name},{file,File},{type,wrap},{size,Size}]),
    report:info(smpp_log, add_handler, [{result, Result}|Args]),
    {ok,#state{name = Name, 
               type = disk_log, 
               pred = Pred, 
               file = File, 
               size = Size, 
               format = Format}}.

%% @spec init_error_logger(Args) -> {ok, State}
%%    Args = term()
%%    State = term()
%%
%% @doc Inits the <i>error_logger</i> handler.
%%
%% @see init/1
%% @see init_disk_log/1
%% @end 
init_error_logger(Args) ->
    Pred = proplists:get_value(pred, Args, ?ERROR_LOGGER_PRED),
    File = proplists:get_value(file, Args, undefined),
    Format = proplists:get_value(format, Args, ?ERROR_LOGGER_FORMAT),
    Result = case error_logger:logfile(filename) of
                 {error, _What} when File /= undefined ->
                     error_logger:logfile({open, File});
                 {error, _What} -> % File undefined
                     ok;
                 File ->           % File already open
                     ok;
                 _Otherwise when File /= undefined ->
                     error_logger:logfile(close),
                     error_logger:logfile({open, File});
                 _Otherwise ->     % File undefined
                     error_logger:logfile(close)
             end,
    report:info(smpp_log, add_handler, [{result, Result}|Args]),
    {ok, #state{type=error_logger, pred=Pred, file=File, format=Format}}.

%% @spec setup_args(State) -> Args
%%    State = state()
%%    Args = [Arg]
%%    Arg = {type, Type} | 
%%          {pred, Pred} | 
%%          {file, File} | 
%%          {size, Size} | 
%%          {format, Format}
%%    Type = error_logger | disk_log
%%    Pred = fun()
%%    File = string() | atom()
%%    Size = int()
%%    Format = fun()
%%
%% @doc Gets the setup <tt>Args</tt> given the <tt>State</tt> record.
%% @end 
setup_args(#state{type = T, pred = P, file = N, size = S, format = F}) ->
    [{type, T}, {pred, P}, {file, N}, {size, S}, {format, F}].


%% @spec merge_args(ArgsList1, ParamList2) -> NewParamList
%%    ParamList1 = [{ParamName, ParamValue}]
%%    ParamList2 = [{ParamName, ParamValue}]
%%    NewParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%
%% @doc Merge two parameter lists.  If an parameter appears on both lists,
%% the value from the first list will be taken.
%% @end
merge_args(Args1, Args2) ->
    my_lists:ukeymerge(1, lists:keysort(1, Args1), lists:keysort(1, Args2)).
