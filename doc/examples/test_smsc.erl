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

%%% @doc Test SMSC
%%%
%%% <p>A very simple test SMSC.</p>
%%%
%%% @copyright 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 1.1, {18 Jun 2004} {@time}.
%%% @end
-module(test_smsc).

-behaviour(gen_smsc).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("oserl.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([start_link/0, deliver_sm/2, stop/0]).

%%%-------------------------------------------------------------------
%%% Internal SMSC exports
%%%-------------------------------------------------------------------
-export([init/1,
         handle_bind/3,
         handle_operation/3,
         handle_unbind/3,
         handle_listen_error/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
-define(SERVER, ?MODULE).
-define(SYSTEM_ID, atom_to_list(?MODULE)).
-define(SMPP_PORT, ?DEFAULT_SMPP_PORT).
-define(DESTINATION_ADDR, "*").

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {state, MessageId, Rx, Tx}
%%
%% %@doc Representation of the SMSC server state.
%%
%% <dl>
%%   <dt>MessageId: </dt><dd>Incomming messages are identified by a sequence.
%%   </dd>
%%   <dt>Rx: </dt><dd>Peer receiver sessions.</dd>
%%   <dt>Tx: </dt><dd>Peer transmitter sessions.</dd>
%% </dl>
%% %@end
-record(state, {message_id = 0, rx = [], tx = []}).

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec start_link() -> Result
%%    Result = {ok, Pid} | ignore | {error, Error}
%%    Pid    = pid()
%%    Error  = {already_started, Pid} | term()
%%
%% @doc Starts the SMSC server.
%%
%% @see gen_server
%% @see start/0
%% @end
start_link() ->
    gen_smsc:start_link({local, ?SERVER}, ?MODULE, [], []),
    gen_smsc:listen_start(?SERVER, ?SMPP_PORT, infinity, ?DEFAULT_SMPP_TIMERS).


%% @spec deliver_sm(SourceAddr, ShortMessage) -> ok
%%     SourceAddr = string()
%%     ShortMessage = string()
%%
%% @doc Delivers a SM to every receiver ESME.
%%
%% @see handle_cast/2
%% @end
deliver_sm(SourceAddr, ShortMessage) ->
    ParamList = [{source_addr, SourceAddr}, 
                 {destination_addr, ?DESTINATION_ADDR}, 
                 {short_message, ShortMessage}],
    gen_smsc:cast(?SERVER, {deliver_sm, ParamList}).


%% @spec stop() -> ok
%%
%% @doc Stops the SMSC server.
%%
%% @see handle_call/3
%%
%% @equiv gen_server:call(SERVER, die, 10000)
%% @end
stop() ->
    gen_smsc:call(?SERVER, die, 10000).

%%%===================================================================
%%% Server functions
%%%===================================================================
%% @spec init(Args) -> Result
%%    Args    = term()
%%    Result  = {ok, State} | {ok, State, Timeout} | ignore | {stop, Reason}
%%    State   = term()
%%    Timeout = int() | infinity
%%    Reason  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#init-1">
%% gen_smsc - init/1</a> callback implementation.
%% 
%% <p>Initiates the server.</p>
%% @end
init([]) ->
    % You may start sessions and issue bind requests here.
    {ok, #state{}}.


%% @spec handle_bind(Bind, From, State) -> Result
%%    Bind = {CmdName, Session, Pdu}
%%    CmdName = bind_receiver | bind_transmitter | bind_transceiver
%%    Session = pid()
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_bind-3">gen_smsc - handle_bind/3</a> callback implementation.
%%
%% <p>Handle <i>bind</i> requests from the peer ESME.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the bind response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the SMSC in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_bind({bind_receiver, Rx, _Pdu}, _From, S) ->
    erlang:monitor(process, Rx),
    io:format("bound_rx: ~p~n", [Rx]),
    ParamList = [{system_id, ?SYSTEM_ID}],
    {reply, {ok, ParamList}, S#state{rx = [Rx|S#state.rx]}};
handle_bind({bind_transmitter, Tx, _Pdu}, _From, S) ->
    erlang:monitor(process, Tx),
    io:format("bound_tx: ~p~n", [Tx]),
    ParamList = [{system_id, ?SYSTEM_ID}],
    {reply, {ok, ParamList}, S#state{tx = [Tx|S#state.tx]}};
handle_bind({bind_transceiver, Trx, _Pdu}, _From, S) ->
    erlang:monitor(process, Trx),
    io:format("bound_trx: ~p~n", [Trx]),
    ParamList = [{system_id, ?SYSTEM_ID}],
    {reply, {ok, ParamList}, S#state{rx = [Trx|S#state.rx],
                                     tx = [Trx|S#state.tx]}}.

%% @spec handle_operation(Operation, From, State) -> Result
%%    Operation = {CmdName, Session, Pdu}
%%    CmdName = broadcast_sm        |
%%              cancel_broadcast_sm |
%%              cancel_sm           |
%%              query_broadcast_sm  |
%%              query_sm            |
%%              replace_sm          |
%%              submit_multi        |
%%              submit_sm           |
%%              data_sm
%%    Session = pid()
%%    Pdu = pdu()
%%    From = term()
%%    State = term()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = {ok, ParamList} | {error, Error, ParamList}
%%    ParamList = [{ParamName, ParamValue}]
%%    ParamName = atom()
%%    ParamValue = term()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_operation-3">gen_smsc - handle_operation/3</a> callback implementation.
%%
%% <p>Handle <i>broadcast_sm</i>, <i>cancel_broadcast_sm</i>,
%% <i>cancel_sm</i>, <i>query_broadcast_sm</i>, <i>query_sm</i>,
%% <i>replace_sm</i>, <i>submit_multi</i>, <i>submit_sm</i> and
%% <i>data_sm</i> operations from peer ESMEs.</p>
%%
%% <p>The <tt>ParamList</tt> included in the response is used to construct
%% the response PDU.  If a command_status other than ESME_ROK is to
%% be returned by the SMSC in the response PDU, the callback should return the
%% term <tt>{error, Error, ParamList}</tt>, where <tt>Error</tt> is the
%% desired command_status error code.</p>
%% @end
handle_operation({CmdName, Session, Pdu}, From, S) when CmdName == submit_sm;
                                                        CmdName == data_sm ->
    N = operation:get_param(destination_addr, Pdu),
    T = element(2, sm:message_user_data(Pdu)),
    I = S#state.message_id,
    io:format("~p: ~p - ~p~n", [CmdName, N, T]),
    {reply, {ok, [{message_id, integer_to_list(I)}]}, S#state{message_id=I+1}};
handle_operation({CmdName, Session, Pdu}, From, S) ->
    % Don't know how to handle CmdName
    io:format("Don't know how to handle ~p~n", [CmdName]),
    {reply, {error, ?ESME_RINVCMDID, []}, S}.


%% @spec handle_unbind(Unbind, From, State) -> Result
%%    Unbind = {unbind, Session, Pdu}
%%    Session = pid()
%%    Pdu = pdu()
%%    Result = {reply, Reply, NewState}          |
%%             {reply, Reply, NewState, Timeout} |
%%             {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, Reply, NewState}   |
%%             {stop, Reason, NewState}
%%    Reply = ok | {error, Error}
%%    Error = int()
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_unbind-3">gen_smsc - handle_unbind/3</a> callback implementation.
%%
%% <p>Handle <i>unbind</i> requests from the peer ESMEs.</p>
%%
%% <p>If <tt>ok</tt> returned an unbind_resp with a ESME_ROK 
%% command_status is sent to the MC and the session moves into the unbound
%% state.  When <tt>{error, Error}</tt> is returned by the ESME, the
%% response PDU sent by the session to the MC will have an <tt>Error</tt>
%% command_status and the session will remain on it's current bound state
%% (bound_rx, bound_tx or bound_trx).</p>
%% @end
handle_unbind({unbind, Session, Pdu}, _From, S) ->
    io:format("unbind: ~p~n", [Session]),
    {reply, ok, S}.


%% @spec handle_listen_error(State) -> Result
%%    State = term()
%%    Result = {noreply, NewState}               |
%%             {noreply, NewState, Timeout}      |
%%             {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout = int()
%%    Reason = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_listen_error-1">gen_smsc - handle_listen_error/1</a> callback implementation.
%%
%% <p>Handle listen failures.</p>
%% @end
handle_listen_error(State) ->
    {stop, {error, listen_error}, State}.


%% @spec handle_call(Request, From, State) -> Result
%%    Request   = term()
%%    From      = {pid(), Tag}
%%    State     = term()
%%    Result    = {reply, Reply, NewState}          |
%%                {reply, Reply, NewState, Timeout} |
%%                {noreply, NewState}               |
%%                {noreply, NewState, Timeout}      |
%%                {stop, Reason, Reply, NewState}   |
%%                {stop, Reason, NewState}
%%    Reply     = term()
%%    NewState  = term()
%%    Timeout   = int() | infinity
%%    Reason    = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_call-3">gen_smsc - handle_call/3</a> callback implementation.
%%
%% <p>Handling call messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, Reply, NewState}</tt>
%%   terminate/2 is called</li>
%%   <li>On <tt>{stop, Reason, NewState}</tt>
%%   terminate/2 is called</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_call(die, _From, State) ->
    {stop, normal, ok, State}.


%% @spec handle_cast(Request, State) -> Result
%%    Request  = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_cast-2"> gen_smsc - handle_cast/2</a> callback implementation.
%%
%% <p>Handling cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_cast({deliver_sm, ParamList}, #state{rx = Rx} = S) ->
    lists:foreach(fun(X) -> 
                          spawn(fun() -> gen_smsc:deliver_sm(X, ParamList) end)
                  end, Rx),
    {noreply, S}.


%% @spec handle_info(Info, State) -> Result
%%    Info     = timeout | term()
%%    State    = term()
%%    Result   = {noreply, NewState}          |
%%               {noreply, NewState, Timeout} |
%%               {stop, Reason, NewState}
%%    NewState = term()
%%    Timeout  = int() | infinity
%%    Reason   = normal | term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#handle_info-2"> gen_smsc - handle_info/2</a> callback implementation.
%%
%% <p>Handling all non call/cast messages.</p>
%%
%% <ul>
%%   <li>On <tt>{stop, Reason, State}</tt> terminate/2 is called.</li>
%% </ul>
%%
%% @see terminate/2
%% @end
handle_info({'DOWN', _MonitorReference, process, Object, Info}, S) ->
    case {lists:member(Object,S#state.rx), lists:member(Object,S#state.tx)} of
        {true, true} ->
            if
                Info == normal ->
                    io:format("trx_session closed: ~p~n", [Object]);
                true ->
                    io:format("trx_session failure: ~p - ~p~n", [Object, Info])
            end,
            {noreply, S#state{rx = lists:delete(Object, S#state.rx),
                              tx = lists:delete(Object, S#state.tx)}};
        {true, false} ->
            if
                Info == normal ->
                    io:format("rx_session closed: ~p~n", [Object]);
                true ->
                    io:format("rx_session failure: ~p - ~p~n", [Object, Info])
            end,
            {noreply, S#state{rx = lists:delete(Object, S#state.rx)}};
        {false, true} ->
            if
                Info == normal ->
                    io:format("tx_session closed: ~p~n", [Object]);
                true ->
                    io:format("tx_session failure: ~p - ~p~n", [Object, Info])
            end,
            {noreply, S#state{tx = lists:delete(Object, S#state.tx)}};
        _NotASession ->
            {stop, Info, S}
    end;
handle_info(Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> ok
%%    Reason = normal | shutdown | term()
%%    State  = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#terminate-2">
%% gen_smsc - terminate/2</a> callback implementation.
%%
%% <p>Shutdown the SMSC server.</p>
%%
%% <p>Return value is ignored by <tt>gen_server</tt>.</p>
%% @end
terminate(kill, S) ->
    ok;
terminate(Reason, S) ->
    L = lists:usort(S#state.rx ++ S#state.tx),
    lists:foreach(fun(X) -> catch gen_smsc:unbind(X) end, L),
    lists:foreach(fun(X) -> catch gen_smsc:session_stop(X) end, L).


%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%%    OldVsn   = undefined | term()
%%    State    = term()
%%    Extra    = term
%%    NewState = term()
%%
%% @doc <a href="http://oserl.sourceforge.net/oserl/gen_smsc.html#code_change-2"> gen_smsc - code_change/2</a> callback implementation.
%%
%% <p>Convert process state when code is changed.</p>
%% @end
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
