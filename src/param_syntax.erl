%%%
% Copyright (C) 2003 Enrique Marcote Peña <mpquique@udc.es>
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
% @doc SMPP Parameter Syntax Library.
%
% <p>Functions for the SMPP parameter syntax manipulation.</p>
%
% <p>As a guideline and to help debugging, this preliminary version includes 
% references to the specific section numbers on [SMPP 5.0].  These comments
% are likely to disappear on further versions.</p>
%
%
% <h2>References</h2>
% <dl>
%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol 
%     Specification. Version 5.0. SMS Forum.
%   </dd>
% </dl>
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {21 Mar 2003} {@time}.
% @end
%%
-module(param_syntax).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("smpp_globals.hrl").
-include("param_syntax.hrl").

%%%-------------------------------------------------------------------
% External exports
%%--------------------------------------------------------------------
-export([chop_tlv/1, decode/2, encode/2, get_name/1]).

%%%-------------------------------------------------------------------
% Internal exports
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------

%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------

%%%===================================================================
% External functions
%%====================================================================
%%%
% @spec chop_tlv(Binary) -> {ok, Tlv, Rest} | {error, Binary}
%    Binary = bin()
%    Tlv    = bin()
%    Rest   = bin()
%
% @doc Returns the leading <tt>Tlv</tt> and the <tt>Rest</tt> of a 
% <tt>Binary</tt>.
% @end
%
% %@see
%
% %@equiv
%%
chop_tlv(<<Tag:16, Len:16, Binary/binary>>) -> 
    case Binary of
        <<Val:Len/binary-unit:8, Rest/binary>> ->
            {ok, <<Tag:16, Len:16, Val/binary>>, Rest};
        _TruncatedVal ->
            {error, Binary}
    end;

chop_tlv(Binary) ->
    {error, Binary}.


%%%
% @spec decode(Binary, ParamType) -> {ok, Value, Rest} | {error, Error}
%    Binary    = bin()
%    ParamType = standard() | tlv()
%    Value     = term()
%    Rest      = bin()
%    Error     = int()
%
% @doc Decodes a <tt>Value</tt> from a <tt>Binary</tt> using a 
% <tt>ParameterType</tt> specifier.  Please refer to the functions
% decode_std/2 and decode_tlv/2 for further details.
%
% @see decode_std/2
% @see decode_tlv/2
% @end
%
% %@equiv
%%
decode(Binary, ParamType) when record(ParamType, standard) ->
    decode_std(Binary, ParamType);

decode(Binary, ParamType) when record(ParamType, tlv) ->
    decode_tlv(Binary, ParamType);

decode(_Binary, _ParamType) ->
    {error, ?ESME_RUNKNOWNERR}.


%%%
% @spec encode(Value, ParamType) -> {ok, Binary} | {error, Error}
%    Value     = term()
%    ParamType = standard() | tlv()
%    Binary    = bin()
%    Error     = int()
%
% @doc Encodes a <tt>Value</tt> using a <tt>ParameterType</tt> 
% specifier.    Please refer to the functions encode_std/2 and encode_tlv/2
% for further details.
%
% @see encode_std/2
% @see encode_tlv/2
% @end
%
% %@equiv
%%
encode(Value, ParamType) when record(ParamType, standard) ->
    encode_std(Value, ParamType);

encode(Value, ParamType) when record(ParamType, tlv) ->
    encode_tlv(Value, ParamType);

encode(_Value, _ParamType) ->
    {error, ?ESME_RUNKNOWNERR}.



%%%
% @spec get_name(ParamType) -> Name
%    ParamType = standard() | tlv()
%    Name      = atom()
%
% @doc Gets the name from a parameter type declaration.
% @end
%
% %@see
%
% %@equiv
%%
get_name(ParamType) when record(ParamType, standard) ->
    ParamType#standard.name;

get_name(ParamType) when record(ParamType, tlv) ->
    ParamType#tlv.name.


%%%===================================================================
% Internal functions
%%====================================================================
%%%
% @spec decode_std(Binary, StdType) -> {ok, Value, Rest} | {error, Error}
%    Binary    = bin()
%    StdType   = {standard, Name, Domain, Error}
%    Name      = atom()
%    Domain    = empty()          |
%                constant()       | 
%                integer()        | 
%                c_octet_string() | 
%                octet_string()   |
%                list()           |
%                composite()      |
%                union()
%    Value     = term()
%    Rest      = bin()
%    Error     = int()
%
% @doc Decodes a <tt>Value</tt> from the head of a <tt>Binary</tt> 
% using a standard type specifier (<tt>StdType</tt>).
%
% <p>An standard parameter is directly decoded according to its domain.</p>
%
% <ol>
%   <li>If the <tt>Value</tt> is successfully decoded according to the 
%     parameter domain using the base_syntax:decode/2 the term 
%     <tt>{ok, Value, Rest}</tt> is returned.
%   </li>
%   <li>If the value can not be decode and the parameter has an associated
%     <tt>Error</tt>, the term <tt>{error, Error}</tt> is returned. 
%     If no Error assigned to the parameter, the term 
%     <tt>{error, ?ESME_RUNKNOWNERR}</tt> is returned.
%   </li>
% </ol>
%
% @see base_syntax:decode/2
% @end
%
% %@equiv
%%
decode_std(Binary, #standard{domain = Domain, error = Error}) ->
    case base_syntax:decode(Binary, Domain) of
        {error, _Reason} ->
            if
                Error == undefined ->
                    {error, ?ESME_RUNKNOWNERR};
                true ->
                    {error, Error}
            end;
        Ok ->
            Ok
    end.


%%%
% @spec decode_tlv(Binary, TlvType) -> Result
%    Binary   = bin()
%    TlvType  = {tlv,Name,Tag,Domain,Reserved,Mandatory,Multiple,Default,Error}
%    Name     = atom()
%    Tag      = int()
%    Domain   = Type
%    Type     = empty()          |
%               constant()       | 
%               integer()        | 
%               c_octet_string() | 
%               octet_string()   |
%               list()           |
%               composite()      |
%               union()
%    Reserved = Type
%    Mandatory= bool()
%    Multiple = bool()
%    Default  = term()
%    Error    = int()
%    Result   = {ok, Value, Rest} | {ok, Values, Rest} | {error, Error}
%    Value    = term()
%    Values   = [Value]
%    Rest     = bin()
%
% @doc Extracts a TLV from a Binary.  This functions searches for the TLV in
% the binary and decodes it.  Assumes that Binary is a concatenation of 
% encoded TLVs.  If the TLV is multiple, the function gets every occurrence of 
% the TLV, otherwise, once the first occurrence is found the function stops
% searching.
%
% <p>The <tt>Rest</tt> of the binary, without the decoded octets, is 
% returned in the resulting term.  The order of the remainder TLVs is
% preserved within <tt>Rest</tt>.</p>
%
% <p>This function returns:</p>
%
% <ul>
%   <li><tt>{ok, undefined, Rest}</tt> if the TLV is optional
%     and no ocurrences where found.  If a TLV having a reserved value was 
%     discarded, <tt>Rest</tt> might not be equal to <tt>Binary</tt>.
%   </li>
%   <li><tt>{ok, ValueList, Rest}</tt> where <tt>ValueList</tt> is a
%     list of terms, if at least one occurrence of a multiple TLV was found.
%     <tt>Rest</tt> is <tt>Binary</tt> without the decoded bytes.
%   </li>
%   <li><tt>{ok, Value, Rest}</tt> where <tt>Value</tt> is a term, if
%     one occurrence of a single TLV was found.
%   </li>
%   <li><tt>{error, ?ESME_RMISSINGTLV}</tt> if a mandatory TLV doesn't 
%     ocurr on <tt>Binary</tt>.
%   </li>
%   <li><tt>{error, ?ESME_RINVTLVSTREAM}</tt> if the value of the TLV is 
%     shorter than the length specified on the TLV or a binary with 1-3 octets 
%     was encountered while trying to decode the TLV, indicating a corrupt PDU.
%   </li>
%   <li><tt>{error, ?ESME_RINVTLVLEN}</tt> is retuned if the TLV has
%     specified a length that is invalid.
%   </li>
%   <li><tt>{error, ?ESME_RINVTLVVAL}</tt> is returned if the TLV value
%     could not be decoded.  If the TLV has a particular error code associated,
%     the TLV error code is reported instead of ?ESME_RINVTLVVAL.
%   </li>
% </ul>
%
% @see decode_tlv/4
% @end
%
% %@equiv
%%
decode_tlv(Binary, TlvType) ->
    case decode_tlv(Binary, TlvType, [], []) of
        {ok, [], Rest} ->
            case TlvType#tlv.mandatory of
                true ->
                    {error, ?ESME_RMISSINGTLV};
                _False ->
                    {ok, undefined, Rest}
            end;
        {ok, Values, Rest} ->
            case TlvType#tlv.multiple of
                true ->
                    {ok, Values, Rest};
                _False ->
                    {ok, hd(Values), Rest}
            end;
        Error ->
            Error
    end.
            
%%%
% @doc Auxiliary function for decode_tlv/2
%
% @see decode_tlv_value/2
% @end
%%
decode_tlv(<<>>, _TlvType, Values, Acc) ->
    {ok, Values, list_to_binary(lists:reverse(Acc))};

decode_tlv(<<T:16, L:16, Bin/binary>>, #tlv{tag = T} = TlvType, Values, Acc) ->
    % The Tag matches, try to decode the TLV
    case Bin of
        <<V:L/binary-unit:8, Rest/binary>> ->
            % The domain is updated to fit the Length (L) of the value (V)
            % of the TLV.  Fit never enlarges the size, see base_syntax:fit/2.
            Domain = base_syntax:fit(TlvType#tlv.domain, L),
            case decode_tlv_value(V, TlvType#tlv{domain = Domain}) of
                {ok, undefined} ->
                    decode_tlv(Rest, TlvType, Values, Acc);
                {ok, Value} ->
                    case TlvType#tlv.multiple of
                        true ->   
                            % If multiple values allowed, keep on searching
                            decode_tlv(Rest, TlvType, [Value|Values], Acc);
                        _False -> 
                            % On single TLVs no further searching needed
                            decode_tlv(<<>>, TlvType, [Value], [Rest|Acc])
                    end;
                Error ->
                    Error
            end;
        _TruncatedVal ->  
            % The Value is shorter than Len octets, report the error
            {error, ?ESME_RINVTLVSTREAM}
    end;

decode_tlv(<<T:16, L:16, Bin/binary>>, TlvType, Values, Acc) ->
    % The Tag doesn't match, move the TLV to Acc and keep on searching.
    case Bin of
        <<V:L/binary-unit:8, Rest/binary>> ->
            Tlv = <<T:16, L:16, V:L/binary-unit:8>>,
            decode_tlv(Rest, TlvType, Values, [Tlv|Acc]);
        TruncatedVal -> 
            % The remainder of the binary is shorter than Len octets.  Error
            % handled at the moment of decoding this particular TLV.
            Tlv = <<T:16, L:16, TruncatedVal/binary>>,
            decode_tlv(<<>>, TlvType, Values, [Tlv|Acc])
    end;

decode_tlv(_Binary, _ParamType, _Values, _Acc) ->
    % 1-3 octets of data remaining, indicating a corrupt PDU.
    {error, ?ESME_RINVTLVSTREAM}.


%%%
% @doc Auxiliary function for decode_tlv/4
%
% @see base_syntax:decode/2
% @end
%%
decode_tlv_value(BinaryValue, TlvType) ->
    case base_syntax:decode(BinaryValue, TlvType#tlv.domain) of
        {ok, Value, <<>>} ->
            {ok, Value};
        {ok, _Value, _Something} ->
            % Uhmm, not all the value was decoded... TLV length is greater
            % than the size permitted by the domain specifier.
            {error, ?ESME_RINVTLVLEN}; 
        {error, _Reason} ->
            % The value doesn't match the declared domain for this TLV, it may
            % be a reserved value though, in such a case the SMPP protocol
            % specification recomends to return the default value.  Notice
            % that the default value could be the atom <tt>undefined</tt>.
            case base_syntax:decode(BinaryValue, TlvType#tlv.reserved) of
                {ok, _Value, <<>>} ->
                    {ok, TlvType#tlv.default};
                _Error ->
                    case TlvType#tlv.error of
                        undefined ->
                            {error, ?ESME_RINVTLVVAL};
                        ErrorCode ->
                            {error, ErrorCode}
                    end
            end
    end.


%%%
% @spec encode_std(Value, StdType) -> {ok, Binary} | {error, Error}
%    Value     = term()
%    StdType   = {standard, Name, Domain, Error}
%    Domain    = empty()          |
%                constant()       | 
%                integer()        | 
%                c_octet_string() | 
%                octet_string()   |
%                list()           |
%                composite()      |
%                union()
%    Binary    = bin()
%    Error     = int()
%
%
% @doc Encodes a <tt>Value</tt> into binary format using a standard type
% specifier (<tt>StdType</tt>).
%
% <p>An standard parameter is directly encoded using its domain specifier.</p>
%
% <ol>
%   <li>If the <tt>Value</tt> is successfully encoded according to the 
%     parameter domain using the base_syntax:encode/2 the term 
%     <tt>{ok, Binary}</tt> is returned.
%   </li>
%   <li>If the value can not be encoded and the parameter has an associated
%     <tt>Error</tt>, the term <tt>{error, Error}</tt> is returned. 
%     If no Error assigned to the parameter, the term 
%     <tt>{error, ?ESME_RUNKNOWNERR}</tt> is returned.
%   </li>
% </ol>
%
% @see base_syntax:encode/2
% @end
%
% %@equiv
%%
encode_std(Value, #standard{domain = Domain, error = Error}) ->
    case base_syntax:encode(Value, Domain) of
        {error, _Reason} ->
            if
                Error == undefined ->
                    {error, ?ESME_RUNKNOWNERR};
                true ->
                    {error, Error}
            end;
        Ok ->
            Ok
    end.

%%%
% @spec encode_tlv(Value, TlvType) -> {ok, Binary} | {error, Error}
%    Value     = term()
%    TlvType  = {tlv,Name,Tag,Domain,Reserved,Mandatory,Multiple,Default,Error}
%    Name     = atom()
%    Tag      = int()
%    Domain   = Type
%    Type     = empty()          |
%               constant()       | 
%               integer()        | 
%               c_octet_string() | 
%               octet_string()   |
%               list()           |
%               composite()      |
%               union()
%    Reserved = Type
%    Mandatory= bool()
%    Multiple = bool()
%    Default  = term()
%    Error    = int()
%    Binary   = bin()
%
% @doc Encodes a TLV into binary format.  The SMPP Protocol Specification 
% (section 3.1 on [SMPP v5.0]) determines that a TLV must be encoded using; 2
% octets for the Tag, 2 octets for Length field and Length octets for the
% Value (in that order).
%
% <ol>
%   <li><tt>Value</tt> is encoded into <tt>Binary</tt> according to 
%     the base type of the parameter using base_syntax:encode/2.  The term
%     <tt>
%       {ok, &lt;&lt;Tag:16/integer, (size(Binary)):16/integer, Binary/binary&gt;&gt;}
%     </tt>
%     is returned.
%   </li>
%   <li><tt>{error, ?ESME_RMISSINGTLV}</tt> is returned if 
%     <tt>Value</tt> is the atom <tt>undefined</tt> on single TLVs.
%   </li>
%   <li><tt>{error, ?ESME_RMISSINGTLV}</tt> is returned if 
%     <tt>Value</tt> is an empty list on multiple TLVs.
%   </li>
%   <li>The term <tt>{error, ?ESME_RINVTLVVAL}</tt> is returned if
%     <tt>Value</tt> doesn't match the TLV domain.  If the parameter has
%     an associated <tt>Error</tt>, the term <tt>{error, Error}</tt> 
%     is returned instead of <tt>{error, ?ESME_RINVTLVVAL}</tt>.
%   </li>
% </ol>
%
% <p>Given an <tt>undefined</tt> value and an optional TLV specifier,
% the term <tt>{ok, &lt;&lt;&gt;&gt;}</tt> is returned.</p>
% @end
%
% %@see
%
% %@equiv
%%
encode_tlv(undefined, #tlv{mandatory = true}) ->
    {error, ?ESME_RMISSINGTLV};

encode_tlv([], #tlv{mandatory = true, multiple = true}) ->
    {error, ?ESME_RMISSINGTLV};

encode_tlv(undefined, _TlvType) ->
    {ok, <<>>};

encode_tlv([], #tlv{multiple = true}) ->
    {ok, <<>>};

encode_tlv(Values, #tlv{multiple = true} = TlvType) when list(Values) ->
    encode_multiple_tlv(Values, TlvType);

encode_tlv(Value, TlvType) ->
    encode_single_tlv(Value, TlvType).


%%%
% @doc Auxiliary function for encode_tlv/2
%
% @see encode_multiple_tlv/3
% @end
%%
encode_multiple_tlv(Values, TlvType) ->
    encode_multiple_tlv(Values, TlvType, []).


%%%
% @doc Auxiliary function for encode_multiple_tlv/2
%
% @see encode_single_tlv/2
% @end
%%
encode_multiple_tlv([], _TlvType, Acc) ->
    {ok, concat_binary(Acc)};

encode_multiple_tlv([Value|Values], TlvType, Acc) ->
    case encode_single_tlv(Value, TlvType) of
        {ok, Binary} ->
            encode_multiple_tlv(Values, TlvType, [Binary|Acc]);
        Error ->
            Error
    end.


%%%
% @doc Auxiliary function for encode_tlv/2
%
% @see base_syntax:encode/2
% @end 
%%
encode_single_tlv(Value, #tlv{tag = Tag, domain = Domain, error = Error}) ->
    case base_syntax:encode(Value, Domain) of
        {ok, Binary} ->
            {ok, <<Tag:16/integer, (size(Binary)):16/integer, Binary/binary>>};
        {error, _Reason} ->
            if
                Error == undefined ->
                    {error, ?ESME_RINVTLVVAL};
                true ->
                    {error, Error}
            end
    end.

