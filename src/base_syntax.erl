%%% Copyright (C) 2003 - 2005 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc SMPP Base Syntax Library.
%%%
%%% <p>Functions for the SMPP base syntax manipulation.</p>
%%%
%%% <p>As a guideline, some comments include references to the specific section
%%% numbers on [SMPP 5.0].</p>
%%%
%%%
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [18 Feb 2004]
%%%
%%% <ul>
%%%   <li>Trailing <tt>NULL_CHARACTER</tt> is transparetly handled by 
%%%     <a href="#encode-2">base_syntax:encode/2</a> and
%%%     <a href="#decode-2">base_syntax:decode/2</a> functions.
%%%   </li>
%%%   <li>
%%%   <a 
%%%   href="http://oserl.sourceforge.net/common_lib/binary.html#take_until-3">
%%%   take_until/3</a> moved to module <a href="binary.html">binary.erl</a> of 
%%%   the <a href="http://oserl.sourceforge.net/common_lib/index.html">
%%%   common_lib</a>.
%%%   </li>
%%%   <li>
%%%   <a 
%%%   href="http://oserl.sourceforge.net/common_lib/my_string.html#is_dec-1">
%%%   is_dec/1</a> and 
%%%   <a 
%%%   href="http://oserl.sourceforge.net/common_lib/my_string.html#is_hex-1">
%%%   is_hex/1</a> moved to module <a href="my_string.html">my_string.erl</a> 
%%%   of the <a href="http://oserl.sourceforge.net/common_lib/index.html">
%%%   common_lib</a>.
%%%   </li>
%%%   <li><tt>format</tt> field in strings is now a Fun. Much more powerful and
%%%     elegant than before.<br/>
%%%     <br/>
%%%      <a href="#decode-2">decode/2</a> and <a href="#encode-2">encode/2</a>
%%%     changed (simplified :-) accordingly.
%%%   </li>
%%% </ul>
%%%
%%%
%%% <h2>References</h2>
%%% <dl>
%%%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol 
%%%     Specification. Version 5.0. SMS Forum.
%%%   </dd>
%%% </dl>
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 0.2, {09 Feb 2003} {@time}.
%%% @end
-module(base_syntax).

%%%-------------------------------------------------------------------
%%% Include files
%%%-------------------------------------------------------------------
-include("smpp_globals.hrl").
-include("base_syntax.hrl").

%%%-------------------------------------------------------------------
%%% External exports
%%%-------------------------------------------------------------------
-export([decode/2, encode/2, fit/2]).

%%%-------------------------------------------------------------------
%%% Internal exports
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------

%%%===================================================================
%%% External functions
%%%===================================================================
%% @spec decode(Binary, Type) -> {ok, Value, Rest} | {error, Reason}
%%    Binary   = bin()
%%    Type     = {constant, Constant}                  | 
%%               {integer, Size, Min, Max}             | 
%%               {c_octet_string, Fixed, Size, Format} | 
%%               {octet_string, Fixed, Size, Format}   |
%%               {list, Type, Size}                    |
%%               {composite, Name, Tuple}              |
%%               {union, Types}
%%    Constant = bin()
%%    Size     = int()
%%    Min      = int()
%%    Max      = int()
%%    Fixed    = bool()
%%    Name     = atom()
%%    Tuple    = term()
%%    Types    = [Type]
%%    Value    = term()
%%    Rest     = bin()
%%    Reason   = {type_mismatch, Type, Details}
%%    Details  = Data | Reason
%%    Data     = bin() | int() | string() 
%%
%% @doc Decodes a Value from the head of a Binary using a Type specifier.  As 
%% the Value is decoded a type checking operation is performed, if successful
%% the term <tt>{ok, Value, Rest}</tt> is returned (where Rest is the
%% remainder of the unused binary), otherwise <tt>{error, {type_mismatch, 
%% Type, Details}}</tt> is returned.
%%
%% <p>where</p>
%%
%% <ul>
%%   <li><tt>Format = fun(Str) -&gt; bool()</tt></li>
%%   <li><tt>Str = string()</tt></li>
%% </ul>
%%
%% <p>Every type is decoded according to [SMPP 5.0].</p>
%%
%% <ol>
%%   <li><a href="#decode-constant">constant</a></li>
%%   <li><a href="#decode-integer">integer</a></li>
%%   <li><a href="#decode-c_octet_string">c_octet_string</a></li>
%%   <li><a href="#decode-octet_string">octet_string</a></li>
%%   <li><a href="#decode-list">list</a></li>
%%   <li><a href="#decode-composite">composite</a></li>
%%   <li><a href="#decode-union">union</a></li>
%% </ol>
%%
%%
%% <h4><a name="decode-constant">constant</a></h4>
%%
%% <p>Binaries, strings and integers are valid constants.</p>
%%
%% <p>If the binary representation of the Constant is at the head of Binary,
%% the Constant is returned as Value.  Otherwise an error is reported.</p>
%%
%%
%% <h4><a name="decode-integer">integer</a></h4>
%%
%% <p>Checks if Value is a valid Size octets integer.  Must be an integer 
%% between 0 and <tt>math:pow(256, Size) - 1</tt>.</p>
%%
%% <p>An integer of Size octets is taken from the head of the Binary, if not
%% enough bytes an error is reported.</p>
%%
%%
%% <h4><a name="decode-c_octet_string">c_octet_string</a></h4>
%%
%% <p>Checks if Value is a valid C-Octet String.  A C-Octet String must be 
%% NULL_CHARACTER ($\0) terminated.</p>
%%
%% <p>When a fixed size is given and the Size-th octet on the Binary is the
%% NULL_CHARACTER, a <tt>Size - 1</tt> long binary is taken an translated into 
%% string using the binary_to_list BIF.</p>
%%
%% <p>On variable length strings, every octet until the NULL_CHARACTER is taken
%% using the function <tt>binary:take_until/3</tt> (common_lib).  The result is
%% translated to a list with the BIF binary_to_list.  Notice that the resulting
%% <tt>Value</tt> is always smaller than <tt>Size</tt> characters long.</p>
%%
%% <p><font color="red">Important (Since version 0.2).</font>  The base syntax 
%% takes care of trailing NULL_CHARACTERs for you so, <i>even you have to count
%% this character when setting the <tt>Size</tt> field</i>, <b>do not</b> add a
%% trailing NULL_CHARACTER in your c_octet_string values, base_syntax:decode/2 
%% handles NULLs on its own.</p>
%%
%% <p>As a rule of thumb.  Set the <tt>Size</tt> of your c_octet_string 
%% accordingly to SMPP specs.  Do not expect decoded c_octet_string values to 
%% have a trailing NULL_CHARACTER.  This character is automatically removed at 
%% decode time.</p>
%%
%%
%% <h4><a name="decode-octet_string">octet_string</a></h4>
%%
%% <p>When a fixed size is given the Value must be exactly 0 or Size 
%% characters long.  For variable lengths any range between 0 and Size 
%% octets is considered to be correct.</p>
%%
%% <p>Fixed size Octet Strings are decoded pretty much like a C-Octet String,
%% but no terminating NULL_CHARACTER is required.</p>
%%
%% <p>It's not easy to guess where a variable Octet String ends, in fact this
%% data-type makes sense only if encapsulated inside a TLV.  In order to let a
%% TLV recursively decode an inner variable Octet String the following rule
%% applies; if size(Binary) less than Size, the hole Binary is translated to a 
%% list (string), otherwise the first Size octets of the Binary are taken.</p>
%%
%%
%% <h4><a name="decode-list">list</a></h4>
%%
%% <p>Every Value in the list must conform the given Type.</p>
%%
%% <p>To decode a list, a (Size / 256) + 1 octets integer indicating the 
%% total number (Num) of values is extracted from the head of the Binary, then
%% a list with Num elements is decoded using the base Type of the list.  
%% This list of Values is returned, Num is discarded.</p>
%%
%%
%% <h4><a name="decode-composite">composite</a></h4>
%%
%% <p>In a composite every field is checked against the corresponding field
%% in the tuple with the types.</p>
%%
%% <p>Every field in the composite is decoded, one at a time, using the 
%% corresponding field in the type descriptor.  On anonymous composites
%% (<tt>Name = undefined</tt>) the returning Value is a tuple built from
%% the individual fields, named composites tuples are translated to a record of
%% type Name.</p>
%%
%%
%% <h4><a name="decode-union">union</a></h4>
%%
%% <p>The Value must conform at least one of the given types.</p>
%%
%% <p>An union value is decoded using the first type descriptor conformed
%% by the value.</p>
%%
%% @see encode/2
%% @see decode_iter/3
%% @see decode_list/2
%% @see decode_try/2
%% @see binary:take_until/3
%% @end
decode(Binary, #constant{value = Value} = Type) ->
    Cons = if
               list(Value) -> 
                   list_to_binary(Value);
               integer(Value) -> 
                   <<Value/integer>>;
               binary(Value) ->
                   <<Value/binary>>;
               true ->                    % Should *NOT* happen
                   term_to_binary(Value)
           end,
    Size = size(Cons),
    case Binary of
        <<Cons:Size/binary-unit:8, Rest/binary>> ->
            {ok, Value, Rest};
        <<Other:Size/binary-unit:8, _Rest/binary>> ->
            {error, {type_mismatch, Type, Other}};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #integer{size = Size} = Type) ->
    case Binary of
        <<Value:Size/integer-unit:8, Rest/binary>> ->
            {ok, Value, Rest};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #c_octet_string{format = F} = Type) when F /= undefined ->
    case decode(Binary, Type#c_octet_string{format = undefined}) of
        {ok, Value, Rest} ->
            case F(Value) of
                true ->
                    {ok, Value, Rest};
                false ->
                    {error, {type_mismatch, Type, Value}}
            end;
        Error ->
            Error
    end;
decode(Binary, #c_octet_string{fixed = true, size = Size} = Type) ->
    Len = Size - 1,
    case Binary of
        <<?NULL_CHARACTER:8, Rest/binary>> ->
            {ok, "", Rest};
        <<Value:Len/binary-unit:8, ?NULL_CHARACTER:8, Rest/binary>> ->
            {ok, binary_to_list(Value), Rest};
        <<NotNullTerminated:Size/binary-unit:8, _Rest/binary>> ->
            {error, {type_mismatch, Type, binary_to_list(NotNullTerminated)}};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #c_octet_string{fixed = false, size = Size} = Type) ->
    case binary:take_until(Binary, <<?NULL_CHARACTER:8>>, Size) of
        {ok, UntilNull, <<?NULL_CHARACTER:8, Rest/binary>>} ->
            {ok, binary_to_list(UntilNull), Rest};
        {error, {not_found, _Null, UntilSize}} ->
            {error, {type_mismatch, Type, binary_to_list(UntilSize)}}
    end;
decode(Binary, #octet_string{format = F} = Type) when F /= undefined ->
    case decode(Binary, Type#octet_string{format = undefined}) of
        {ok, Value, Rest} ->
            case F(Value) of
                true ->
                    {ok, Value, Rest};
                false ->
                    {error, {type_mismatch, Type, Value}}
            end;
        Error ->
            Error
    end;
decode(Binary, #octet_string{fixed = true, size = Size} = Type) ->
    case Binary of
        <<Value:Size/binary-unit:8, Rest/binary>> ->
            {ok, binary_to_list(Value), Rest};
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #octet_string{fixed = false, size = Size}) ->
    case Binary of
        <<Value:Size/binary-unit:8, Rest/binary>> ->
            {ok, binary_to_list(Value), Rest};
        <<Value/binary>> ->
            {ok, binary_to_list(Value), <<>>}
    end;
decode(Binary, #list{type = InnerType, size = Size} = Type) ->
    Len = trunc(Size / 256) + 1,
    case Binary of
        <<Times:Len/integer-unit:8, Bin/binary>> ->
            case decode_iter(Bin, InnerType, Times) of
                {error, Reason} ->
                    {error, {type_mismatch, Type, Reason}};
                Ok ->
                    Ok
            end;
        _Mismatch ->
            {error, {type_mismatch, Type, Binary}}
    end;
decode(Binary, #composite{name = undefined, tuple = Tuple} = Type) ->
    case decode_list(Binary, tuple_to_list(Tuple)) of
        {ok, Values, Rest} ->
            {ok, list_to_tuple(Values), Rest};
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}}
    end;
decode(Binary, #composite{name = Name, tuple = Tuple} = Type) ->
    case decode_list(Binary, tuple_to_list(Tuple)) of
        {ok, Values, Rest} ->
            {ok, list_to_tuple([Name|Values]), Rest};
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}}
    end;
decode(Binary, #union{types = Types} = Type) when length(Types) > 0 ->
    case decode_try(Binary, Types) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok            
    end;
decode(Binary, Type) ->
    {error, {type_mismatch, Type, Binary}}.


%% @doc Auxiliary function for decode/2
%%
%% <p>This function decodes Times occurrences of a given Type or until an
%% error is reported.</p>
%%
%% <p>If Times is the atom <tt>until_error</tt>, the function tries to 
%% decode a Value of Type until an error is reported, at this point the list of
%% Values previously decoded are returned (Notice that in this case the error
%% is *NOT* reported).</p>
%%
%% @see decode_iter/4
%% @end
decode_iter(Binary, Type, Times) ->
    decode_iter(Binary, Type, Times, []).

%% @doc Auxiliary function for decode_iter/3
%%
%% @see decode/2
%% @end
decode_iter(Binary, Type, until_error, Acc) ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            decode_iter(Rest, Type, until_error, [Value|Acc]);
        _Error ->
            {ok, lists:reverse(Acc), Binary}
    end;
decode_iter(Binary, Type, Times, Acc) when length(Acc) < Times ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            decode_iter(Rest, Type, Times, [Value|Acc]);
        Error ->
            Error
    end;
decode_iter(Binary, _Type, _Times, Acc) ->
    {ok, lists:reverse(Acc), Binary}.


%% @doc Auxiliary function for decode/2.
%%
%% <p>This functions decodes a list of Values using the corresponding type
%% on the list of types, the list of decoded Values is returned.  Used to 
%% transverse complex data structures (lists and composites).</p>
%%
%% @see decode_list/3
%% @end
decode_list(Binary, Types) ->
    decode_list(Binary, Types, []).

%% @doc Auxiliary function for decode_list/2
%%
%% @see decode/2
%% @end
decode_list(Binary, [], Values) ->
    {ok, lists:reverse(Values), Binary};
decode_list(Binary, [Type|Types], Values) ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            decode_list(Rest, Types, [Value|Values]);
        Error ->
            Error
    end.


%% @doc Auxiliary function for decode/2
%%
%% <p>Tries to decode a Value using all <tt>Types</tt> on a list, first 
%% successful match is returned.  If none of the <tt>Types</tt> apply, this 
%% function tries to guess the closest error description (error with highest 
%% priority).</p>
%%
%% @see decode_try/4
%% @end
decode_try(Binary, Types) ->
    decode_try(Binary, Types, {}, 0).

%% @doc Auxiliary function for decode_try/2
%%
%% @see decode/2
%% @see error_priority/1
%% @end
decode_try(_Binary, [], Error, _Priority) ->
    Error;
decode_try(Binary, [Type|Types], Error, Priority) ->
    case decode(Binary, Type) of
        {ok, Value, Rest} ->
            {ok, Value, Rest};
        NewError ->
            NewPriority = error_priority(NewError),
            if
                NewPriority >= Priority ->
                    decode_try(Binary, Types, NewError, NewPriority);
                true ->
                    decode_try(Binary, Types, Error, Priority)
            end
    end.


%% @spec encode(Value, Type) -> {ok, Binary} | {error, Reason}
%%    Value    = term()
%%    Type     = {constant, Constant}                  | 
%%               {integer, Size, Min, Max}             | 
%%               {c_octet_string, Fixed, Size, Format} | 
%%               {octet_string, Fixed, Size, Format}   |
%%               {list, Type, Size}                    |
%%               {composite, Name, Tuple}              |
%%               {union, Types}
%%    Constant = bin()
%%    Size     = int()
%%    Min      = int()
%%    Max      = int()
%%    Fixed    = bool()
%%    Name     = atom()
%%    Tuple    = term()
%%    Types    = [Type]
%%    Binary   = bin()
%%    Reason   = {type_mismatch, Type, Details}
%%    Details  = Data | Reason 
%%    Data     = bin() | int() | string()
%%
%% @doc Encodes a Value using a Type specifier.  Before encoding the value
%% a type checking operation is done, if unsuccessful the term 
%% <tt>{error, {type_mismatch, Type, Details}}</tt> is returned.
%%
%% <p>where</p>
%%
%% <ul>
%%   <li><tt>Format = fun(Str) -&gt; bool()</tt></li>
%%   <li><tt>Str = string()</tt></li>
%% </ul>
%%
%% <p>Every type is encoded according to [SMPP 5.0].</p>
%%
%% <ol>
%%   <li><a href="#encode-constant">constant</a></li>
%%   <li><a href="#encode-integer">integer</a></li>
%%   <li><a href="#encode-c_octet_string">c_octet_string</a></li>
%%   <li><a href="#encode-octet_string">octet_string</a></li>
%%   <li><a href="#encode-list">list</a></li>
%%   <li><a href="#encode-composite">composite</a></li>
%%   <li><a href="#encode-union">union</a></li>
%% </ol>
%%
%%
%% <h4><a name="encode-constant">constant</a></h4>
%%
%% <p>Value must be identical to given constant.  Binaries, strings and 
%% integers are valid constants.</p>
%%
%% <p>The Value is directly encoded into a binary.</p>
%%
%%
%% <h4><a name="encode-integer">integer</a></h4>
%%
%% <p>Checks if Value is a valid Size octets integer.  Must be an integer 
%% between 0 and <tt>math:pow(256, Size) - 1</tt>.</p>
%%
%% <p>The integer is translated into binary using Size octets.</p>
%%
%%
%% <h4><a name="encode-c_octet_string">c_octet_string</a></h4>
%%
%% <p>Checks if Value is a valid C-Octet String: Just a string with Size - 1 
%% characters.  Notice that C-Octet Strings (erlang) Values should not include 
%% the terminating NULL_CHARACTER ($\0), it is automatically included by the 
%% encode function.</p>
%%
%% <p>When a fixed size is given, the <tt>Value</tt> (having no terminating 
%% NULL_CHARACTER) must be exactly 0 or Size - 1 characters long.  For variable
%% lengths any range between 0 and Size - 1 octets is considered to be correct.
%% </p>
%%
%% <p>The string is translated into binary using the BIF <tt>list_to_binary
%% </tt>, thus one octet per character.</p>
%%
%% <p><font color="red">Important (Since version 0.2).</font>  The base syntax 
%% takes care of trailing NULL_CHARACTERs for you so, <i>even you have to count
%% this character when setting the <tt>Size</tt> field</i>, <b>do not</b> add a
%% trailing NULL_CHARACTER in your c_octet_string values, base_syntax:encode/2 
%% handles NULLs on its own.</p>
%%
%% <p>As a rule of thumb.  Set the <tt>Size</tt> of your c_octet_string 
%% accordingly to SMPP specs.  Do not add the trailing NULL_CHARACTER to the
%% values of the c_octet_string parameters.  The NULL_CHARACTER is 
%% automatically included at encode time.</p>
%%
%%
%% <h4><a name="encode-octet_string">octet_string</a></h4>
%%
%% <p>When a fixed size is given the Value must be exactly 0 or Size 
%% characters long.  For variable lengths any range between 0 and Size 
%% octets is considered to be correct.</p>
%%
%% <p>An string is translated into binary using the BIF <tt>list_to_binary
%% </tt>, thus one octet per character.</p>
%%
%%
%% <h4><a name="encode-list">list</a></h4>
%%
%% <p>Every Value in the list must conform the given Type.</p>
%%
%% <p>To encode a list, a (Size / 256) + 1 octets integer indicating the
%% total number of values is prepended to the list of encoded values.  Every
%% element on the list is encoded (and appended) according to the given type.
%% </p>
%%
%%
%% <h4><a name="encode-composite">composite</a></h4>
%%
%% <p>In a composite every field is checked against the corresponding field
%% in the tuple with the types.</p>
%%
%% <p>Every field in the composite is encoded using the corresponding field
%% in the type descriptor.  The global result is obtained concatenating every
%% encoded field in the same order of appearance in the composite.  On named
%% composites the identifier of the record is discarded (not encoded).</p>
%%
%%
%% <h4><a name="encode-union">union</a></h4>
%%
%% <p>The Value must conform at least one of the given types.</p>
%%
%% <p>An union value is encoded using the first type descriptor conformed
%% by the value.</p>
%%
%% @see decode/2
%% @see encode_iter/2
%% @see encode_list/2
%% @see encode_try/2
%% @end
encode(Value, #constant{value = Value}) when list(Value) -> 
    {ok, list_to_binary(Value)};
encode(Value, #constant{value = Value}) when integer(Value) -> 
    {ok, <<Value/integer>>};
encode(Value, #constant{value = Value}) when binary(Value) -> 
    {ok, <<Value/binary>>};
encode(Value, #integer{size = Size, min = Min, max = Max}) 
  when integer(Value), Value >= Min, Value =< Max  ->
    {ok, <<Value:Size/integer-unit:8>>};
encode(Value, #c_octet_string{format = F} = Type) when F /= undefined ->
    case F(Value) of
        true ->
            encode(Value, Type#c_octet_string{format = undefined});
        false ->
            {error, {type_mismatch, Type, Value}}
    end;
encode(Value, #c_octet_string{fixed = true, size = Size})
  when list(Value), (length(Value) == Size - 1) or (length(Value) == 0) ->
    {ok, list_to_binary(Value ++ [?NULL_CHARACTER])};
encode(Value, #c_octet_string{size = Size})
  when list(Value), length(Value) < Size -> % was =<
    {ok, list_to_binary(Value ++ [?NULL_CHARACTER])};
encode(Value, #octet_string{format = F} = Type) when F /= undefined ->
    case F(Value) of
        true ->
            encode(Value, Type#octet_string{format = undefined});
        false ->
            {error, {type_mismatch, Type, Value}}
    end;
encode(Value, #octet_string{fixed = true, size = Size}) 
  when list(Value), (length(Value) == Size) or (length(Value) == 0) ->
    {ok, list_to_binary(Value)};
encode(Value, #octet_string{size = Size})
  when list(Value), length(Value) =< Size ->
    {ok, list_to_binary(Value)};
encode(Values, #list{type = InnerType, size = Size} = Type) 
  when list(Values), length(Values) =< Size ->
    case encode_iter(Values, InnerType) of
        {ok, Binary} ->
            LenSize = trunc(Size / 256) + 1,
            {ok, <<(length(Values)):LenSize/integer-unit:8, Binary/binary>>};
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}}
    end;
encode(Value, #composite{name = undefined, tuple = Tuple} = Type) 
  when tuple(Value), size(Value) == size(Tuple) ->
    case encode_list(tuple_to_list(Value), tuple_to_list(Tuple)) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok
    end;
encode(Value, #composite{name = Name, tuple = Tuple} = Type) 
  when element(1, Value) == Name, size(Value) - 1 == size(Tuple) ->
    case encode_list(tl(tuple_to_list(Value)), tuple_to_list(Tuple)) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok
    end;
encode(Value, #union{types = Types} = Type) when length(Types) > 0 ->
    case encode_try(Value, Types) of
        {error, Reason} ->
            {error, {type_mismatch, Type, Reason}};
        Ok ->
            Ok            
    end;
encode(Value, Type) ->
    {error, {type_mismatch, Type, Value}}.


%% @doc Auxiliary function for encode/2
%%
%% <p>This function encodes a list of Values using the same Type or until an
%% error is reported.</p>
%%
%% @see encode_iter/3
%% @end
encode_iter(Values, Type) ->
    encode_iter(Values, Type, []).

%% @doc Auxiliary function for encode_iter/2
%%
%% @see encode/2
%% @end
encode_iter([], _Type, Acc) ->
    {ok, concat_binary(lists:reverse(Acc))};
encode_iter([Value|Values], Type, Acc) ->
    case encode(Value, Type) of
        {ok, Binary} ->
            encode_iter(Values, Type, [Binary|Acc]);
        Error ->
            Error
    end.


%% @doc Auxiliary function for encode/2.
%%
%% <p>This functions encodes a list of values using the corresponding type
%% on the list of types and returns the concatenation of every encoded binary.
%% Used to transverse complex data structures (lists and composites).</p>
%%
%% @see encode_list/3
%% @end
encode_list(Values, Types) ->
    encode_list(Values, Types, []).

%% @doc Auxiliary function for encode_list/2
%%
%% @see encode/2
%% @end
encode_list([], _Types, Acc) ->
    {ok, concat_binary(lists:reverse(Acc))};
encode_list([Value|Values], [Type|Types], Acc) ->
    case encode(Value, Type) of
        {ok, Binary} ->
            encode_list(Values, Types, [Binary|Acc]);
        Error ->
            Error
    end.


%% @doc Auxiliary function for encode/2
%%
%% <p>Tries to encode a Value against a list of Types, first successful match
%% is returned.  If none of the Types apply, this function tries to guess
%% the closest error description (error with greatest priority).</p>
%%
%% @see encode_try/4
%% @end
encode_try(Value, Types) ->
    encode_try(Value, Types, {}, 0).

%% @doc Auxiliary function for encode_try/2
%%
%% @see encode/2
%% @see error_priority/1
%% @end
encode_try(_Value, [], Error, _Priority) ->
    Error;
encode_try(Value, [Type|Types], Error, Priority) ->
    case encode(Value, Type) of
        {ok, Binary} ->
            {ok, Binary};
        NewError ->
            NewPriority = error_priority(NewError),
            if
                NewPriority >= Priority ->
                    encode_try(Value, Types, NewError, NewPriority);
                true ->
                    encode_try(Value, Types, Error, Priority)
            end
    end.


%% @spec fit(Type, Size) -> NewType
%%    Type     = {constant, Constant}                  | 
%%               {integer, Size, Min, Max}             | 
%%               {c_octet_string, Fixed, Size, Format} | 
%%               {octet_string, Fixed, Size, Format}   |
%%               {list, Type, Size}                    |
%%               {composite, Name, Tuple}              |
%%               {union, Types}
%%    Constant = bin()
%%    Size     = int()
%%    Min      = int()
%%    Max      = int()
%%    Fixed    = bool()
%%    Name     = atom()
%%    Tuple    = term()
%%    Types    = [Type]
%%    NewType  = Type
%%
%% @doc Fits a type specifier to a given Size.
%%
%% <p>where</p>
%%
%% <ul>
%%   <li><tt>Format = fun(Str) -&gt; bool()</tt></li>
%%   <li><tt>Str = string()</tt></li>
%% </ul>
%%
%% <ul>
%%   <li>constants, composites and union specifiers are left unchanged.
%%   </li>
%%   <li>On strings (c_octet_string and octet_string), besides the size field,
%%     the length is set to fixed (<tt>fixed = true</tt>).
%%   </li>
%%   <li>The min and max fields are not changed on integers, the size is 
%%     changed with no further checking.
%%   </li>
%%   <li>If the new Size is greater than the one permitted by the Type, the
%%     Type is returned unchanged.
%%   </li>
%% </ul>
%%
%% <p>This function was mainly conceived to adapt the type specification to a
%% TLV parameter definition.</p>
%% @end
fit(#integer{size = Size} = Type, NewSize) when NewSize < Size ->
    Type#integer{size = NewSize};
fit(#c_octet_string{size = Size} = Type, NewSize) when NewSize =< Size ->
    Type#c_octet_string{size = NewSize, fixed = true};
fit(#octet_string{size = Size} = Type, NewSize) when NewSize =< Size ->
    Type#octet_string{size = NewSize, fixed = true};
fit(#list{size = Size} = Type, NewSize) when NewSize < Size ->
    Type#list{size = Size};
fit(Type, _Size) ->
    Type.


%%%===================================================================
%%% Internal functions
%%%===================================================================
%% @spec error_priority(Error) -> int()
%%    Error     = {error, Reason}
%%    Reason    = {Id, Type, Details}
%%    Id        = type_mismatch
%%    Type      = term()
%%    Details   = Value | Reason
%%    Value     = bin() | int() | string()
%%
%% @doc Gets the priority of an Error report, the biggest the highest.
%%
%% <p>FIXME:  This patch is a *SIMPLE* way of adding some sort of priorities 
%% on errors.  It works fine for PDU encoding but a better approach should be
%% provided in the future.</p>
%%
%% <p>Just about every PDU failure has an error_depth of 2, produced by the 
%% command_id constant mismatch.  Whenever the command_id constant test 
%% succeeds, if an error ocurrs on a later field of the PDU, even this error 
%% would have the same depth (2), it should be consider a better match...
%% <i>well this is just what this patch does</i>.</p>
%%
%% @see error_priority/2
%% @end
error_priority({error, {type_mismatch, _Type, Reason}}) ->
    error_priority(Reason, 1);
error_priority(_Error) ->
    0.

%% @doc Auxiliary function for error_priority/1
%% @end
error_priority({type_mismatch, _Type, {type_mismatch, Type, Reason}}, Depth) ->
    error_priority({type_mismatch, Type, Reason}, Depth + 1);
error_priority({type_mismatch, T, _R}, Depth) when record(T, integer); 
                                                   record(T, c_octet_string);
                                                   record(T, octet_string) ->
    (Depth * 3) + 1;
error_priority({type_mismatch, T, _R}, Depth) when record(T, union);
                                                   record(T, list); 
                                                   record(T, composite) ->
    (Depth * 3) + 2;
error_priority(_Other, Depth) -> % contants and unknown errors
    (Depth * 3) + 0.
