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
% @doc SMPP Base Syntax definition.
%
% <p>Base syntax used for the SMPP implementation.  Even the content of this
% header file can be considered implementation-specific, the definitions 
% herein included are inspired on the conventions used on [SMPP 5.0].</p>
%
% <p>As a guideline some comments include references to the specific section
% numbers on [SMPP 5.0].</p>
%
%
% <h2>Implementation notes</h2>
%
% <p>The best way to understand the meaning of the syntax defined on this 
% header file and how everything works, is by a complete example.</p>
%
% <p>On page 72, section 4.2.3.1 of the SMPP specification [SMPP 5.0], the
% following fields are defined as part of the <i>submit_multi</i> operation 
% PDU:</p>
%
% <table width="100%" border="1" cellpadding="2">
% <caption>Fragment of Table 4-18 submit_multi PDU.</caption>
%   <tr> 
%     <th width="60%" valign="middle" align="center">Field Name</th>
%     <th width="20%" valign="middle" align="center">Size Octets</th>
%     <th width="20%" valign="middle" align="center">Type</th>
%   </tr>
%   <tr> 
%     <td width="60%" valign="middle" align="center">number_of_dests</td>
%     <td width="20%" valign="middle" align="center">1</td>
%     <td width="20%" valign="middle" align="center">Integer</td>
%   </tr>
%   <tr> 
%     <td width="60%" valign="middle" align="center">dest_address</td>
%     <td width="20%" valign="middle" align="center">Var. max 24</td>
%     <td width="20%" valign="middle" align="center">Composite</td>
%   </tr>
%   <tr>
%     <td width="60%" valign="middle" align="center">
%       <table width="100%" border="0" cellpadding="2">
%         <tr>
%           <td width="100%" align="center">dest_flag</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">dest_addr_ton</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">dest_addr_npi</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">destination_addr</td>
%         </tr>
%       </table>
%     </td>
%     <td width="20%" valign="middle" align="center">
%       <table width="100%" border="0" cellpadding="2">
%         <tr>
%           <td width="100%" align="center">1</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">1</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">1</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">Var. max 21</td>
%         </tr>
%       </table>
%     </td>
%     <td width="20%" valign="middle" align="center">
%       <table width="100%" border="0" cellpadding="2">
%         <tr>
%           <td width="100%" align="center">Integer</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">Integer</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">Integer</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">C-Octet String</td>
%         </tr>
%       </table>
%     </td>
%   </tr>
%   <tr> 
%     <td width="60%" valign="middle" align="center">dest_address</td>
%     <td width="20%" valign="middle" align="center">Var. max 23</td>
%     <td width="20%" valign="middle" align="center">Composite</td>
%   </tr>
%   <tr>
%     <td width="60%" valign="middle" align="center">
%       <table width="100%" border="0" cellpadding="2">
%         <tr>
%           <td width="100%" align="center">dest_flag</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">dl_name</td>
%         </tr>
%       </table>
%     </td>
%     <td width="20%" valign="middle" align="center">
%       <table width="100%" border="0" cellpadding="2">
%         <tr>
%           <td width="100%" align="center">1</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">Var. max 21</td>
%         </tr>
%       </table>
%     </td>
%     <td width="20%" valign="middle" align="center">
%       <table width="100%" border="0" cellpadding="2">
%         <tr>
%           <td width="100%" align="center">Integer</td>
%         </tr>
%         <tr>
%           <td width="100%" align="center">C-Octet String</td>
%         </tr>
%       </table>
%     </td>
%   </tr>
% </table>
% 
% <p><tt>dest_address</tt> is a composite field containing a mandatory 
% <tt>dest_flag</tt> field (<tt>dest_flag</tt> is a constant) and either 
% an SME address (when <tt>dest_flag</tt> is 0x01) or a distribution list name 
% (<tt>dest_flag</tt> is 0x02); thus <tt>dest_address</tt> is an union.  
% Additionally the field can be encoded multiple times according to the value 
% specified in the <tt>number_of_dests</tt> field; <tt>dest_address</tt> is 
% also a list.</p>
%
% <p>This declaration has the following internal representation:</p>
%
% <pre>
% -define(DEST_FLAG_SME_DATATYPE, ?CONSTANT(16#01)).
% -define(DEST_FLAG_DL_DATATAYPE, ?CONSTANT(16#02)).
% -define(DEST_ADDR_TON_DATATAYPE, ?BOUND_INTEGER(1, 2#00000110)).
% -define(DEST_ADDR_NPI_DATATAYPE, ?BOUND_INTEGER(1, 2#00010010)).
% -define(DESTINATION_ADDR_DATATAYPE, ?VAR_C_OCTET_STRING(21)).
% -define(DL_NAME_DATATAYPE, ?VAR_C_OCTET_STRING(21)).
%
% -define(DEST_ADDRESS_SME_DATATAYPE,
%         ?COMPOSITE(dest_address_sme,
%                    {?DEST_FLAG_SME_DATATAYPE,
%                     ?DEST_ADDR_TON_DATATAYPE,
%                     ?DEST_ADDR_NPI_DATATAYPE,
%                     ?DESTINATION_ADDR_DATATAYPE})).
% -define(DEST_ADDRESS_DL_DATATAYPE,
%         ?COMPOSITE(dest_address_dl,
%                    {?DEST_FLAG_DL_DATATAYPE,
%                     ?DL_NAME_DATATAYPE})).
%
% -define(DEST_ADDRESS_DATATAYPE, 
%         ?UNION([?DEST_ADDRESS_SME_DATATAYPE, 
%                 ?DEST_ADDRESS_DL_DATATAYPE])).
%
% -define(DEST_ADDRESS_MULTI_DATATAYPE, ?LIST(?DEST_ADDRESS_DATATAYPE)).
% </pre>
%
% <p>Notice that <tt>DEST_ADDRESS_MULTI_DATATAYPE</tt> embraces the 
% definition of both fields; number_of_dests and dest_address, since a 
% list is encoded in a Length ++ List fashion, number_of_dests doesn't 
% need to be explicitly declared.</p>
%
% <p>Additionally two records must be defined in order to successfully decode 
% named composites:</p>
%
% <pre>
% -record(dest_address_sme, 
%         {dest_flag, dest_addr_ton, dest_addr_npi, destination_addr}).
% -record(dest_address_dl, 
%         {dest_flag, dl_name}).
% </pre>
%
% <p>Record identifiers must match the names provided in the composite 
% declaration for the decoding function to work propertly.  These names are 
% ignored by the encode function though.</p>
%
% <p>Please read comments preceding record and function definitions for
% further details.</p>
%
%
% <h2>References</h2>
% <dl>
%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification.
%     Version 5.0. SMS Forum.
%   </dd>
% </dl>
%
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {14 Mar 2003} {@time}.
% @end
%%

-ifndef(base_syntax).
-define(base_syntax, true).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
%%%
% %@doc SMPP Parameter Field Size Notation.
%
% <p>Macros based on record data-types definitions.  Read comments preceding
% each record declaration for a complete description of the fields.</p>
%
% <p>See below for a simplified variant of these macros.</p>
% %@end
%%
-define(EMPTY,
        #empty{}).
-define(CONSTANT(Value), 
        #constant{value = Value}).
-define(INTEGER(Size), 
        #integer{size = Size, min = 0, max = math:pow(256, Size) - 1}).
-define(C_OCTET_STRING(Fixed, Size),
        #c_octet_string{fixed = Fixed, size = Size, format = free}).
-define(OCTET_STRING(Fixed, Size),
        #octet_string{fixed = Fixed, size = Size, format = free}).
-define(LIST(Type), 
        #list{type = Type, size = 255}).
-define(COMPOSITE(Name, Tuple), 
        #composite{name = Name, tuple = Tuple}).
-define(UNION(Types), 
        #union{types = Types}).

%%%
% %@doc Simplified Field Size Notation Macros for readability.
%
% <p>Some options are implicitly assigned.  Most of the SMPP PDU fields 
% definitions are more readable using these macros.</p>
% %@end
%%
-define(BOUND_INTEGER(Size, Max), 
        #integer{size = Size, min = 0, max = Max}).
-define(RANGE_INTEGER(Size, Min, Max), 
        #integer{size = Size, min = Min, max = Max}).

-define(HEX_C_OCTET_STRING(Fixed, Size),
        #c_octet_string{fixed = Fixed, size = Size, format = hex}).
-define(HEX_OCTET_STRING(Fixed, Size),
        #octet_string{fixed = Fixed, size = Size, format = hex}).

-define(DEC_C_OCTET_STRING(Fixed, Size),
        #c_octet_string{fixed = Fixed, size = Size, format = dec}).
-define(DEC_OCTET_STRING(Fixed, Size),
        #octet_string{fixed = Fixed, size = Size, format = dec}).

-define(ANONYMOUS_COMPOSITE(Tuple), #composite{tuple = Tuple}).

-define(VAR_C_OCTET_STRING(Size),   ?C_OCTET_STRING(false, Size)).
-define(VAR_OCTET_STRING(Size),     ?OCTET_STRING(false, Size)).
-define(FIXED_C_OCTET_STRING(Size), ?C_OCTET_STRING(true, Size)).
-define(FIXED_OCTET_STRING(Size),   ?OCTET_STRING(true, Size)).

-define(VAR_HEX_C_OCTET_STRING(Size),   ?HEX_C_OCTET_STRING(false, Size)).
-define(VAR_HEX_OCTET_STRING(Size),     ?HEX_OCTET_STRING(false, Size)).
-define(FIXED_HEX_C_OCTET_STRING(Size), ?HEX_C_OCTET_STRING(true, Size)).
-define(FIXED_HEX_OCTET_STRING(Size),   ?HEX_OCTET_STRING(true, Size)).

-define(VAR_DEC_C_OCTET_STRING(Size),   ?DEC_C_OCTET_STRING(false, Size)).
-define(VAR_DEC_OCTET_STRING(Size),     ?DEC_OCTET_STRING(false, Size)).
-define(FIXED_DEC_C_OCTET_STRING(Size), ?DEC_C_OCTET_STRING(true, Size)).
-define(FIXED_DEC_OCTET_STRING(Size),   ?DEC_OCTET_STRING(true, Size)).

-define(SIZED_LIST(Type, Size), 
        #list{type = Type, size = Size}).

%%%
% Set of constants
%%
-define(SET(List), ?UNION(lists:map(fun(C) -> ?CONSTANT(C) end, List))).


%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%
% %@spec {empty} 
%
% %@doc Empty datatype.  A type with no elements.
%%
-record(empty, {}).

%%%
% %@spec {constant, Value} 
%    Value = bin()
%
% %@doc Constant declaration.  Constants are defined in binary format and
% their value is packed as is.
%
% <dl>
%   <dt>Value: </dt><dd>Binary constant value.</dd>
% </dl>
% %@end
%%
-record(constant, {value}).

%%%
% %@spec {integer, Size, Min, Max} 
%    Size = int()
%    Min  = int()
%    Max  = int()
%
% %@doc Integer data-type declaration.
%
% <dl>
%   <dt>Size: </dt><dd>Size in octets.</dd>
%   <dt>Min: </dt><dd>Lower limit (included).</dd>
%   <dt>Max: </dt><dd>Upper limit (included).</dd>
% </dl>
%
% %@see section 3.1 on [SMPP 5.0]
% %@end
%%
-record(integer, {size, min, max}).

%%%
% %@spec {c_octet_string, Fixed, Size, Format} 
%    Fixed  = bool()
%    Size   = int()
%    Format = free | hex | dec
%
% %@doc C-Octet String data-type declaration.  A C-Octet String must always 
% be NULL terminated, thus the minimun length allowed is 1 octet.
%
% <dl>
%   <dt>Fixed: </dt><dd>String size may be of variable or fixed length.  When
%     fixed the length must be exactly Size octets long, if not fixed it may
%     be from 1 up to Size octets long.
%   </dd>
%   <dt>Size: </dt><dd>Size in octets.</dd>
%   <dt>Format: </dt><dd>
%     <ul>
%       <li>free: a free fomat string.</li>
%       <li>hex: the string represents an hexadecimal digit sequence.</li>
%       <li>dec: the string represents a decimal digit sequence.</li>
%     </ul>
%   </dd>
% </dl>
%
% %@see section 3.1 on [SMPP 5.0]
% %@end
%%
-record(c_octet_string, {fixed, size, format}).

%%%
% %@spec {octet_string, Fixed, Size, Format} 
%    Fixed  = bool()
%    Size   = int()
%    Format = free | hex | dec
%
% %@doc Octet String data-type declaration.  An Octet String is not necessary
% to be NULL terminated, thus the minimun length allowed is 0 octets.
%
% <dl>
%   <dt>Fixed: </dt><dd>String size may be of variable or fixed length.  When
%     fixed the length must be exactly Size octets long, if not fixed it may
%     be from 0 up to Size octets long.
%   </dd>
%   <dt>Size: </dt><dd>Size in octets.</dd>
%   <dt>Format: </dt><dd>
%     <ul>
%       <li>free: a free fomat string.</li>
%       <li>hex: the string represents an hexadecimal digit sequence.</li>
%       <li>dec: the string represents a decimal digit sequence.</li>
%     </ul>
%   </dd>
% </dl>
%
% %@see section 3.1 on [SMPP 5.0]
% %@end
%%
-record(octet_string, {fixed, size, format}).

%%%
% %@spec {list, Type, Size} 
%    Type     = {constant, Value}                     | 
%               {integer, Size, Min, Max}             | 
%               {c_octet_string, Fixed, Size, Format} | 
%               {octet_string, Fixed, Size, Format}   |
%               {list, Type, Size}                    |
%               {composite, Name, Tuple}              |
%               {union, Types}
%    Value    = bin()
%    Size     = int()
%    Min      = int()
%    Max      = int()
%    Format   = free | hex | dec
%    Fixed    = bool()
%    Name     = atom()
%    Tuple    = term()
%    Types    = [Type]
%
% %@doc List data-type declaration.  Represents a list with elements of
% the same Type.
%
% <p>Notice that nested data-type definitions are allowed.  Type could be:</p>
%
% <ul>
%   <li>a constant, even it doesn't make much sense.</li>
%   <li>basic data-type; integer, c_octet_string or octet_string.  A list
%     of basic values.
%   </li>
%   <li>complex data-type such as tlvs lists, composites or union
%      declarations.
%   </li>
% </ul>
%
% <dl>
%   <dt>Type: </dt><dd>Defines the type of the elements on the list.  Any
%     kind of data-type declaration is valid.
%   </dd>
%   <dt>Size: </dt><dd>Maximum number of elements.</dd>
% </dl>
% %@end
%%
-record(list, {type, size}).

%%%
% %@spec {composite, Name, Tuple} 
%    Name  = atom()
%    Tuple = term()
%
% %@doc Composite data-type declaration.  If <tt>Name</tt> is undefined the
% composite is said to be anonymous and thus represented by a tuple.  Named 
% composites are represented by records of type <tt>Name</tt>.  It is 
% responsibility of the programmer to provide the appropriate record 
% definitions.  
%
% <p><tt>Name</tt> is ignored when encoding the value into a binary stream.  It
% is only considered to translate the binary representation of a composite
% into erlang records.  It also helps to document the declaration of nested
% data structures.  It'll be a good practice to give the composite (and the
% associated record) the same <tt>Name</tt> that the SMPP Protocol
% Specification uses [SMPP 5.0].</p>  
%
% <p>Whether named or not, <tt>Tuple</tt> is a tuple (never a record) defining 
% the type structure of the composite.  For example, in a composite with two 
% fields; an integer and an octet_string, <tt>Tuple</tt> would be something 
% like:</p>
%
% <p><tt>{?INTEGER_4, VAR_OCTET_STRING(21)}</tt></p>
%
% <p>Nested data-type definitions are allowed.  An element on the composite may
% be of the type:</p>
%
% <ul>
%   <li>constants.</li>
%   <li>basic data-types; integer, c_octet_string or octet_string.</li>
%   <li>complex data-types such as lists, composites or union declarations.</li>
% </ul>
%
% <dl>
%   <dt>Name: </dt><dd>Identifier of the composite.  Must be left 
%     <tt>undefined</tt> if the composite is anonymous.
%   </dd>
%   <dt>Tuple: </dt><dd>Defines the structure of the composite.  Any kind of 
%     data-type declaration is a valid term of this tuple.
%   </dd>
% </dl>
% %@end
%%
-record(composite, {name, tuple}).

%%%
% %@spec {union, Types} 
%    Types    = [Type]
%    Type     = {constant, Value}                     | 
%               {integer, Size, Min, Max}             | 
%               {c_octet_string, Fixed, Size, Format} | 
%               {octet_string, Fixed, Size, Format}   |
%               {list, Type, Size}                    |
%               {composite, Name, Tuple}              |
%               {union, Types}
%    Value    = bin()
%    Size     = int()
%    Min      = int()
%    Max      = int()
%    Fixed    = bool()
%    Format   = free | hex | dec
%    Name     = atom()
%    Tuple    = term()
%
% %@doc Union data-type declaration.  Defines a new type which is the union of
% <tt>types</tt>.  For example the destination address in a <i>submit_multi</i>
% command represents either a distribution list or an SME address (these 
% addresses have different data-types).</p>
%
% <p><tt>{union, [SmeAddress, DlAddress]}</tt></p>
%
% <p>where <tt>SmeAddress</tt> and <tt>DlAddress</tt> are composite 
% data-type definitions.  Notice that nested data-type declarations are
% allowed.  In SMPP the union declaration is mainly used in examples like
% the one above mentioned, where the union types are composites.</p>
%
% <dl>
%   <dt>Types: </dt><dd>A list with the types of the union.</dd>
% </dl>
% %@end
%%
-record(union, {types}).

-endif.  % -ifndef(base_syntax)
