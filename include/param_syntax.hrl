%%% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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

%%% @doc SMPP Parameter Syntax definition.
%%%
%%% <p>Standard and TLV parameter syntax definition.  This syntax complements
%%% the base syntax defined in the file <b>base_syntax.hrl</b>, giving
%%% to the parameter definitions the possibility to specify an associated error
%%% code, a default value and a tag (on TLVs only).  A complete list of the 
%%% SMPP parameters specification is defined in <b>smpp_param.hrl</b>, based
%%% on macros defined below and the syntax declarations included in
%%% <b>smpp_base.hrl</b>.</p>
%%%
%%% <p>Even this syntax is considered implementation-specific, the definitions 
%%% herein included try to reflect the conventions used on [SMPP 5.0].</p>
%%%
%%% <p>As a guideline, some comments include references to the specific section
%%% numbers on [SMPP 5.0].</p>
%%%
%%%
%%% <h2>Domain and Datatype</h2>
%%% 
%%% <p>To see how these concepts are used along this SMPP implementation, 
%%% consider the <tt>addr_ton</tt> field.</p>
%%%
%%% <p>Its <b>datatype</b> is a 1 octet integer: ?INTEGER(1).</p>
%%%
%%% <p>Its <b>domain</b> is the set of all possible TON Values: 
%%% ?BOUND_INTEGER(1, 6).</p>
%%%
%%%
%%% <h2>Implementation notes</h2>
%%%
%%% <p>The best way to understand the meaning of the data-types defined on this
%%% header file and how everything works, is by a complete example.</p>
%%%
%%% <p>On page 72, section 4.2.3.1 of the SMPP specification [SMPP 5.0] the
%%% following fields are defined:</p>
%%%
%%% <table width="100%" border="1" cellpadding="2">
%%% <caption>Fragment of Table 4-18 submit_multi PDU.</caption>
%%%   <tr> 
%%%     <th width="60%" valign="middle" align="center">Field Name</th>
%%%     <th width="20%" valign="middle" align="center">Size Octets</th>
%%%     <th width="20%" valign="middle" align="center">Type</th>
%%%   </tr>
%%%   <tr> 
%%%     <td width="60%" valign="middle" align="center">number_of_dests</td>
%%%     <td width="20%" valign="middle" align="center">1</td>
%%%     <td width="20%" valign="middle" align="center">Integer</td>
%%%   </tr>
%%%   <tr> 
%%%     <td width="60%" valign="middle" align="center">dest_address</td>
%%%     <td width="20%" valign="middle" align="center">Var. max 24</td>
%%%     <td width="20%" valign="middle" align="center">Composite</td>
%%%   </tr>
%%%   <tr>
%%%     <td width="60%" valign="middle" align="center">
%%%       <table width="100%" border="0" cellpadding="2">
%%%         <tr>
%%%           <td width="100%" align="center">dest_flag</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">dest_addr_ton</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">dest_addr_npi</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">destination_addr</td>
%%%         </tr>
%%%       </table>
%%%     </td>
%%%     <td width="20%" valign="middle" align="center">
%%%       <table width="100%" border="0" cellpadding="2">
%%%         <tr>
%%%           <td width="100%" align="center">1</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">1</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">1</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">Var. max 21</td>
%%%         </tr>
%%%       </table>
%%%     </td>
%%%     <td width="20%" valign="middle" align="center">
%%%       <table width="100%" border="0" cellpadding="2">
%%%         <tr>
%%%           <td width="100%" align="center">Integer</td>
%%%         </tr>
%%%         <tr>
%%%             <td width="100%" align="center">Integer</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">Integer</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">C-Octet String</td>
%%%         </tr>
%%%       </table>
%%%     </td>
%%%   </tr>
%%%   <tr> 
%%%     <td width="60%" valign="middle" align="center">dest_address</td>
%%%     <td width="20%" valign="middle" align="center">Var. max 23</td>
%%%     <td width="20%" valign="middle" align="center">Composite</td>
%%%   </tr>
%%%   <tr>
%%%     <td width="60%" valign="middle" align="center">
%%%       <table width="100%" border="0" cellpadding="2">
%%%         <tr>
%%%           <td width="100%" align="center">dest_flag</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">dl_name</td>
%%%         </tr>
%%%       </table>
%%%     </td>
%%%     <td width="20%" valign="middle" align="center">
%%%       <table width="100%" border="0" cellpadding="2">
%%%         <tr>
%%%           <td width="100%" align="center">1</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">Var. max 21</td>
%%%         </tr>
%%%       </table>
%%%     </td>
%%%     <td width="20%" valign="middle" align="center">
%%%       <table width="100%" border="0" cellpadding="2">
%%%         <tr>
%%%           <td width="100%" align="center">Integer</td>
%%%         </tr>
%%%         <tr>
%%%           <td width="100%" align="center">C-Octet String</td>
%%%         </tr>
%%%       </table>
%%%     </td>
%%%   </tr>
%%% </table>
%%%
%%% <p><tt>dest_address</tt> is a composite field containing a mandatory 
%%% <tt>dest_flag</tt> field (<tt>dest_flag</tt> is a constant) and either 
%%% an SME address (when <tt>dest_flag</tt> is 0x01) or a distribution list 
%%% name (<tt>dest_flag</tt> is 0x02); thus <tt>dest_address</tt> is an union. 
%%% Additionally the field can be encoded multiple times according to the value
%%% specified in the <tt>number_of_dests</tt> field; <tt>dest_address</tt> is 
%%% also a list.</p>
%%%
%%% <p>Assuming we already have the base types defined as described on file
%%% <b>base_syntax.hrl</b>, the parameter dest_address is translated
%%% into "param syntax" as follows:</p>
%%%
%%% <pre>
%%% -define(DEST_ADDRESS,
%%%         ?STANDARD(dest_address, 
%%%                   ?DEST_ADDRESS_DOMAIN,
%%%                   undefined,
%%%                   ?ESME_RINVDSTADR)).
%%% </pre>
%%%
%%% <p>Notice that <tt>DEST_ADDRESS</tt> embraces the definition of both
%%% fields; <tt>number_of_dests</tt> and <tt>dest_address</tt>, since a the 
%%% base type is a list and this type is encoded in a Length ++ List fashion, 
%%% thus <tt>number_of_dests</tt> doesn't need to be explicitly declared.</p>
%%%
%%% <p>The <tt>broadcast_area_success</tt> (page 139, section 4.8.4.5 on 
%%% [SMPP 5.0]) is a sample of a mandatory TLV, <tt>broadcast_content_type_info
%%% </tt> is an optional TLV.  Their internal representation is:</p>
%%%
%%% <pre>
%%% -define(BROADCAST_AREA_SUCCESS, 
%%%         ?MULTIPLE_MANDATORY_TLV(broadcast_area_success,
%%%                                 16#0608, 
%%%                                 ?BROADCAST_AREA_SUCCESS_DOMAIN, 
%%%                                 ?BROADCAST_AREA_SUCCESS_RESERVED, 
%%%                                 undefined, 
%%%                                 undefined)).
%%% -define(BROADCAST_CONTENT_TYPE_INFO, 
%%%         ?OPTIONAL_TLV(broadcast_content_type_info,
%%%                       16#0602, 
%%%                       ?BROADCAST_CONTENT_TYPE_INFO_DOMAIN,
%%%                       ?BROADCAST_CONTENT_TYPE_INFO_RESERVED,
%%%                       undefined,
%%%                       undefined)).
%%% </pre>
%%%
%%% <p>Please read comments preceding record and function definitions for
%%% further details.</p>
%%%
%%%
%%% <h2>Changes 0.1 -&gt; 0.2</h2>
%%%
%%% [17 Feb 2004]
%%% 
%%% <ul>
%%%   <li>Field <tt>default</tt> added to <tt>standard</tt> record definition.
%%%   <br/>
%%%   <br/><tt>STANDARD</tt> macro accordingly updated.
%%%   </li>
%%% </ul>
%%%
%%%
%%% <h2>References</h2>
%%% <dl>
%%%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification.
%%%     Version 5.0. SMS Forum.
%%%   </dd>
%%% </dl>
%%%
%%%
%%% @copyright 2003 - 2004 Enrique Marcote Peña
%%% @author Enrique Marcote Peña <mpquique_at_users.sourceforge.net>
%%%         [http://oserl.sourceforge.net/]
%%% @version 0.1, {14 Mar 2003} {@time}.
%%% @end
-ifndef(param_syntax).
-define(param_syntax, true).

%%%-------------------------------------------------------------------
%%% Macros
%%%-------------------------------------------------------------------
%% %@doc SMPP Parameter Format.
%%
%% <p>Macros based on record param-types definitions.  Read comments preceding
%% each record declaration for a complete description of the fields.</p>
%%
%% %@see sections 3.2.1.5 and 3.2.1.6 on [SMPP 5.0]
%% %@end
-define(STANDARD(Name, Domain, Default, Error), 
        #standard{
            name    = Name, 
            domain  = Domain, 
            default = Default, 
            error   = Error}).
-define(TLV(Name, Tag, Domain, Reserved, Mandatory, Multiple, Default, Error), 
        #tlv{
            name      = Name,
            tag       = Tag, 
            domain    = Domain,
            reserved  = Reserved, 
            mandatory = Mandatory,
            multiple  = Multiple,
            default   = Default, 
            error     = Error}).

%% %@doc Simplified TLV Macros for Readability.
%%
%% <p>Some options are implicitly assigned.  SMPP PDU TLV definitions are 
%% more readable using these macros.</p>
%% %@end
-define(MANDATORY_TLV(Name, Tag, Domain, Reserved, Default, Error), 
        ?TLV(Name, Tag, Domain, Reserved, true, false, Default, Error)).
-define(OPTIONAL_TLV(Name, Tag, Domain, Reserved, Default, Error), 
        ?TLV(Name, Tag, Domain, Reserved, false, false, Default, Error)).
-define(MULTIPLE_MANDATORY_TLV(Name, Tag, Domain, Reserved, Default, Error), 
        ?TLV(Name, Tag, Domain, Reserved, true, true, Default, Error)).
-define(MULTIPLE_OPTIONAL_TLV(Name, Tag, Domain, Reserved, Default, Error), 
        ?TLV(Name, Tag, Domain, Reserved, false, true, Default, Error)).

%%%-------------------------------------------------------------------
%%% Records
%%%-------------------------------------------------------------------
%% %@spec {standard, Name, Domain, Default, Error}
%%    Name    = atom()
%%    Domain  = Type
%%    Default = term()
%%    Type    = constant()       | 
%%              integer()        | 
%%              c_octet_string() | 
%%              octet_string()   |
%%              multivalue()     |
%%              composite()      |
%%              polymorphic()
%%    Error   = int()
%%
%% %@doc Standard Parameter declaration.
%%
%% <dl>
%%   <dt>Name: </dt><dd>Name of the parameter.</dd>
%%   <dt>Domain: </dt><dd>Domain of the parameter.  Defined by means of the
%%     base syntax (<b>base_syntax.hrl</b>).
%%   </dd>
%%   <dt>Default: </dt><dd>Default value for the parameter.  Used by 
%%     encoding functions whenever the parameter is left undefined.
%%   </dd>
%%   <dt>Error: </dt><dd>Error code associated to the parameter.</dd>
%% </dl>
%%
%% %@TODO Consider replacing the field Name by a Tag, pretty much like the 
%% TLVs. The standard tags could be taken from the range reserved for MC Vendor
%% specific TLV tags (0x1400-0x3FFF).  Define a a macro for every new Tag for
%% readability.
%% %@end
-record(standard, {name, domain, default, error}).

%% %@spec {tlv, Name, Tag, Domain, Reserved, Mandatory, Multiple, Default, Err}
%%    Name      = atom()
%%    Tag       = int()
%%    Domain    = Type
%%    Type      = constant()       | 
%%                integer()        | 
%%                c_octet_string() | 
%%                octet_string()   |
%%                multivalue()     |
%%                composite()      |
%%                polymorphic()
%%    Reserved  = Type
%%    Mandatory = bool()
%%    Multiple  = bool()
%%    Default   = term()
%%    Err       = int()
%%
%% %@doc Tagged Length Value Parameter declaration.
%%
%% <dl>
%%   <dt>Name: </dt><dd>Name of the parameter.</dd>
%%   <dt>Tag: </dt><dd>Identifier of the TLV.</dd>
%%   <dt>Domain: </dt><dd>Domain of the parameter.  Defined by means of the
%%     base syntax (<b>base_syntax.hrl</b>).
%%   </dd>
%%   <dt>Reserved: </dt><dd>Set of reserved values for the parameter.  Defined
%%     by means of the base syntax (<b>base_syntax.hrl</b>).
%%   </dd>
%%   <dt>Mandatory: </dt><dd>Boolean.  <tt>true</tt> if the TLV is mandatory.
%%   </dd>
%%   <dt>Multiple: </dt><dd>Boolean.  <tt>true</tt> if the TLV can be encoded 
%%     multiple times.
%%   </dd>
%%   <dt>Default: </dt><dd>Default value for the parameter.  If the 
%%     encoding (or decoding) operation fails the default value should be used
%%     (if provided).
%%   </dd>
%%   <dt>Err: </dt><dd>Error code associated to the parameter.</dd>
%% </dl>
%%
%% %@TODO Consider removing the field Name and use the Tag on the PDU
%% dictionary.   Define a macro for every Tag for readability
%% %@end
-record(tlv, 
        {name, tag, domain, reserved, mandatory, multiple, default, error}).

-endif.  % -ifndef(param_syntax)
