%%%
% Copyright (C) 2003 Enrique Marcote Peña <mpquique@users.sourceforge.net>
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
% @doc SMPP PDU Syntax definition.
%
% <p>SMPP PDU syntax definition.  This syntax extends the <i>param syntax</i>
% and gives support to PDU defintions.</p>
%
%
% @copyright 2003 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@users.sourceforge.net>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.1 alpha, {17 Mar 2003} {@time}.
% @end
%%

-ifndef(pdu_syntax).
-define(pdu_syntax, true).

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
%%%
% SMPP PDU Format.
%
% %@doc Macros based on pdu record definition.  Read comments preceding the
% record declaration for a complete description of the fields.
% %@end
%%
-define(PDU(StdsTypes, TlvsTypes),
        #pdu{stds_types = StdsTypes, tlvs_types = TlvsTypes}).


%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%
% %@spec {pdu, StdsTypes, TlvsTypes}
%    StdsTypes = [standard()]
%    TlvsTypes = [tlv()]
%    
% %@doc PDU definitions.
%
% <dl>
%   <dt>StdsTypes: </dt><dd>List with the types of the standard parameters.
%     There is one descriptor per standard parameter on the PDU.  This list
%     must have the same exact order than the one defined on the SMPP protocol
%     specification.
%   </dd>
%   <dt>TlvsTypes: </dt><dd>List with the types of every permitted TLV of the
%     PDU.  The SMPP protocol specification does not establish any kind of
%     ordering for the TLV parameters.  The order on this list might only be
%     relevant from an efficiency point of view, refer to the 
%     <a href="pdu_syntax.html#pack-3">packing</a>/
%     <a href="pdu_syntax.html#unpack-3">unpacking</a> functions 
%     implementation on <a href="pdu_syntax.html">pdu_syntax.erl</a>.
%   </dd>
% </dl>
% %@end
%%
-record(pdu, {stds_types, tlvs_types}).

-endif.  % -ifndef(pdu_syntax)

