%%%
% Copyright (C) 2003 - 2004 Enrique Marcote Peña <mpquique@udc.es>
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
% @doc SMPP Base definitions.
%
% <p>Basic definitions of the SMPP protocol specification using the syntax
% defined in <b>base_syntax.hrl</b>.</p>
%
% <p>As a guideline, some comments reference the section number of the
% document [SMPP 5.0].</p>
%
%
% <h2>Domain and Datatype</h2>
% 
% <p>To see how these concepts are used along this SMPP implementation, 
% consider the addr_ton field.</p>
%
% <p>Its <b>datatype</b> is a 1 octet integer: ?INTEGER(1).</p>
%
% <p>Its <b>domain</b> is the set of all possible TON Values: 
% ?BOUND_INTEGER(1, 6).</p>
%
% <p>Datatype definitions are not currently used, but they're preserved until
% definitely discarded</p>
%
%
% <h2>References</h2>
% <dl>
%   <dt>[SMPP 5.0]</dt><dd>Short Message Peer-to-Peer Protocol Specification.
%     Version 5.0. SMS Forum.
%   </dd>
%   <dt>[3GPP TS 23.040]</dt><dd>Technical Realization of the Short Message
%     Service (SMS) Release 4.  Version 5.0.0. 
%     <a href="http://www.3gpp.org">3GPP</a>.
%   </dd>
% </dl>
%
%
% @copyright 2003 - 2004 Enrique Marcote Peña
% @author Enrique Marcote Peña <mpquique@udc.es>
%         [http://www.des.udc.es/~mpquique/]
% @version 0.2 alpha, {14 Mar 2003} {@time}.
% @end
%%

-ifndef(smpp_base).
-define(smpp_base, true).

%%%-------------------------------------------------------------------
% Include files
%%--------------------------------------------------------------------
-include("smpp_globals.hrl").  % Some global definitions
-include("base_syntax.hrl").   % The syntax used in this file

%%%-------------------------------------------------------------------
% Macros
%%--------------------------------------------------------------------
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Common Definitions
%
% <p>Some syntax definitions shared by several fields/TLVs</p>
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% addr
%
% %@doc Specifies the SME address. For mobile terminated/originated messages,
% this is the directory number of the recipient/originating MS. IP address
% must be in "aaa.bbb.ccc.ddd" notation.
%
% <p>Used on: destination_addr, dest_address, esme_addr, source_addr, 
% unsuccess_sme</p>
% %@end
%%
-define(ADDR_21_DATATYPE, ?VAR_C_OCTET_STRING(21)).
-define(ADDR_21_DOMAIN,   ?VAR_C_OCTET_STRING(21)).
-define(ADDR_21_RESERVED, ?EMPTY).

-define(ADDR_65_DATATYPE, ?VAR_C_OCTET_STRING(65)).
-define(ADDR_65_DOMAIN,   ?VAR_C_OCTET_STRING(65)).
-define(ADDR_65_RESERVED, ?EMPTY).

%%%
% addr_subunit
%
% %@doc Used on: dest_addr_subunit, source_addr_subunit
% %@end
%%
-define(ADDR_SUBUNIT_DATATYPE, ?INTEGER(1)).
-define(ADDR_SUBUNIT_DOMAIN,   ?BOUND_INTEGER(1, 16#04)).
-define(ADDR_SUBUNIT_RESERVED, ?RANGE_INTEGER(1, 16#05, 16#FF)).

% addr_subunit Values
-define(ADDR_SUBUNIT_UNKNOWN,          16#00). % Default
-define(ADDR_SUBUNIT_MS_DISPLAY,       16#01).
-define(ADDR_SUBUNIT_MOBILE_EQUIPMENT, 16#02).
-define(ADDR_SUBUNIT_SMART_CARD_1,     16#03). % Expected to be SIM
-define(ADDR_SUBUNIT_EXTERNAL_UNIT_2,  16#04).

%%%
% bearer_type
%
% %@doc Used on: dest_bearer_type, source_bearer_type
% %@end
%%
-define(BEARER_TYPE_DATATYPE, ?INTEGER(1)).
-define(BEARER_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 16#08)).
-define(BEARER_TYPE_RESERVED, ?RANGE_INTEGER(1, 16#09, 16#FF)).

% bearer_type Values
-define(BEARER_TYPE_UNKNOWN,        16#00).
-define(BEARER_TYPE_SMS,            16#01).
-define(BEARER_TYPE_CSD,            16#02).  % Circuit Switched Data
-define(BEARER_TYPE_PACKET_DATA,    16#03).
-define(BEARER_TYPE_USSD,           16#04).
-define(BEARER_TYPE_CDPD,           16#05).
-define(BEARER_TYPE_DATATAC,        16#06).  % DataTAC
-define(BEARER_TYPE_FLEX_REFLEX,    16#07).  % FLEX/ReFLEX
-define(BEARER_TYPE_CELL_BROADCAST, 16#08).  % Cell Broadcast (cell cast)

%%%
% broadcast_area
%
% %@doc A broadcast_area value should be defined using the broadcast_area 
% record.
%
% <p>Used on: broadcast_area_identifier, failed_broadcast_area_identifier.</p>
%
% %@see broadcast_area record definition below.
% %@end
%%
-define(BROADCAST_AREA_FORMAT_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_AREA_FORMAT_DOMAIN,   ?BOUND_INTEGER(1, 16#02)).
-define(BROADCAST_AREA_FORMAT_RESERVED, ?RANGE_INTEGER(1, 16#03, 16#FF)).

-define(BROADCAST_AREA_DETAILS_DATATYPE, ?VAR_OCTET_STRING(100)).
-define(BROADCAST_AREA_DETAILS_DOMAIN,   ?VAR_OCTET_STRING(100)).
-define(BROADCAST_AREA_DETAILS_RESERVED, ?VAR_OCTET_STRING(100)).

-define(BROADCAST_AREA_DATATYPE, 
        ?COMPOSITE(broadcast_area,
                   {?BROADCAST_AREA_FORMAT_DATATYPE,
                    ?BROADCAST_AREA_DETAILS_DATATYPE})).
-define(BROADCAST_AREA_DOMAIN, 
        ?COMPOSITE(broadcast_area,
                   {?BROADCAST_AREA_FORMAT_DOMAIN,
                    ?BROADCAST_AREA_DETAILS_DOMAIN})).
-define(BROADCAST_AREA_RESERVED, 
        ?COMPOSITE(broadcast_area,
                   {?BROADCAST_AREA_FORMAT_RESERVED,
                    ?BROADCAST_AREA_DETAILS_RESERVED})).

% broadcast_area_format Values
-define(BROADCAST_AREA_FORMAT_ALIAS,         16#00).
-define(BROADCAST_AREA_FORMAT_ELLIPSOID_ARC, 16#01).
-define(BROADCAST_AREA_FORMAT_POLYGON,       16#02).

%%%
% encoding_scheme
%
% %@doc Used on: data_coding, callback_num_atag.
% %@end
%%
-define(ENCODING_SCHEME_DATATYPE, ?INTEGER(1)).
-define(ENCODING_SCHEME_DOMAIN,   
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00001010),
		        ?RANGE_INTEGER(1, 2#00001101, 2#00001110),
                ?RANGE_INTEGER(1, 2#11000000, 2#11011111),
                ?RANGE_INTEGER(1, 2#11110000, 2#11111111)])).
-define(ENCODING_SCHEME_RESERVED,   
        ?UNION([?RANGE_INTEGER(1, 2#00001011, 2#00001100),
                ?RANGE_INTEGER(1, 2#00001111, 2#10111111),
                ?RANGE_INTEGER(1, 2#11100000, 2#11101111)])).

% encoding_scheme Values
-define(ENCODING_SCHEME_MC_SPECIFIC,  2#00000000). % Default alphabet assumed
                                                   % by MC
-define(ENCODING_SCHEME_IA5_ASCII,    2#00000001). % IA5 (CCITT T.50)/ASCII 
                                                   % (ANSI X3.4)
-define(ENCODING_SCHEME_OCTET,        2#00000010). % Octet unspecified
-define(ENCODING_SCHEME_LATIN_1,      2#00000011). % Latin 1 (ISO-8859-1)
-define(ENCODING_SCHEME_BINARY,       2#00000100). % 8-bit binary: WAP, Logos,
                                                   % tones ...
-define(ENCODING_SCHEME_JIS,          2#00000101). % X 0208-1990
-define(ENCODING_SCHEME_CYRILLIC,     2#00000110). % Cyrillic (ISO-8859-5)
-define(ENCODING_SCHEME_LATIN_HEBREW, 2#00000111). % Latin/Hebrew (ISO-8859-8)
-define(ENCODING_SCHEME_UCS2,         2#00001000). % ISO/IEC-10646
-define(ENCODING_SCHEME_PICTOGRAM,    2#00001001). % Pictogram Encoding
-define(ENCODING_SCHEME_ISO_2022_JP,  2#00001010). % Music Codes
-define(ENCODING_SCHEME_KANJI_JIS,    2#00001101). % Extended Kanji JIS,
                                                   % (X 0212-1990)
-define(ENCODING_SCHEME_KS_C_5601,    2#00001110). % KS C 5601

%%%
% message_identifier
%
% %@doc Set according to MC implementation.
%
% <p>Used by: message_id, receipted_message_id</p>
% %@end
%%
-define(MESSAGE_IDENTIFIER_DATATYPE, ?VAR_C_OCTET_STRING(65)).
-define(MESSAGE_IDENTIFIER_DOMAIN,   ?VAR_C_OCTET_STRING(65)).
-define(MESSAGE_IDENTIFIER_RESERVED, ?EMPTY).

%%%
% message_state
%
% %@doc Used by: message_state_std, message_state_tlv
% %@end
%%
-define(MESSAGE_STATE_DATATYPE, ?INTEGER(1)).
-define(MESSAGE_STATE_DOMAIN,   ?BOUND_INTEGER(1, 9)).
-define(MESSAGE_STATE_RESERVED, ?EMPTY).

% message_state Values
-define(MESSAGE_STATE_SCHEDULED,     0).  % Intermediate
-define(MESSAGE_STATE_ENROUTE,       1).  % Intermediate
-define(MESSAGE_STATE_DELIVERED,     2).  % Final
-define(MESSAGE_STATE_EXPIRED,       3).  % Final
-define(MESSAGE_STATE_DELETED,       4).  % Final
-define(MESSAGE_STATE_UNDELIVERABLE, 5).  % Final
-define(MESSAGE_STATE_ACCEPTED,      6).  % Final
-define(MESSAGE_STATE_UNKNOWN,       7).  % N/A
-define(MESSAGE_STATE_REJECTED,      8).  % Final
-define(MESSAGE_STATE_SKIPPED,       9).  % Final

%%
% network_id
%
% %@doc Unique address that may be derived and assigned by the node owner 
% without establishing a central assignment and management authority.
%
% <p>Used on: dest_network_id, source_network_id</p>
% %@end
%%
-define(NETWORK_ID_DATATYPE, ?VAR_C_OCTET_STRING(65)).
-define(NETWORK_ID_DOMAIN,   ?VAR_C_OCTET_STRING(65)).
-define(NETWORK_ID_RESERVED, ?EMPTY).

%%%
% network_type
%
% %@doc Used by: dest_network_type, source_network_type
% %@end
%%
-define(NETWORK_TYPE_DATATYPE, ?INTEGER(1)).
-define(NETWORK_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 16#08)).
-define(NETWORK_TYPE_RESERVED, ?RANGE_INTEGER(1, 16#09, 16#FF)).

% network_type Values
-define(NETWORK_TYPE_UNKNOWN,        16#00).
-define(NETWORK_TYPE_GENERIC,        16#00).
-define(NETWORK_TYPE_GSM,            16#01).
-define(NETWORK_TYPE_TDMA,           16#02).  % ANSI-136/TDMA
-define(NETWORK_TYPE_CDMA,           16#03).  % IS-95/CDMA
-define(NETWORK_TYPE_PDC,            16#04).
-define(NETWORK_TYPE_PHS,            16#05).
-define(NETWORK_TYPE_IDEN,           16#06).  % iDEN
-define(NETWORK_TYPE_AMPS,           16#07).
-define(NETWORK_TYPE_PAGING_NETWORK, 16#08).

%%%
% node_id
%
% %@doc Sequence of 6 decimal digits identifying the node.
%
% <p>Used on: dest_node_id, source_node_id</p>
% %@end
%%
-define(NODE_ID_DATATYPE, ?FIXED_DEC_OCTET_STRING(6)).
-define(NODE_ID_DOMAIN,   ?FIXED_DEC_OCTET_STRING(6)).
-define(NODE_ID_RESERVED, ?EMPTY).

%%%
% npi
%
% %@doc Used on: addr_npi, source_addr_npi, dest_addr_npi, esme_addr_npi, 
% dest_address, unsuccess_sme, callback_num
% %@end
%%
-define(NPI_DATATYPE, ?INTEGER(1)).
-define(NPI_DOMAIN, 
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00000001),
                ?RANGE_INTEGER(1, 2#00000011, 2#00000100),
                ?RANGE_INTEGER(1, 2#00000110, 2#00000110),
                ?RANGE_INTEGER(1, 2#00001000, 2#00001010),
                ?RANGE_INTEGER(1, 2#00001110, 2#00001110),
                ?RANGE_INTEGER(1, 2#00010010, 2#00010010)])).
-define(NPI_RESERVED, 
        ?UNION([?RANGE_INTEGER(1, 2#00000010, 2#00000010),
		        ?RANGE_INTEGER(1, 2#00000101, 2#00000101),
                ?RANGE_INTEGER(1, 2#00000111, 2#00000111),
                ?RANGE_INTEGER(1, 2#00001011, 2#00001101),
		        ?RANGE_INTEGER(1, 2#00010011, 2#11111111)])).

% npi Values
-define(NPI_UNKNOWN,       2#00000000).
-define(NPI_ISDN,          2#00000001). % E163/E164
-define(NPI_DATA,          2#00000011). % X.121
-define(NPI_TELEX,         2#00000100). % F.69
-define(NPI_LAND_MOBILE,   2#00000110). % E.212
-define(NPI_NATIONAL,      2#00001000).
-define(NPI_PRIVATE,       2#00001001).
-define(NPI_ERMES,         2#00001010).
-define(NPI_INTERNET,      2#00001110). % IP
-define(NPI_WAP_CLIENT_ID, 2#00010010). % To be defined by WAP Forum

%%%
% port
%
% %@doc All values allowed 
%
% <p>Used on: dest_port, source_port</p>
% %@end
%%
-define(PORT_DATATYPE, ?INTEGER(2)).
-define(PORT_DOMAIN,   ?INTEGER(2)).
-define(PORT_RESERVED, ?EMPTY).

%%%
% protocol_identifier
%
% %@doc According to [3GPP TS 23.040] GSM values are listed below.
%
% <p>On both, TDMA and CDMA, protocol_id is Ignored for mobile terminated
% messages and set to NULL by the MC for mobile originated messages.</p>
%
% <p>[3GPP TS 23.040] recommends to reject messages with a TP-Protocol 
% Identifier containing a reserced value or one which is not supported.
% That's why PROTOCOL_IDENTIFIER_RESERVED is set to  <tt>?EMPTY</tt>.</p>
%
% <p>Used on: protocol_id</p>
%
% %@TODO Review the domain declaration.  What about the SM-AL protocol
% identifier values (when bits 7, 6 and 5 are 0).
% %@end
%%
-define(PROTOCOL_IDENTIFIER_DATATYPE, ?INTEGER(1)).
-define(PROTOCOL_IDENTIFIER_DOMAIN,   
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00011111), 
                ?RANGE_INTEGER(1, 2#00100000, 2#00101101),
                ?RANGE_INTEGER(1, 2#00110000, 2#00110010),
                ?RANGE_INTEGER(1, 2#00111000, 2#00111111),
                ?RANGE_INTEGER(1, 2#01000000, 2#01000111),
                ?RANGE_INTEGER(1, 2#01011110, 2#01011111),
                ?RANGE_INTEGER(1, 2#01111100, 2#01111111),
                ?RANGE_INTEGER(1, 2#11000000, 2#11111111)])). % SC use
-define(PROTOCOL_IDENTIFIER_RESERVED, ?EMPTY).

% protocol_identifier Values
-define(PROTOCOL_IDENTIFIER_GSM,  ?NULL_INTEGER).
-define(PROTOCOL_IDENTIFIER_TDMA, ?NULL_INTEGER).
-define(PROTOCOL_IDENTIFIER_CDMA, ?NULL_INTEGER).

%%%
% smpp_error
%
% %@doc A list with all the SMPP Error Codes can be found on 
% <b>smpp_globals.hrl</b>
%
% <p>Used on: command_status, error_status_code, unsuccess_sme</p>
%
% %@see smpp_globals.hrl
% %@end
%%
-define(SMPP_ERROR_DATATYPE, ?INTEGER(4)).
-define(SMPP_ERROR_DOMAIN,  
        ?UNION([?RANGE_INTEGER(4, 16#00000000, 16#00000008),
                ?RANGE_INTEGER(4, 16#0000000A, 16#0000000F),
                ?RANGE_INTEGER(4, 16#00000011, 16#00000011),
                ?RANGE_INTEGER(4, 16#00000013, 16#00000015),
                ?RANGE_INTEGER(4, 16#00000033, 16#00000034),
                ?RANGE_INTEGER(4, 16#00000040, 16#00000040),
                ?RANGE_INTEGER(4, 16#00000042, 16#00000045),
                ?RANGE_INTEGER(4, 16#00000048, 16#00000051),
                ?RANGE_INTEGER(4, 16#00000053, 16#00000055),
                ?RANGE_INTEGER(4, 16#00000058, 16#00000058),
                ?RANGE_INTEGER(4, 16#00000061, 16#00000067),
                ?RANGE_INTEGER(4, 16#000000C0, 16#000000C4),
                ?RANGE_INTEGER(4, 16#000000FE, 16#000000FF),
                ?RANGE_INTEGER(4, 16#00000100, 16#00000112)])).
-define(SMPP_ERROR_RESERVED,  
        ?UNION([?RANGE_INTEGER(4, 16#00000009, 16#00000009),
                ?RANGE_INTEGER(4, 16#00000010, 16#00000010),
                ?RANGE_INTEGER(4, 16#00000012, 16#00000012),
                ?RANGE_INTEGER(4, 16#00000016, 16#00000032),
                ?RANGE_INTEGER(4, 16#00000035, 16#0000003F),
                ?RANGE_INTEGER(4, 16#00000041, 16#00000041),
                ?RANGE_INTEGER(4, 16#00000046, 16#00000047),
                ?RANGE_INTEGER(4, 16#00000052, 16#00000052),
                ?RANGE_INTEGER(4, 16#00000056, 16#00000057),
                ?RANGE_INTEGER(4, 16#00000059, 16#00000060),
                ?RANGE_INTEGER(4, 16#00000068, 16#000000BF),
                ?RANGE_INTEGER(4, 16#000000C5, 16#000000FD),
                ?RANGE_INTEGER(4, 16#00000113, 16#FFFFFFFF)])).

%%%
% smpp_version
%
% %@doc Used on: interface_version, sc_interface_version
% %@end
%%
-define(SMPP_VERSION_DATATYPE, ?INTEGER(1)).
-define(SMPP_VERSION_DOMAIN,   
        ?UNION([?RANGE_INTEGER(1, 16#00, 16#34),
                ?CONSTANT(16#50)])).
-define(SMPP_VERSION_RESERVED, 
        ?UNION([?RANGE_INTEGER(1, 16#35, 16#49),
                ?RANGE_INTEGER(1, 16#51, 16#FF)])).


% smpp_version Values
-define(SMPP_VERSION_EARLIER, 16#00).  % Earlier version
-define(SMPP_VERSION_3_3,     16#33).  % Version 3.3
-define(SMPP_VERSION_3_4,     16#34).  % Version 3.4
-define(SMPP_VERSION_5_0,     16#50).  % Version 5.0

%%%
% subaddress
%
% %@doc A subaddress value should be defined using the subaddress record.
%
% <p>Used on: dest_subaddress, source_subaddress</p>
%
% %@see subaddress record definition below
% %@end
%%
-define(SUBADDRESS_TAG_DATATYPE, ?INTEGER(1)).
-define(SUBADDRESS_TAG_DOMAIN,   ?SET([2#10000000, 2#10001000, 2#10100000])).
-define(SUBADDRESS_TAG_RESERVED,
        ?UNION([?RANGE_INTEGER(1, 2#00000001, 2#01111111),
                ?RANGE_INTEGER(1, 2#10000001, 2#10000111),
                ?RANGE_INTEGER(1, 2#10001001, 2#10011111),
                ?RANGE_INTEGER(1, 2#10100001, 2#11111111)])).

-define(SUBADDRESS_DATA_DATATYPE, ?VAR_OCTET_STRING(22)).
-define(SUBADDRESS_DATA_DOMAIN,   ?VAR_OCTET_STRING(22)).
-define(SUBADDRESS_DATA_RESERVED, ?VAR_OCTET_STRING(22)).

-define(SUBADDRESS_DATATYPE, 
        ?COMPOSITE(subaddress, 
                   {?SUBADDRESS_TAG_DATATYPE, 
                    ?SUBADDRESS_DATA_DATATYPE})).
-define(SUBADDRESS_DOMAIN, 
        ?COMPOSITE(subaddress, 
                   {?SUBADDRESS_TAG_DOMAIN, 
                    ?SUBADDRESS_DATA_DOMAIN})).
-define(SUBADDRESS_RESERVED, 
        ?COMPOSITE(subaddress, 
                   {?SUBADDRESS_TAG_RESERVED, 
                    ?SUBADDRESS_DATA_RESERVED})).

% subaddress_tag Values
-define(SUBADDRESS_TAG_NSAP_EVEN, 2#10000000). % NSAP (Even) [ITUT X.213]
-define(SUBADDRESS_TAG_NSAP_ODD,  2#10001000). % NSAP (Odd) [ITUT X.213]
-define(SUBADDRESS_TAG_USER,      2#10100000). % User Specific

%%%
% telematics_id
%
% %@doc A telematics_id value should be defined using the telematics_id record.
%
% <p>Used on: dest_telematics_id, source_telematics_id</p>
%
% %@see telematics_id record definition below
% %@see protocol_identifier definition above
% %@end
%%
-define(TELEMATICS_ID_RESERVED_DATATYPE, ?INTEGER(1)).
-define(TELEMATICS_ID_RESERVED_DOMAIN,   ?INTEGER(1)).

-define(TELEMATICS_ID_DATATYPE, 
        ?COMPOSITE(telematics_id, 
                   {?PROTOCOL_IDENTIFIER_DATATYPE, 
                    ?TELEMATICS_ID_RESERVED_DATATYPE})).
-define(TELEMATICS_ID_DOMAIN, 
        ?COMPOSITE(telematics_id, 
                   {?PROTOCOL_IDENTIFIER_DOMAIN, 
                    ?TELEMATICS_ID_RESERVED_DOMAIN})).
-define(TELEMATICS_ID_RESERVED, ?EMPTY).

%%%
% time_relative, time_absolute and time
%
% %@doc Specifies either absolute time format or relative time from the 
% current MC time.  "YYMMDDhhmmsstnnp"
%
% <p>Used on: schedule_delivery_time, validity_period, final_date</p>
%
% %@see time record definition below
% %@end
%%
-define(TIME_YEAR_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_YEAR_DOMAIN,   ?FIXED_DEC_OCTET_STRING(2)).

-define(TIME_MONTH_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_MONTH_DOMAIN, 
        ?SET(["01","02","03","04","05","06","07","08","09","10","11","12"])).

-define(TIME_DAY_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_DAY_DOMAIN,              
        ?SET(["01","02","03","04","05","06","07","08","09","10",
              "11","12","13","14","15","16","17","18","19","20",
              "21","22","23","24","25","26","27","28","29","30","31"])).

-define(TIME_HOUR_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_HOUR_DOMAIN,
        ?SET(["00","01","02","03","04","05","06","07","08","09",
              "10","11","12","13","14","15","16","17","18","19",
              "20","21","22","23"])).

-define(TIME_MINUTE_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_MINUTE_DOMAIN,
        ?SET(["00","01","02","03","04","05","06","07","08","09",
              "10","11","12","13","14","15","16","17","18","19",
              "20","21","22","23","24","25","26","27","28","29",
              "30","31","32","33","34","35","36","37","38","39",
              "40","41","42","43","44","45","46","47","48","49",
              "50","51","52","53","54","55","56","57","58","59"])).

-define(TIME_SECONDS_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_SECONDS_DOMAIN,
        ?SET(["00","01","02","03","04","05","06","07","08","09",
              "10","11","12","13","14","15","16","17","18","19",
              "20","21","22","23","24","25","26","27","28","29",
              "30","31","32","33","34","35","36","37","38","39",
              "40","41","42","43","44","45","46","47","48","49",
              "50","51","52","53","54","55","56","57","58","59"])).

-define(TIME_TENTHS_OF_SECOND_ABSOLUTE_DATATYPE, ?FIXED_DEC_OCTET_STRING(1)).
-define(TIME_TENTHS_OF_SECOND_ABSOLUTE_DOMAIN,   ?FIXED_DEC_OCTET_STRING(1)).

-define(TIME_TENTHS_OF_SECOND_RELATIVE_DATATYPE, ?FIXED_DEC_OCTET_STRING(1)).
-define(TIME_TENTHS_OF_SECOND_RELATIVE_DOMAIN,   ?CONSTANT("0")).

-define(TIME_DIFFERENCE_ABSOLUTE_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_DIFFERENCE_ABSOLUTE_DOMAIN, 
        ?SET(["00","01","02","03","04","05","06","07","08","09",
              "10","11","12","13","14","15","16","17","18","19",
              "20","21","22","23","24","25","26","27","28","29",
              "30","31","32","33","34","35","36","37","38","39",
              "40","41","42","43","44","45","46","47","48"])).

-define(TIME_DIFFERENCE_RELATIVE_DATATYPE, ?FIXED_DEC_OCTET_STRING(2)).
-define(TIME_DIFFERENCE_RELATIVE_DOMAIN,   ?CONSTANT("00")).

-define(TIME_RELATION_RELATIVE_DATATYPE, ?CONSTANT("R")).
-define(TIME_RELATION_RELATIVE_DOMAIN,   ?CONSTANT("R")).

-define(TIME_RELATION_ABSOLUTE_DATATYPE, ?SET(["+", "-"])).
-define(TIME_RELATION_ABSOLUTE_DOMAIN,   ?SET(["+", "-"])).

-define(TIME_NULL_TERMINATOR_DATATYPE, ?CONSTANT(?NULL_C_OCTET_STRING)).
-define(TIME_NULL_TERMINATOR_DOMAIN,   ?CONSTANT(?NULL_C_OCTET_STRING)).

-define(TIME_COMPOSITE_RELATIVE_DATATYPE,
        ?COMPOSITE(time,
                   {?TIME_YEAR_DATATYPE, 
                    ?TIME_MONTH_DATATYPE,
                    ?TIME_DAY_DATATYPE,
                    ?TIME_HOUR_DATATYPE,
                    ?TIME_MINUTE_DATATYPE,
                    ?TIME_SECONDS_DATATYPE,
                    ?TIME_TENTHS_OF_SECOND_RELATIVE_DATATYPE,
                    ?TIME_DIFFERENCE_RELATIVE_DATATYPE,
                    ?TIME_RELATION_RELATIVE_DATATYPE,
                    ?TIME_NULL_TERMINATOR_DATATYPE})).
-define(TIME_COMPOSITE_RELATIVE_DOMAIN,
        ?COMPOSITE(time,
                   {?TIME_YEAR_DOMAIN, 
                    ?TIME_MONTH_DOMAIN,
                    ?TIME_DAY_DOMAIN,
                    ?TIME_HOUR_DOMAIN,
                    ?TIME_MINUTE_DOMAIN,
                    ?TIME_SECONDS_DOMAIN,
                    ?TIME_TENTHS_OF_SECOND_RELATIVE_DOMAIN,
                    ?TIME_DIFFERENCE_RELATIVE_DOMAIN,
                    ?TIME_RELATION_RELATIVE_DOMAIN,
                    ?TIME_NULL_TERMINATOR_DOMAIN})).

-define(TIME_COMPOSITE_ABSOLUTE_DATATYPE,
        ?COMPOSITE(time,
                   {?TIME_YEAR_DATATYPE, 
                    ?TIME_MONTH_DATATYPE,
                    ?TIME_DAY_DATATYPE,
                    ?TIME_HOUR_DATATYPE,
                    ?TIME_MINUTE_DATATYPE,
                    ?TIME_SECONDS_DATATYPE,
                    ?TIME_TENTHS_OF_SECOND_ABSOLUTE_DATATYPE,
                    ?TIME_DIFFERENCE_ABSOLUTE_DATATYPE,
                    ?TIME_RELATION_ABSOLUTE_DATATYPE,
                    ?TIME_NULL_TERMINATOR_DATATYPE})).
-define(TIME_COMPOSITE_ABSOLUTE_DOMAIN,
        ?COMPOSITE(time,
                   {?TIME_YEAR_DOMAIN, 
                    ?TIME_MONTH_DOMAIN,
                    ?TIME_DAY_DOMAIN,
                    ?TIME_HOUR_DOMAIN,
                    ?TIME_MINUTE_DOMAIN,
                    ?TIME_SECONDS_DOMAIN,
                    ?TIME_TENTHS_OF_SECOND_ABSOLUTE_DOMAIN,
                    ?TIME_DIFFERENCE_ABSOLUTE_DOMAIN,
                    ?TIME_RELATION_ABSOLUTE_DOMAIN,
                    ?TIME_NULL_TERMINATOR_DOMAIN})).

-define(TIME_RELATIVE_DATATYPE, ?TIME_COMPOSITE_RELATIVE_DATATYPE).
-define(TIME_RELATIVE_DOMAIN, 
        ?UNION([?CONSTANT("\0"), ?TIME_COMPOSITE_RELATIVE_DOMAIN])).
-define(TIME_RELATIVE_RESERVED, ?EMPTY).

-define(TIME_ABSOLUTE_DATATYPE, ?TIME_COMPOSITE_ABSOLUTE_DATATYPE).
-define(TIME_ABSOLUTE_DOMAIN, 
        ?UNION([?CONSTANT("\0"), ?TIME_COMPOSITE_ABSOLUTE_DOMAIN])).
-define(TIME_ABSOLUTE_RESERVED, ?EMPTY).

-define(TIME_DATATYPE, 
        ?UNION([?TIME_COMPOSITE_ABSOLUTE_DATATYPE,
                ?TIME_COMPOSITE_RELATIVE_DATATYPE])).
-define(TIME_DOMAIN, 
        ?UNION([?CONSTANT("\0"), 
                ?TIME_COMPOSITE_ABSOLUTE_DOMAIN,
                ?TIME_COMPOSITE_RELATIVE_DOMAIN])).
-define(TIME_RESERVED, ?EMPTY).

%%%
% ton 
%
% %@doc Used on: addr_ton, source_addr_ton, dest_addr_ton, esme_addr_ton, 
% dest_address, unsuccess_sme, callback_num
% %@end
%%
-define(TON_DATATYPE, ?INTEGER(1)).
-define(TON_DOMAIN,   ?BOUND_INTEGER(1, 2#00000110)).
-define(TON_RESERVED, ?RANGE_INTEGER(1, 2#00000111, 2#11111111)).

% ton Values
-define(TON_UNKNOWN,           2#00000000).
-define(TON_INTERNATIONAL,     2#00000001).
-define(TON_NATIONAL,          2#00000010).
-define(TON_NETWORK_SPECIFIC,  2#00000011).
-define(TON_SUBSCRIBER_NUMBER, 2#00000100).
-define(TON_ALPHANUMERIC,      2#00000101).
-define(TON_ABBREVIATED,       2#00000110).

%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% PDU Field Base Syntax Definitions
%
% %@see section 4.7 on [SMPP 5.0]
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% addr_ton, source_addr_ton, dest_addr_ton, esme_addr_ton 
%
% %@see ton definition above
%%
-define(ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(ADDR_TON_RESERVED, ?TON_RESERVED).

-define(SOURCE_ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(SOURCE_ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(SOURCE_ADDR_TON_RESERVED, ?TON_RESERVED).

-define(DEST_ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(DEST_ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(DEST_ADDR_TON_RESERVED, ?TON_RESERVED).

-define(ESME_ADDR_TON_DATATYPE, ?TON_DATATYPE).
-define(ESME_ADDR_TON_DOMAIN,   ?TON_DOMAIN).
-define(ESME_ADDR_TON_RESERVED, ?TON_RESERVED).

%%%
% addr_npi, source_addr_npi, dest_addr_npi, esme_addr_npi
%
% %@see npi definition above
%%
-define(ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(ADDR_NPI_RESERVED, ?NPI_RESERVED).

-define(SOURCE_ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(SOURCE_ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(SOURCE_ADDR_NPI_RESERVED, ?NPI_RESERVED).

-define(DEST_ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(DEST_ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(DEST_ADDR_NPI_RESERVED, ?NPI_RESERVED).

-define(ESME_ADDR_NPI_DATATYPE, ?NPI_DATATYPE).
-define(ESME_ADDR_NPI_DOMAIN,   ?NPI_DOMAIN).
-define(ESME_ADDR_NPI_RESERVED, ?NPI_RESERVED).

%%%
% address_range
%
% %@doc 
% 
% <ul> 
%   <li>Single SME address</li>
%   <li>Range of addresses using UNIX Regular Expression notation</li>
% </ul>
% %@end
%%
-define(ADDRESS_RANGE_DATATYPE, ?VAR_C_OCTET_STRING(41)).
-define(ADDRESS_RANGE_DOMAIN,   ?VAR_C_OCTET_STRING(41)).
-define(ADDRESS_RANGE_RESERVED, ?EMPTY).

%%%
% command_length
%
% %@doc Represents the actual size of the PDU including the PDU header and body
% (command_length included).
%
% <p>On current implementation, encoding/decoding this value is implicitly 
% done by unpacking/packing functions, thus this field must not be included in
% the PDU definitions.  Never used.</p>
% %@end
%%
% -define(COMMAND_LENGTH_DATATYPE, ?INTEGER(4)).
% -define(COMMAND_LENGTH_DOMAIN,   ?INTEGER(4)).
% -define(COMMAND_LENGTH_RESERVED, ?EMPTY).

%%%
% command_id
%
% %@doc A list with all the SMPP Commmand Ids can be found on 
% <b>smpp_globals.hrl</b>
%
% <p>On current implementation, encoding/decoding this value is implicitly 
% done by unpacking/packing functions, thus this field must not be included in
% the PDU definitions.  Never used.</p>
%
% %@see smpp_globals.hrl
% %@end
%%
% -define(COMMAND_ID_DATATYPE, ?INTEGER(4)).
% -define(COMMAND_ID_DOMAIN, 
%         ?UNION([?RANGE_INTEGER(4, 16#00000001, 16#00000009),
%                 ?RANGE_INTEGER(4, 16#0000000B, 16#0000000B),
%                 ?RANGE_INTEGER(4, 16#00000015, 16#00000015),
%                 ?RANGE_INTEGER(4, 16#00000021, 16#00000021),
%                 ?RANGE_INTEGER(4, 16#00000102, 16#00000103),
%                 ?RANGE_INTEGER(4, 16#00000111, 16#00000113),
%                 ?RANGE_INTEGER(4, 16#80000000, 16#80000009),
%                 ?RANGE_INTEGER(4, 16#80000015, 16#80000015),
%                 ?RANGE_INTEGER(4, 16#80000021, 16#80000021),
%                 ?RANGE_INTEGER(4, 16#80000103, 16#80000103),
%                 ?RANGE_INTEGER(4, 16#80000111, 16#80000113)])).
% -define(COMMAND_ID_RESERVED, 
%         ?UNION([?RANGE_INTEGER(4, 16#0000000A, 16#0000000A),
%                 ?RANGE_INTEGER(4, 16#0000000C, 16#00000014),
%                 ?RANGE_INTEGER(4, 16#00000016, 16#00000020),
%                 ?RANGE_INTEGER(4, 16#00000022, 16#00000101),
%                 ?RANGE_INTEGER(4, 16#00000104, 16#00000110),
%                 ?RANGE_INTEGER(4, 16#00000114, 16#7FFFFFFF),
%                 ?RANGE_INTEGER(4, 16#8000000A, 16#80000014),
%                 ?RANGE_INTEGER(4, 16#80000016, 16#80000020),
%                 ?RANGE_INTEGER(4, 16#80000022, 16#80000102),
%                 ?RANGE_INTEGER(4, 16#80000104, 16#80000110),
%                 ?RANGE_INTEGER(4, 16#80000114, 16#FFFFFFFF)])).

%%%
% command_status, error_status_code
%
% <p>On current implementation, encoding/decoding the command_status value is
% implicitly done by unpacking/packing functions, thus this field must not be
% included in the PDU definitions.  Never used.</p>
%
% %@see smpp_error definition above
% %@end
%%
% -define(COMMAND_STATUS_DATATYPE, ?SMPP_ERROR_DATATYPE).
% -define(COMMAND_STATUS_DOMAIN,   ?SMPP_ERROR_DOMAIN).
% -define(COMMAND_STATUS_RESERVED, ?SMPP_ERROR_RESERVED).

-define(ERROR_STATUS_CODE_DATATYPE, ?SMPP_ERROR_DATATYPE).
-define(ERROR_STATUS_CODE_DOMAIN,   ?SMPP_ERROR_DOMAIN).
-define(ERROR_STATUS_CODE_RESERVED, ?SMPP_ERROR_RESERVED).

%%%
% data_coding
%
% %@see encoding_scheme definition above
% %@end
%%
-define(DATA_CODING_DATATYPE, ?ENCODING_SCHEME_DATATYPE).
-define(DATA_CODING_DOMAIN,   ?ENCODING_SCHEME_DOMAIN).
-define(DATA_CODING_RESERVED, ?ENCODING_SCHEME_RESERVED).

%%%
% destination_addr
%
% %@doc Specifies the destination SME address.
%
% %@see addr definition above
% %@end
%%
-define(DESTINATION_ADDR_21_DATATYPE, ?ADDR_21_DATATYPE).
-define(DESTINATION_ADDR_21_DOMAIN,   ?ADDR_21_DOMAIN).
-define(DESTINATION_ADDR_21_RESERVED, ?ADDR_21_RESERVED).

-define(DESTINATION_ADDR_65_DATATYPE, ?ADDR_65_DATATYPE).
-define(DESTINATION_ADDR_65_DOMAIN,   ?ADDR_65_DOMAIN).
-define(DESTINATION_ADDR_65_RESERVED, ?ADDR_65_RESERVED).

%%%
% dest_flag
%
% %@doc This field is used to identify the type of destination address, values 
% are defined as constant within the type descriptors.
% %@end
%%
-define(DEST_FLAG_DATATYPE,   ?INTEGER(1)).
-define(DEST_FLAG_SME_DOMAIN, ?CONSTANT(16#01)).  % SME Address
-define(DEST_FLAG_DL_DOMAIN,  ?CONSTANT(16#02)).  % Distribution List Name
-define(DEST_FLAG_RESERVED,   ?EMPTY).

% dest_flag Values
-define(DEST_FLAG_SME, 16#01).  % SME Address
-define(DEST_FLAG_DL,  16#02).  % Distribution List Name

%%%
% dl_name
%%
-define(DL_NAME_DATATYPE, ?VAR_C_OCTET_STRING(21)).
-define(DL_NAME_DOMAIN,   ?VAR_C_OCTET_STRING(21)).
-define(DL_NAME_RESERVED, ?EMPTY).

%%%
% dest_address_multi
%
% %@doc This field is a composite field containing a mandatory field and then
% either an SME address or a Distribution List.  Additionally this field
% can be encoded multiple times.
%
% %@see dest_address_sme and dest_address_dl record definitions below
% %@see ton definition above
% %@see npi definition above
% %@see addr definition above
% %@end
%%
-define(DEST_ADDRESS_SME_DATATYPE,
        ?COMPOSITE(dest_address_sme,
                   {?DEST_FLAG_DATATYPE,
                    ?TON_DATATYPE,
                    ?NPI_DATATYPE,
                    ?ADDR_21_DATATYPE})).
-define(DEST_ADDRESS_SME_DOMAIN,
        ?COMPOSITE(dest_address_sme,
                   {?DEST_FLAG_SME_DOMAIN,
                    ?TON_DOMAIN,
                    ?NPI_DOMAIN,
                    ?ADDR_21_DOMAIN})).

-define(DEST_ADDRESS_DL_DATATYPE,
        ?COMPOSITE(dest_address_dl,
                   {?DEST_FLAG_DATATYPE,
                    ?DL_NAME_DATATYPE})).
-define(DEST_ADDRESS_DL_DOMAIN,
        ?COMPOSITE(dest_address_dl,
                   {?DEST_FLAG_DL_DOMAIN,
                    ?DL_NAME_DOMAIN})).

-define(DEST_ADDRESS_ITEM_DATATYPE, 
        ?UNION([?DEST_ADDRESS_SME_DATATYPE, 
                ?DEST_ADDRESS_DL_DATATYPE])).
-define(DEST_ADDRESS_ITEM_DOMAIN, 
        ?UNION([?DEST_ADDRESS_SME_DOMAIN, 
                ?DEST_ADDRESS_DL_DOMAIN])).

-define(DEST_ADDRESS_DATATYPE, ?LIST(?DEST_ADDRESS_ITEM_DATATYPE)).
-define(DEST_ADDRESS_DOMAIN,   ?LIST(?DEST_ADDRESS_ITEM_DOMAIN)).

%%%
% esme_addr
%
% %@doc Specifies the address of an ESME to which an alert_notification should 
% be routed.
%
% %@see addr definition above
% %@end
%%
-define(ESME_ADDR_DATATYPE, ?ADDR_65_DATATYPE).
-define(ESME_ADDR_DOMAIN,   ?ADDR_65_DOMAIN).
-define(ESME_ADDR_RESERVED, ?ADDR_65_RESERVED).

%%%
% esm_class
%%
-define(ESM_CLASS_DATATYPE, ?INTEGER(1)).
-define(ESM_CLASS_DOMAIN,
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00000111),
                ?RANGE_INTEGER(1, 2#00100000, 2#00100011),
                ?RANGE_INTEGER(1, 2#00001000, 2#00001011),
                ?RANGE_INTEGER(1, 2#00010000, 2#00010011),
                ?RANGE_INTEGER(1, 2#00011000, 2#00011011),
                ?RANGE_INTEGER(1, 2#01000000, 2#00000111),
                ?RANGE_INTEGER(1, 2#01100000, 2#00100011),
                ?RANGE_INTEGER(1, 2#01001000, 2#00001011),
                ?RANGE_INTEGER(1, 2#01010000, 2#00010011),
                ?RANGE_INTEGER(1, 2#01011000, 2#00011011),
                ?RANGE_INTEGER(1, 2#10000000, 2#00000111),
                ?RANGE_INTEGER(1, 2#10100000, 2#00100011),
                ?RANGE_INTEGER(1, 2#10001000, 2#00001011),
                ?RANGE_INTEGER(1, 2#10010000, 2#00010011),
                ?RANGE_INTEGER(1, 2#11000000, 2#00000111),
                ?RANGE_INTEGER(1, 2#11100000, 2#00100011),
                ?RANGE_INTEGER(1, 2#11001000, 2#00001011),
                ?RANGE_INTEGER(1, 2#11010000, 2#00010011)])).
% never happens?      ?RANGE_INTEGER(1, 2#10011000, 2#00011011), CDMA & GSM?
% never happens?      ?RANGE_INTEGER(1, 2#11011000, 2#00011011), CDMA & GSM?
-define(ESM_CLASS_RESERVED, ?EMPTY).

% esm_class Values
-define(ESM_CLASS_DEFAULT, ?NULL_INTEGER).

% esm_class Bits Values  
%
% <p>To compound the complete esm_class value from the Bits, pick a Bit Value
% from every type and do the bitwise "or" of the parts.</p>
%
% <p><center><tt>
% ?ESM_CLASS_MODE_DEFAULT bor ?ESM_CLASS_TYPE_DEFAULT bor ?ESM_CLASS_GSM_UDHI
% </tt></center></p>.
% %@end
%
% Messaging Mode (bits 1-0)
-define(ESM_CLASS_MODE_DEFAULT,        2#00000000).
-define(ESM_CLASS_MODE_DATAGRAM,       2#00000001).
-define(ESM_CLASS_MODE_FORWARD,        2#00000010).
-define(ESM_CLASS_MODE_STORE_FORWARD,  2#00000011).

% Message Type (bits 5-2)
-define(ESM_CLASS_TYPE_DEFAULT,                            2#00000000).
-define(ESM_CLASS_TYPE_MC_DELIVERY_RECEIPT,                2#00000100).
-define(ESM_CLASS_TYPE_INTERMEDIATE_DELIVERY_NOTIFICATION, 2#00100000).
                                               
% ASNI-41 Specific (bits 5-2)
-define(ESM_CLASS_ANSI41_DELIVERY_ACK,       2#00001000).
-define(ESM_CLASS_ANSI41_MANUAL_ACK,         2#00010000).
-define(ESM_CLASS_ANSI41_CONVERSATION_ABORT, 2#00011000).

% GSM Specific (bits 7-6)
-define(ESM_CLASS_GSM_NO_FEATURES,     2#00000000).
-define(ESM_CLASS_GSM_UDHI,            2#01000000).
-define(ESM_CLASS_GSM_REPLY_PATH,      2#10000000).
-define(ESM_CLASS_GSM_UDHI_REPLY_PATH, 2#11000000).

%%%
% interface_version
%
% %@see smpp_version definition above
%%
-define(INTERFACE_VERSION_DATATYPE, ?SMPP_VERSION_DATATYPE).
-define(INTERFACE_VERSION_DOMAIN,   ?SMPP_VERSION_DOMAIN).
-define(INTERFACE_VERSION_RESERVED, ?SMPP_VERSION_RESERVED).

%%%
% message_id
%
% %@see message_identifier definition above
%%
-define(MESSAGE_ID_DATATYPE, ?MESSAGE_IDENTIFIER_DATATYPE).
-define(MESSAGE_ID_DOMAIN,   ?MESSAGE_IDENTIFIER_DOMAIN).
-define(MESSAGE_ID_RESERVED, ?MESSAGE_IDENTIFIER_RESERVED).

%%%
% message_state_std
%
% %@see message_state definition above
%%
-define(MESSAGE_STATE_STD_DATATYPE, ?MESSAGE_STATE_DATATYPE).
-define(MESSAGE_STATE_STD_DOMAIN,   ?MESSAGE_STATE_DOMAIN).
-define(MESSAGE_STATE_STD_RESERVED, ?MESSAGE_STATE_RESERVED).

%%%
% password
%
% %@doc The password is normally issued by the MC system administrator.
% %@end
%%
-define(PASSWORD_DATATYPE, ?VAR_C_OCTET_STRING(9)).
-define(PASSWORD_DOMAIN,   ?VAR_C_OCTET_STRING(9)).
-define(PASSWORD_RESERVED, ?EMPTY).

%%%
% priority_flag
%%
-define(PRIORITY_FLAG_DATATYPE, ?INTEGER(1)).
-define(PRIORITY_FLAG_DOMAIN,   ?BOUND_INTEGER(1, 4)).
-define(PRIORITY_FLAG_RESERVED, ?RANGE_INTEGER(1, 5, 255)).

% priority_flag Values
-define(PRIORITY_FLAG_GSM_SMS_NON_PRIORITY,        0).
-define(PRIORITY_FLAG_GSM_SMS_PRIORITY,            1).  % 2-3 have same meaning
-define(PRIORITY_FLAG_GSM_CBS_NORMAL,              0).
-define(PRIORITY_FLAG_GSM_CBS_IMMEDIATE_BROADCAST, 1).
-define(PRIORITY_FLAG_GSM_CBS_HIGH_PRIORITY,       2).
-define(PRIORITY_FLAG_GSM_CBS_RESERVED,            3).
-define(PRIORITY_FLAG_GSM_CBS_PRIORITY_BACKGROUND, 4).
-define(PRIORITY_FLAG_TDMA_BULK,                   0).
-define(PRIORITY_FLAG_TDMA_NORMAL,                 1).
-define(PRIORITY_FLAG_TDMA_URGENT,                 2).
-define(PRIORITY_FLAG_TDMA_VERY_URGENT,            3).
-define(PRIORITY_FLAG_CDMA_NORMAL,                 0).
-define(PRIORITY_FLAG_CDMA_INTERACTIVE,            1).
-define(PRIORITY_FLAG_CDMA_URGENT,                 2).
-define(PRIORITY_FLAG_CDMA_EMERGENCY,              3).
-define(PRIORITY_FLAG_ANSI_41_CBS_NORMAL,          0).
-define(PRIORITY_FLAG_ANSI_41_CBS_INTERACTIVE,     1).
-define(PRIORITY_FLAG_ANSI_41_CBS_URGENT,          2).
-define(PRIORITY_FLAG_ANSI_41_CBS_EMERGENCY,       3).

%%%
% protocol_id
%
% %@see protocol_identifier definition above
%%
-define(PROTOCOL_ID_DATATYPE, ?PROTOCOL_IDENTIFIER_DATATYPE).
-define(PROTOCOL_ID_DOMAIN,   ?PROTOCOL_IDENTIFIER_DOMAIN).
-define(PROTOCOL_ID_RESERVED, ?PROTOCOL_IDENTIFIER_RESERVED).

%%%
% registered_delivery
%%
-define(REGISTERED_DELIVERY_DATATYPE, ?INTEGER(1)).
-define(REGISTERED_DELIVERY_DOMAIN,   ?BOUND_INTEGER(1, 2#00011111)).
-define(REGISTERED_DELIVERY_RESERVED, 
        ?RANGE_INTEGER(1, 2#00100000, 2#11111111)).

% registered_delivery Value
-define(REGISTERED_DELIVERY_DEFAULT, ?NULL_INTEGER).  % No receipts requested

% registered_delivery Bits Values  
%
% <p>To compound the complete esm_class value from the Bits, pick a Bit Value
% from every type and do the bitwise "or" of the parts.</p>
%
% <p><center><tt>
% ?REGISTERED_DELIVERY_MC_FAILURE bor ?REGISTERED_DELIVERY_SME_BOTH
% </tt></center></p>
%
% MC Delivery Receipt (bits 1-0)
-define(REGISTERED_DELIVERY_MC_NEVER,   2#00000000). % Never request a receipt
-define(REGISTERED_DELIVERY_MC_ALWAYS,  2#00000001). % Always, whatever outcome
-define(REGISTERED_DELIVERY_MC_FAILURE, 2#00000010). % Failure delivery outcome
-define(REGISTERED_DELIVERY_MC_SUCCESS, 2#00000011). % Success delivery outcome

% SME Originated Acknowledgement (bits 3-2)
-define(REGISTERED_DELIVERY_SME_NEVER,    2#00000000). % No Ack requested
-define(REGISTERED_DELIVERY_SME_DELIVERY, 2#00000100). % Delivery Ack requested
-define(REGISTERED_DELIVERY_SME_MANUAL,   2#00001000). % Manual/User Ack
-define(REGISTERED_DELIVERY_SME_BOTH,     2#00001100). % Delivery & Manual/User

% Intermediate Notification (bin 4)
-define(REGISTERED_DELIVERY_INTERMEDIATE_NO,  2#00000000). % Not requested
-define(REGISTERED_DELIVERY_INTERMEDIATE_YES, 2#00010000). % Notific. Requested

%%%
% replace_if_present_flag
%%
-define(REPLACE_IF_PRESENT_FLAG_DATATYPE, ?INTEGER(1)).
-define(REPLACE_IF_PRESENT_FLAG_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(REPLACE_IF_PRESENT_FLAG_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

% replace_if_present_flag Values
-define(REPLACE_IF_PRESENT_FLAG_DO_NOT_REPLACE, 0).
-define(REPLACE_IF_PRESENT_FLAG_REPLACE,        1).

%%%
% schedule_delivery_time
%
% %@doc Either absolute or relative.
%
% %@see time definition above
% %@end
%%
-define(SCHEDULE_DELIVERY_TIME_DATATYPE, ?TIME_DATATYPE).
-define(SCHEDULE_DELIVERY_TIME_DOMAIN,   ?TIME_DOMAIN).
-define(SCHEDULE_DELIVERY_TIME_RESERVED, ?TIME_RESERVED).

% schedule_delivery_time Values
-define(SCHEDULE_DELIVERY_TIME_IMMEDIATE, ?NULL_C_OCTET_STRING).  % Immediate

%%%
% validity_period
%
% %@doc Either absolute or relative.
%
% %@see time definition above
% %@end
%%
-define(VALIDITY_PERIOD_DATATYPE, ?TIME_DATATYPE).
-define(VALIDITY_PERIOD_DOMAIN,   ?TIME_DOMAIN).
-define(VALIDITY_PERIOD_RESERVED, ?TIME_RESERVED).

% validity_period Values
-define(VALIDITY_PERIOD_DEFAULT, ?NULL_C_OCTET_STRING).  % Use MC defaults

%%%
% final_date
%
% %@doc It must be specified in absolute time format.
%
% %@see time_absolute definition above
% %@end
%%
-define(FINAL_DATE_DATATYPE, ?TIME_ABSOLUTE_DATATYPE).
-define(FINAL_DATE_DOMAIN,   ?TIME_ABSOLUTE_DOMAIN).
-define(FINAL_DATE_RESERVED, ?TIME_ABSOLUTE_RESERVED).

% final_date Values
-define(FINAL_DATE_FINAL_STATE_NOT_REACHED, ?NULL_C_OCTET_STRING).

%%%
% sequence_number
%
% %@doc Allows a response PDU to be correlated with a request PDU.  The allowed
% sequence_number range is from 16#00000001 to 16#7FFFFFFFFF.  16#00000000 is
% recomended for use when issuing a generic_nack where the original PDU was
% deemed completely invalid and its PDU header, was not used to derive a
% sequence_number for the response PDU.
%
% <p>On current implementation, encoding/decoding this value is implicitly 
% done by unpacking/packing functions, thus this field must not be included in
% the PDU definitions.  Never used.</p>
% %@end
%%
% -define(SEQUENCE_NUMBER_DATATYPE, ?INTEGER(4)).
% -define(SEQUENCE_NUMBER_DOMAIN, ?RANGE_INTEGER(4, 16#00000000, 16#7FFFFFFFFF)).
% -define(SEQUENCE_NUMBER_RESERVED, ?EMPTY).

%%%
% service_type
%%
-define(SERVICE_TYPE_DATATYPE, ?VAR_C_OCTET_STRING(6)).
-define(SERVICE_TYPE_DOMAIN,   ?VAR_C_OCTET_STRING(6)).
-define(SERVICE_TYPE_RESERVED, ?EMPTY).

% service_type Values
%
% <p>The following generic service_types are defined, all other values are
% carrier specific and defined by mutual agreement between the MC Service
% provider and the ESME application.</p>
-define(SERVICE_TYPE_NULL, ?NULL_C_OCTET_STRING).  % Default
-define(SERVICE_TYPE_CMT,  "CMT\0").   % Cellular Messaging
-define(SERVICE_TYPE_CPT,  "CPT\0").   % Cellular Paging
-define(SERVICE_TYPE_VMN,  "VMN\0").   % Voice Mail Notification
-define(SERVICE_TYPE_VMA,  "VMA\0").   % Voice Mail Alerting
-define(SERVICE_TYPE_WAP,  "WAP\0").   % Wireless Application Protocol
-define(SERVICE_TYPE_USSD, "USSD\0").  % Unstructured Supplementary 
                                       % Services Data
-define(SERVICE_TYPE_CBS,  "CBS\0").   % Cell Broadcast Service
-define(SERVICE_TYPE_GUTS, "GUTS\0").  % Generic UDP Transport Service

%%%
% short_message
%%
-define(SHORT_MESSAGE_DATATYPE, ?LIST(?INTEGER(1))).
-define(SHORT_MESSAGE_DOMAIN,   ?LIST(?INTEGER(1))).
-define(SHORT_MESSAGE_RESERVED, ?EMPTY).

%%%
% sm_default_msg_id
%%
-define(SM_DEFAULT_MSG_ID_DATATYPE, ?INTEGER(1)).
-define(SM_DEFAULT_MSG_ID_DOMAIN,   ?INTEGER(1)).
-define(SM_DEFAULT_MSG_ID_RESERVED, ?EMPTY).

%%%
% sm_length
%
% %@doc This field is implicitly encoded within short_message.
% %@end
%%
-define(SM_LENGTH_DATATYPE, ?INTEGER(1)).
-define(SM_LENGTH_DOMAIN,   ?INTEGER(1)).
-define(SM_LENGTH_RESERVED, ?EMPTY).

% sm_length Values
-define(SM_LENGTH_NO_DATA, ?NULL_INTEGER).

%%%
% source_addr
%
% %@doc Specifies the address of the SME which originated this message.
%
% %@see addr definition above
% %@end
%%
-define(SOURCE_ADDR_21_DATATYPE, ?ADDR_21_DATATYPE).
-define(SOURCE_ADDR_21_DOMAIN,   ?ADDR_21_DOMAIN).
-define(SOURCE_ADDR_21_RESERVED, ?ADDR_21_RESERVED).

-define(SOURCE_ADDR_65_DATATYPE, ?ADDR_65_DATATYPE).
-define(SOURCE_ADDR_65_DOMAIN,   ?ADDR_65_DOMAIN).
-define(SOURCE_ADDR_65_RESERVED, ?ADDR_65_RESERVED).

%%%
% system_id
%
% %@doc Identifies an ESME or a MC at bind time.
% %@end
%%
-define(SYSTEM_ID_DATATYPE, ?VAR_C_OCTET_STRING(16)).
-define(SYSTEM_ID_DOMAIN,   ?VAR_C_OCTET_STRING(16)).
-define(SYSTEM_ID_RESERVED, ?EMPTY).

%%%
% system_type
%
% %@doc Used to categorize the type of ESME that is binding to the MC.
%
% <p>Some MCs may not require this parameter, in this case a NULL can be
% used.</p>
% %@end
%%
-define(SYSTEM_TYPE_DATATYPE, ?VAR_C_OCTET_STRING(13)).
-define(SYSTEM_TYPE_DOMAIN,   ?VAR_C_OCTET_STRING(13)).
-define(SYSTEM_TYPE_RESERVED, ?EMPTY).

% system_type Values
-define(SYSTEM_TYPE_UNKNOWN, ?NULL_C_OCTET_STRING).
-define(SYSTEM_TYPE_VMS,     "VMS\0").  % Voice Mail System
-define(SYSTEM_TYPE_OTA,     "OTA\0").  % Over-The-Air Activation System

%%%
% error_code
%
% %@doc The range of values returned depends on the underlying 
% telecommunications network.
% %@end
%%
-define(ERROR_CODE_DATATYPE, ?INTEGER(1)).
-define(ERROR_CODE_DOMAIN,   ?INTEGER(1)).

%%%
% unsuccess_sme
%
% %@doc This field is a composite field containing an SME address and an error
% code.  Additionally this field can be encoded multiple times.
%
% %@see unsuccess_sme record definition below
% %@see ton definition above
% %@see npi definition above
% %@see addr_21 definition above
% %@see smpp_error definition above
% %@end
%%
-define(UNSUCCESS_SME_ITEM_DATATYPE,
        ?COMPOSITE(unsuccess_sme,
                   {?TON_DATATYPE,
                    ?NPI_DATATYPE,
                    ?ADDR_21_DATATYPE,
                    ?SMPP_ERROR_DATATYPE})).
-define(UNSUCCESS_SME_ITEM_DOMAIN,
        ?COMPOSITE(unsuccess_sme,
                   {?TON_DOMAIN,
                    ?NPI_DOMAIN,
                    ?ADDR_21_DOMAIN,
                    ?SMPP_ERROR_DOMAIN})).

-define(UNSUCCESS_SME_DATATYPE, ?LIST(?UNSUCCESS_SME_ITEM_DATATYPE)).
-define(UNSUCCESS_SME_DOMAIN,   ?LIST(?UNSUCCESS_SME_ITEM_DOMAIN)).


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% PDU TLV Base Syntax Definitions
%
% %@see section 4.8 on [SMPP 5.0]
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% additional_status_info_text
%
% %@doc Free format text. 
% %@end
%%
-define(ADDITIONAL_STATUS_INFO_TEXT_DATATYPE, ?VAR_C_OCTET_STRING(256)).
-define(ADDITIONAL_STATUS_INFO_TEXT_DOMAIN,   ?VAR_C_OCTET_STRING(256)).
-define(ADDITIONAL_STATUS_INFO_TEXT_RESERVED, ?EMPTY).

%%%
% alert_on_message_delivery
%%
-define(ALERT_ON_MESSAGE_DELIVERY_DATATYPE, ?INTEGER(1)).
-define(ALERT_ON_MESSAGE_DELIVERY_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(ALERT_ON_MESSAGE_DELIVERY_RESERVED, ?RANGE_INTEGER(1, 4, 255)).

% alert_on_message_delivery Values
-define(ALERT_ON_MESSAGE_DELIVERY_DEFAULT, 0).  % Use mobile default alert
-define(ALERT_ON_MESSAGE_DELIVERY_LOW,     1).  % Use low-priority alert
-define(ALERT_ON_MESSAGE_DELIVERY_MEDIUM,  2).  % Use medium-priority alert
-define(ALERT_ON_MESSAGE_DELIVERY_HIGH,    3).  % Use high-priority alert

%%%
% billing_identification
%
% %@doc The first octet represents the Billing Format tag and indicates the 
% format of the billing information in the remaining octets. 
% %@end
%%
-define(BILLING_IDENTIFICATION_DATATYPE, ?VAR_OCTET_STRING(1024)).
-define(BILLING_IDENTIFICATION_DOMAIN,   ?VAR_OCTET_STRING(1024)).
-define(BILLING_IDENTIFICATION_RESERVED, ?EMPTY).

%%%
% broadcast_area_identifier, failed_broadcast_area_identifier
%
% %@see broadcast_area definition above.
%%
-define(BROADCAST_AREA_IDENTIFIER_DATATYPE,        ?BROADCAST_AREA_DATATYPE).
-define(BROADCAST_AREA_IDENTIFIER_DOMAIN,          ?BROADCAST_AREA_DOMAIN).
-define(BROADCAST_AREA_IDENTIFIER_RESERVED,        ?BROADCAST_AREA_RESERVED).

-define(FAILED_BROADCAST_AREA_IDENTIFIER_DATATYPE, ?BROADCAST_AREA_DATATYPE).
-define(FAILED_BROADCAST_AREA_IDENTIFIER_DOMAIN,   ?BROADCAST_AREA_DOMAIN).
-define(FAILED_BROADCAST_AREA_IDENTIFIER_RESERVED, ?BROADCAST_AREA_RESERVED).

%%%
% broadcast_area_success
%
% %@doc 
%
% <ul>
%   <li>0-100 = allowed range.</li>
%   <li>255 = Information not available</li>
% </ul>
% %@end
%%
-define(BROADCAST_AREA_SUCCESS_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_AREA_SUCCESS_DOMAIN, 
        ?UNION([?BOUND_INTEGER(1, 100), ?CONSTANT(255)])).
-define(BROADCAST_AREA_SUCCESS_RESERVED, ?RANGE_INTEGER(1, 101, 254)).

% broadcast_area_success Values
-define(BROADCAST_AREA_SUCCESS_INFORMATION_NOT_AVAILABLE, 255).

%%%
% broadcast_content_type_info
%
% %@doc The value is a free format Octet String
% %@end
%%
-define(BROADCAST_CONTENT_TYPE_INFO_DATATYPE, ?VAR_OCTET_STRING(255)).
-define(BROADCAST_CONTENT_TYPE_INFO_DOMAIN,   ?VAR_OCTET_STRING(255)).
-define(BROADCAST_CONTENT_TYPE_INFO_RESERVED, ?EMPTY).

%%%
% broadcast_channel_indicator
%%
-define(BROADCAST_CHANNEL_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_CHANNEL_INDICATOR_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(BROADCAST_CHANNEL_INDICATOR_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

% broadcast_channel_indicator Values
-define(BROADCAST_CHANNEL_INDICATOR_BASIC,    0). % Basic Channel (Default)
-define(BROADCAST_CHANNEL_INDICATOR_EXTENDED, 1). % Extended Channel

%%%
% broadcast_content_type
%
% %@doc A broadcast_content_type value should be defined using the 
% broadcast_content_type record.
%
% %@see broadcast_content_type record definition below.
% %@end
%%
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_RESERVED, ?RANGE_INTEGER(1,4,255)).

-define(BROADCAST_CONTENT_TYPE_SERVICE_DATATYPE,      ?INTEGER(2)).
-define(BROADCAST_CONTENT_TYPE_SERVICE_DOMAIN,      
        ?UNION([?RANGE_INTEGER(2, 16#0000, 16#0002),
                ?RANGE_INTEGER(2, 16#0010, 16#0023),
                ?RANGE_INTEGER(2, 16#0030, 16#0041),
                ?RANGE_INTEGER(2, 16#0070, 16#0071),
                ?RANGE_INTEGER(2, 16#0080, 16#0085),
                ?RANGE_INTEGER(2, 16#0100, 16#0100)])).
-define(BROADCAST_CONTENT_TYPE_SERVICE_RESERVED,      
        ?UNION([?RANGE_INTEGER(2, 16#0003, 16#0009),
                ?RANGE_INTEGER(2, 16#0024, 16#002F),
                ?RANGE_INTEGER(2, 16#0042, 16#006F),
                ?RANGE_INTEGER(2, 16#0072, 16#007F),
                ?RANGE_INTEGER(2, 16#0086, 16#009F),
                ?RANGE_INTEGER(2, 16#0101, 16#FFFF)])).

-define(BROADCAST_CONTENT_TYPE_DATATYPE, 
        ?COMPOSITE(broadcast_content_type,
                   {?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DATATYPE,
                    ?BROADCAST_CONTENT_TYPE_SERVICE_DATATYPE})). 
-define(BROADCAST_CONTENT_TYPE_DOMAIN, 
        ?COMPOSITE(broadcast_content_type,
                   {?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_DOMAIN,
                    ?BROADCAST_CONTENT_TYPE_SERVICE_DOMAIN})). 
-define(BROADCAST_CONTENT_TYPE_RESERVED, 
        ?COMPOSITE(broadcast_content_type,
                   {?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_RESERVED,
                    ?BROADCAST_CONTENT_TYPE_SERVICE_RESERVED})). 

% broadcast_content_type Values
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GENERIC, 0).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GSM,     1).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_TDMA,    2).
-define(BROADCAST_CONTENT_TYPE_NETWORK_TYPE_CDMA,    3).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INDEX,                        16#0000).
-define(BROADCAST_CONTENT_TYPE_SERVICE_EMERGENCY_BROADCASTS,         16#0001).
-define(BROADCAST_CONTENT_TYPE_SERVICE_IRDB_DOWNLOAD,                16#0002).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NEWS_FLASHES,                 16#0010).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_GENERAL_NEWS,           16#0011).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_GENERAL_NEWS,        16#0012).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_GENERAL_NEWS,        16#0013).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_GENERAL_NEWS,   16#0014).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_FINANCIAL_NEWS,         16#0015).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_FINANCIAL_NEWS,      16#0016).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_FINANCIAL_NEWS,      16#0017).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_FINANCIAL_NEWS, 16#0018).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_SPORTS_NEWS,            16#0019).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_SPORTS_NEWS,         16#001A).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_SPORTS_NEWS,         16#001B).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_SPORTS_NEWS,    16#001C).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_ENTERTAINMENT_NEWS,     16#001D).
-define(BROADCAST_CONTENT_TYPE_SERVICE_REGIONAL_ENTERTAINMENT_NEWS,  16#001E).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_ENTERTAINMENT_NEWS,  16#001F).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_ENTERTAINMENT_NEWS,
        16#0020).
-define(BROADCAST_CONTENT_TYPE_SERVICE_MEDICAL,                      16#0021).
-define(BROADCAST_CONTENT_TYPE_SERVICE_DOCTORS,                      16#0022).
-define(BROADCAST_CONTENT_TYPE_SERVICE_PHARMACY,                     16#0023).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_TRAFFIC,                16#0030).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LONG_DISTANCE_TRAFFIC,        16#0031).
-define(BROADCAST_CONTENT_TYPE_SERVICE_TAXIS,                        16#0032).
-define(BROADCAST_CONTENT_TYPE_SERVICE_WEATHER,                      16#0033).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_AIRPORT_FLIGHT_SCHEDULES, 
        16#0034).
-define(BROADCAST_CONTENT_TYPE_SERVICE_RESTAURANTS,                  16#0035).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LODGINGS,                     16#0036).
-define(BROADCAST_CONTENT_TYPE_SERVICE_RETAIL_DIRECTORY,             16#0037).
-define(BROADCAST_CONTENT_TYPE_SERVICE_ADVERTISEMENTS,               16#0038).
-define(BROADCAST_CONTENT_TYPE_SERVICE_STOCK_QUOTES,                 16#0039).
-define(BROADCAST_CONTENT_TYPE_SERVICE_EMPLOYMENT_OPPORTUNITIES,     16#0040).
-define(BROADCAST_CONTENT_TYPE_SERVICE_TECHNOLOGY_NEWS,              16#0041).
-define(BROADCAST_CONTENT_TYPE_SERVICE_BASE_STATION_INFORMATION,     16#0070).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NETWORK_INFORMATION,          16#0071).
-define(BROADCAST_CONTENT_TYPE_SERVICE_OPERATOR_SERVICES,            16#0080).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_DIRECTORY_ENQUIRIES, 16#0081).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_DIRECTORY_ENQUIRIES, 
        16#0082).
-define(BROADCAST_CONTENT_TYPE_SERVICE_NATIONAL_CUSTOMER_CARE,       16#0083).
-define(BROADCAST_CONTENT_TYPE_SERVICE_INTERNATIONAL_CUSTOMER_CARE,  16#0084).
-define(BROADCAST_CONTENT_TYPE_SERVICE_LOCAL_TIME_ZONE,              16#0085).
-define(BROADCAST_CONTENT_TYPE_SERVICE_MULTI_CATEGORY_SERVICES,      16#0100).

%%%
% broadcast_end_time
%
% %@doc It must be specified in absolute time format "YYMMDDhhmmsstnnp"
%
% %@see time_absolute definition above
% %@end
%%
-define(BROADCAST_END_TIME_DATATYPE, ?TIME_ABSOLUTE_DATATYPE).
-define(BROADCAST_END_TIME_DOMAIN,   ?TIME_ABSOLUTE_DOMAIN).
-define(BROADCAST_END_TIME_RESERVED, ?TIME_ABSOLUTE_RESERVED).

%%%
% broadcast_error_status
% 
% %@doc The value is one of the SMPP Error Code Values as defined for
% error_status_code
%
% %@see error_status_code definition above
% %@end
%%
-define(BROADCAST_ERROR_STATUS_DATATYPE, ?ERROR_STATUS_CODE_DATATYPE).
-define(BROADCAST_ERROR_STATUS_DOMAIN,   ?ERROR_STATUS_CODE_DOMAIN).
-define(BROADCAST_ERROR_STATUS_RESERVED, ?ERROR_STATUS_CODE_RESERVED).

%%%
% broadcast_frequency_interval
%%
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DOMAIN, 
        ?UNION([?CONSTANT(16#00), ?RANGE_INTEGER(1, 16#08, 16#0E)])).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_RESERVED, 
        ?UNION([?RANGE_INTEGER(1, 16#01, 16#07),
                ?RANGE_INTEGER(1, 16#0F, 16#FF)])).

-define(BROADCAST_FREQUENCY_INTERVAL_NUMBER_DATATYPE, ?INTEGER(2)).
-define(BROADCAST_FREQUENCY_INTERVAL_NUMBER_DOMAIN,   ?INTEGER(2)).
-define(BROADCAST_FREQUENCY_INTERVAL_NUMBER_RESERVED, ?INTEGER(2)).

-define(BROADCAST_FREQUENCY_INTERVAL_DATATYPE,
        ?COMPOSITE(broadcast_frequency_interval,
                   {?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DATATYPE,
                    ?BROADCAST_FREQUENCY_INTERVAL_NUMBER_DATATYPE})).
-define(BROADCAST_FREQUENCY_INTERVAL_DOMAIN,
        ?COMPOSITE(broadcast_frequency_interval,
                   {?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DOMAIN,
                    ?BROADCAST_FREQUENCY_INTERVAL_NUMBER_DOMAIN})).
-define(BROADCAST_FREQUENCY_INTERVAL_RESERVED,
        ?COMPOSITE(broadcast_frequency_interval,
                   {?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_RESERVED,
                    ?BROADCAST_FREQUENCY_INTERVAL_NUMBER_RESERVED})).

% broadcast_frequency_interval Values
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_AS_FREQUENTLY_AS_POSSIBLE, 0).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_SECONDS, 16#08).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MINUTES, 16#09).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_HOURS,   16#0A).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_DAYS,    16#0B).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_WEEKS,   16#0C).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MONTHS,  16#0D).
-define(BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_YEARS,   16#0E).

%%%
% broadcast_message_class
%%
-define(BROADCAST_MESSAGE_CLASS_DATATYPE, ?INTEGER(1)).
-define(BROADCAST_MESSAGE_CLASS_DOMAIN,   ?BOUND_INTEGER(1, 16#03)).
-define(BROADCAST_MESSAGE_CLASS_RESERVED, ?RANGE_INTEGER(1, 16#04, 16#FF)).

% broadcast_message_class Values
-define(BROADCAST_MESSAGE_CLASS_NO_CLASS, 16#00).% No Class Specified (Default)
-define(BROADCAST_MESSAGE_CLASS_CLASS_1,  16#01).% Class 1 (User Defined)
-define(BROADCAST_MESSAGE_CLASS_CLASS_2,  16#02).% Class 2 (User Defined)
-define(BROADCAST_MESSAGE_CLASS_CLASS_3,  16#03).% Class 3 (Terminal Equipment)

%%%
% broadcast_rep_num
%
% %@doc The value 0 has the following significance:
%
% <ul>
%   <li>If no validity_period has been specified for a broadcast, then the
%     broadcasts should be repeated indefinitely.
%   </li>
%   <li>If a validity_period and a broadcast_frequency_interval have been
%     specified, then 0 in this field indicates that the broadcast_rep_num is
%     implicit according to the settings of these parameters.
%   </li>
% </ul>
%
% <p>Where a broadcast priority (i.e. priority_flag setting) of 1 (Immediate
% Broadcast) has been requested, then the broadcast_rep_num parameter should
% not be supplied and be ignored if supplied.</p>
% %@end
%%
-define(BROADCAST_REP_NUM_DATATYPE, ?INTEGER(2)).
-define(BROADCAST_REP_NUM_DOMAIN,   ?INTEGER(2)).
-define(BROADCAST_REP_NUM_RESERVED, ?EMPTY).

% broadcast_rep_num Values
-define(BROADCAST_REP_NUM_DEFAULT, 1).  % Implementation specific

%%%
% broadcast_service_group
%
% %@doc The value is a free format Octet String.
% %@end
%%
-define(BROADCAST_SERVICE_GROUP_DATATYPE, ?VAR_OCTET_STRING(255)).
-define(BROADCAST_SERVICE_GROUP_DOMAIN,   ?VAR_OCTET_STRING(255)).
-define(BROADCAST_SERVICE_GROUP_RESERVED, ?EMPTY).

%%%
% callback_num
%
% %@doc A callback_num value should be defined using the callback_num record.
%
% %@see ton definition above
% %@see npi definition above
% %@see callback_num record definition below
% %@end
%%
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_DOMAIN,   ?BOUND_INTEGER(1, 1)).

-define(CALLBACK_NUM_NUMBER_DIGITS_DATATYPE, ?VAR_OCTET_STRING(16)).
-define(CALLBACK_NUM_NUMBER_DIGITS_DOMAIN,   ?VAR_OCTET_STRING(16)).

-define(CALLBACK_NUM_DATATYPE,
        ?COMPOSITE(callback_num,
                   {?CALLBACK_NUM_DIGIT_MODE_INDICATOR_DATATYPE,
                    ?TON_DATATYPE,
                    ?NPI_DATATYPE,
                    ?CALLBACK_NUM_NUMBER_DIGITS_DATATYPE})).
-define(CALLBACK_NUM_DOMAIN,
        ?COMPOSITE(callback_num,
                   {?CALLBACK_NUM_DIGIT_MODE_INDICATOR_DOMAIN,
                    ?TON_DOMAIN,
                    ?NPI_DOMAIN,
                    ?CALLBACK_NUM_NUMBER_DIGITS_DOMAIN})).
-define(CALLBACK_NUM_RESERVED, ?EMPTY).

% callback_num Values
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_TBCD,  0).
-define(CALLBACK_NUM_DIGIT_MODE_INDICATOR_ASCII, 1).

%%%
% callback_num_atag
%
% %@doc A callback_num_atag value should be defined using the callback_num 
% record.
%
% %@see callback_num_atag record definition below
% %@see encoding_scheme definition above
% %@end
%%
-define(CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DATATYPE, ?VAR_OCTET_STRING(64)).
-define(CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DOMAIN,   ?VAR_OCTET_STRING(64)).

-define(CALLBACK_NUM_ATAG_DATATYPE, 
        ?COMPOSITE(callback_num_atag,
                   {?ENCODING_SCHEME_DATATYPE,
                     ?CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DATATYPE})). 
-define(CALLBACK_NUM_ATAG_DOMAIN, 
        ?COMPOSITE(callback_num_atag,
                   {?ENCODING_SCHEME_DOMAIN,
                     ?CALLBACK_NUM_ATAG_DISPLAY_CHARACTERS_DOMAIN})). 
-define(CALLBACK_NUM_ATAG_RESERVED, ?EMPTY).

%%%
% callback_num_pres_ind
%%
-define(CALLBACK_NUM_PRES_IND_DATATYPE, ?INTEGER(1)).
-define(CALLBACK_NUM_PRES_IND_DOMAIN,   
        ?RANGE_INTEGER(1, 2#00000000, 2#00001011)).
-define(CALLBACK_NUM_PRES_IND_RESERVED,   
        ?RANGE_INTEGER(1, 2#00001100, 2#00001111)).

% callback_num_pres_ind Values
-define(CALLBACK_NUM_PRES_IND_ALLOWED_NOT_SCREENED,       2#00000000).
-define(CALLBACK_NUM_PRES_IND_ALLOWED_PASSED,             2#00000001).
-define(CALLBACK_NUM_PRES_IND_ALLOWED_FAILED,             2#00000010).
-define(CALLBACK_NUM_PRES_IND_ALLOWED_NETWORK,            2#00000011).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_NOT_SCREENED,    2#00000100).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_PASSED,          2#00000101).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_FAILED,          2#00000110).
-define(CALLBACK_NUM_PRES_IND_RESTRICTED_NETWORK,         2#00000111).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_NOT_SCREENED, 2#00001000).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_PASSED,       2#00001001).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_FAILED,       2#00001010).
-define(CALLBACK_NUM_PRES_IND_NOT_AVAILABLE_NETWORK,      2#00001011).

%%%
% congestion_state
%%
-define(CONGESTION_STATE_DATATYPE, ?INTEGER(1)).
-define(CONGESTION_STATE_DOMAIN,   ?BOUND_INTEGER(1, 99)).
-define(CONGESTION_STATE_RESERVED, ?RANGE_INTEGER(1, 100, 255)).

% congestion_state Values
-define(CONGESTION_STATE_IDLE,                0).  % 
-define(CONGESTION_STATE_LOW_LOAD,           15).  %  1-29
-define(CONGESTION_STATE_MEDIUM_LOAD,        40).  % 30-49
-define(CONGESTION_STATE_HIGH_LOAD,          65).  % 50-79
-define(CONGESTION_STATE_OPTIMUM_LOAD,       85).  % 80-89
-define(CONGESTION_STATE_NEARING_CONGESTION, 95).  % 90-99

%%%
% delivery_failure_reason
%%
-define(DELIVERY_FAILURE_REASON_DATATYPE, ?INTEGER(1)).
-define(DELIVERY_FAILURE_REASON_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(DELIVERY_FAILURE_REASON_RESERVED, ?RANGE_INTEGER(1, 4, 255)).

% delivery_failure_reason Values
-define(DELIVERY_FAILURE_REASON_UNAVAILABLE,     0).  % Destination unavailable
-define(DELIVERY_FAILURE_REASON_INVALID,         1).  % Destination Address
                                                      % invalid (e.g. suspended
                                                      % no SMS capability, etc)
-define(DELIVERY_FAILURE_REASON_PERMANENT_ERROR, 2).  % Permanent network error
-define(DELIVERY_FAILURE_REASON_TEMPORARY_ERROR, 3).  % Temporary network error

%%%
% dest_addr_np_country
%
% %@doc A list with all countries and global destinations with a country codes
% can be found on e164.hrl
%
% %@see e164.hrl
% %@end
%%
-define(DEST_ADDR_NP_COUNTRY_DATATYPE, ?INTEGER(5)).
-define(DEST_ADDR_NP_COUNTRY_DOMAIN,   ?INTEGER(5)).
-define(DEST_ADDR_NP_COUNTRY_RESERVED, ?EMPTY).

% dest_addr_np_country Values

%%%
% dest_addr_np_information
%
% %@TODO Review the domain declaration for this parameter.
%%
-define(DEST_ADDR_NP_INFORMATION_DATATYPE, ?FIXED_OCTET_STRING(10)).
-define(DEST_ADDR_NP_INFORMATION_DOMAIN,   ?FIXED_OCTET_STRING(10)).
-define(DEST_ADDR_NP_INFORMATION_RESERVED, ?EMPTY).

%%%
% dest_addr_np_resolution
%
% %@TODO Confirm that there are no reserved values.
%%
-define(DEST_ADDR_NP_RESOLUTION_DATATYPE, ?INTEGER(1)).
-define(DEST_ADDR_NP_RESOLUTION_DOMAIN,   ?BOUND_INTEGER(1, 2)).
-define(DEST_ADDR_NP_RESOLUTION_RESERVED, ?EMPTY).

% dest_addr_np_resolution Values
-define(DEST_ADDR_NP_RESOLUTION_NO_QUERY_PERFORMED, 0). % Default
-define(DEST_ADDR_NP_RESOLUTION_NUMBER_NOT_PORTED,  1). % Query performed
-define(DEST_ADDR_NP_RESOLUTION_NUMBER_PORTED,      2). % Query performed

%%%
% dest_addr_subunit
%
% %@see addr_subunit definition above
%%
-define(DEST_ADDR_SUBUNIT_DATATYPE, ?ADDR_SUBUNIT_DATATYPE).
-define(DEST_ADDR_SUBUNIT_DOMAIN,   ?ADDR_SUBUNIT_DOMAIN).
-define(DEST_ADDR_SUBUNIT_RESERVED, ?ADDR_SUBUNIT_RESERVED).

%%%
% dest_bearer_type
%
% %@see bearer_type definition above
%%
-define(DEST_BEARER_TYPE_DATATYPE, ?BEARER_TYPE_DATATYPE).
-define(DEST_BEARER_TYPE_DOMAIN,   ?BEARER_TYPE_DOMAIN).
-define(DEST_BEARER_TYPE_RESERVED, ?BEARER_TYPE_RESERVED).

%%%
% dest_network_id
%
% %@doc When this TLV is specified, it must be accompanied with a dest_node_id
% TLV.
%
% %@see network_id definition above
% %@end
%%
-define(DEST_NETWORK_ID_DATATYPE, ?NETWORK_ID_DATATYPE).
-define(DEST_NETWORK_ID_DOMAIN,   ?NETWORK_ID_DOMAIN).
-define(DEST_NETWORK_ID_RESERVED, ?NETWORK_ID_RESERVED).

%%%
% dest_network_type
%
% %@see network_type definition above
%%
-define(DEST_NETWORK_TYPE_DATATYPE, ?NETWORK_TYPE_DATATYPE).
-define(DEST_NETWORK_TYPE_DOMAIN,   ?NETWORK_TYPE_DOMAIN).
-define(DEST_NETWORK_TYPE_RESERVED, ?NETWORK_TYPE_RESERVED).

%%%
% dest_node_id
%
% %@see node_id definition above
%%
-define(DEST_NODE_ID_DATATYPE, ?NODE_ID_DATATYPE).
-define(DEST_NODE_ID_DOMAIN,   ?NODE_ID_DOMAIN).
-define(DEST_NODE_ID_RESERVED, ?NODE_ID_RESERVED).

%%%
% dest_subaddress
%
% %@see subaddress definition above
%%
-define(DEST_SUBADDRESS_DATATYPE, ?SUBADDRESS_DATATYPE).
-define(DEST_SUBADDRESS_DOMAIN,   ?SUBADDRESS_DOMAIN).
-define(DEST_SUBADDRESS_RESERVED, ?SUBADDRESS_RESERVED).

%%%
% dest_telematics_id
%
% %@see telematics_id definition above
%%
-define(DEST_TELEMATICS_ID_DATATYPE, ?TELEMATICS_ID_DATATYPE).
-define(DEST_TELEMATICS_ID_DOMAIN,   ?TELEMATICS_ID_DOMAIN).
-define(DEST_TELEMATICS_ID_RESERVED, ?TELEMATICS_ID_RESERVED).

%%%
% dest_port
%
% %@see port definition above
%%
-define(DEST_PORT_DATATYPE, ?PORT_DATATYPE).
-define(DEST_PORT_DOMAIN,   ?PORT_DOMAIN).
-define(DEST_PORT_RESERVED, ?PORT_RESERVED).

%%%
% display_time
%%
-define(DISPLAY_TIME_DATATYPE, ?INTEGER(1)).
-define(DISPLAY_TIME_DOMAIN,   ?BOUND_INTEGER(1, 2)).
-define(DISPLAY_TIME_RESERVED, ?RANGE_INTEGER(1, 3, 255)).

% display_time Values
-define(DISPLAY_TIME_TEMPORARY, 0).
-define(DISPLAY_TIME_DEFAULT,   1).  % Default
-define(DISPLAY_TIME_INVOKE,    3).

%%%
% dpf_result
%%
-define(DPF_RESULT_DATATYPE, ?INTEGER(1)).
-define(DPF_RESULT_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(DPF_RESULT_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

% dpf_result Values
-define(DPF_RESULT_NOT_SET, 0).  % DPF not set
-define(DPF_RESULT_SET,     1).  % DPF set

%%%
% its_reply_type
%%
-define(ITS_REPLY_TYPE_DATATYPE, ?INTEGER(1)).
-define(ITS_REPLY_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 8)).
-define(ITS_REPLY_TYPE_RESERVED, ?RANGE_INTEGER(1, 9, 255)).

% its_reply_type Values
-define(ITS_REPLY_TYPE_DIGIT,          0).
-define(ITS_REPLY_TYPE_NUMBER,         1).
-define(ITS_REPLY_TYPE_TELEPHONE_NO,   2).
-define(ITS_REPLY_TYPE_PASSWORD,       3).
-define(ITS_REPLY_TYPE_CHARACTER_LINE, 4).
-define(ITS_REPLY_TYPE_MENU,           5).
-define(ITS_REPLY_TYPE_DATE,           6).
-define(ITS_REPLY_TYPE_TIME,           7).
-define(ITS_REPLY_TYPE_CONTINUE,       8).

%%%
% its_session_info
%
% %@doc A its_session_info value should be defined using the its_session_info
% record.
% %@end
%%
-define(ITS_SESSION_INFO_SESSION_NUMBER_DATATYPE, ?INTEGER(1)).
-define(ITS_SESSION_INFO_SESSION_NUMBER_DOMAIN,   ?INTEGER(1)).

-define(ITS_SESSION_INFO_SEQUENCE_NUMBER_DATATYPE, ?INTEGER(1)).
-define(ITS_SESSION_INFO_SEQUENCE_NUMBER_DOMAIN,   ?INTEGER(1)).

-define(ITS_SESSION_INFO_DATATYPE, 
        ?COMPOSITE(its_session_info,
                   {?ITS_SESSION_INFO_SESSION_NUMBER_DATATYPE,
                    ?ITS_SESSION_INFO_SEQUENCE_NUMBER_DATATYPE})).
-define(ITS_SESSION_INFO_DOMAIN, 
        ?COMPOSITE(its_session_info,
                   {?ITS_SESSION_INFO_SESSION_NUMBER_DOMAIN,
                    ?ITS_SESSION_INFO_SEQUENCE_NUMBER_DOMAIN})).
-define(ITS_SESSION_INFO_RESERVED, ?EMPTY).

%%%
% language_indicator
%%
-define(LANGUAGE_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(LANGUAGE_INDICATOR_DOMAIN,   ?INTEGER(1)).
-define(LANGUAGE_INDICATOR_RESERVED, ?EMPTY).

% language_indicator Values
-define(LANGUAGE_INDICATOR_UNSPECIFIED, 0).  % Default
-define(LANGUAGE_INDICATOR_ENGLISH,     1).
-define(LANGUAGE_INDICATOR_FRENCH,      2).
-define(LANGUAGE_INDICATOR_SPANISH,     3).
-define(LANGUAGE_INDICATOR_GERMAN,      4).
-define(LANGUAGE_INDICATOR_PORTUGUESE,  5).

%%%
% message_payload
%
% %@doc The maximun size is MC and network implementation specific.
% %@end
%%
-define(MESSAGE_PAYLOAD_DATATYPE, ?VAR_OCTET_STRING(65536)).
-define(MESSAGE_PAYLOAD_DOMAIN,   ?VAR_OCTET_STRING(65536)).
-define(MESSAGE_PAYLOAD_RESERVED, ?EMPTY).

%%%
% message_state_tlv
%
% %@see message_state definition above
%%
-define(MESSAGE_STATE_TLV_DATATYPE, ?MESSAGE_STATE_DATATYPE).
-define(MESSAGE_STATE_TLV_DOMAIN,   ?MESSAGE_STATE_DOMAIN).
-define(MESSAGE_STATE_TLV_RESERVED, ?MESSAGE_STATE_RESERVED).

%%%
% more_messages_to_send
%%
-define(MORE_MESSAGES_TO_SEND_DATATYPE, ?INTEGER(1)).
-define(MORE_MESSAGES_TO_SEND_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(MORE_MESSAGES_TO_SEND_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

% more_messages_to_send Values
-define(MORE_MESSAGES_TO_SEND_NO,  0).  % No more messages to follow
-define(MORE_MESSAGES_TO_SEND_YES, 1).  % More messages to follow (default)

%%%
% ms_availability_status
%%
-define(MS_AVAILABILITY_STATUS_DATATYPE, ?INTEGER(1)).
-define(MS_AVAILABILITY_STATUS_DOMAIN,   ?BOUND_INTEGER(1, 2)).
-define(MS_AVAILABILITY_STATUS_RESERVED, ?RANGE_INTEGER(1, 3, 255)).

% ms_availability_status Values
-define(MS_AVAILABILITY_STATUS_AVAILABLE,   0).  % MS Available (Default)
-define(MS_AVAILABILITY_STATUS_DENIED,      1).  % Suspended, no SMS capability
-define(MS_AVAILABILITY_STATUS_UNAVAILABLE, 2).  % Unavailable

%%%
% ms_msg_wait_facilities
%%
-define(MS_MSG_WAIT_FACILITIES_DATATYPE, ?INTEGER(1)).
-define(MS_MSG_WAIT_FACILITIES_DOMAIN,  
        ?UNION([?RANGE_INTEGER(1, 2#00000000, 2#00000011),
                ?RANGE_INTEGER(1, 2#10000000, 2#10000011)])).
-define(MS_MSG_WAIT_FACILITIES_RESERVED, ?EMPTY).
  
% ms_msg_wait_facilities Values
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_VOICEMAIL, 2#00000000).
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_FAX,       2#00000001).
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_EMAIL,     2#00000010).
-define(MS_MSG_WAIT_FACILITIES_INACTIVE_OTHER,     2#00000011).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_VOICEMAIL,   2#10000000).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_FAX,         2#10000001).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_EMAIL,       2#10000010).
-define(MS_MSG_WAIT_FACILITIES_ACTIVE_OTHER,       2#10000011).

%%%
% ms_validity
%
% %@doc A ms_validity value should be defined using the ms_validity_absolute or
% ms_validity_relative record.
%
% %@see ms_validity_absolute and ms_validity_relative record definitions below
% %@end
%%
-define(MS_VALIDITY_TIME_BEHAVIOUR_DATATYPE, ?INTEGER(1)).
-define(MS_VALIDITY_ABSOLUTE_TIME_DOMAIN,    ?BOUND_INTEGER(1, 3)).
-define(MS_VALIDITY_RELATIVE_TIME_DOMAIN,    ?CONSTANT(4)).
-define(MS_VALIDITY_TIME_BEHAVIOUR_RESERVED, ?RANGE_INTEGER(1, 5, 255)).

-define(MS_VALIDITY_TIME_UNIT_DATATYPE, ?INTEGER(1)).
-define(MS_VALIDITY_TIME_UNIT_DOMAIN,   ?BOUND_INTEGER(1, 2#00000110)).
-define(MS_VALIDITY_TIME_UNIT_RESERVED, 
        ?RANGE_INTEGER(1, 2#00000111, 2#11111111)).

-define(MS_VALIDITY_NUMBER_DATATYPE, ?INTEGER(2)).
-define(MS_VALIDITY_NUMBER_DOMAIN,   ?INTEGER(2)).
-define(MS_VALIDITY_NUMBER_RESERVED, ?INTEGER(2)).

-define(MS_VALIDITY_ABSOLUTE_DATATYPE,
        ?COMPOSITE(ms_validity_absolute,
                   {?MS_VALIDITY_TIME_BEHAVIOUR_DATATYPE})).
-define(MS_VALIDITY_ABSOLUTE_DOMAIN,
        ?COMPOSITE(ms_validity_absolute,
                   {?MS_VALIDITY_ABSOLUTE_TIME_DOMAIN})).
-define(MS_VALIDITY_ABSOLUTE_RESERVED,
        ?COMPOSITE(ms_validity_absolute,
                   {?MS_VALIDITY_TIME_BEHAVIOUR_RESERVED})).

-define(MS_VALIDITY_RELATIVE_DATATYPE,
        ?COMPOSITE(ms_validity_relative,      
                   {?MS_VALIDITY_TIME_BEHAVIOUR_DATATYPE,
                    ?MS_VALIDITY_TIME_UNIT_DATATYPE,
                    ?MS_VALIDITY_NUMBER_DATATYPE})).
-define(MS_VALIDITY_RELATIVE_DOMAIN,
        ?COMPOSITE(ms_validity_relative,      
                   {?MS_VALIDITY_RELATIVE_TIME_DOMAIN,
                    ?MS_VALIDITY_TIME_UNIT_DOMAIN,
                    ?MS_VALIDITY_NUMBER_DOMAIN})).
-define(MS_VALIDITY_RELATIVE_RESERVED,
        ?COMPOSITE(ms_validity_relative,      
                   {?MS_VALIDITY_TIME_BEHAVIOUR_RESERVED,
                    ?MS_VALIDITY_TIME_UNIT_RESERVED,
                    ?MS_VALIDITY_NUMBER_RESERVED})).

-define(MS_VALIDITY_DATATYPE, 
        ?UNION([?MS_VALIDITY_ABSOLUTE_DATATYPE, 
                ?MS_VALIDITY_RELATIVE_DATATYPE])).
-define(MS_VALIDITY_DOMAIN, 
        ?UNION([?MS_VALIDITY_ABSOLUTE_DOMAIN, 
                ?MS_VALIDITY_RELATIVE_DOMAIN])).
-define(MS_VALIDITY_RESERVED, 
        ?UNION([?MS_VALIDITY_ABSOLUTE_RESERVED, 
                ?MS_VALIDITY_RELATIVE_RESERVED])).

% ms_validity_time_behaviour Values
-define(MS_VALIDITY_STORE_INDEFINITELY,              0). % Default
-define(MS_VALIDITY_UNTIL_POWER_DOWN,                1).
-define(MS_VALIDITY_UNTIL_REGISTRATION_AREA_CHANGES, 2).
-define(MS_VALIDITY_DISPLAY_ONLY,                    3).
-define(MS_VALIDITY_RELATIVE_TIME_PERIOD,            4).

% ms_validity_time_unit Values
-define(MS_VALIDITY_DEFAULT,           #ms_validity_absolute{}).
-define(MS_VALIDITY_TIME_UNIT_SECONDS, 2#00000000).
-define(MS_VALIDITY_TIME_UNIT_MINUTES, 2#00000001).
-define(MS_VALIDITY_TIME_UNIT_HOURS,   2#00000010).
-define(MS_VALIDITY_TIME_UNIT_DAYS,    2#00000011).
-define(MS_VALIDITY_TIME_UNIT_WEEKS,   2#00000100).
-define(MS_VALIDITY_TIME_UNIT_MONTHS,  2#00000101).
-define(MS_VALIDITY_TIME_UNIT_YEARS,   2#00000110).

%%%
% network_error_code 
%
% %@doc A network_error_code value should be defined using the 
% network_error_code record.
%
% %@see network_error_code record definition below
% %@end
%%
-define(NETWORK_ERROR_CODE_TYPE_DATATYPE, ?INTEGER(1)).
-define(NETWORK_ERROR_CODE_TYPE_DOMAIN,   ?RANGE_INTEGER(1, 1, 8)).
-define(NETWORK_ERROR_CODE_TYPE_RESERVED,   
        ?UNION([?CONSTANT(0), ?RANGE_INTEGER(1, 9, 255)])).

-define(NETWORK_ERROR_CODE_ERROR_DATATYPE, ?INTEGER(2)).
-define(NETWORK_ERROR_CODE_ERROR_DOMAIN,   ?INTEGER(2)).
-define(NETWORK_ERROR_CODE_ERROR_RESERVED, ?INTEGER(2)).

-define(NETWORK_ERROR_CODE_DATATYPE, 
        ?COMPOSITE(network_error_code,
                   {?NETWORK_ERROR_CODE_TYPE_DATATYPE,
                    ?NETWORK_ERROR_CODE_ERROR_DATATYPE})).
-define(NETWORK_ERROR_CODE_DOMAIN, 
        ?COMPOSITE(network_error_code,
                   {?NETWORK_ERROR_CODE_TYPE_DOMAIN,
                    ?NETWORK_ERROR_CODE_ERROR_DOMAIN})).
-define(NETWORK_ERROR_CODE_RESERVED, 
        ?COMPOSITE(network_error_code,
                   {?NETWORK_ERROR_CODE_TYPE_RESERVED,
                    ?NETWORK_ERROR_CODE_ERROR_RESERVED})).

% network_error_code_type Values
-define(NETWORK_ERROR_CODE_TYPE_ANSI_136_ACCESS_DENIED_REASON, 1).
-define(NETWORK_ERROR_CODE_TYPE_IS_95_ACCESS_DENIED_REASON,    2).
-define(NETWORK_ERROR_CODE_TYPE_GSM,                           3).
-define(NETWORK_ERROR_CODE_TYPE_ANSI_136_CAUSE_CODE,           4).
-define(NETWORK_ERROR_CODE_TYPE_IS_95_COUSE_CODE,              5).
-define(NETWORK_ERROR_CODE_TYPE_ANSI_41_ERROR,                 6).
-define(NETWORK_ERROR_CODE_TYPE_SMPP_ERROR,                    7).
-define(NETWORK_ERROR_CODE_TYPE_MESSAGE_CENTER_SPECIFIC,       8).

%%%
% number_of_messages
%%
-define(NUMBER_OF_MESSAGES_DATATYPE, ?INTEGER(1)).
-define(NUMBER_OF_MESSAGES_DOMAIN,   ?BOUND_INTEGER(1, 99)).
-define(NUMBER_OF_MESSAGES_RESERVED, ?RANGE_INTEGER(1, 100, 255)).

%%%
% payload_type
%%
-define(PAYLOAD_TYPE_DATATYPE, ?INTEGER(1)).
-define(PAYLOAD_TYPE_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(PAYLOAD_TYPE_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

% payload_type Values
-define(PAYLOAD_TYPE_DEFAULT, 0).  % Default
-define(PAYLOAD_TYPE_WDP,     0).  % WAP
-define(PAYLOAD_TYPE_WCMP,    1).  % Wireless Control Message Protocol.

%%%
% privacy_indicator
%%
-define(PRIVACY_INDICATOR_DATATYPE, ?INTEGER(1)).
-define(PRIVACY_INDICATOR_DOMAIN,   ?BOUND_INTEGER(1, 3)).
-define(PRIVACY_INDICATOR_RESERVED, ?RANGE_INTEGER(1, 4, 255)).

% privacy_indicator Values
-define(PRIVACY_INDICATOR_NOT_RESTRICTED, 0).  % Privacy Level 0 (Default)
-define(PRIVACY_INDICATOR_RESTRICTED,     1).  % Privacy Level 1
-define(PRIVACY_INDICATOR_CONFIDENTIAL,   2).  % Privacy Level 2
-define(PRIVACY_INDICATOR_SECRET,         3).  % Privacy Level 3

%%%
% qos_time_to_live 
%
% %@doc If not present, the MC may apply a default value. 
% %@end
%%
-define(QOS_TIME_TO_LIVE_DATATYPE, ?INTEGER(4)).
-define(QOS_TIME_TO_LIVE_DOMAIN,   ?INTEGER(4)).
-define(QOS_TIME_TO_LIVE_RESERVED, ?EMPTY).

%%%
% receipted_message_id 
%
% %@see message_identifier definition above
%%
-define(RECEIPTED_MESSAGE_ID_DATATYPE, ?MESSAGE_IDENTIFIER_DATATYPE).
-define(RECEIPTED_MESSAGE_ID_DOMAIN,   ?MESSAGE_IDENTIFIER_DOMAIN).
-define(RECEIPTED_MESSAGE_ID_RESERVED, ?MESSAGE_IDENTIFIER_RESERVED).

%%%
% sar_msg_ref_num
%
% %@doc Current implementation automatically fills this field with the lower 
% order bytes of the sequence_number of the first segment.
% %@end
%%
-define(SAR_MSG_REF_NUM_DATATYPE, ?INTEGER(2)).
-define(SAR_MSG_REF_NUM_DOMAIN,   ?INTEGER(2)).
-define(SAR_MSG_REF_NUM_RESERVED, ?EMPTY).

%%%
% sar_segment_seqnum
%
% %@doc A Value in the range 1 to 255 indicating the sequence number of a
% particular message within the concatenated short message.
% %@end
%%
-define(SAR_SEGMENT_SEQNUM_DATATYPE, ?INTEGER(1)).
-define(SAR_SEGMENT_SEQNUM_DOMAIN,   ?RANGE_INTEGER(1, 1, 255)).
-define(SAR_SEGMENT_SEQNUM_RESERVED, ?EMPTY).

% sar_segment_seqnum Values
-define(SAR_SEGMENT_SEQNUM_FIRST, 1).

%%%
% sar_total_segments
%
% %@doc A Value in the range 1 to 255 indicating the total number of fragments
% within the concatenated short message.
% %@end
%%
-define(SAR_TOTAL_SEGMENTS_DATATYPE, ?INTEGER(1)).
-define(SAR_TOTAL_SEGMENTS_DOMAIN,   ?RANGE_INTEGER(1, 1, 255)).
-define(SAR_TOTAL_SEGMENTS_RESERVED, ?EMPTY).

% sar_total_segments Values
-define(SAR_TOTAL_SEGMENTS_SINGLE, 1).

%%%
% sc_interface_version
%
% %@see smpp_version definition above
%%
-define(SC_INTERFACE_VERSION_DATATYPE, ?SMPP_VERSION_DATATYPE).
-define(SC_INTERFACE_VERSION_DOMAIN,   ?SMPP_VERSION_DOMAIN).
-define(SC_INTERFACE_VERSION_RESERVED, ?SMPP_VERSION_RESERVED).

%%%
% set_dpf
%%
-define(SET_DPF_DATATYPE, ?INTEGER(1)).
-define(SET_DPF_DOMAIN,   ?BOUND_INTEGER(1, 1)).
-define(SET_DPF_RESERVED, ?RANGE_INTEGER(1, 2, 255)).

% set_dpf Values
-define(SET_DPF_NOT_REQUESTED, 0). % DPF for delivery to MS not requested
-define(SET_DPF_REQUESTED,     1). % Delivery Pending Flag requested (default)

%%%
% sms_signal
%
% %@TODO Review the domain and reserved values of this parameter.
%%
-define(SMS_SIGNAL_DATATYPE, ?INTEGER(2)).
-define(SMS_SIGNAL_DOMAIN,   ?INTEGER(2)).
-define(SMS_SIGNAL_RESERVED, ?EMPTY).

%%%
% source_addr_subunit
%
% %@see addr_subunit definition above
%%
-define(SOURCE_ADDR_SUBUNIT_DATATYPE, ?ADDR_SUBUNIT_DATATYPE).
-define(SOURCE_ADDR_SUBUNIT_DOMAIN,   ?ADDR_SUBUNIT_DOMAIN).
-define(SOURCE_ADDR_SUBUNIT_RESERVED, ?ADDR_SUBUNIT_RESERVED).

%%%
% source_bearer_type
%
% %@see bearer_type definition above
%%
-define(SOURCE_BEARER_TYPE_DATATYPE, ?BEARER_TYPE_DATATYPE).
-define(SOURCE_BEARER_TYPE_DOMAIN,   ?BEARER_TYPE_DOMAIN).
-define(SOURCE_BEARER_TYPE_RESERVED, ?BEARER_TYPE_RESERVED).

%%%
% source_network_id
%
% %@doc When this TLV is specified, it must be accompanied with a 
% source_node_id TLV.
%
% %@see network_id definition above
% %@end
%%
-define(SOURCE_NETWORK_ID_DATATYPE, ?NETWORK_ID_DATATYPE).
-define(SOURCE_NETWORK_ID_DOMAIN,   ?NETWORK_ID_DOMAIN).
-define(SOURCE_NETWORK_ID_RESERVED, ?NETWORK_ID_RESERVED).

%%%
% source_network_type
%
% %@see network_type definition above
%%
-define(SOURCE_NETWORK_TYPE_DATATYPE, ?NETWORK_TYPE_DATATYPE).
-define(SOURCE_NETWORK_TYPE_DOMAIN,   ?NETWORK_TYPE_DOMAIN).
-define(SOURCE_NETWORK_TYPE_RESERVED, ?NETWORK_TYPE_RESERVED).

%%%
% source_node_id
%
% %@see node_id definition above
%%
-define(SOURCE_NODE_ID_DATATYPE, ?NODE_ID_DATATYPE).
-define(SOURCE_NODE_ID_DOMAIN,   ?NODE_ID_DOMAIN).
-define(SOURCE_NODE_ID_RESERVED, ?NODE_ID_RESERVED).

%%%
% source_port
%
% %@see port definition above
%%
-define(SOURCE_PORT_DATATYPE, ?PORT_DATATYPE).
-define(SOURCE_PORT_DOMAIN,   ?PORT_DOMAIN).
-define(SOURCE_PORT_RESERVED, ?PORT_RESERVED).

%%%
% source_subaddress
%
% %@see subaddress definition above
%%
-define(SOURCE_SUBADDRESS_DATATYPE, ?SUBADDRESS_DATATYPE).
-define(SOURCE_SUBADDRESS_DOMAIN,   ?SUBADDRESS_DOMAIN).
-define(SOURCE_SUBADDRESS_RESERVED, ?SUBADDRESS_RESERVED).

%%%
% source_telematics_id
%
% %@see telematics_id definition above
%%
-define(SOURCE_TELEMATICS_ID_DATATYPE, ?TELEMATICS_ID_DATATYPE).
-define(SOURCE_TELEMATICS_ID_DOMAIN,   ?TELEMATICS_ID_DOMAIN).
-define(SOURCE_TELEMATICS_ID_RESERVED, ?TELEMATICS_ID_RESERVED).

%%%
% user_message_reference 
%
% %@doc All values allowed.
%%
-define(USER_MESSAGE_REFERENCE_DATATYPE, ?INTEGER(2)).
-define(USER_MESSAGE_REFERENCE_DOMAIN,   ?INTEGER(2)).
-define(USER_MESSAGE_REFERENCE_RESERVED, ?EMPTY).

%%%
% user_response_code 
%
% %@doc Application specific.  
%
% <ul>
%   <li>0 to 255 (IS-96 CDMA)</li> 
%   <li>0 to 15 (CMT-136 TDMA)</li>
% </ul>
% %@end
%%
-define(USER_RESPONSE_CODE_DATATYPE,    ?INTEGER(1)).
-define(USER_RESPONSE_CODE_CDMA_DOMAIN, ?INTEGER(1)).
-define(USER_RESPONSE_CODE_TDMA_DOMAIN, ?BOUND_INTEGER(1, 15)).
-define(USER_RESPONSE_CODE_RESERVED,    ?EMPTY).

%%%
% ussd_service_op
%%
-define(USSD_SERVICE_OP_DATATYPE, ?INTEGER(1)).
-define(USSD_SERVICE_OP_DOMAIN,   
        ?UNION([?RANGE_INTEGER(1,  0,   3),
                ?RANGE_INTEGER(1, 16,  19),
                ?RANGE_INTEGER(1, 32, 255)])).
-define(USSD_SERVICE_OP_RESERVED,   
        ?UNION([?RANGE_INTEGER(1,  4,  15), 
                ?RANGE_INTEGER(1, 20,  31)])).

% ussd_service_op Values
-define(USSD_SERVICE_OP_PSSD_INDICATION, 0).
-define(USSD_SERVICE_OP_PSSR_INDICATION, 1). 
-define(USSD_SERVICE_OP_USSR_REQUEST,    2). 
-define(USSD_SERVICE_OP_USSN_REQUEST,    3). 
-define(USSD_SERVICE_OP_PSSD_RESPONSE,  16). 
-define(USSD_SERVICE_OP_PSSR_RESPONSE,  17). 
-define(USSD_SERVICE_OP_USSR_CONFIRM,   18). 
-define(USSD_SERVICE_OP_USSN_CONFIRM,   19).


%%%-------------------------------------------------------------------
% Records
%%--------------------------------------------------------------------
%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% PDU Composite Field Record Definitions
%
% %@see sections 4.1 to 4.6 on [SMPP 5.0]
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% %@spec {dest_address_sme, 
%         DestFlag, 
%         DestAddrTon, 
%         DestAddrNpi,
%         DestinationAddr}
%    DestFlag        = int()
%    DestAddrTon     = int()
%    DestAddrNpi     = int()
%    DestinationAddr = string()
%
% %@doc dest_address_sme composite record definition.
%
% <p>The macro ?DEST_ADDRESS_SME_DATATYPE defines the type specifier for this
% field.</p>
%
% <dl>
%   <dt>DestFlag: </dt><dd>Identifies the kind of address, 0x01 for SME 
%     address.  Integer, 1 octets (default is ?DEST_FLAG_SME).
%   </dd>
%   <dt>DestAddrTon: </dt><dd>Indicates Type of Number for destination.  
%     Integer, 1 octet (default is ?TON_INTERNATIONAL).
%   </dd>
%   <dt>DestAddrNpi: </dt><dd>Numbering Plan Indicator for destination.  
%     Integer, 1 octet  (default is ?NPI_ISDN).
%   </dd>
%   <dt>DestinationAddr: </dt><dd>Destination address of this short message.
%     For mobile terminated messages, this is the directory number of the
%     recipient MS.  C-Octet String, Var. max 21 octets.
%   </dd>
% </dl>
% %@end
%%
-record(dest_address_sme, 
        {dest_flag       = ?DEST_FLAG_SME,
         dest_addr_ton   = ?TON_INTERNATIONAL,
         dest_addr_npi   = ?NPI_ISDN,
         destination_addr}).

%%%
% %@spec {dest_address_dl, DestFlag, DlName}
%    DestFlag  = int()
%    DlName    = string()
%
% %@doc dest_address_dl composite record definition.
%
% <p>The macro ?DEST_ADDRESS_DL_DATATYPE defines the type specifier for this
% field.</p>
%
% <dl>
%   <dt>DestFlag: </dt><dd>Identifies the kind of address, 0x02 for 
%     Distribution List.  Integer, 1 octets (default is ?DEST_FLAG_DL)).
%   </dd>
%   <dt>DlName: </dt><dd>Name of the Distribution List.  C-Octet String, 
%     Var. max 21 octets.
%   </dd>
% </dl>
% %@end
%%
-record(dest_address_dl, 
        {dest_flag = ?DEST_FLAG_DL,
         dl_name}).

%%%
% %@spec {time, 
%         Year,
%         Month,
%         Day,
%         Hour,
%         Minute,
%         Second,
%         TenthsOfSecond,
%         Difference,
%         RelationToUtc,
%         NullTerminator}
%    Year           = DecimalString
%    DecimalString  = [Digit, Digit]
%    Digit          = $0 | $1 | $2 | $3 | $4 | $5 | $6 | $7 | $8 | $9
%    Month          = DecimalString
%    Day            = DecimalString
%    Hour           = DecimalString
%    Minute         = DecimalString
%    Second         = DecimalString
%    TenthsOfSecond = Digit
%    Difference     = DecimalString
%    RelationToUtc  = "+" | "-" | "R"
%    NullTerminator = "\0"
%
% %@doc time composite record definition.
%
% <p>The macro ?TIME_DATATYPE defines the syntax for this field.</p>
%
% <dl>
%   <dt>Year: </dt><dd>Last two digits of the year.  Decimal Octet String, 
%     Fixed 2 octets.
%   </dd>
%   <dt>Month: </dt><dd>Digits of the month.  Decimal Octet String, Fixed 2 
%     octets.
%   </dd>
%   <dt>Day: </dt><dd>Digits of the day.  Decimal Octet String, Fixed 2 octets.
%   </dd>
%   <dt>Hour: </dt><dd>Digits of the hour.  Decimal Octet String, Fixed 2
%     octets.
%   </dd>
%   <dt>Minute: </dt><dd>Digits of the minute.  Decimal Octet String, Fixed 2
%     octets.
%   </dd>
%   <dt>Second: </dt><dd>Digits of the second.  Decimal Octet String, Fixed 2
%     octets.
%   </dd>
%   <dt>Tenths of Second: </dt><dd>Digit of the tenths of second.  Decimal 
%     Octet String, Fixed 1 octet.
%   </dd>
%   <dt>Difference: </dt><dd>Time difference in quarter hours between local
%     time (as expressed in the first 13 octets) and UTC (Universal Time 
%     Constant) time (00-48).  Decimal Octet String, Fixed 2 octets.
%   </dd>
%   <dt>RelationToUtc: </dt><dd>Local time is in quarter hours advanced "+" or
%     retarded "-" in relation to UTC time or relative to the current MC time
%     "R".  Octet String, Fixed 1 octet.
%   </dd>
%   <dt>NullTerminator: </dt><dd>A dummy field holding the NULL terminating 
%     character of the C-Octet String representing the time.  Constant 
%     ?NULL_C_OCTET_STRING.
%
%     <p>This field is always initialized to the correct value and must be 
%     ignored by the programmer using this type definition.  It's only present
%     to let this record fulfill the SMPP time declarations. 
%   </dd>
% </dl>
% %@end
%%
-record(time, 
        {year,
         month,
         day,
         hour,
         minute,
         second,
         tenths_of_second,
         difference,
         relation_to_utc,
         null_terminator = ?NULL_C_OCTET_STRING}).

%%%
% %@spec {unsuccess_sme, 
%         DestAddrTon, 
%         DestAddrNpi, 
%         DestinationAddr,
%         ErrorStatusCode}
%    DestAddrTon     = int()
%    DestAddrNpi     = int()
%    DestinationAddr = string()
%    ErrorStatusCode = int()
%
% %@doc unsuccess_sme composite record definition.
%
% <p>The macro ?UNSUCCESS_SME_DATATYPE defines the type specifier for this 
% field.</p>
%
% <dl>
%   <dt>DestAddrTon: </dt><dd>Indicates Type of Number for destination.  
%     Integer, 1 octet (default is ?TON_INTERNATIONAL).
%   </dd>
%   <dt>DestAddrNpi: </dt><dd>Numbering Plan Indicator for destination.  
%     Integer, 1 octet  (default is ?NPI_ISDN).
%   </dd>
%   <dt>DestinationAddr: </dt><dd>Destination address of this short message.
%     For mobile terminated messages, this is the directory number of the
%     recipient MS.  C-Octet String, Var. max 21 octets.
%   </dd>
%   <dt>ErrorStatusCode: </dt><dd>Indicates the success or failure of the 
%     submit_multi request to this SME address.  Check command_status 
%     macros for a complete list of SMPP Error codes.  Integer, 4 octets.
%   </dd>
% </dl>
% %@end
%%
-record(unsuccess_sme, 
        {dest_addr_ton     = ?TON_INTERNATIONAL,
         dest_addr_npi     = ?NPI_ISDN,
         destination_addr,
         error_status_code}).


%%%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% PDU TLV Record Definitions
%
% %@see section 4.8.4 on [SMPP 5.0]
%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
%%%
% %@spec {broadcast_area, Format, Details} 
%    Format  = int()
%    Details = string()
%
% %@doc broadcast_area TLV record definition.
%
% <p>The macro ?BROADCAST_AREA_DATATYPE defines the type specifier for this
% TLV.</p>
%
% <dl>
%   <dt>Format: </dt><dd>Used to specify the area format.  Integer, 1 octet
%     (default is ?BROADCAST_AREA_FORMAT_ALIAS).
%   </dd>
%   <dt>Details: </dt><dd>Used to specify the broadcast area details.  Octet 
%     String, Var. max 100 octets.
%   </dd>
% </dl>
% %@end
%%
-record(broadcast_area, 
        {format = ?BROADCAST_AREA_FORMAT_ALIAS,
         details}).

%%%
% %@spec {broadcast_content_type, NetworkType, Service} 
%    NetworkType = int()
%    Service     = int()
%
% %@doc broadcast_content_type TLV record definition.
%
% <p>The macro ?BROADCAST_CONTENT_TYPE_DATATYPE defines the type specifier for
% this TLV.</p>
%
% <dl>
%   <dt>NetworkType: </dt><dd>Tag indicating the network type.  Integer, 1 
%     octet (default is ?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GSM).
%   </dd>
%   <dt>Service: </dt><dd>Broadcast service type.  Integer, 2 octets (default
%     is ?BROADCAST_CONTENT_TYPE_SERVICE_MULTI_CATEGORY_SERVICES).
%   </dd>
% </dl>
% %@end
%%
-record(broadcast_content_type, 
        {network_type = ?BROADCAST_CONTENT_TYPE_NETWORK_TYPE_GSM,
         service = ?BROADCAST_CONTENT_TYPE_SERVICE_MULTI_CATEGORY_SERVICES}).

%%%
% %@spec {broadcast_frequency_interval, TimeUnit, Number} 
%    TimeUnit = int()
%    Number   = int()
%
% %@doc broadcast_frequency_interval TLV record definition.
%
% <p>The macro ?BROADCAST_FREQUENCY_INTERVALY_DATATYPE defines the type 
% specifier for this TLV.</p>
%
% <dl>
%   <dt>TimeUnit: </dt><dd>Specifies the Units of Time.  Integer, 1 
%     octet (default is ?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MINUTES).
%   </dd>
%   <dt>Number: </dt><dd>Number of the specified time units.  Integer, 
%     2 octets.
%   </dd>
% </dl>
% %@end
%%
-record(broadcast_frequency_interval, 
        {time_unit = ?BROADCAST_FREQUENCY_INTERVAL_TIME_UNIT_MINUTES,
         number}).

%%%
% %@spec {subaddress, Tag, Data} 
%    Tag  = int()
%    Data = string()
%
% %@doc dest_subaddress and source_subaddress TLV record definition.
%
% <p>The macro ?SUBADDRESS_DATATYPE defines the type specifier for these 
% TLVs.</p>
%
% <dl>
%   <dt>Tag: </dt><dd>Indicates the type of sub-addressing information 
%     included in Data, and implies the type and length of sub-addressing
%     information which can accompany this tag value in the Data field.
%     Integer, 1 octet (default is ?SUBADDRESS_TAG_USER).
%   </dd>
%   <dt>Data: </dt><dd>Contain the subaddress.  Octet String, Var. max 22 
%     octets.
%   </dd>
% </dl>
% %@end
%%
-record(subaddress, 
        {tag = ?SUBADDRESS_TAG_USER,
         data}).

%%%
% %@spec {callback_num, 
%         DigitModeIndicator, 
%         AddrTon, 
%         AddrNpi,
%         NumberDigits}
%    DigitModeIndicator = int()
%    AddrTon            = int()
%    AddrNpi            = int()
%    NumberDigits       = string()
%
% %@doc callback_num TLV record definition.
%
% <p>The macro ?CALLBACK_NUM_DATATYPE defines the type specifier for this 
% TLV.</p>
%
% <dl>
%   <dt>DigitModeIndicator: </dt><dd>Indicates that the Call Back Number is 
%     sent to the mobile as DTMF digits encoded in TBCC (Value = 0) or as 
%     ASCII digits (Value 1).  Integer, 1 octet (default is 
%     ?CALLBACK_NUM_DIGIT_MODE_INDICATOR_ASCII).
%   </dd>
%   <dt>AddrTon: </dt><dd>Indicates Type of Number for destination.  
%     Integer, 1 octet (default is ?TON_INTERNATIONAL).
%   </dd>
%   <dt>AddrNpi: </dt><dd>Numbering Plan Indicator for destination.  
%     Integer, 1 octet  (default is ?NPI_ISDN).
%   </dd>
%   <dt>NumberDigits: </dt><dd>The Call Back Number Digits.  Octet String, 
%     Var. max 16 octets.
%   </dd>
% </dl>
% %@end
%%
-record(callback_num, 
        {digit_mode_indicator = ?CALLBACK_NUM_DIGIT_MODE_INDICATOR_ASCII,
         addr_ton             = ?TON_INTERNATIONAL,
         addr_npi             = ?NPI_ISDN,
         number_digits}).

%%%
% %@spec {callback_num_atag, DataCoding, DisplayCharacters} 
%    DataCoding        = int()
%    DisplayCharacters = string()
%
% %@doc callback_num_atag TLV record definition.
%
% <p>The macro ?CALLBACK_NUM_ATAG_DATATYPE defines the type specifier for this
% TLV.</p>
%
% <dl>
%   <dt>DataCoding: </dt><dd>Defines the encoding scheme of the Alpha Tag 
%     display characters.  Integer, 1 octet (default is 
%     ?ENCODING_SCHEME_LATIN_1).
%   </dd>
%   <dt>DisplayCharacters: </dt><dd>The Alpha Tag display Characters.  Octet
%     String, Var. max 64 octets.
%   </dd>
% </dl>
% %@end
%%
-record(callback_num_atag, 
        {data_coding = ?ENCODING_SCHEME_LATIN_1,
         display_characters}).


%%%
% %@spec {telematics_id, ProtocolId, Reserved} 
%    ProtocolId = int()
%    Reserver   = int()
%
% %@doc dest_telematics_id and source_telematics_id TLV record definition.
%
% <p>The macro ?TELEMATICS_ID_DATATYPE defines the type specifier for these 
% TLV.</p>
%
% <dl>
%   <dt>ProtocolId: </dt><dd>Protocol Identifier.  Network specific field.  
%     Integer, 1 octet (default is ?PROTOCOL_IDENTIFIER_GSM).
%   </dd>
%   <dt>Reserved: </dt><dd>Reserved. Integer, 1 octet (default is 
%     ?NULL_INTEGER).
%   </dd>
% </dl>
% %@end
%%
-record(telematics_id, 
        {protocol_id = ?PROTOCOL_IDENTIFIER_GSM,
         reserved    = ?NULL_INTEGER}).

%%%
% %@spec {its_session_info, SessionNumber, SequenceNumber} 
%    SessionNumber  = int()
%    SequenceNumber = int()
%
% %@doc its_session_info TLV record definition.
%
% <p>The macro ?ITS_SESSION_INFO_DATATYPE defines the type specifier for this
% TLV.</p>
%
% <dl>
%   <dt>SessionNumber: </dt><dd>Remains constant for each session.  
%     Integer, 1 octet (default is 0).
%   </dd>
%   <dt>SequenceNumber: </dt><dd>Sequence number of the dialogue unit (as
%     assigned bye the ESME) within the session is encoded in bits 7..1. The
%     End of Session Indicator indicates the message is the end of the 
%     conversation session and is encoded in bit 0 as follows:
%
%     <ul>
%       <li>0 = End of Session Indicator Inactive.</li>
%       <li>1 = End of Session Indicator Active.</li>
%     </ul>
%
%     <p>While the end of session is inactive the SequenceNumber is an even 
%     number and is increased by 2.  The Session Indicator becomes active it 
%     should be incremented by 1, (an odd number).  Integer, 1 octet (default 
%     is 0).</p>
%   </dd>
% </dl>
% %@end
%%
-record(its_session_info, 
        {session_number  = 0,
         sequence_number = 0}).

%%%
% %@spec {ms_validity_absolute, Behaviour} 
%    Behaviour = int()
%
% %@doc ms_validity_absolute TLV record definition.
%
% <p>The macro ?MS_VALIDITY_ABSOLUTE_DATATYPE defines the type specifier for
% this TLV.</p>
%
% <dl>
%   <dt>Behaviour: </dt><dd>Validity behaviour.  Integer, 1 octet 
%     (default is ?MS_VALIDITY_STORE_INDEFINITELY).
%   </dd>
% </dl>
% %@end
%%
-record(ms_validity_absolute, {behaviour = ?MS_VALIDITY_STORE_INDEFINITELY}).

%%%
% %@spec {ms_validity_relative, Behaviour, TimeUnit, Number} 
%    Behaviour = int()
%    TimeUnit  = int()
%    Number    = int()
%
% %@doc ms_validity_relative TLV record definition.
%
% <p>The macro ?MS_VALIDITY_RELATIVE_DATATYPE defines the type specifier for
% this TLV.</p>
%
% <dl>
%   <dt>Behaviour: </dt><dd>Validity behaviour.  Integer, 1 octet 
%     (default is ?MS_VALIDITY_RELATIVE_TIME_PERIOD).
%   </dd>
%   <dt>TimeUnit: </dt><dd>Specifies the Units of Time.  Integer, 1 octet 
%     (default is ?TIME_UNIT_SECONDS).
%   </dd>
%   <dt>Number: </dt><dd>The number of the specified time units.  Integer, 2 
%     octet.
%   </dd>
% </dl>
% %@end
%%
-record(ms_validity_relative, 
        {behaviour = ?MS_VALIDITY_RELATIVE_TIME_PERIOD,
         time_unit = ?MS_VALIDITY_TIME_UNIT_SECONDS,
         number}).

%%%
% %@spec {network_error_code, Type, Error} 
%    Type  = int()
%    Error = int()
%
% %@doc network_error_code TLV record definition.
%
% <p>The macro ?NETWORK_ERROR_CODE_DATATYPE defines the type specifier for this
% TLV.</p>
%
% <dl>
%   <dt>Type: </dt><dd>Network type.  Integer, 1 octet (default is 
%     ?NETWORK_ERROR_CODE_TYPE_GSM).
%   </dd>
%   <dt>Error: </dt><dd>Specify the actual network error code approproate to 
%     the network type.  Integer, 2 octet.
%   </dd>
% </dl>
% %@end
%%
-record(network_error_code, 
        {type  = ?NETWORK_ERROR_CODE_TYPE_GSM,
         error}).

-endif.  % -ifndef(smpp_base)
