use v6;

# Taken/Copied with relatively minor translation to Perl6
# from RFC 3986 (http://www.ietf.org/rfc/rfc3986.txt)

#`{{

There was a bug with combined character classes and kebab
character class names that was reported on irc and fixed on 9/18/2015.
Most rules have been translated to kebab case conformant to the
RFC but character classes had to be left with underscores for now.
The latest rakudos have a fix and sometime soon that will be published
as part of a rakudo star release and then I can kebab case the character
classes in this grammar.

}}

use IETF::RFC_Grammar::IPv6;

unit grammar IETF::RFC_Grammar::URI:version<0.02> is IETF::RFC_Grammar::IPv6;

token TOP               { <URI-reference> };
token TOP-non-empty     { <URI> | <relative-ref-non-empty> };
token TOP-validating    { ^ <URI-reference> $ };
token URI-reference     { <URI> | <relative-ref> };

token absolute-URI      { <scheme> ':' <.hier-part> [ '?' query ]? };

token relative-ref      {
    <relative-part> [ '?' <query> ]? [ '#' <fragment> ]?
};
token relative-part     {
    | <?[/]> '//' <authority> <path-abempty>
    | <path-absolute>
    | <path-noscheme>
    | <path-empty>
};

token relative-ref-non-empty      {
    <relative-part-non-empty> [ '?' <query> ]? [ '#' <fragment> ]?
};
token relative-part-non-empty     {
    | <?[/]> '//' <authority> <path_abempty>
    | <path-absolute>
    | <path-noscheme>
};

token URI               {
    <scheme> ':' <hier-part> ['?' <query> ]?  [ '#' <fragment> ]?
};

token hier-part     {
    | '//' <authority> <path-abempty>
    | <path-absolute>
    | <path-rootless>
    | <path-empty>
};

token scheme            { <.uri_alpha> <[\-+.] +uri_alpha +digit>* };
    
token authority         { [ <userinfo> '@' ]? <host> [ ':' <port> ]? };
token userinfo          {
    [ ':' | <likely-userinfo-component> ]*
};
# the rfc refers to username:password as deprecated
token likely-userinfo-component {
    <+unreserved +sub_delims>+ | <.pct-encoded>+
};
token host              { <IPv4address> | <IP-literal> | <reg-name> };
token port              { <.digit>* };

token IP-literal        { '[' [ <IPv6address> | <IPvFuture> ] ']' };
token IPvFuture         {
    'v' <.xdigit>+ '.' <[:] +unreserved +sub_delims>+
};
token reg-name          { [ <+unreserved +sub_delims> | <.pct-encoded> ]* };

token path-abempty      { [ '/' <segment> ]* };
token path-absolute     { '/' [ <segment-nz> [ '/' <segment> ]* ]? };
token path-noscheme     { <segment-nz-nc> [ '/' <segment> ]* };
token path-rootless     { <segment-nz> [ '/' <segment> ]* };
token path-empty        { <.pchar> ** 0 }; # yes - zero characters

token   segment         { <.pchar>* };
token   segment-nz      { <.pchar>+ };
token   segment-nz-nc   { [ <+unenc_pchar - [:]> | <.pct-encoded> ] + };

token query             { <.fragment> };
token fragment          { [ <[/?] +unenc_pchar> | <.pct-encoded> ]* };

token pchar             { <.unenc_pchar> | <.pct-encoded> };
token unenc_pchar       { <[:@] +unreserved +sub_delims> };

token pct-encoded       { '%' <.xdigit> <.xdigit> };

token unreserved        { <[\-._~] +uri_alphanum> };

token reserved          { <+gen_delims +sub_delims> };

token gen_delims        { <[:/?\#\[\]@]> };
token sub_delims        { <[;!$&'()*+,=]> };

token uri_alphanum      { <+uri_alpha +digit> };   
token uri_alpha         { <[A..Za..z]> };

# vim:ft=perl6
