use v6;

module URI::Escape;

use IETF::RFC_Grammar::URI;

my %escapes = (^256).for: {
    .chr => sprintf '%%%02X', $_
};

# in moving from RFC 2396 to RFC 3986 this selection of characters
# may be due for an update ...
grammar escape-charset is IETF::RFC_Grammar::URI {
    token escape_unreserved {
        <[!*'()] +unreserved>
    }
    token escape_chars {
        <- escape_unreserved>
    }
}

sub uri_escape($s, Bool :$no_utf8 = False) is export {
    return $s unless defined $s;
    $s.subst(:g, /<escape-charset::escape_chars>/,
        {
            ( $no_utf8 || .Str.ord < 128 ) ?? %escapes{ .Str } !!
                # as unpack progresses the line below can improve ...
                .Str.encode.unpack('H*').uc.subst(:g, /../, { '%' ~ $_ })
        }
    );
}

# todo - automatic invalid UTF-8 detection
# see http://www.w3.org/International/questions/qa-forms-utf-8
#     find first sequence of %[89ABCDEF]<.xdigit>
#         use algorithm from url to determine if it's valid UTF-8
sub uri_unescape(*@to_unesc, Bool :$no_utf8 = False) is export {
    my @rc = @to_unesc.for: {
        .trans('+' => ' ')\
        .subst(:g, / [ '%' (<.xdigit> ** 2 ) ]+ /,
            -> ( $escaped_octets ) {
                $no_utf8
                    ?? ($escaped_octets.map: { :16(~$_).chr }).join
                    !! Buf.new($escaped_octets.map: { :16(~$_) }).\
                        decode('UTF-8')
            }
        )
    }

    @rc.elems > 1 ?? @rc !! @rc[0];
}

=begin pod

=head NAME

URI::Escape - Escape and unescape unsafe characters

=head SYNOPSYS

    use URI::Escape;
    
    my $escaped = uri_escape("10% is enough\n");
    my $un_escaped = uri_unescape('10%25%20is%20enough%0A');

=end pod

# vim:ft=perl6
