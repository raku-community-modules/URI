use v6;

# This logic seems to belong somewhere related to URI but not in the URI
# module itself.

=begin pod

=head1 URI::DefaultPort

C<URI::DefaultPort> Default ports for URI schemes.

=head1 Synopsis

    use URI::DefaultPort
    my $port = scheme_default_port('ftp');

=end pod

use IETF::RFC_Grammar::URI;

unit module URI::DefaultPort;

# Uncommenting constant waiting on RT 121296
my Int #`(constant) %default_port{ Str } = (
    ftp     =>      21,
    sftp    =>      22,
    ssh     =>      22,
    telnet  =>      23,
    tn3270  =>      23,
    smtp    =>      25,
    gopher  =>      70,
    http    =>      80,
    shttp   =>      80,
    pop     =>      110,
    news    =>      119,
    nntp    =>      119,
    imap    =>      143,
    ldap    =>      389,
    https   =>      443,
    rlogin  =>      513,
    rtsp    =>      554,
    rtspu   =>      554,
    snews   =>      563,
    ldaps   =>      636,
    rsync   =>      873,
    mms     =>      1755,
    sip     =>      5060,
    sips    =>      5061,
    git     =>      9418
);
    
our sub _scheme_port(Str $scheme) {
    return %default_port{$scheme};
}

sub scheme_default_port(
    Str $scheme where /^<IETF::RFC_Grammar::URI::scheme>$/
) is export {
    return _scheme_port($scheme);
}

# vim:ft=perl6
