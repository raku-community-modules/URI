unit class URI;

use IETF::RFC_Grammar;
use IETF::RFC_Grammar::URI;
use URI::Escape;
need URI::DefaultPort;

class X::URI::Invalid is Exception {
    has $.source;
    method message { "Could not parse URI: $!source" }
}

our subset Scheme of Str
    where /^ [
           ''
        || <IETF::RFC_Grammar::URI::scheme>
    ] $/;
our subset Userinfo of Str
    where /^ [
           ''
        || <IETF::RFC_Grammar::URI::userinfo>
    ] $/;
our subset Host of Str
    where /^ [
           ''
        || <IETF::RFC_Grammar::URI::host>
    ] $/;
our subset Port of UInt;

class X::URI::Authority::Invalid is Exception {
    has $.source;
    method message { "Could not parse authority: $!source" }
}

class Authority {
    has Userinfo $.userinfo is rw = '';
    has Host $.host is required;
    has Port $.port is rw;

    multi method new(Authority:U: IETF::RFC_Grammar:D $grammar, Str:D $authority) {
        $grammar.parse($authority, rule => 'authority');
        if not $grammar.parse_result {
            X::URI::Authority::Invalid.new(source => $authority).throw;
        }

        self.new($grammar.parse_result);
    }

    multi method new(Authority:U: Match:D $auth) {
        my Str $userinfo = do with $auth<userinfo> { .Str } else { '' }
        my Str $host     = "$auth<host>".lc;
        my UInt $port    = do with $auth<port> { .Int } else { Nil }

        self.new(:$userinfo, :$host, :$port);
    }

    multi method gist() {
        my $authority = '';
        $authority ~= "$!userinfo@" if $!userinfo;
        $authority ~= $!host;
        $authority ~= ":$!port" if $!port;
        $authority;
    }

    multi method Str() { $.gist }
}

# Caveat: This subset is not sensitive to context, so may permit a path that is
# not valid for the current URI. So, the mutator is more particular.
our subset Path of Str
    where /^ [
           <IETF::RFC_Grammar::URI::path-abempty>
        || <IETF::RFC_Grammar::URI::path-absolute>
        || <IETF::RFC_Grammar::URI::path-noscheme>
        || <IETF::RFC_Grammar::URI::path-rootless>
        || <IETF::RFC_Grammar::URI::path-empty>
    ] $/;

our subset Query of Str
    where /^ <IETF::RFC_Grammar::URI::query> $/;

our subset Fragment of Str
    where /^ <IETF::RFC_Grammar::URI::fragment> $/;

has Str $.query-form-delimiter;
has $.grammar;
has Bool $.match-prefix = False;
has Path $.path = '';
has Scheme $.scheme is rw = '';
has Authority $.authority is rw;
has Query $.query = '';
has Fragment $.fragment is rw = '';
has %!query-form; # cache query-form
has $!uri;  # use of this now deprecated

has @.segments;

method parse (Str $str, :$match-prefix) {

    # clear string before parsing
    my Str $c_str = $str;
    $c_str .= subst(/^ \s* ['<' | '"'] /, '');
    $c_str .= subst(/ ['>' | '"'] \s* $/, '');

    $!scheme    = '';
    $!authority = Nil;
    $!path      = '';
    $!query     = '';
    $!fragment  = '';
    $!uri = Mu;
    %!query-form := Map.new();
    @!segments := ();

    if ($!match-prefix or $match-prefix) {
        $!grammar.subparse($c_str);
    }
    else {
        $!grammar.parse($c_str);
    }

    # hacky but for the time being an improvement
    if (not $!grammar.parse_result) {
        X::URI::Invalid.new(source => $str).throw
    }

    # now deprecated
    $!uri = $!grammar.parse_result;

    my $comp_container = $!grammar.parse_result<URI-reference><URI> ||
        $!grammar.parse_result<URI-reference><relative-ref>;

    $!scheme = .lc with $comp_container<scheme>;
    $!query = .Str with $comp_container<query>;
    $!fragment = .lc with $comp_container<fragment>;
    $comp_container = $comp_container<hier-part> || $comp_container<relative-part>;

    with $comp_container<authority> -> $auth {
        $!authority = Authority.new($auth);
    }

    # share code with the path mutator
    self!make-path($comp_container);

    try {
        %!query-form := split-query( $!query, :immutable ) if $!query;
        CATCH {
            default {
                %!query-form = ();
            }
        }
    }
}

our sub split-query(Str $query, Bool :$immutable = False) {
    my %query-form;

    for map { [split(/<[=]>/, $_) ]}, split(/<[&;]>/, $query) -> $qmap {
        for (0, 1) -> $i { # could go past 1 in theory ...
            $qmap[ $i ] = uri-unescape($qmap[ $i ]);
        }
        if %query-form{$qmap[0]}:exists {
            if %query-form{ $qmap[0] } ~~ Array  {
                %query-form{ $qmap[0] }.push($qmap[1])
            }
            else {
                %query-form{ $qmap[0] } = [
                    %query-form{ $qmap[0] }, $qmap[1]
                ]
            }
        }
        else {
            %query-form{ $qmap[0]} = $qmap[1]
        }
    }

    if $immutable {
        # Convert to Lists and Maps to make immutable
        for %query-form.kv -> $k, $v {
            %query-form{$k} = $v.List
                if $v ~~ Array;
        }
        %query-form.Map;
    }
    else {
        %query-form;
    }
}

# artifact form
our sub split_query(Str $query) { split-query($query) }

# deprecated old call for parse
method init ($str) {
    warn "init method now deprecated in favor of parse method";
    $.parse($str);
}

# new can pass alternate grammars some day ...
submethod BUILD(:$match-prefix) {
    $!match-prefix = ? $match-prefix;
    $!grammar = IETF::RFC_Grammar.new('rfc3986');
}

multi method new(Str $uri, :$match-prefix) {
    my $obj = self.bless(:$match-prefix);

    $obj.parse($uri) if $uri.defined;
    return $obj;
}

multi method new(Str :$uri, :$match-prefix) {
    return self.new($uri, :$match-prefix);
}

multi method userinfo(URI:D:) returns Userinfo {
    .userinfo with $!authority
}

multi method userinfo(URI:D: Userinfo $new) {
    with $!authority {
        .userinfo = $new;
    }
    else {
        X::URI::Authority::Invalid.new(
            soruce => "$new@",
        ).throw
    }
}

multi method host(URI:D:) returns Host {
    .host with $!authority;
}

multi method host(URI:D: Host $new) {
    with $!authority {
        .host = $new;
    }
    else {
        $!authority .= new(host => $new);
    }
}

method default-port {
    URI::DefaultPort.scheme-port($.scheme)
}
method default_port { $.default-port() } # artifact form

multi method _port(URI:D:) returns Port {
    return-rw .port with $!authority;
}

multi method port(URI:D:) returns Port { $._port // $.default-port }

multi method port(URI:D: Port $new) {
    with $!authority {
        .port = $new;
    }
    else {
        X::URI::Authority::Invalid.new(
            source => ":$new",
        ).throw
    }
}

multi method port(URI:D: Nil) {
    .port = Nil with $!authority;
}

multi method authority(URI:D: Str:D $authority) returns Authority:D {
    $!authority = Authority.new($!grammar, $authority);
}

multi method authority(URI:D:) is rw returns Authority:D {
    return-rw $!authority;
}

my regex URI-paths {
    ^ [
            $<path-abempty> = <IETF::RFC_Grammar::URI::path-abempty>
        |   $<path-absolute> = <IETF::RFC_Grammar::URI::path-absolute>
        |   $<path-rootless> = <IETF::RFC_Grammar::URI::path-rootless>
        |   $<path-empty> = <IETF::RFC_Grammar::URI::path-empty>
    ] $
}
my regex relative-ref-paths {
    ^ [
            $<path-abempty> = <IETF::RFC_Grammar::URI::path-abempty>
        |   $<path-absolute> = <IETF::RFC_Grammar::URI::path-absolute>
        |   $<path-noscheme> = <IETF::RFC_Grammar::URI::path-noscheme>
        |   $<path-empty> = <IETF::RFC_Grammar::URI::path-empty>
    ] $
}

method !make-path($comp_container) {
    my $path = $comp_container<path-abempty>       ||
               $comp_container<path-absolute>      ;

    $path ||=  $comp_container<path-noscheme>      ||
               $comp_container<path-rootless>      ;

    $!path = "$path" with $path;

    @!segments := $path<segment>.list.map({.Str}).list || ('',);
    if my $first_chunk = $path<segment-nz-nc> || $path<segment-nz> {
        @!segments := ($first_chunk, |@!segments);
    }
    if @!segments.elems == 0 {
        @!segments := ('',);
    }
}

multi method path(URI:D:) returns Path:D { $!path }

multi method path(URI:D: Path:D $new) {
    # Scheme implies a <URI> parse, no scheme <relative-ref>
    my &path-regex := $!scheme ?? &URI-paths !! &relative-ref-paths;

    if $new ~~ &path-regex -> $comp {
        self!make-path($comp);
    }
    else {
        X::URI::Invalid.new(
            source => self!gister(path => $new),
        ).throw
    }
}

my $warn-deprecate-abs-rel = q:to/WARN-END/;
    The absolute and relative methods are artifacts carried over from an old
    version of the p6 module.  The perl 5 module does not provide such
    functionality.  The Ruby equivalent just checks for the presence or
    absence of a scheme.  The URI rfc does identify absolute URIs and
    absolute URI paths and these methods somewhat confused the two.  Their
    functionality at the URI level is no longer seen as needed and is
    being removed.
WARN-END

method absolute {
    warn "deprecated -\n$warn-deprecate-abs-rel";
    return Bool.new;
}

method relative {
    warn "deprecated -\n$warn-deprecate-abs-rel";
    return Bool.new;
}

multi method query(URI:D:) { $!query }

multi method query(URI:D: Query $new) {
    $!query = $new;
    if $!query {
        %!query-form := split-query($!query, :immutable);
    }
    else {
        %!query-form := Map.new();
    }
}

method path-query {
    $.query ?? "$.path?$.query" !! $.path
}

method path_query { $.path-query } #artifact form

method frag { $.fragment }

method !gister(
    :$scheme = $.scheme,
    :$authority = $.authority,
    :$path = $.path,
    :$query = $.query,
    :$fragment = $.fragment,
) {
    my Str $s;
    $s ~= $scheme if $scheme;
    $s ~= '://' ~ $authority if $authority;
    $s ~= $path;
    $s ~= '?' ~ $query if $query;
    $s ~= '#' ~ $fragment if $fragment;
    $s;
}

method gist() { self!gister }
method Str() { $.gist }

# chunks now strongly deprecated
# it's segments in p5 URI and segment is part of rfc so no more chunks soon!
method chunks {
    warn "chunks attribute now deprecated in favor of segments";
    @!segments;
}

method uri {
    warn "uri attribute now deprecated in favor of .grammar.parse_result";
    $!uri;
}

multi method query-form() returns Map:D {
    %!query-form;
}

method !query-from-query-form(:$delimiter is copy) {
    my @query = gather for %!query-form.kv -> $key is copy, $vals {
		$key //= '';
        $key = uri-escape($key);
        for @($vals) -> $val is copy {
            $val //= '';
            $val  = uri-escape($val);
            $val .= trans(" " => "+");
            take "$key=$val";
        }
    }

    unless $delimiter {

        # if no delimiter spec'd, prefer the object delim
        $delimiter //= $.query-form-delimiter;

        # if no object delim, detect from previous value
        if !$delimiter && $!query ~~ /(<[&;]>)/ -> $d {
            $delimiter = $d[0];
        }

        # if nothing detected, use the ultimate default
        $delimiter //= '&';
    }

	$!query = join $delimiter, @query;
}

multi method query-form(*@replace, *%replace, :$delimiter) {
    my @pairs = flat @replace».kv, %replace.kv;

    my %qacc;
    for @pairs -> $k, $v {
        if %qacc{$k} ~~ Array {
            %qacc{$k}.push: $v;
        }
        elsif %qacc{$k}:exists {
            %qacc{$k} = [ %qacc{$k}, $v ];
        }
        else {
            %qacc{$k} = $v;
        }
    }

    # %!query-form must be immutable
    for %qacc.kv -> $k, $v {
        %qacc{$k} = $v.List
            if $v ~~ Array;
    }
    %!query-form := %qacc.Map;

    self!query-from-query-form(:$delimiter);

    %!query-form;
}

method query_form { $.query-form }

=begin pod

=head1 NAME

URI — Uniform Resource Identifiers (absolute and relative)

=head1 SYNOPSIS

    use URI;
    my $u = URI.new('http://her.com/foo/bar?tag=woow#bla');

    my $scheme = $u.scheme;
    my $authority = $u.authority;
    my $host = $u.host;
    my $port = $u.port;
    my $path = $u.path;
    my $query = $u.query;
    my $frag = $u.frag; # or $u.fragment;
    my $tag = $u.query-form<tag>; # should be woow

    # something p5 URI without grammar could not easily do !
    my $host_in_grammar =
        $u.grammar.parse_result<URI-reference><URI><hier-part><authority><host>;
    if ($host_in_grammar<reg-name>) {
        say 'Host looks like registered domain name - approved!';
    }
    else {
        say 'Sorry we do not take ip address hosts at this time.';
        say 'Please use registered domain name!';
    }

    {
    # require whole string matches URI and throw exception otherwise ..
    my $u_v = URI.new('http://?#?#');
    CATCH { when X::URI::Invalid { ... } }
    }

    my $u_pfx = URI.new('http://example.com } function(var mm){',
        match-prefix => True);
=end pod


# vim:ft=perl6
