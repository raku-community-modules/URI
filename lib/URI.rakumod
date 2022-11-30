use URI::Path;
use URI::Query;

unit class URI:auth<github:raku-community-modules>:ver<v0.3.5>;

use IETF::RFC_Grammar;
use IETF::RFC_Grammar::URI;
use URI::Escape;
need URI::DefaultPort;

constant &split-query is export(:split-query) = &URI::Query::split-query;

class X::URI::Invalid is Exception {
    has $.source;
    method message { "Could not parse URI: $!source" }
}

class X::URI::Path::Invalid is X::URI::Invalid {
    has $.path;
    has $.bad-segment;
    method message {
        qq[Could not parse path "$!path" as part of URI: $.source]
        ~ ($!bad-segment ?? qq[ with bad segment "$!bad-segment"] !! '')
    }
}

our subset Scheme of Str
    where /^ [
           ''
        || <IETF::RFC_Grammar::URI::scheme>
    ] $/;
our subset Userinfo of Str
    where /^ [ <IETF::RFC_Grammar::URI::userinfo> ] $/;
our subset Host of Str
    where /^ [ <IETF::RFC_Grammar::URI::host> ] $/;
our subset Port of UInt;

class Authority {
    has Userinfo:D $.userinfo is default('') is rw = '';
    has Host:D $.host is default('') is rw = '';
    has Port $.port is rw;

    multi method new(Authority:U: Match:D $auth) {
        my Str $userinfo = do with $auth<userinfo> { .Str } else { '' }
        my Str $host     = "$auth<host>".lc;
        my UInt $port    = do with $auth<port> { .Int } else { UInt }

        self.new(:$userinfo, :$host, :$port);
    }

    multi method gist(Authority:D: --> Str ) {
        my $authority = "$!userinfo@" if $!userinfo;
        $authority ~= $!host;
        $authority ~= ":$!port" if $!port;
        $authority;
    }

    multi method Str(Authority:D: --> Str ) { $.gist }
}



our subset Fragment of Str
    where /^ <IETF::RFC_Grammar::URI::fragment> $/;

has Str $.query-form-delimiter;
has $.grammar;
has Bool $.match-prefix = False;
has Path $.path = Path.new;
has Scheme $.scheme = '';
has Authority $.authority is rw;
has Query $.query = Query.new('');
has Fragment $.fragment is rw = '';
has %!query-form; # cache query-form

method parse(URI:D: Str() $str, Bool :$match-prefix = $!match-prefix) {

    # clear string before parsing
    my Str $c_str = $str;
    $c_str .= subst(/^ \s* ['<' | '"'] /, '');
    $c_str .= subst(/ ['>' | '"'] \s* $/, '');

    $!scheme    = '';
    $!authority = Nil;
    $!path      = Path.new;
    $!query.query('');
    $!fragment  = '';

    $!match-prefix = $match-prefix;
    if $!match-prefix {
        $!grammar.subparse($c_str);
    }
    else {
        $!grammar.parse($c_str);
    }

    # hacky but for the time being an improvement
    if (not $!grammar.parse-result) {
        X::URI::Invalid.new(source => $str).throw
    }

    my $comp_container = $!grammar.parse-result<URI-reference><URI> ||
        $!grammar.parse-result<URI-reference><relative-ref>;

    $!scheme = .lc with $comp_container<scheme>;
    $!query.query(.Str) with $comp_container<query>;
    $!fragment = .lc with $comp_container<fragment>;
    $comp_container = $comp_container<hier-part> || $comp_container<relative-part>;

    with $comp_container<authority> -> $auth {
        $!authority = Authority.new($auth);
    }

    $!path = Path.new($comp_container, :$!scheme);
}

# deprecated old call for parse
method init ($str) is DEPRECATED("parse") {
    $.parse($str);
}

# new can pass alternate grammars some day ...
submethod BUILD(:$match-prefix) {
    $!match-prefix = ? $match-prefix;
    $!grammar = IETF::RFC_Grammar.new('rfc3986');
}

multi method new(URI:U: $uri, Bool :$match-prefix) {
    my $obj = self.bless(:$match-prefix);

    $obj.parse($uri) if $uri.defined;
    $obj;
}

multi method new(URI:U: :$uri, Bool :$match-prefix) {
    self.new($uri, :$match-prefix);
}

multi method COERCE(Str:D $uri) { self.new($uri) }

method has-scheme(URI:D: --> Bool:D ) {
    $!scheme.chars > 0;
}
method has-authority(URI:D: --> Bool:D ) {
    $!authority.defined;
}

my regex path-authority {
    [ $<path-abempty> = <IETF::RFC_Grammar::URI::path-abempty> ]
}
my regex path-scheme {
    [   $<path-absolute> = <IETF::RFC_Grammar::URI::path-absolute>
    |   $<path-rootless> = <IETF::RFC_Grammar::URI::path-rootless>
    |   $<path-empty>    = <IETF::RFC_Grammar::URI::path-empty>
    ]
}
my regex path-plain {
    [   $<path-absolute> = <IETF::RFC_Grammar::URI::path-absolute>
    |   $<path-noscheme> = <IETF::RFC_Grammar::URI::path-noscheme>
    |   $<path-empty>    = <IETF::RFC_Grammar::URI::path-empty>
    ]
}

method !check-path(
    :$has-authority = $.has-authority,
    :$has-scheme    = $.has-scheme,
    :$path          = $.path,
    :$source        = $.gist,
) {
    given X::URI::Path::Invalid.new(:$path, :$source) -> \ex {
        if $has-authority {
            ex.throw unless $path ~~ /^ <path-authority> $/;
            return $<path-authority>;
        }
        elsif $has-scheme {
            ex.throw unless $path ~~ /^ <path-scheme> $/;
            return $<path-scheme>;
        }
        else {
            ex.throw unless $path ~~ /^ <path-plain> $/;
            return $<path-plain>;
        }
    }
}

multi method scheme(URI:D: --> Scheme:D ) { $!scheme }
multi method scheme(URI:D: Str() $scheme --> Scheme:D ) {
    self!check-path(:has-scheme, :source(self!gister(:$scheme)));
    $!scheme = $scheme;
}

multi method userinfo(URI:D: --> Userinfo ) {
    return .userinfo with $!authority;
    '';
}

multi method userinfo(URI:D: Str() $userinfo --> Userinfo ) {
    with $!authority {
        .userinfo = $userinfo;
    }
    elsif $userinfo {
        self!check-path(:has-authority, :source(self!gister(:authority("$userinfo@"))));
        $!authority .= new(:$userinfo);
        $!authority.userinfo;
    }
}

multi method host(URI:D: --> Host ) {
    return .host with $!authority;
    '';
}

multi method host(URI:D: Str() $host --> Host ) {
    with $!authority {
        .host = $host;
    }
    elsif $host {
        self!check-path(:has-authority, :source(self!gister(:authority($host))));
        $!authority .= new(:$host);
        $!authority.host;
    }
}

method default-port(URI:D: --> Port ) {
    URI::DefaultPort.scheme-port($.scheme)
}
method default_port(URI:D: --> Port ) { $.default-port } # artifact form

multi method _port(URI:D: --> Port ) {
    return .port with $!authority;
    Nil;
}

multi method _port(URI:D: Nil --> Port ) {
    .port = Nil with $!authority;
    Nil;
}

multi method _port(URI:D: Int() $port --> Port ) {
    with $!authority {
        .port = $port;
    }
    else {
        self!check-path(:has-authority, :source(self!gister(:authority(":$port"))));
        $!authority .= new(:$port);
        $!authority.port;
    }
}


multi method port(URI:D: --> Port ) { $._port // $.default-port }

multi method port(URI:D: |c --> Port ) { self._port(|c) // $.default-port }

proto method authority(|c) { * }

multi method authority(URI:D: Str() $authority --> Authority:D ) {
    my $gist;
    without $!authority {
        self!check-path(:has-authority,
            source => $gist //= self!gister(:$authority),
        );
    }

    $!grammar.parse($authority, rule => 'authority');
    if not $!grammar.parse-result {
        X::URI::Invalid.new(
            source => $gist // self!gister(:$authority),
        ).throw;
    }

    $!authority = Authority.new($!grammar.parse-result);
}

multi method authority(URI:D: Nil --> Authority:U ) {
    with $!authority {
        self!check-path(:!has-authority,
            source => self!gister(authority => ''),
        );
    }

    $!authority = Nil;
}

multi method authority(URI:D: --> Authority ) is rw {
    $!authority;
}

multi method path(URI:D: --> Path:D ) { $!path }

method !make-path(Str $path --> Path:D ) {
    if $path {
        my $comp = self!check-path(
            :$path,
            source => self!gister(:$path),
        );

        Path.new($comp);
    }
    else {
        Path.new;
    }
}

multi method path(URI:D: Str() $path --> Path:D ) {
    $!path = self!make-path($path);
}

multi method segments(URI:D: --> List:D ) { $!path.segments }

multi method segments(URI:D: @segments where *.elems > 0 --> List:D ) {
    my $path = @segments.join('/');
    given self!gister(:$path) -> $source {
        for @segments -> $bad-segment {
            X::URI::Path::Invalid.new(
                :$source,
                :$path,
                :$bad-segment,
            ).throw if $bad-segment.contains("/");
        }

        self!check-path(:$path, :$source);
    }

    $!path = Path.new(
        path     => $path,
        segments => @segments.map(*.Str),
    );
    $!path.segments;
}

multi method segments(URI:D: $first-segment, *@remaining-segments --> List:D ) {
    my @segments = $first-segment, |@remaining-segments;
    self.segments(@segments);
}

# The absolute and relative methods are artifacts carried over from an old
# version of the p6 module.  The perl 5 module does not provide such
# functionality.  The Ruby equivalent just checks for the presence or
# absence of a scheme.  The URI rfc does identify absolute URIs and
# absolute URI paths and these methods somewhat confused the two.  Their
# functionality at the URI level is no longer seen as needed and is
# being removed.

method absolute is DEPRECATED {
    return Bool.new;
}

method relative is DEPRECATED {
    return Bool.new;
}

method is-absolute { so ($!scheme || $.host) }
method is-relative { ! $.is-absolute }

method directory(URI:D:) {
    my Str $dir = $!path.Str;
    $dir .= subst(/<- [/]>*$/, '');
    $dir ||= self.is-absolute ?? '/' !! './';
    my Path $path = self!make-path($dir);
    self.clone: :$path;
}

method rel2abs(URI:D: URI:D $base --> URI:D) {
    if $.is-absolute {
        self;
    }
    else {
        my URI::Path $path = $!path.rel2abs($base);
        $base.clone: :$path;
    }
}

multi method query(URI:D: --> URI::Query:D ) { $!query }

multi method query(URI:D: Str() $new --> URI::Query:D ) {
    $!query.query($new);
    $!query
}

multi method query(URI:D: *@new, *%bad --> URI::Query:D ) {
    warn 'The query was passed values as named arguments which are ignored. Did you mean to make sure your pairs were quoted or passed in a list instead?' if %bad;

    $!query.query-form(@new);
    $!query
}


proto method query-form(|c) { * }

multi method query-form(URI:D: |c) is DEPRECATED("method query") {
    $!query.query-form(|c)
}

method query_form() is DEPRECATED("method query")  {
    $.query
}

method path-query(URI:D: --> Str:D ) {
    $.query ?? "$.path?$.query" !! "$.path"
}

method path_query { $.path-query } #artifact form

multi method fragment(URI:D: --> Fragment ) { return-rw $!fragment }
multi method fragment(URI:D: Str() $new --> Fragment ) { $!fragment = $new }

method frag(URI:D:) { $.fragment }

method !gister(
    :$scheme = $.scheme,
    :$authority = $.authority,
    :$path = $.path,
    :$query = $.query,
    :$fragment = $.fragment,
) {
    my Str $s;
    $s ~= "$scheme:"     if $scheme;
    $s ~= "//$authority" if $authority;
    $s ~= $path;
    $s ~= "?$query"      if $query;
    $s ~= "#$fragment"   if $fragment;
    $s;
}

multi method gist(URI:D: --> Str:D ) { self!gister }
multi method Str(URI:D: --> Str:D ) { $.gist }

# chunks now strongly deprecated
# it's segments in p5 URI and segment is part of rfc so no more chunks soon!
method chunks() is DEPRECATED("method segments") {
    $!path.segments;
}

multi method query-form() { $!query }

multi method query-form(|c) { $!query.query-form(|c) }


=begin pod

=head1 NAME

URI — Uniform Resource Identifiers (absolute and relative)

=head1 SYNOPSIS

    use URI;
    my $u = URI.new('http://example.com/foo/bar?tag=woow#bla');

    # Look at the parts
    say $u.scheme;     #> http
    say $u.authority;  #> example.com
    say $u.host;       #> example.com
    say $u.port;       #> 80
    say $u.path;       #> /foo/bar
    say $u.query;      #> tag=woow
    say $u.fragment;   #> bla

    # Modify the parts
    $u.scheme('https');
    $u.authority('example.com:8443');
    $u.path('/bar/foo');
    $u.query('x=1&y=2'); # OR
    $u.fragment('cool');
    say "$u"; #> https://example.com:8443/bar/foo?x=1&y=2#cool

    # Authority is an object, but may be undefined
    with $u.authority {
        say .userinfo; # none set, no output
        say .host;     #> example.com
        say .port;     #> 8443

        # It is mutable too
        .userinfo('bob');
        say $u.authority; #> bob@example.com:8443
    }

    # Path is an object, always defined, but immutable
    with $u.path {
        say .path;          #> /bar/foo
        .say for .segments; #> bar\nfoo\n
    }

    # Query is an object, always defined, mutable
    with $u.query {
        say .query;              #> x=1&y=2
        .query('foo=1&foo=2');
        say .query-form<foo>[0]; #> 1
        say .query-form<foo>[1]; #> 2

        .query-form.push: 'bar' => 'ok';
        say .query;              #> foo=1&foo=2&bar=ok

        .query('');
        .query-form<abc> = 123;
        say .query;              #> abc=123
    }

=head1 DESCRIPTION

A URI object represents a parsed string that abides by the grammar defined in RFC 3986 for Universal Resource Identifiers. The object then works to enforce the rules defined in the RFC for any modifications made to it.

As of this writing, The URI class is scheme agnostic. It will verify that URI is correct, in general, but not correct for a given scheme. For example, C<http:foo> would be considered a valid URI even though that is not a valid URI according to the rules specific to the C<http> scheme.

This class uses "heavy accessors". From the SYNOPSIS, you may have noted that assignment to accessors is not used. This is because nearly all accessors in this class do some extra work of parsing, validating, and cross-referencing with other fields to guarantee that the URI is correct before making a change.

    my $u = URI.new;
    $u.path = '/foo/bar'; # ERROR: «Cannot modify an immutable URI::Path»
    $u.path('/foo/bar');  # WORKS!

This mutator pattern is meant to reflect the internal complexity.

=head2 SCHEME, AUTHORITY, AND PATH

In RFC 3986 URIs, the scheme, the authority, and the path are related. This should not matter most of the time, but to avoid problems when setting these three, it is safest to set them in this order:

    my $u = URI.new;
    $u.path('');
    $u.scheme($my-scheme);
    $u.authority($my-host);
    $u.path($my-path);

This is because an empty path is permitted in any case, but the format of the path is limited whether an authority and scheme are set.

With an authority set (i.e., the URI either starts with "//" or with "scheme://"), a non-empty path must start with a "/" and may start with empty path segments, e.g. "scheme://foo//" is valid.

When there's no authority set, but a scheme is used (e.g., it starts with "scheme:", but not "scheme://"), a non-empty path may either start with a "/" or not, but must contain one or more other characters in the first segment of the path. Thus, the following code will fail:

    my $u = URI.new('scheme:');
    $u.path('//'); # ERROR: «Could not parse path "//" as part of URI: scheme://»

When there's no authority and no scheme used, a non-empty path may start with a "/" or not, but must contain one or more other characters in the first segment.

These rules are enforced whenever setting or clearing the scheme, authority, or path. If the resulting URI object would be invalid a C<X::URI::Path::Invalid> exception will be thrown.

=head2 QUERY

The C<query> method of this class returns a C<URI::Query> object.This is a special object that is C<Positional>, C<Associative>, and C<Iterable>. That is, it can be bound to an array variable, a hash variable, and iterated using a loop. If stringified, it will return a URI-encoded query. If used as an array, will act like somewhat like an array of C<Pair>s. If used as a hash, will provide a map from query keys to query values.

The native internal representation is a list of C<Pair>s as this is the best match for how queries are defined. Because of this, there's a problem when using a query as a hash: duplicate pairs are valid. To handle this, the C<URI::Query> object always returns a list of values, sorted in the order they appear for each key.

For example:

    my $u = URI.new('?foo=1&bar=2&foo=3');
    say $u.query<foo>[0]; #> 1
    say $u.query<foo>[1]; #> 3
    say $u.query<bar>[0]; #> 2

Older versions of the URI module handled this differently, using a mixed value representation. In order to gain some backwards compatibility, this is still supported by setting the C<hash-format>:

    # Continues from previous
    $u.query.hash-format = URI::Query::Mixed;
    say $u.query<foo>[0]; #> 1
    say $u.query<foo>[1]; #> 3

    # The bar value is NOT a list now
    say $u.query<bar>;    #> 2

    # Return to the default mode
    $u.query.hash-format = URI::Query::Lists;

However, the list version is safer and may likely work with most existing code that worked with mixed before.

Another mode is provided to force single values, which is often how applications treat these out of convenience. In that case, only the last value will be kept:

    # Continues from previous
    $u.query.hash-format = URI::Query::Singles;

    # These are never lists now
    say $u.query<foo>; #> 3
    say $u.query<bar>; #> 2

The C<URI::Query::Lists> mode is default and recommended mode.

=head2 GRAMMAR

This class will keep a copy of the result of parsing the URI string for you. If you are interested in precise details of the parse tree, you get them using the C<grammar> method:

    my $host-in-grammar =
        $u.grammar.parse-result<URI-reference><URI><hier-part><authority><host>;
    if $host-in-grammar<reg-name> {
        say 'Host looks like registered domain name - approved!';
    }
    else {
        say 'Sorry we do not take ip address hosts at this time.';
        say 'Please use registered domain name!';
    }

The C<IETF::RFC_Grammar::URI> grammar sticks close to the BNF defined in RFC 3986.

=head2 PARTIAL MATCHING

Many times a URI you are interested in is embedded within another string. This class will allow you to parse URIs out of a larger string, so long as the URI is at the start. This is done by setting the C<:match-prefix> option during construction or when calling C<parse>:

    {
        # require whole string matches URI and throw exception otherwise ..
        my $u_v = URI.new('http://?#?#');
        CATCH { when X::URI::Invalid { ... } }
    }

    my $u_pfx = URI.new('http://example.com } function(var mm){', :match-prefix);

=head1 METHODS

=head2 method new

    multi method new(URI:U: Str() $uri, Bool :$match-prefix) returns URI:D
    multi method new(URI:U: Str() :$uri, Bool :$match-prefix) returns URI:D

These construct a new C<URI> object and return it. The given C<$uri> value is converted to a string and then parsed using the C<parse> method.

If C<:match-prefix> is set, then the grammar will be allowed to match a prefix of the given input string rather than requiring a total match. The C<:match-prefix> given also becomes the default value for any figure calls to C<parse>.

Throws a C<X::URI::Invalid> exception if the URI cannot be parsed.

=head2 method parse

    method parse(URI:D: Str() $str, Bool :$match-prefix = $.match-prefix)

This method allows an existing URI object to be reused to parse another string. This parses the given string and replaces all internal state of the object with values for the new parse.

The given C<:match-prefix> flag becomes the new default when set.

Throws a C<X::URI::Invalid> exception if the URI cannot be parsed.

=head2 method grammar

    method grammar(URI:D:) returns IETF::RFC_Grammar:D

Returns the object used to parse and store the state of the parse.

=head2 method match-prefix

    method match-prefix(URI:D:) returns Bool:D

Returns True if the most recent call to C<parse> (or C<new>) allowed a prefix match or False if a total match was required. This is the default value of any future call to C<parse>.

=head2 method scheme

    multi method scheme(URI:D:) returns URI::Scheme:D
    multi method scheme(URI:D: Str() $scheme) returns URI::Scheme:D

Returns the scheme part of the URI. This is a string that must match the C<URI::Scheme> subset type.

The second form allows the scheme to be replaced with a new scheme. It is legal for the scheme to be set to an empty string, which has the effect of making the URI scheme-less.

This will throw an C<X::URI::Path::Invalid> exception if adding or removing the scheme will make the URI invalid. See SCHEME, AUTHORITY, AND PATH section for additional details.

=head2 method authority

    multi method authority(URI:D:) returns URI::Authority
    multi method authority(URI:D: Nil) returns URI::Authority:U
    multi method authority(URI:D: Str() $new) returns URI::Authority:D

Returns the C<URI::Authority> for the current URI object. This may be an undefined type object if no authority has been set or found during parse.

When passed a string, the string will have the authority parsed out of it and a new authority object will be used to store the parsed information. An empty authority is valid.

When passed C<Nil>, the authority will be cleared.

When passing an argument, this will throw an C<X::URI::Path::Invalid> exception if setting or clearing the authority on the URI will make the URI invalid. See SCHEME, AUTHORITY, AND PATH section for details.

The authority is made up of three components: userinfo, host, and port. Additional methods are provided for accessing each of those parts separately.

=head2 method userinfo

    multi method userinfo(URI:D:) returns URI::Userinfo:D
    multi method userinfo(URI:D: Str() $new) returns URI::Userinfo:D

The userinfo is an optional component of the URI authority. This method returns the current userinfo or an empty string.

Setting this method will cause a C<URI::Authority> to be constructed and C<authority> to be set, if it is not already defined. This may result in a C<X::URI::Path::Invalid> exception being thrown if adding an authority will make the path invalid.

=head2 method host

    multi method host(URI:D:) returns URI::Host:D
    multi method host(URI:D: Str() $new) returns URI::Host:D

The host is a component of the URI authority. This method returns the current host or an empty string.

Setting this method will cause a C<URI::Authority> to be constructed and C<authority> to be set, if it is not already defined. This may result in a C<X::URI::Path::Invalid> exception being thrown if adding an authority will make the path invalid.

=head2 method default-port

    method default-port(URI:D:) returns URI::Port

This method applies the C<scheme-port> method of C<URI::DefaultPort> to the scheme set on this object. Basically, a shortcut for:

    my $u = URI.new("...");
    my $port = URI::DefaultPort.scheme-port($u.scheme);

It returns the usual port for the named scheme.

=head2 method _port

    multi method _port(URI:D:) returns URI::Port
    multi method _port(URI:D: Nil) returns URI::Port:U
    multi method _port(URI:D: Int() $new) returns URI::Port:D

When an authority is set on the URI, this gets or sets the authority's port. This differs from C<port>, which returns either the port set or the C<default-port>. This method returns just the port.

If no authority is set or no port is set, this returns an undefined value (i.e., an C<Int> type object).

Setting this method will cause a C<URI::Authority> to be constructed and C<authority> to be set, if it is not already defined. This may result in a C<X::URI::Path::Invalid> exception being thrown if adding an authority will make the path invalid.

=head2 method port

    multi method port(URI:D:) returns URI::Port
    multi method port(URI:D: Nil) returns URI::Port
    multi method port(URI:D: Int() $new) returns URI::Port

When retrieving a value from the object, this method returns either the port set on the authority or the default port for the current URI scheme. It may return an undefined value (i.e., an C<Int> type object) if there is no port set and no known default port for the current scheme or no scheme set.

When setting a value on the object, this method sets the port on the authority.

Setting this method will cause a C<URI::Authority> to be constructed and C<authority> to be set, if it is not already defined. This may result in a C<X::URI::Path::Invalid> exception being thrown if adding an authority will make the path invalid.

=head2 method path

    multi method path(URI:D:) returns URI::Path:D
    multi method path(URI:D: Str() $path) returns URI::Path:D

Path is the main required element of a URI, but may be an empty string. This method returns the current setting for the path as a C<URI::Path> object. It also allows setting a new path, which will construct a new C<URI::Path> object.

This method will throw a C<X::URI::Path::Invalid> exception if the path is not valid for the current scheme and authority settings.

=head2 method segments

    multi method segments(URI:D:) returns List:D
    multi method segments(URI:D: @segments where *.elems > 0) returns List:D
    multi method segments(URI:D: $first-segment, *@remaining-segments) returns List:D

Returns the path segments (i.e., the parts between slashes). This is a shortcut for:

    my $u = URI.new("...");
    my @s = $u.path.segments;

The number of segments is equal to the number of slashes in the original path plus one. The segments are constructed so that joining the segments by a slash (/) will give you the original path.

You may pass a list in to replace the segments with a new value. This will throw a C<X::URI::Path::Invalid> exception if any of the segments contain a slash or if the set path will cause the URI to become invalid.

Be sure when setting segments to include an initial empty string if you want the path to start with a slash.

=head2 method query

    multi method query(URI:D:) returns URI::Query:D
    multi method query(URI:D: Str() $new) returns URI::Query:D
    multi method query(URI:D: *@new) returns URI::Query:D

Accesses or updates the query associated with the URI. This is returns as a C<URI::Query> object. This will always be defined, but may be empty.

When passed a string, the string will be parsed using C<split-query> and the query object will be updated. When passed a list, the list of C<Pair>s given will be used to setup a new query that way.

When the list form is used, any named parameters passed will be ignored (they will not be used to fill the query) and a warning will be issued.

=head2 method path-query

    method path-query(URI:D:) returns Str:D

Returns the path and query as a string joined by a question mark ("?"). It will return just the path as a string if the query is empty.

=head2 method fragment

    multi method fragment(URI:D:) returns URI::Fragment:D
    multi method fragment(URI:D: Str() $new) returns URI::Fragment:D

Returns the URI fragment, which is always defined, but may be an empty string. If passed a value, it will set the fragment.

=head2 method gist

    multi method gist(URI:D:) returns Str:D

Reconstructs the URI from the components and returns it as a string.

=head2 method Str

    multi method Str(URI:D:) returns Str:D

Reconstructs the URI from the components and returns it as a string.

=head1 SUBROUTINES

=head2 sub split-query

    sub split-query(Str() $query, :$hash-format = URI::Query::None)

This routine will slice and dice a query string, which is useful when parsing URIs and may also be useful when parsing POST entities that are encoded as C<application/x-www-form-urlencoded>.

This routine is exported with the C<:split-query> tag or can be used with the full namespace, C<URI::split-query>.

With just the required string, this routine will parse that string and return a list of C<Pair>s mapping each query form key to its respective value. The order is preserved. This is used by C<URI::Query> during construction.

For example:

    my @qf = URI::split-query("foo=1&bar%20=2&foo=3");
    dd @qf; #> Array @qf = [:foo("1"), "bar " => ("2"), :foo("3")]

Notice that this will perform a C<uri-escape> operation of keys and values in the process so the values you receive have had the URI encoded characters decoded.

You can retrieve the query string as a hash instead by passing the C<:hash-format> option. This works exactly as it does for C<URI::Query>.

The options for C<:hash-format> include:

=head3 URI::Query::Lists

Every key is mapped to a list of one or more values. From the example input, a structure like the following is returned:

    my %qf = URI::split-query("foo=1&bar%20=2&foo=3",
        hash-format => URI::Query::Lists,
    );
    dd %qf; #> Hash %qf = {"bar " => (["2"]), :foo(["1", "3"])}

This is also the default if you just pass C<:hash-format> as a boolean option.

=head3 URI::Query::Mixed

Every key is mapped to either a list of two or more values or directly to a single value, like the following:

    my %qf = URI::split-query("foo=1&bar%20=2&foo=3",
        hash-format => URI::Query::Mixed,
    );
    dd %qf; #> Hash %qf = {"bar " => ("2"), :foo(["1", "3"])}

=head3 URI::Query::Singles

Every key is mapped to a single value, which will be the last value encountered in the input, like this:

    my %qf = URI::split-query("foo=1&bar%20=2&foo=3",
        hash-format => URI::Query::Mixed,
    );
    dd %qf; #> Hash %qf = {"bar " => ("2"), :foo("3")}

=head1 HELPER SUBSETS

=head2 URI::Scheme

This is a subset of C<Str> that only accepts valid schemes.

=head2 URI::Userinfo

This is a subset of C<Str> that only accepts valid userinfo.

=head2 URI::Host

This is a subset of C<Str> that only accepts valid hosts.

=head2 URI::Port

This is a subset of C<UInt>.

=head2 URI::Query::ValidQuery

This is a subset of C<Str> that only accepts valid query strings.

=head2 URI::Fragment

This is a subset of C<Str> that only accepts valid URI fragments.

=head1 HELPER CLASSES

=head2 URI::Authority

The C<authority> method of a URI constructs or returns this object.

It is recommended that you do not costruct a C<URI::Authority> object directly, but let the methods of C<URI> handle construction.

=head3 method userinfo

    method userinfo(URI::Authority:D:) is rw returns URI::Userinfo:D

This is a simple setter/getter for the userinfo on the authority. It must be defined, but may be the empty string. If not empty, it must be valid for the userinfo component of a URI.

=head3 method host

    method host(URI::Authority:D:) is rw returns URI::Host:D

This is a simple setter/getter for the host part of the URI authority. It must be defined, but may be the empty string. If not empty, must be a valid host value, which may be an IP address or registered name.

=head3 method port

    method port(URI::Authority:D:) is rw returns URI::Port

This is a simple setter/getter for the port part of the URI authority. It may be set to an undefined value if no explicit port is set in the authority. If defined, it must an unsigned integer.

=head3 method gist

    multi method gist(URI::Authority:D:) returns Str:D

Returns the string representation of the URI authority. For example,

    my $u = URI.new("http://steve@example.com:8008");

    # say calls .gist
    say $u.authority; #> "steve@example.com:8080";

=head3 method Str

    multi method gist(URI::Authority:D:) returns Str:D

Stringifies identical to C<gist>.

=head2 URI::Path

This class is used to represent URI path components.

It is recommended that you do not construct a C<URI::Path> object directly, but rely on the C<path> setter in C<URI> instead. These objects are immutable. Please use methods on C<URI> to make changes.

=head3 method path

    method path(URI::Path:D:) returns Str:D

Returns the string representation of the path.

=head3 method segments

    method segments(URI::Path:D:) returns List:D

Returns a list representation of the path segments. In a URI, the path segments are the strings between slashes ("/").

=head3 method gist

    method gist(URI::Path:D:) returns Str:D

Returns the C<path>.

=head3 method Str

    method Str(URI::Path:D:) returns Str:D

Returns the C<path>.

=head2 URI::Query

This class is used to represent a URI query component. This class may be safely constructed and used independently of the URI object.

It behaves as both a positional and associative object and is iterable. Internally, it is stored as an C<Array> of C<Pair>s. You must not treat this object purely as you would an C<Array> or purely as you would a C<Hash> as some methods will not work the way you expect.

The performance of the associative methods is not guaranteed and is probably going to be relatively slow. This implementation values simplicity and accuracy of representation to CPU performance. If you need something that is better for CPU performance, you should investigate the use of another library, such as C<Hash::MultiValue> or sub-class to provide a higher performance implementation of C<URI::Query>'s associative methods.

=head3 method new

    multi method new(Str() $query, URI::Query::HashFormat :$hash-format = URI::Query::Lists) returns URI::Query:D
    multi method new(:$query, URI::Query::HashFormat :$hash-format = URI::Query::Lists) returns URI::Query:D

Constructs a new C<URI::Query> from the given string, which may be empty.

Unlike C<split-query>, which permits boolean values for the C<hash-format> option, C<URI::Query> requires a C<URI::Query::HashFormat> value and will reject a boolean (because the boolean does not make sense in this case).

=head3 enum HashFormat

This enumeration provides the values that may be set on C<hash-format> on a C<URI::Query>. Possible values are as follows.

=head4 URI::Query::List

This is the default and recommended value. When set in C<hash-format>, each key in the hash representation will point to a list of one or more values.

This is recommended as it is the most accurate representation to what is actually possible in a query string. The values within each key will be sorted in the same order as they appear in the query.

=head4 URI::Query::Mixed

When set in C<hash-format>, each key in the hash representation may be mapped to either a list of two or more values, or to the value itself if there is only one.

This is not recommended because it means that setting a key to an iterable value will be treated as multiple key-value pairs when it comes time to render the query as a string. This could have unintended consequences.

=head4 URI::Query::Singles

When set in C<hash-format>, each key in the hash representation will be mapped directly to a single value. If there are multiple values that have been set in the query, then only the last will be visible through the hash representation.

This is not recommended because it may hide certain values, but may be useful for simple applications that treat the query string as having unique values. Just note that the trade-off is that your application may be confused when multiple values are present.

=head4 URI::Query::None

This value should not be used, but will be treated the same as C<URI::Query::List> if used here. It is provided for use with C<split-query> only.

=head3 method hash-format

    method hash-format(URI::Query:D) is rw returns URI::Query::HashFormat

This is a simple setter/getter for setting the way in which associative lookups are performed. See C<enum URI::Query::HashFormat> for a description of each mode.

=head3 method query

    method query(Query:D:) returns URI::Query::ValidQuery:D
    method query(Query:D: Str() $new) returns URI::Query::ValidQuery:D

This method returns the string representation of the URI query component. When passed a string, it will replace the query with the new value and will use C<split-query> to parse that query into an array of C<Pair>s.

The primary representation of the queryo object is the value returned by C<query-form>. The C<query> is cached to keep the value stored in it when this method is called to set it or when C<URI::Query> is constructed from a string. Any modification to the internal array of pairs, though, will clear this cache. It  will be generated the next time the C<query> method is called and that string will be cached.

=head3 method query-form

    method query-form(Query:D:) returns Array:D
    method query-form(Query:D: *@new, *%new) returns Array:D

This method returns the array of C<Pair>s that store the internal representation of the URI query component. When passed an array of C<Pair>s, it will replace the current value with that array.

A quick note about the way pairs are passed as parameters to a method, you most likely want to avoid passing values as named parameters. If values are passed using unquoted strings, they will be treated as named parameters, which is most likely what you want:

    my $q = URI::Query.new;

    # Prefer this
    $q.query-form('foo' => 1, 'bar' => 2, 'foo' => 3);

    # Avoid these
    $q.query-form(foo => 1, bar => 2, foo => 3);
    $q.query-form(:foo(1), :bar(2), :foo(3));

The latter two will result in the first "foo" pair being lost. Named parameters assume unique names and the latter "foo" key will effectively override the former.

That said, the method will allow hashes to be passed in, if that is your preference.

=head3 method of

    method of()

Always returns C<Pair>.

=head3 method iterator

    method iterator(Query:D:) returns Iterator:D

Returns the iterator on the internal array of C<Pair>s.

=head3 method postcircumflex:<[ ]>

    method postcircumflex:<[ ]> returns Pair

This permits positional access to each C<Pair> stored internally. You may use this to get a C<Pair>, set a C<Pair>, test for existence, or delete.

=head3 method postcircumflex:<{ }>

    method postcircumflex:<{ }>

This permits associative access to the values stored internally by key. What is returned here when fetching values depends on the setting in C<hash-format>, a list of one or more values or C<Nil>, by default. You can use this for getting, setting, existence testing, or deletion.

=head3 method keys

    method keys(Query:D:) returns Seq:D

This method returns all the keys of the query in order.

=head3 method values

    method values(Query:D:) returns Seq:D

This method returns all the values of the query in order.

=head3 method kv

    method kv(Query:D:) returns Seq:D

This method returns a sequence alternating the keys and values of the query in order.

=head3 method pairs

    method kv(Query:D:) returns Seq:D

This method returns a copy of the internal representation of the query string array.

=head3 method pop

    method pop(Query:D:) returns Pair

This method removes the last Pair from the array of pairs and returns it.

=head3 method push

    method push(Query:D: *@new)

This method adds the given pairs to the end of the array of pairs in the query using push semantics.

=head3 method append

    method append(Query:D: *@new)

This method adds the given pairs to the end of the array of pairs in the query using append semantics.

=head3 method shift

    method shift(Query:D:) returns Pair

This method removes the first Pair from the array of pairs and returns it.

=head3 method unshift

    method unshift(Query:D: *@new)

This method adds the given pairs to the front of the array of pairs in the query using unshift semantics.

=head3 method prepend

    method prepend(Query:D: *@new)

This method adds the given pairs to the front of the array of pairs in the query using prepend semantics.

=head3 method splice

    method splice(Query:D: $start, $elems?, *@replacement)

This method removes a C<$elems> number of pairs from the array of pairs in the query starting at index C<$start>. It then inserts the pairs in C<@replacement> into that part of the array (if any are given).

=head3 method elems

    method elems(Query:D:) returns Int:D

Returns the number of pairs stored in the query.

=head3 method end

    method end(Query:D:) returns Int:D

Returns the index of the last pair stored in the query.

=head3 method Bool

    method Bool(Query:D:) returns Bool:D

Returns C<True> if the at least one pair is stored in the query or C<False> otherwise.

=head3 method Int

    method Int(Query:D:) returns Int:D

Returns C<elems>.

=head3 method Numeric

    method Numeric(Query:D:) returns Int:D

Returns C<elems>.

=head3 method gist

    method gist(Query:D:) returns Str:D

Returns the C<query>.

=head3 method Str

    method Str(Query:D:) returns Str:D

Returns the C<query>.

=head1 EXCEPTIONS

=head2 X::URI::Invalid

This exception is thrown in many places where the URI is being parsed or manipulated. If the string being parsed is not a valid URI or if certain manipulations of the URI object would cause it to become an invalid URI, this exception may be used.

It provides a C<source> accessor, which returns the string that was determined to be invalid.

=head2 X::URI::Path::Invalid

In some cases where an attempt is made to set C<path> to an invalid value, this exception is thrown.

The C<source> field will name the invalid URI. Strictly speaking, the URI might be valid, but will not parse the same way as given. To make it clear that this is the case, the C<path> field names the invalid path part.

In cases where the segments have been modified in an invalid way, the first invalid segment will be set in C<bad-segment>.

=head1 AUTHORS

Contributors to this module include:

=item Ronald Schmidt (ronaldxs)

=item Moritz Lentz (moritz)

=item Nick Logan (ugexe)

=item Tobias Leich (FROGGS)

=item Jonathan Stowe (jonathanstowe)

=item Justin DeVuyst (jdv)

=item Solomon Foster (colomon)

=item Roman Baumer (rba)

=item Zoffix Znet (zoffixznet)

=item Ahmad M. Zawawi (azawawi)

=item Gabor Szabo (szabgab)

=item Samantha McVey (samcv)

=item Pawel Pabian (bbkr)

=item Rob Hoelz (hoelzro)

=item radiak

=item Paul Cochrane (paultcochrane)

=item Steve Mynott (stmuk)

=item timo

=item David Warring (dwarring)

=item Sterling Hanenkamp (zostay)

=head1 COPYRIGHT & LICENSE

Copyright 2017 Ronald Schmidt.

This software is licensed under the same license as Perl 6 itself.

=end pod


# vim:ft=perl6
