use v6.d;
use Test;

plan 50;

use URI;
use URI::Escape;

ok(1,'We use URI et. al and we are still alive');

my $u = URI.new('http://example.com:80/about/us?foo#bar');

is($u.scheme, 'http', 'scheme');

is($u.host, 'example.com', 'host');
is($u.port, '80', 'port');
is($u.path, '/about/us', 'path');
is($u.query, 'foo', 'query');
is($u.frag, 'bar', 'frag');
is($u.segments.join('/'), '/about/us', 'segments');
is($u.segments[0], '', 'first chunk');
is($u.segments[1], 'about', 'second chunk');
is($u.segments[2], 'us', 'third chunk');

is( ~$u, 'http://example.com:80/about/us?foo#bar',
    'Complete path stringification');

# Test with unicode characters
$u = URI.new('http://test.de/ö');
is($u.path, '/%C3%B6', 'path with unicode');
say $u;

$u = URI.new('http://127.0.0.1:1234/echo2/☃');
is($u, 'http://127.0.0.1:1234/echo2/%E2%98%83', "Punycoding is correct");

# allow uri as named argument too
$u = URI.new(uri => 'https://eXAMplE.COM');

is($u.scheme, 'https', 'scheme');
is($u.host, 'example.com', 'host');
is( "$u", 'https://example.com',
    'https://eXAMplE.COM stringifies to https://example.com');
is($u.port, 443, 'default https port');
ok(! $u._port.defined, 'no specified port');

$u.parse('/foo/bar/baz');
is($u.segments.join('/'), '/foo/bar/baz', 'segments from absolute path');
$u.parse('foo/bar/baz');
is($u.segments.join('/'), 'foo/bar/baz', 'segments from relative path');

is($u.segments[0], 'foo', 'first segment');
is($u.segments[1], 'bar', 'second segment');
is($u.segments[*-1], 'baz', 'last segment');

# actual uri parameter not required
$u = URI.new;
$u.parse('http://foo.com');
is-deeply($u.segments, ('',), ".segments return ('',) for empty path");
is $u.segments.join('/'), '', '.segments joined to empty string';;
is($u.port, 80, 'default http port');

# test URI parsing with <> or "" and spaces
$u.parse("<http://foo.com> ");
is("$u", 'http://foo.com', '<> removed from str');

$u.parse(' "http://foo.com"');
is("$u", 'http://foo.com', '"" removed from str');
my $host_in_grammar =
    $u.grammar.parse_result<URI-reference><URI><hier-part><authority><host>;
ok(! $host_in_grammar<IPv4address>.defined, 'grammar detected host not ip'
);
is($host_in_grammar<reg-name>, 'foo.com', 'grammar detected registered domain style');

$u.parse('http://10.0.0.1');
is($u.host, '10.0.0.1', 'numeric host');
$host_in_grammar =
    $u.grammar.parse_result<URI-reference><URI><hier-part><authority><host>;

is($host_in_grammar<IPv4address>, '10.0.0.1', 'grammar detected ipv4');
ok(! $host_in_grammar<reg-name>.defined, 'grammar detected no registered domain style');

$u.parse('http://example.com:80/about?foo=cod&bell=bob#bar');
is($u.query, 'foo=cod&bell=bob', 'query with form params');
is($u.query<foo>, 'cod', 'query param foo');
is($u.query<foo>, 'cod', 'snake case query param foo');
is($u.query<bell>, 'bob', 'query param bell');

$u.parse('http://example.com:80/about?foo=cod&foo=trout#bar');
is($u.query<foo>[0], 'cod', 'query param foo - el 1');
is($u.query<foo>[1], 'trout', 'query param foo - el 2');
is($u.frag, 'bar', 'test query and frag capture');

$u.parse('http://example.com:80/about?foo=cod&foo=trout');
is($u.query<foo>[1], 'trout', 'query param foo - el 2 without frag');

$u.parse('about/perl6uri?foo=cod&foo=trout#bar');
is($u.query<foo>[1], 'trout', 'query param foo - el 2 relative path');

$u.parse('about/perl6uri?foo=cod&foo=trout');
is($u.query<foo>[1], 'trout', 'query param foo - el 2 relative path without frag');

throws-like {URI.new('http:://?#?#')}, X::URI::Invalid,
    'Bad URI raises exception x:URI::Invalid';

my $uri-w-js = 'http://example.com } function(var mm){ alert(mm) }';
throws-like {URI.new($uri-w-js)}, X::URI::Invalid,
    'URI followed by trailing javascript raises exception';
my $uri-pfx = URI.new($uri-w-js, :match-prefix);
is(~$uri-pfx, 'http://example.com', 'Pulled of prefix URI');
nok(URI.new('foo://bar.com').port, '.port without default value lives');
lives-ok { URI.new('/foo/bar').port }, '.port on relative URI lives';

my Str $user-info = URI.new(
    'http://user%2Cn:deprecatedpwd@obscurity.com:8080/ucantcme'
).userinfo,
is( uri-unescape($user-info), 'user,n:deprecatedpwd',
    'extracted userinfo correctly');

