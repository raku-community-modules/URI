use v6;
use Test;
use URI;
use IETF::RFC_Grammar::URI;

cmp-ok 'http', '~~', URI::Scheme, 'URI::Scheme matches http';
cmp-ok '', '~~', URI::Scheme, 'URI::Scheme matches ""';
cmp-ok '-asdf', '!~~', URI::Scheme, 'URI::Scheme refused to match -asdf';

my $u = URI.new('http://example.com:80/about/us?foo#bar');

$u.scheme = 'https';
$u.port(443);
is "$u", 'https://example.com:443/about/us?foo#bar', 'modified URI looks good';

$u.port(Nil);
is "$u", 'https://example.com/about/us?foo#bar', 'URI with no port looks good';

$u._port = 567;
is "$u", 'https://example.com:567/about/us?foo#bar', 'setting _port works too';

$u._port = Nil;
is "$u", 'https://example.com/about/us?foo#bar', 'clearing with _port works too';

$u.authority("larry@perl6.org:1234");
is "$u", 'https://larry@perl6.org:1234/about/us?foo#bar', 'setting authority works';
is $u.userinfo, 'larry', 'setting authority set userinfo';
is $u.host, 'perl6.org', 'setting authority set host';
is $u.port, 1234, 'setting authority set port';

$u.authority("example.com");
is "$u", 'https://example.com/about/us?foo#bar', 'setting authority with only a host works';
is $u.userinfo, '', 'setting authority clears userinfo';
is $u.host, 'example.com', 'setting authority set host';
ok !$u._port.defined, 'setting authority clears port';

$u.path("/");
is "$u", 'https://example.com/?foo#bar', 'setting path works';
is-deeply $u.segments, ('',), '/ has empty segments';

$u.path("");
is "$u", 'https://example.com?foo#bar', 'empty path works';
is-deeply $u.segments, ('',), '"" has empty segments';

$u.path("/careers/are/good");
is "$u", 'https://example.com/careers/are/good?foo#bar', 'empty path works';
is-deeply $u.segments, ('careers', 'are', 'good'), '/careers/are/good has three segments';

$u.query('foo=cod&foo=trout');
is "$u", 'https://example.com/careers/are/good?foo=cod&foo=trout#bar', 'setting query works';
is $u.query-form<foo>[0], 'cod', 'query form foo.0 is good';
is $u.query-form<foo>[1], 'trout', 'query form foo.1 is good';

$u.query-form('bar' => 'lion', 'bar' => 'tiger');
is "$u", 'https://example.com/careers/are/good?bar=lion&bar=tiger#bar', 'setting query-form works';
is $u.query-form<bar>[0], 'lion', 'query form bar.0 is good';
is $u.query-form<bar>[1], 'tiger', 'query form bar.1 is good';


done-testing;
