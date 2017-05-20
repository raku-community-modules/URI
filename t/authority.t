use v6;
use Test;
use URI;

my $u = URI.new('foo://me@localhost:4321');

isa-ok $u.authority, URI::Authority;
ok $u.authority.defined;

with $u.authority {
    is .userinfo, 'me';
    is .host, 'localhost';
    is .port, 4321;

    is "$_", 'me@localhost:4321';
    is .gist, "$_";

    .userinfo = 'steve';
    is "$_", 'steve@localhost:4321';

    .host = 'xyz';
    is "$_", 'steve@xyz:4321';

    .port = 1234;
    is "$_", 'steve@xyz:1234';

    .userinfo = '';
    is "$_", 'xyz:1234';

    .userinfo = 'steve';
    is "$_", 'steve@xyz:1234';

    .userinfo = Nil;
    is "$_", 'xyz:1234';

    .port = Nil;
    is "$_", 'xyz';
}

$u.authority('me@localhost:4321');
is "$u", 'foo://me@localhost:4321';

$u.authority('');
is "$u", 'foo:';

throws-like {
    $u.userinfo('me');
}, X::URI::Authority::Invalid;

throws-like {
    $u.port(4321);
}, X::URI::Authority::Invalid;

$u.host('localhost');
is "$u", 'foo://localhost';

$u.userinfo('me');
$u.port(4321);
is "$u", 'foo://me@localhost:4321';

done-testing;
