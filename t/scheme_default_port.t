use v6;

# Some tests for URI scheme_default_port public/export function. 

use Test;
plan 3;

use URI::DefaultPort;
ok(1,'We use URI::Escape and we are still alive');

is(scheme_default_port('https'), 443, 'normal default port');
dies_ok( { scheme_default_port('??//&') }, 'reject invalid URI scheme');

# vim:ft=perl6
