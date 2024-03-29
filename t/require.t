#!/usr/bin/env raku

use v6.d;

use Test;
plan 2;

use lib $*PROGRAM.parent.child('lib').Str;

require TestURIRequire;

my $port;

lives-ok { $port = ::("TestURIRequire").test('http://example.com'); }, "can use URI in a module that is itself required rather than used";
is $port, 80, "and got the right thing back";


done-testing;
