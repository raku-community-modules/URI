use v6;

use Test;
use URI;

plan 2;

# This tests that we aren't double encoding path parts
# The test string has an encoded ':'

my Str $test-uri = 'http://localhost:5984/_users/org.couchdb.user%3Ayotjfgnr';

my $uri;
lives-ok { $uri = URI.new($test-uri) }, "create the URI object";

is $uri.Str, $test-uri, "and the URI stringifies to the same as the source";

