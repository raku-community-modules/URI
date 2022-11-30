use Test;
use URI;

plan 2;

ok URI.new(uri=>''), "Empty URI is OK";
ok URI.new(uri=>'http:'), "Scheme-only URI is OK";
