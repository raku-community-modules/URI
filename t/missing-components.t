use Test;
use URI;
use IETF::RFC_Grammar::URI;

plan 5;

ok URI.new(uri=>''), "Empty URI is OK";
ok URI.new(uri=>'http:'), "Scheme-only URI is OK";

nok 'foo' ~~ IETF::RFC_Grammar::URI.new().TOP-non-empty, "TOP-non-empty works";
ok '#foo' ~~ IETF::RFC_Grammar::URI.new().URI-reference, "Relative URIs work";
ok '' ~~ IETF::RFC_Grammar::URI.new().path-abempty, "path-abempty work";
