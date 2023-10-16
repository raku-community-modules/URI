use v6.d;

# Copied, with minor translation to Perl6, from the escape.t file
# in the CPAN URI distribution

use Test;
plan 11;

use URI::Escape;

ok(1,'We use URI::Escape and we are still alive');

is uri-escape('abcDEF?$%@h&m'), 'abcDEF%3F%24%25%40h%26m',
    'basic ascii escape test';

is uri-escape(no-utf8 => True, '|abcå'), '%7Cabc%E5', 'basic latin-1 escape test';
is uri_escape(no_utf8 => True, '|abcå'), '%7Cabc%E5', 'snake case basic latin-1 escape test';
is uri-escape('|abcå'), '%7Cabc%C3%A5', 'basic utf-8 escape test';
is uri-escape('|a  b cå'),'%7Ca%20%20b%20c%C3%A5',
    'basic utf-8 escape test w spaces';

is uri-unescape(no-utf8 => True, '%7C%25abc%E5'), '|%abcå', 'basic latin-1 unescape test';
is uri_unescape(no_utf8 => True, '%7C%25abc%E5'), '|%abcå', 'snake case basic latin-1 unescape test';
is uri-unescape('%7C%25abc%C3%A5'), '|%abcå', 'basic utf8 unescape test';

is uri-unescape("%40A%42", "CDE", "F%47++H"), ['@AB', 'CDE', 'FG  H'],
    'unescape list';
ok not uri-escape(Str).defined, 'undef returns undef';

# vim:ft=perl6
