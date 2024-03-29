use v6.d;
use Test;
use URI;

plan 8;

sub dir(Str $u --> URI:D) {
    URI.new($u).directory;
}

is dir("ccc"), "./";
is dir("/ccc"), "/";
is dir("/ccc/"), "/ccc/";
is dir("ccc/"), "ccc/";
is dir("path/doc.html"), "path/";
is dir("https://mysite.com"), "https://mysite.com/";
is dir("https://mysite.com/"), "https://mysite.com/";
is dir("https://mysite.com/path/doc.html"), "https://mysite.com/path/";
