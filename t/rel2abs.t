use v6;
use Test;
use URI;

plan 10;

sub rel2abs($r, $b) {
    my $rel = URI.new($r);
    my $base = URI.new($b);
    $rel.rel2abs($base).Str;
}

nok URI.new("foo:ccc").is-relative;
ok URI.new("/ccc").is-relative;

is rel2abs("ccc", "/aaa/bbb"), "/aaa/bbb/ccc";
is rel2abs("ccc", "/aaa/bbb/"), "/aaa/bbb/ccc";
is rel2abs("ccc", "/aaa/bbb//"), "/aaa/bbb/ccc";
is rel2abs("/ccc", "/aaa/bbb"), "/ccc";
is rel2abs("/ccc", "foo:/aaa/bbb"), "foo:/ccc";
is rel2abs("/ccc", "foo://aaa/bbb"), "foo://aaa/ccc";
is rel2abs("foo:ccc", "/aaa/bbb"), "foo:ccc";
is rel2abs("foo:ccc", "foo:/aaa/bbb"), "foo:ccc";
