use v6;
use Test;
use URI;

my $q = URI.new('foo:a/b/c');
isa-ok $q.path, URI::Path;
ok $q.path.defined;

with $q.path {
    is $_, "a/b/c";
    is .path, "a/b/c";
    is-deeply .segments, $('a', 'b', 'c');
}

# with auth, path starting with / is required
throws-like {
    $q.host('localhost');
}, X::URI::Invalid;

$q.path('/a/b/c');
lives-ok {
    $q.host('localhost');
};
is "$q", 'foo://localhost/a/b/c';

$q.host('');
is "$q", 'foo:///a/b/c';

$q.authority(Nil);
is "$q", 'foo:/a/b/c';

# without auth first segment after / must be non-zero length
throws-like {
    $q.path('//a/b/c');
}, X::URI::Invalid;

done-testing;
