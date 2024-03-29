use v6.d;
use Test;
use URI :split-query;

constant $qs = 'foo=1&bar=2&foo=3&baz';

subtest {
    my @q = split-query('');
    is-deeply @q, [];
}, 'split-query of empty string';

subtest {
    my @q = split-query($qs);
    is-deeply @q, [foo => '1', bar => '2', foo => '3', baz => True];
}, 'split-query to array';

subtest {
    my @q = split-query($qs, :!hash-format);
    is-deeply @q, [foo => '1', bar => '2', foo => '3', baz => True];
}, 'split-query to array (boolean :!hash-format)';

subtest {
    my %q = split-query($qs, :hash-format);
    is-deeply %q, %(foo => ['1', '3'], bar => ['2'], baz => [ True ]);
}, 'split-query to lists hash (boolean :hash-format)';

subtest {
    my %q = split-query($qs, hash-format => URI::Query::Mixed);
    is-deeply %q, %(foo => ['1', '3'], bar => '2', baz => True);
}, 'split-query to mixed hash';

subtest {
    my %q = split-query($qs, hash-format => URI::Query::Singles);
    is-deeply %q, %(foo => '3', bar => '2', baz => True);
}, 'split-query to singles hash';

subtest {
    my %q = split-query($qs, hash-format => URI::Query::Lists);
    is-deeply %q, %(foo => ['1', '3'], bar => ['2'], baz => [ True ]);
}, 'split-query to lists hash';

subtest {
    dies-ok {
        URI::Query.new(query => $qs, query-form => [ a => 1 ]);
    };
}, 'bad .new';

subtest {
    my $q = URI::Query.new($qs);

    is "$q", 'foo=1&bar=2&foo=3&baz', 'query is cached';

    is $q<baz>, True, 'query baz is True';
    $q<baz> = True;
    is "$q", 'foo=1&bar=2&foo=3&baz=', 'query is recomputed';
}, 'query caching';

subtest {
    my $q = URI::Query.new($qs);

    is $q[0].key, 'foo';
    is $q[0].value, '1';
    is $q[1].key, 'bar';
    is $q[1].value, '2';
    is $q[2].key, 'foo';
    is $q[2].value, '3';
    is $q[3].key, 'baz';
    is $q[3].value, True;

    is $q[$_]:exists, True for ^3;
    is $q[4]:exists, False;

    $q[0] = 'qux' => 4;

    is "$q", "qux=4&bar=2&foo=3&baz=";

    my $bar = $q[1]:delete;
    is $bar.key, 'bar';
    is $bar.value, '2';

    is "$q", "qux=4&foo=3&baz=";
}, 'array-ish bits';

subtest {
    my $q = URI::Query.new($qs);

    is-deeply $q<foo>, $('1', '3');
    is-deeply $q<bar>, $('2',);
    is-deeply $q<baz>, $(True,);

    throws-like {
        $q<foo>[0] = '0';
    }, X::Assignment::RO;

    is $q{$_}:exists, True for <foo bar baz>;
    is $q<qux>:exists, False;

    my $foo = $q<foo>:delete;
    is $foo, $('1', '3');
    is "$q", 'bar=2&baz=';

    $q<foo> = '4';
    is "$q", 'bar=2&baz=&foo=4';

    $q<foo> = '5', '6';
    is "$q", 'bar=2&baz=&foo=5&foo=6';
}, 'hash-ish lists bits';

subtest {
    my $q = URI::Query.new($qs, :hash-format(URI::Query::Mixed));

    is-deeply $q<foo>, $('1', '3');
    is-deeply $q<bar>, '2';
    is-deeply $q<baz>, True;

    throws-like {
        $q<foo>[0] = '0';
    }, X::Assignment::RO;

    is $q{$_}:exists, True for <foo bar baz>;
    is $q<qux>:exists, False;

    my $foo = $q<foo>:delete;
    is $foo, $('1', '3');
    is "$q", 'bar=2&baz=';

    $q<foo> = '4';
    is "$q", 'bar=2&baz=&foo=4';

    $q<foo> = '5', '6';
    is "$q", 'bar=2&baz=&foo=5&foo=6';
}, 'hash-ish mixed bits';

subtest {
    my $q = URI::Query.new($qs, :hash-format(URI::Query::Singles));

    is-deeply $q<foo>, '3';
    is-deeply $q<bar>, '2';
    is-deeply $q<baz>, True;

    is $q{$_}:exists, True for <foo bar baz>;
    is $q<qux>:exists, False;

    my $foo = $q<foo>:delete;
    is $foo, '3';
    is "$q", 'bar=2&baz=';

    $q<foo> = '4';
    is "$q", 'bar=2&baz=&foo=4';

    $q<foo> = '5', '6';
    is "$q", 'bar=2&baz=&foo=5%206';
}, 'hash-ish singles bits';

subtest {
    my $q = URI::Query.new($qs);

    is-deeply $q.keys, <foo bar foo baz>;
    is-deeply $q.values, $('1', '2', '3', True);
    is-deeply $q.kv, $('foo', '1', 'bar', '2', 'foo', '3', 'baz', True);
    is-deeply [|$q.pairs], [foo => '1', bar => '2', foo => '3', baz => True];

    is $q.elems, 4;
    is $q.end, 3;

    is +$q, 4;

    is "$q", "foo=1&bar=2&foo=3&baz";
    is $q.gist, "$q";
    is $q.Str, $q.gist;
}, 'iterator methods';

subtest {
    my $q = URI::Query.new($qs);

    my $baz = $q.pop;
    is $baz.key, 'baz';
    is $baz.value, True;
    is "$q", "foo=1&bar=2&foo=3";
}, 'pop';

subtest {
    my $q = URI::Query.new($qs);

    $q.push: 'foo' => 4, 'foo' => 5;
    is "$q", 'foo=1&bar=2&foo=3&baz=&foo=4&foo=5';
}, 'push';

subtest {
    my $q = URI::Query.new($qs);

    my @more-foo = 'foo' => 4, 'foo' => 5;
    $q.append: @more-foo;
    is "$q", 'foo=1&bar=2&foo=3&baz=&foo=4&foo=5';
}, 'append';

subtest {
    my $q = URI::Query.new($qs);

    my $foo = $q.shift;
    is $foo.key, 'foo';
    is $foo.value, '1';
    is "$q", "bar=2&foo=3&baz=";
}, 'shift';

subtest {
    my $q = URI::Query.new($qs);

    $q.unshift: 'foo' => 4, 'foo' => 5;
    is "$q", 'foo=4&foo=5&foo=1&bar=2&foo=3&baz=';
}, 'unshift';

subtest {
    my $q = URI::Query.new($qs);

    my @more-foo = 'foo' => 4, 'foo' => 5;
    $q.prepend: @more-foo;
    is "$q", 'foo=4&foo=5&foo=1&bar=2&foo=3&baz=';
}, 'prepend';

subtest {
    my $q = URI::Query.new($qs);

    my @more-foo = 'foo' => 4, 'foo' => 5;
    $q.splice: 1, 2, @more-foo;
    is "$q", 'foo=1&foo=4&foo=5&baz=';
}, 'splice';

subtest {
    my $q = URI::Query.new($qs);

    is ?$q, True;
    $q.query('');
    is ?$q, False;
}, 'Bool';

subtest {
    my $q = URI::Query.new;

    is "$q", '';
    is $q.query , '';

    $q.query($qs);
    is "$q", $qs;
    is $q.query, $qs;

    $q<baz> = True;
    is "$q", "foo=1&bar=2&foo=3&baz=";
    is $q.query, "foo=1&bar=2&foo=3&baz=";
}, 'query';

subtest {
    my $q = URI::Query.new;

    is-deeply [|$q.query-form], [];

    $q.query-form('foo' => 1, 'bar' => 2, 'foo' => 3, 'baz' => True);
    is "$q", "foo=1&bar=2&foo=3&baz=";
}, 'query-form';

done-testing;
