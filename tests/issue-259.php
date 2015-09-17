/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/259
 *
 * This code shows how writing a closure which imports external
 * variables with the `use` keyword causes php-mode to incorrectly
 * indent one level deeper when the closure is used as the actual
 * parameter of another function.
 *
 * Expected indentation:
 *
 * $foo(function($par1, $par2) use ($ext) {
 *     return $par1 + $par2 + $ext;
 * });
 */

$foo(function($par1, $par2) use ($ext) {
    return $par1 + $par2 + $ext;
});

array_map(function($par1, $par2) use ($ext) {
    return $par1 + $par2 + $ext;
}, $foo);


class Foo
{
    protected function foo()
    {
        $foo(function($par1, $par2) use ($ext) {
            return $par1 + $par2 + $ext;
        });

        array_map(function($par1, $par2) use ($ext) {
            return $par1 + $par2 + $ext;
        }, $foo);
    }
}

function foo() {
    $foo(function($par1, $par2) use ($ext) {
        return $par1 + $par2 + $ext;
    });

    array_map(function($par1, $par2) use ($ext) {
        return $par1 + $par2 + $ext;
    }, $foo);
}
