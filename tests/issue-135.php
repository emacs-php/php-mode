<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/135
 *
 * Test indentation of method calls
 */

// Test indentation cascaded calls
$variable->method()   // ###php-mode-test### ((indent 0))
/*       | column 9 */
         ->chained(); // ###php-mode-test### ((indent 9))

// Test indentation of statement continuations (statement-cont)
$variable
    ->method()  // ###php-mode-test### ((indent 4))
    ->method(); // ###php-mode-test### ((indent 4))

ClassName::method()
    ->chained(); // ###php-mode-test### ((indent 4))

// Test same behaviour inside a block
function foo() {
    $variable->method()   // ###php-mode-test### ((indent 4))
/*           | column 13 */
             ->chained(); // ###php-mode-test### ((indent 13))

    ClassName::method()
        ->chained();      // ###php-mode-test### ((indent 8))
}

// Test same behaviour inside an arglist
foo($db->baz()
/*     | column 7 */
       ->quux()); // ###php-mode-test### ((indent 7))

foo(
    $variable->method()   // ###php-mode-test### ((indent 4))
/*           | column 13 */
             ->chained(), // ###php-mode-test### ((indent 13))


    // Note that below indentation is different than when not inside
    // an arglist. When inside an arglist, cc-mode doesn't provide the
    // information that we're in a statement-cont as well.
    //
    // Future improvement: create a function like c-lineup-cascaded-calls
    // to have this indented like statement-cont.
    ClassName::method()
    ->chained()       // ###php-mode-test### ((indent 4))
);

// Test same behaviour inside method
class Test {
    public function test() {
        $variable->method()   // ###php-mode-test### ((indent 8))
/*               | column 17 */
                 ->chained(); // ###php-mode-test### ((indent 17))

        ClassName::method()   // ###php-mode-test### ((indent 8))
            ->chained();      // ###php-mode-test### ((indent 12))
    }
}
