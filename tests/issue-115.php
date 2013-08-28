<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/115
 *
 * This tests for aligning method calls within arrays, similar to what
 * we do in the test for issue 19.  The method calls below should all
 * align vertically along the '->' characters.
 *
 */

$x = ["x" => $this->foo()
                  ->bar()   // ###php-mode-test### ((indent c-lineup-cascaded-calls))
                  ->baz()]; // ###php-mode-test### ((indent c-lineup-cascaded-calls))

$y = array("y" => $this->foo()
                       ->bar()   // ###php-mode-test### ((indent c-lineup-cascaded-calls))
                       ->baz()); // ###php-mode-test### ((indent c-lineup-cascaded-calls))
