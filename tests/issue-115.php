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
/*                | column 18 */
                  ->bar()   // ###php-mode-test### ((indent 18))
                  ->baz()]; // ###php-mode-test### ((indent 18))

$y = array("y" => $this->foo()
/*                     | column 23 */
                       ->bar()   // ###php-mode-test### ((indent 23))
                       ->baz()); // ###php-mode-test### ((indent 23))

// Test the combination of arglist indentation and cascaded calls
$x = ['x' => M_PI,
/*    | column 6 */
      'x' => 123,          // ###php-mode-test### ((indent 6))
      'x' => $y->method()  // ###php-mode-test### ((indent 6))
/*             | column 15 */
               ->method()] // ###php-mode-test### ((indent 15))