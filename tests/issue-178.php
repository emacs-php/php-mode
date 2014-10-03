<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/178
 *
 * Test highlighting of "as" keyword
 */

use Test as Alias;

foreach ($values as $key => $value) {}

class Test {
    use TestTrait { test as protected; }
}
