<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/66
 *
 * A large battery of tests to exercise the new cc-mode-based indentation and
 * fontification code.
 *
 * This is mainly just code copy-pasted from the PHP manual.
 */

// http://www.php.net/manual/en/language.exceptions.php
function inverse($x) {
    if (!$x) {
        throw new Exception('Division by zero.');
    }
    else return 1/$x;
}

try {
    echo inverse(5) . "\n";
    echo inverse(0) . "\n";
} catch (Exception $e) {
    echo 'Caught exception: ',  $e->getMessage(), "\n";
}

// http://www.php.net/manual/en/functions.anonymous.php

$greet = function($name)
{
    printf("Hello %s\r\n", $name);
};

$greet('World');
$greet('PHP');