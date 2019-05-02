<?php

/**
 * GitHub-Issue: https://github.com/emacs-php/php-mode/issues/506
 *
 * @see https://wiki.php.net/rfc/arrow_functions_v2
 */

$fn1 = fn($x) => $x + $y;

$fn2 = [
    fn($x) => $x + $y
];

$z = 1;
$fn3 = fn($x) => fn($y) => $x * $y + $z;
