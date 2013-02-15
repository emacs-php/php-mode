<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/18
 *
 * If we run indent-region on the code below then it should appear
 * like so:
 *
 * $a = 'github';
 *
 * if ($a === 'github') {
 *     header('Location: http://github.com');
 * }
 *
 * This tests for a bug where '//' inside of a string would cause an
 * indentation error.
 */

$a = 'github';

if ($a === 'github') {
    header('Location: http://github.com'); // ###php-mode-test### ((indent c-basic-offset))
}