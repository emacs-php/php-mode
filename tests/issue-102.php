<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/102
 *
 * Every line of code in the form of
 *
 *     $broken = true;
 *
 * in the code below should have no indentation.  This test makes sure
 * that files containing magic constants, e.g. __FILE__, do not
 * incorrectly affect the indentation of following lines.
 *
 */

$x = some_function(__FILE__) . '';
$broken = true;

some_function(__FILE__) . '';
$broken = true;

some_function(__FILE__) + 1;
$broken = true;
