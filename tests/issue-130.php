<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/130
 *
 * This script helps us test array indentation for the PEAR coding
 * style as described here:
 *
 *     http://pear.php.net/manual/en/standards.arrays.php
 *
 * What is most important is that the array keys must line up.
 * 
 */

$some_array = array(
    'foo'  => 'bar', // ###php-mode-test### ((indent php-lineup-arglist-intro))
    'spam' => 'ham', // ###php-mode-test### ((indent php-lineup-arglist-intro))
);

$levels = array(E_USER_NOTICE  => 'notice',
                E_USER_WARNING => 'warning', // ###php-mode-test### ((indent c-lineup-arglist-intro-after-paren))
                E_USER_ERROR   => 'error');  // ###php-mode-test### ((indent c-lineup-arglist-intro-after-paren))
