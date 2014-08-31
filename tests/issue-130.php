<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/130
 *
 * Test indentation arrays
 */

$some_array = array(
    'foo'  => 'bar', // ###php-mode-test### ((indent 4))
    'spam' => 'ham', // ###php-mode-test### ((indent 4))
);

$levels = array(E_USER_NOTICE  => 'notice',
/*              | column 16 */
                E_USER_WARNING => 'warning', // ###php-mode-test### ((indent 16))
                E_USER_ERROR   => 'error');  // ###php-mode-test### ((indent 16))

array( 'a' => 1,
/*     | column 7 */
       'b' => 2 ); // ###php-mode-test### ((indent 7))
