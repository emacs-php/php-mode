<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/227
 *
 * Indentation of multi-line strings
 */

function my_func() {
    return "a really long string with = inside " .
"some more text";   // ###php-mode-test### ((indent 49))
}
