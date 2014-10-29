<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/199
 *
 * Test issue in syntactic analysis where function inside if failed
 */

if (!function_exists('boolval'))
{
    function boolval($v)
    {
        return (bool)$v; // ###php-mode-test### ((indent 8))
    }                    // ###php-mode-test### ((indent 4))
}                        // ###php-mode-test### ((indent 0))