<?php

PHP_VERSION_ID === 80000
    ? 'foo'
    : 'bar';

$a = [
    'key' => PHP_VERSION_ID === 80000
        ? 'foo'
        : 'bar',
    true &&
        false,
    false
        || true,
    'value1'
    ,
    'value2'
    ,
];

var_dump(
    PHP_VERSION_ID === 80000
        ? 'foo'
        : 'bar',
    true && // ###php-mode-test### ((indent 4))
        false,
    false // ###php-mode-test### ((indent 4))
        || true, // ###php-mode-test### ((indent 8))
    // ###php-mode-test### ((indent 4))
    1 // ###php-mode-test### ((indent 4))
        + 2 // ###php-mode-test### ((indent 8))
        / 3, // ###php-mode-test### ((indent 8))
    'value1' // ###php-mode-test### ((indent 4))
    , // ###php-mode-test### ((indent 4))
    'value2' // ###php-mode-test### ((indent 4))
    , // ###php-mode-test### ((indent 4))
);
