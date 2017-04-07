<?php

// Array should be treated like a normal keyword
$test = array();

$test = function($test = array()) {
};

// Array should be treated as a type instead of a keyword in the following situations:
// - cast, should look like `(int)`
// - type hint, should look like `int $var`
// - return type, should look like `: int`
$test = (array)$test;

$test = function(array $test): array {
};
