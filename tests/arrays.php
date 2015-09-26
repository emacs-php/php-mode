<?php

// Array should be treated like a normal keyword
$test = array();

$test = function(array $test = array()) {
};

// Unless it's a cast, then it should behave like (int)
$test = (array)$test;
