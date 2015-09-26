<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/66
 *
 * Namespace examples from
 * http://www.php.net/manual/en/language.namespaces.rationale.php
 *
 * The different namespace tests need/should be in different files because
 * this is what PHP requires.
 */

namespace my\name; // see "Defining Namespaces" section

class MyClass {}
function myfunction() {}
const MYCONST = 1;

$a = new MyClass;
$c = new \my\name\MyClass; // see "Global Space" section

$a = strlen('hi'); // see "Using namespaces: fallback to global
                   // function/constant" section

$d = namespace\MYCONST; // see "namespace operator and __NAMESPACE__
                        // constant" section
$d = __NAMESPACE__ . '\MYCONST';
echo constant($d); // see "Namespaces and dynamic language features" section
