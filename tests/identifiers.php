<?php

$object = new UnqualifiedClassName;
$object = new \FullyQualifiedClassName;
$object = new SpaceName\NamespacedClassName;
$object = new NOTACONSTANT\ClassName;

// php-mode must understand that \ is not part of any symbol, just
// identifiers
$var\syntaxerror;

// When calling a method, the class name should be highlighted with
// the constant face. Just like c++-mode "NS::Class::method()"
ClassName::method();
\SpaceName\ClassName::method();