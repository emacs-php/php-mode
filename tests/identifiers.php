<?php

$object = new UnqualifiedClassName;
$object = new \FullyQualifiedClassName;
$object = new SpaceName\NamespacedClassName;
$object = new NOTACONSTANT\ClassName;

// When calling a method, the class name should be highlighted with
// the constant face. Just like c++-mode "NS::Class::method()"
ClassName::method();
\SpaceName\ClassName::method();
\My_Class::method();

__halt_compiler();

// php-mode must understand that \ is not part of any symbol, just
// identifiers
$var\syntaxerror;
