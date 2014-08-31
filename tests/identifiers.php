<?php

$object = new UnqualifiedClassName;
$object = new \FullyQualifiedClassName;
$object = new SpaceName\NamespacedClassName;
$object = new NOTACONSTANT\ClassName;

// php-mode must understand that \ is not part of any symbol, just
// identifiers
$var\syntaxerror;