<?php

// Test c-lang-defconst c-constant-kwds
true === TRUE;
false === FALSE;
null === NULL;

// Test built-in font-lock-keywords
IS_CONSTANT;
__IS_CONSTANT__;
IS_CONSTANT99;
no_constant;
no_CONSTANT;

// Everything called statically should be highlighted with the constant face
// (like c++-mode)
ClassName::$test;

// Class name resolution is a special case
stdClass::class;
SomeClass::classIdentifier();

__halt_compiler();

// Invalid constant name
2FOO;
