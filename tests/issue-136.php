<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/136
 *
 * The code below contains strings with interpolated variables.  PHP
 * Mode must use font-lock-variable-name-face to highlight all of
 * those variables.  The page
 *
 *     http://www.php.net/manual/en/language.types.string.php#language.types.string.parsing
 *
 * describes all of the variable syntaxes which PHP Mode must
 * recognize and highlight appropriately.
 * 
 */

$name = "Eric";
$user = (object) [ "name" => "Eric" ];

ob_start();

echo "My name is $name";
echo "My name is {$name}";
echo "My name is {$user->name}";

ob_end_clean();
