<?php

$object = new DateTime();

$object->something()
    ->something(); // ###php-mode-test### ((indent 4))

var_dump(
    $object->something() // ###php-mode-test### ((indent 4))
        ->something(), // ###php-mode-test### ((indent 8))
); // ###php-mode-test### ((indent 0))

$arr = [
    $object->something() // ###php-mode-test### ((indent 4))
        /* comment */ ->something() // ###php-mode-test### ((indent 8))
        ->something(), // ###php-mode-test### ((indent 8))
]; // ###php-mode-test### ((indent 0))
