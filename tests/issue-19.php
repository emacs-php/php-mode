<?php

/**
 * Github Issue:    https://github.com/ejmr/php-mode/issues/19
 *
 * If we run indent-region on the code below then it should appear
 * like this with the '->' operators aligned:
 *
 * $object = new SomeClass();
 * $object->method()
 *        ->another();
 *
 */

$object = new SomeClass();
$object->method()
       ->another();