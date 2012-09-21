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
 * Alignment should occur on the '->' characters regardless of the
 * scope in which the method calls appear.
 */

$object = new StdClass();
$object->call()
       ->something();

class Whatever
{
    public function __construct()
    {
        $object = new StdClass();
        $object->call()
               ->something();
    }

    public function something()
    {
        $object = new StdClass();
        $object->call()
               ->something();
    }
}

$closure = function() {
    $object = new StdClass();
    $object->call()
           ->something();
};

function something()
{
    $object = new StdClass();
    $object->call()
           ->something();
}
