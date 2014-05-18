<?php

/**
 * GitHub Issue:    https://github.com/ejmr/php-mode/issues/53
 *
 * Test if switching from Drupal coding style to other coding styles updates
 * the whitespace effects.
 *
 * This file contains arbitrary PHP code with some whitespace added to the end
 * of some lines.
 *
 */

class Foo {

    private $bar;

    public function __construct($bar) {
        $this->bar = $bar;
    }

    public function getBar() {
        // here is the whitespace
        return $this->bar;    
    }

}

$foo = new Foo('bar');
$bar = $foo->getBar();
